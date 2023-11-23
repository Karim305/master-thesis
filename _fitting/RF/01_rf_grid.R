## RF Hyperparameter Tuning via "manual" grid-search (only for RF2 model)


# Load data & create model formulas----------------------------------------
code_path <- "XXXX"
source(paste0(code_path, "/01_setup.R"))
# install.packages("regr", repos="http://R-forge.R-project.org")
library(regr)
library(glmnet)
library(sfsmisc)
# install.packages("plgraphics", repos="http://R-forge.R-project.org")
library(plgraphics)
library(splines)
library(dplyr)
library(ranger) # faster
library(caret)


# Create and set model and plot directories
# model_name <- "lm_test"
model_name <- "rf_reg2_new"

model_path <- paste0(model_dir,"_",model_name,"/")
fig_path <- paste0(fig_dir,"_",model_name,"/")

# dir_create(fig_path)
# dir_create(model_path)

dir_create(fig_path)
dir_create(model_path)




muni_vars2 <- c("muni_pop_18", "muni_dpop_pct_10to18", "muni_popdensity_perkm2_18","muni_hh_avgsize_18",
                "muni_popshareforeigners_18", "muni_ageshare20to64_18", "muni_ageshare65plus_18",
                "muni_promil_births_18", "muni_promil_deaths_18", "muni_socialbenefits_receiver_share_18",
                "muni_share_1st_sector_17","muni_share_2nd_sector_17", 
                "district", "lab_region"
                # Muni Type is likely highly correlated with population density and combinations of other of the above characteristics
                #, "muni_type"
                # Voting behavior
                # "muni_voting_share_19_fdp", "muni_voting_share_19_svp", "muni_voting_share_19_glp",
                # "muni_voting_share_19_gps","muni_voting_share_19_sp", "muni_voting_share_19_cvp"
)



# Create RHS
(v0 <- paste(additional_term, dummy_term, hh_term, sep = " + "))

(v1 <- paste(muni_vars2, collapse = " + "))

(v2 <- paste(sign_term, sep = " + ", collapse = " + "))

(v3 <- paste(sign_term, shares_term, sep = " + ", collapse = " + "))

(v4 <- paste(vars4, collapse = " + "))

# Create formulas
f0 <- formula(paste("cbind(w,y) ~ age + ", v0))
f02 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", v0))
# f02 <- formula(paste("cbind(w,y) ~ age + ", paste(v1, muni_term, sep = " + ")))

f1 <- formula(paste("cbind(w,y) ~ age + ", paste(v0,v1,sep = " + ")))
f12 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", paste(v0,v1,sep = " + ")))
# f12 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", paste(v1, muni_term, sep = " + ")))

f2 <- formula(paste("cbind(w,y) ~ age + ", paste(v0, v1, v2, sep = " + ")))
f22 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", paste(v0, v1, v2, sep = " + ")))
# f22 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", paste(v1, v2, muni_term, sep = " + ")))

f3 <- formula(paste("cbind(w,y) ~ age + ", paste(v0, v1, v2, v3, sep = " + ")))
f32 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", paste(v0, v1, v2, v3, sep = " + ")))
# f32 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", paste(v1, v2, v3, muni_term, sep = " + ")))

f4 <- formula(paste("cbind(w,y) ~ age + ", paste(v0, v1, v2, v3, v4, sep = " + ")))
f42 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", paste(v0, v1, v2, v3, v4, sep = " + ")))
# f42 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", paste(v1, v2, v3, v4, muni_term, sep = " + ")))



f0 <- f02
f1 <- f12
f2 <- f22
f3 <- f32
f4 <- f42

df_rf <- df


train <- readRDS(paste0(model_dir,"train.rds"))

df_train <- df_rf[train,]
df_test <- df_rf[!train,]

Y_train <- df_train[,1:2]
Y_test <- df_test[,1:2]

# X_train <- model.matrix(update.formula(f2, .~.-1-ns(age, df = 3) + age),df_train)
# Y_train <- sign_w[train]

saveRDS(train, paste0(model_path,"train.rds"))

# df_train <- df_rf[train,]


library(parallel)
library(doParallel)
# cluster <- makeCluster(detectCores()/4) # convention to leave 1 core for OS

ncores <- detectCores()/4

# cluster <- makePSOCKcluster(ncores)
cluster <- makeCluster(ncores)
registerDoParallel(cluster)

# Random Forests for Income Y ---------------------------------------------

.f <- f2

set.seed(2023)

## hyperparameters:
# - num.trees: number of trees
# - mtry: number of variables considered at each split
# - splitrule: criteria to split tree: Gini or entropy for sign_wy, MSE or MAE for reg
# - min.node.size: minimal size of resulting nodes for split to happen
# - max.depth: of ind. tree, control to avoid overfitting

tuning_grid <- expand.grid(
  mtry = c(5, 7, 10, 15, 20, 25, 30, 40),
  splitrule = c("variance"),
  min.node.size = c(1, 5, 10, 25, 100, 200),
  num.trees = c(500, 1000)
)

for(row in 1:nrow(tuning_grid)){
  
  pars <- tuning_grid[row,]
  
  
  rf_y <- ranger::ranger(
    x = model.matrix(update.formula(.f, .~.-1-ns(age, df = 3) + age),df_train),
    y = Y_train$y,
    oob.error = TRUE,
    splitrule = pars$splirule,
    min.node.size = pars$min.node.size,
    mtry = pars$mtry,
    num.trees = pars$num.trees,
    num.threads = 8,
    seed = 123
  )
  
  r2_y <- rf_y$r.squared
  mse_y <- rf_y$prediction.error
  
  tuning_grid[row, "mse_y"] <- mse_y
  tuning_grid[row, "r2_y"] <- r2_y
  
  # random forest for wealth
  rf_w <- ranger::ranger(
    x = model.matrix(update.formula(.f, .~.-1-ns(age, df = 3) + age),df_train),
    y = Y_train$w,
    oob.error = TRUE,
    splitrule = pars$splirule,
    min.node.size = pars$min.node.size,
    mtry = pars$mtry,
    num.trees = pars$num.trees,
    num.threads = 8,
    seed = 123
  )
  
  r2_w <- rf_w$r.squared
  mse_w <- rf_w$prediction.error
  
  tuning_grid[row, "mse_w"] <- mse_w
  tuning_grid[row, "r2_w"] <- r2_w
  
  print("--------------------------------------------------------------------")
  print("--------------------------------------------------------------------")
  print(paste0("Grid Row ", row, "/", nrow(tuning_grid), 
               " completed - R2 w = ",round(r2_w,3), "; y = ", round(r2_y,3)))
  print(tuning_grid[row,])
  print("--------------------------------------------------------------------")
  print("--------------------------------------------------------------------")
  
  
  
}

saveRDS(tuning_grid,paste0(model_path,"tuning_grid_f2.rds"))


library(tidyverse)
library(ggtheme)

plot_grid1 <- tuning_grid %>% 
  tidyr::pivot_longer(cols = c(starts_with("r2_"), starts_with("mse_"))) %>% 
  filter(stringr::str_detect(name,"^mse_.*")) %>% 
  mutate(
    name = toupper(str_extract(name, "(?<=\\_)\\w$")),
    min.node.size = factor(min.node.size, ordered = TRUE))





pdf(paste0(fig_path, "1_tuning_RF.pdf"))
plot_grid1 %>% 
  ggplot(aes(x = mtry, y = value, color = min.node.size)) +
  geom_line() + 
  geom_point() + 
  facet_grid(name~num.trees, scales = "free_y") +
  ggthemes::theme_base() +
  ylab("MSE") +
  theme(legend.position = "bottom") +
  ggtitle("OOB MSE for Random Forest f2")
dev.off()

plot_grid1 %>% 
  filter(name == "Y") %>% 
  ggplot(aes(x = mtry, y = value, color = min.node.size)) +
  geom_line() + 
  geom_point() + 
  facet_grid(~num.trees, scales = "free_y") +
  ggthemes::theme_base() +
  ylab("MSE") +
  theme(legend.position = "bottom")

## it seems to be the case that num.trees has no effect. Let's plot the difference
df_500 <- plot_grid1 %>% 
  filter(num.trees == 500) 

df_1000 <- plot_grid1 %>% 
  filter(num.trees == 1000)

pdf(paste0(fig_path,"1_tuning_MSE_diff.pdf"))
df_1000 %>% 
  left_join(df_500, suffix = c("_1000", "_500"), 
            by = c("mtry", "splitrule", "min.node.size", "name")) %>% 
  mutate(diff = value_500 - value_1000) %>% 
  ggplot(aes(x = mtry, y = diff, color = min.node.size)) +
  geom_line() + 
  geom_point() + 
  facet_wrap(~name, nrow = 2) +
  ggthemes::theme_base() +
  ylab("MSE difference") +
  theme(legend.position = "bottom") +
  ggtitle("MSE difference between 500 & 1000 trees")
dev.off()

sorted_y <- plot_grid1 %>% 
  arrange(value) %>% 
  filter(name == "Y")



# Plot difference in R2 between num.trees 500 & 1000 ----------------------



r2_grid1 <- tuning_grid %>% 
  tidyr::pivot_longer(cols = c(starts_with("r2_"), starts_with("mse_"))) %>% 
  filter(stringr::str_detect(name,"^r2_.*")) %>% 
  mutate(
    name = toupper(str_extract(name, "(?<=\\_)\\w$")),
    min.node.size = factor(min.node.size, ordered = TRUE))



df_500 <- r2_grid1 %>% 
  filter(num.trees == 500) 

df_1000 <- r2_grid1 %>% 
  filter(num.trees == 1000)

pdf(paste0(fig_path,"1_tuning_r2_diff.pdf"))
df_1000 %>% 
  left_join(df_500, suffix = c("_1000", "_500"), 
            by = c("mtry", "splitrule", "min.node.size", "name")) %>% 
  mutate(diff = value_500 - value_1000) %>% 
  ggplot(aes(x = mtry, y = diff, color = min.node.size)) +
  geom_line() + 
  geom_point() + 
  facet_wrap(~name, nrow = 2) +
  ggthemes::theme_base() +
  ylab("R-Squared difference") +
  theme(legend.position = "bottom") +
  ggtitle("R-Squared difference between 500 & 1000 trees")
dev.off()



# Histogram of R2

r2_grid1 %>% 
  mutate(num.trees = factor(num.trees, ordered = TRUE)) %>% 
  ggplot(aes(x = value, fill = num.trees)) +
  # geom_histogram(aes(x = value, fill = num.trees), bins = 96, alpha = 0.5) +
  geom_density(alpha = 0.3, bw = "sj") +
  # facet_wrap(~name, scales = "free_y", nrow = 2) +
  facet_grid(num.trees~name, scales = "free_y") +
  geom_rug() +
  ggthemes::theme_base() +
  ggtitle("Distribution of OOB R-Squared during grid-search") +
  theme(legend.position = "bottom")




stopCluster(cluster)
registerDoSEQ()