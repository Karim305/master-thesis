# Fit Ranger RF Models for parameter combinations obtained via Bayesian Optimization

# Load Packages, custom functions, data, and common variables
code_path <- "XXXX"
source(paste0(code_path, "/01_setup.R"))

library(mlr3)
library(mlr3learners)
library(mlr3tuning)
library(mlr3mbo)
library(paradox)
library(bbotk)
library(parallel)
library(mlr3verse)
library(ggplot2)
library(tidyverse)
library(ranger)
library(plot3D)
library(purrr)

# update default ggplot theme
theme_set(theme_bw())
theme_update(text = element_text(size=12),
             # panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             strip.background = element_blank(),
             legend.background = element_rect(fill='transparent'),
             legend.box.background = element_rect(fill='transparent')
)


# Create and set model and plot directories
model_name <- "rf_reg_mlr3mbo_tuned2"

model_path <- paste0(model_dir,"_",model_name,"/")
fig_path <- paste0(fig_dir,"_",model_name,"/")

dir_create(fig_path)
dir_create(model_path)


# Create train and test data (common split across all scripts)
df_train <- df[train,]
df_test <- df[!train,]



## Get mlr3mbo results
# list.files(paste0(model_dir,"_rf_reg_mlr3mbo_parallel2"))

# Load and transform MBO results to dataframe
rf_path <- list.files(paste0(model_dir,"_rf_reg_mlr3mbo_parallel2"), full.names = TRUE)
rf_names<- list.files(paste0(model_dir,"_rf_reg_mlr3mbo_parallel2"))

all_rf <- lapply(rf_path, readRDS) %>% set_names(rf_names)
all_mbo <- all_rf[-length(all_rf)] # exclude grid search results

opt_pars_grid <- all_rf$opt_pars_grid.rds # grid search results 

df_mbo <- map(all_mbo, ~.x$result) %>% set_names(names(all_mbo)) %>% bind_rows(.id = "file_name") %>% 
  mutate(formula = str_extract(file_name, "^f\\d"),
         variable = str_extract(file_name, "(?<=\\w)[wy](?=\\_)")) 

# load MBO results with fixed where models 3 and 4 use shares without interactions
rf_path <- list.files(paste0(model_dir,"_rf_reg_mlr3mbo_parallel3"), full.names = TRUE)
rf_names<- list.files(paste0(model_dir,"_rf_reg_mlr3mbo_parallel3"))

all_rf <- lapply(rf_path, readRDS) %>% set_names(rf_names)
all_mbo2 <- all_rf[-length(all_rf)]

opt_pars_grid <- all_rf$opt_pars_grid.rds

df_mbo2 <- map(all_mbo2, ~.x$result) %>% set_names(names(all_mbo2)) %>% bind_rows(.id = "file_name") %>% 
  mutate(formula = str_extract(file_name, "^f\\d"),
         variable = str_extract(file_name, "(?<=\\w)[wy](?=\\_)")) 


# 
# Create RHS
(v0 <- paste(additional_term, dummy_term, hh_term, sep = " + "))

(v1 <- paste(muni_vars2, collapse = " + "))

(v2 <- paste(sign_term, sep = " + ", collapse = " + "))

(v3 <- paste(shares_term, sep = " + ", collapse = " + "))
(v32 <- paste(vars3, sep = " + ", collapse = " + "))

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

# f3 <- formula(paste("cbind(w,y) ~ age + ", paste(v0, v1, v2, v3, sep = " + ")))
# f32 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", paste(v0, v1, v2, v3, sep = " + ")))
# # f32 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", paste(v1, v2, v3, muni_term, sep = " + ")))
# 
# f4 <- formula(paste("cbind(w,y) ~ age + ", paste(v0, v1, v2, v3, v4, sep = " + ")))
# f42 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", paste(v0, v1, v2, v3, v4, sep = " + ")))
# # f42 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", paste(v1, v2, v3, v4, muni_term, sep = " + ")))

f3 <- formula(paste("cbind(w,y) ~ age + ", paste(v0, v1, v2, v3, sep = " + ")))
f32 <- formula(paste("cbind(w,y) ~ age + ", paste(v0, v1, v2, v32, sep = " + ")))
# f32 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", paste(v1, v2, v3, muni_term, sep = " + ")))

f4 <- formula(paste("cbind(w,y) ~ age + ", paste(v0, v1, v2, v3, v4, sep = " + ")))
f42 <- formula(paste("cbind(w,y) ~ age + ", paste(v0, v1, v2, v32, v4, sep = " + ")))
# f42 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", paste(v1, v2, v3, v4, muni_term, sep = " + ")))




# Plot MBO loss surfaces and optimization paths ---------------------------

# Function that creates 3D plots of loss surface
plot_loss_surface <- function(mbo, zlim = NULL, file_suffix = "", to_disk = TRUE,show_legend=TRUE){
  
  file_name <- names(mbo)
  form <- str_extract(file_name, "^f\\d")
  var <- str_extract(file_name, "(?<=\\w)[wy](?=\\_)") 
  
  pars_w <- mbo[[1]]$archive %>% mutate(loss = oob_error)
  
  w_id_opt_grid <- pars_w %>% filter(is.na(.already_evaluated)) %>% nrow()
  col_w <- ifelse(is.na(pars_w$.already_evaluated), "init", "proposed (EI)")
  col_w[which.min(pars_w$loss)] <- "Solution BO"
  col_w[w_id_opt_grid] <- "Solution Grid-Search"
  # cv_prop <- which(pars_w$mtry == cv_opt_w$mtry & pars_w$min.node.size == cv_opt_w$min.node.size)  
  col_w <- as.factor(col_w)
  
  
  opt_w <- (pars_w %>% arrange(loss) %>% select(min.node.size,mtry,loss))[1,]
  main_w <- paste0("RF",str_extract(form,"\\d"), " MBO: ", ifelse(var == "w","Wealth","Income")," \n optimal (mtry, min.node.size, loss) = (",
                   paste(opt_w$mtry,opt_w$min.node.size,round(opt_w$loss,3),sep = ", "),")")
  
  colvar <- as.numeric(as.factor(col_w))
  col <- c("black","blue","green", "red")#[colvar]
  # col <- 1:4
  pch <- c(19,19,18,18)[colvar]
  cex <- c(0.5,0.5,2,2)[colvar]
  if(to_disk){
    pdf(paste0(fig_path,var,"_RF",str_extract(form,"\\d"),"_mbo_3d",file_suffix,".pdf"))
  }
  with(pars_w,
       
       scatter3D(x = mtry, y = min.node.size, z = loss, colvar = colvar,
                 col = col, xlab = "mtry", zlim = zlim,
                 ylab = "min.node.size", zlab = "MSE", theta = 40,
                 phi = 0, bty = "g",  type = "h", colkey = FALSE,
                 ticktype = "detailed", pch = pch, cex = cex, main = main_w)
       
  )
  if(show_legend){
    legend("right", c("Initial Design", "Proposal by EI", "BO Solution", "Grid-Search Solution"), 
           col = col, pch = c(19,19,18,18), title = "Proposal")
  }
  if(to_disk){
    dev.off()
  }
  
  
  
}

# Function that converts colors from names (e.g. 'red') to hex
col2hex <- function(x, alpha = FALSE) {
  args <- as.data.frame(t(col2rgb(x, alpha = alpha)))
  args <- c(args, list(names = x, maxColorValue = 255))
  do.call(rgb, args)
}

# Function that plots evolution of loss over iterations of MBO
plot_loss_path <- function(mbo, zlim = NULL, file_suffix = "", to_disk = TRUE){
  
  file_name <- names(mbo)
  form <- str_extract(file_name, "^f\\d")
  var <- str_extract(file_name, "(?<=\\w)[wy](?=\\_)") 
  
  pars_w <- mbo[[1]]$archive %>% mutate(loss = oob_error)
  
  w_id_opt_grid <- pars_w %>% filter(is.na(.already_evaluated)) %>% nrow()
  col_w <- ifelse(is.na(pars_w$.already_evaluated), "init", "proposed (EI)")
  col_w[which.min(pars_w$loss)] <- "Solution BO"
  col_w[w_id_opt_grid] <- "Solution Grid-Search"
  # cv_prop <- which(pars_w$mtry == cv_opt_w$mtry & pars_w$min.node.size == cv_opt_w$min.node.size)  
  col_w <- as.factor(col_w)
  
  
  opt_w <- (pars_w %>% arrange(loss) %>% select(min.node.size,mtry,loss))[1,]
  
  colvar <- as.numeric(as.factor(col_w))
  cols <- c("black","blue","green", "red")#[colvar]
  
  
  
  col_groups <- sapply(cols, col2hex)
  names(col_groups) <- c("init","proposed (EI)","Solution BO","Solution Grid-Search")
  
  p <- pars_w %>% mutate(n = row_number(), col = col_w) %>% 
    pivot_longer(cols = c(oob_error, acq_ei)) %>% 
    mutate(name = ifelse(name == "oob_error", "OOB Error", "Expected Improvement")) %>% 
    ggplot(aes(x = n, y = value)) +
    geom_line() +
    geom_point(aes(color = col)) +
    facet_wrap(~name, ncol = 1, scales = "free") +
    scale_y_log10() +
    theme(legend.position = "bottom") +
    ggtitle(paste0("RF",str_extract(form,"\\d")," ", ifelse(var == "w","Wealth","Income"),": BO Progress - Expected Improvement & OOB Error")) +
    xlab("Iteration") +
    scale_color_manual(values = col_groups, 
                       labels = c("Initial Design", "Proposal by EI", "BO Solution", "Grid-Search Solution")) +
    guides(color = guide_legend(""))
  
  if(to_disk){
    pdf(paste0(fig_path,var,"_RF",str_extract(form,"\\d"),"_EI_error_path",file_suffix,".pdf"))
    print(p)
    dev.off()
  } else {
    p
  }
  
}


# Plot loss surface and optimization path 
plot_loss_surface(all_mbo[3], to_disk = FALSE)
plot_loss_surface(all_mbo[1], to_disk = FALSE)
plot_loss_surface(all_mbo2[1], to_disk = FALSE)
plot_loss_surface(all_mbo[4], to_disk = FALSE)

# For mbo1
for(i in seq_along(all_mbo)) plot_loss_surface(all_mbo[i])
plot_loss_surface(all_mbo[3], file_suffix = "scaled", zlim = c(2.065, 2.25), show_legend = FALSE, to_disk = TRUE)
plot_loss_surface(all_mbo[2], file_suffix = "scaled", zlim = c(0.125, 0.15), show_legend = FALSE)
for(i in seq_along(all_mbo)) plot_loss_path(all_mbo[i])

# for mbo2
for(i in seq_along(all_mbo2)) plot_loss_surface(all_mbo2[i], file_suffix = "_2")
plot_loss_surface(all_mbo2[1], file_suffix = "scaled_2", zlim = c(2.065, 2.25), show_legend = FALSE, to_disk = TRUE)
plot_loss_surface(all_mbo2[2], file_suffix = "scaled_2", zlim = c(0.1155, 0.15), show_legend = F, to_disk = TRUE)
for(i in seq_along(all_mbo2)) plot_loss_path(all_mbo2[i], file_suffix = "_2")






# Fit MBO Solution --------------------------------------------------------


formulas <- c("f0" = f0, "f1" = f1, "f2" = f2, "f3" = f3, "f4" = f4)
fit_tbl <- tibble()
for(i in seq_along(all_mbo[1:4])){
  set.seed(321)
  mbo <- all_mbo[i]
  file_name <- names(mbo)
  form <- str_extract(file_name, "^f\\d")
  var <- str_extract(file_name, "(?<=\\w)[wy](?=\\_)") 
  
  f <- update.formula(formulas[[form]], paste0(var,"~."))
  model_frame <- model.frame(f, df)
  y <- model_frame[,var]
  x <- model_frame[,-1]
  
  opt_pars <- mbo[[1]]$result
  
  print(file_name)
  
  
  rf <- ranger(y = y[train], x = x[train,], oob.error = TRUE,
               keep.inbag = TRUE, min.node.size = opt_pars$min.node.size,
               mtry = opt_pars$mtry, num.trees = 500,
               seed = 123, num.threads = 8, importance = 'permutation')
  
  
  rf1000 <- ranger(y = y[train], x = x[train,], oob.error = TRUE,
                   keep.inbag = TRUE, min.node.size = opt_pars$min.node.size,
                   mtry = opt_pars$mtry, num.trees = 1000,
                   seed = 123, num.threads = 8, importance = 'permutation')
  
  
  rf_hat <- tibble(predict(rf, df_test)$predictions)
  rf_hat1000 <- tibble(predict(rf1000, df_test)$predictions)
  
  
  
  names(rf_hat) <- var
  names(rf_hat1000) <- var
  # fit_list <- list(rf)
  
  target <- df_test[,var]
  
  fit_tbl1000 <- tibble(model = paste("RF", str_extract(form, "\\d")), var = var, target = list(target),
                      fit = list(rf1000), predictions = list(tibble(rf_hat1000)), pars = list(opt_pars),
                      var_imp = list(data.frame(rf1000$variable.importance)), num.trees = 1000)  
  
  
  fit_tbl500 <- tibble(model = paste("RF", str_extract(form, "\\d")), var = var, target = list(target),
                       fit = list(rf), predictions = list(tibble(rf_hat)), pars = list(opt_pars),
                       var_imp = list(data.frame(rf$variable.importance)), num.trees = 500)  
  
  
  fit_tbl_i <- bind_rows(fit_tbl500, fit_tbl1000)
  
  fit_tbl <- bind_rows(fit_tbl, fit_tbl_i)
  
}

rf_out <- fit_tbl %>%
  mutate(fitted_values = map(fit,~tibble(predict(.x,df_train)$predictions))
         , fitted_values_oob = map(fit, ~tibble(z=.x$predictions))
         ) %>% 
  group_by(model, num.trees) %>%
  summarise(
    fit = list(map2(fit, var, ~ setNames(list(.x),.y))),
    # fit = map2(fit,var,~map2(fit, var, ~ list(.y = .x))),
    predictions = list(map2(predictions, var, ~ setNames(.x, .y))),
    fitted_values = list(map2(fitted_values, var, ~ setNames(.x, .y))),
    fitted_values_oob = list(map2(fitted_values_oob, var, ~ setNames(.x, .y))),
    var_imp = list(map2(var_imp, var, ~ setNames(.x, .y))),
    target = list(map2(target, var, ~ setNames(.x, .y))), .groups = "drop"
  ) %>%
  ungroup() %>% 
  mutate(predictions = map(predictions, bind_cols), 
         fitted_values = map(fitted_values, bind_cols), 
         fitted_values_oob = map(fitted_values_oob, bind_cols), 
         target = map(target, bind_cols),
         model = str_remove(model, "\\s"),
         name = paste0(model,"mbo")) %>% 
  mutate(fit = map(fit, flatten))


rf_out$fit[[1]] 

rf_out <- rf_out %>% mutate(target_train = list(df_train[,c("w","y")]))

saveRDS(rf_out, paste0(model_path, "rf_fitted_mbo.rds"))

rf_out$model


test$predictions[[1]]
test$var_imp[[1]]




# Fit RF model for f32 and f42 (no share_ranges) --------------------------

formulas <- c("f3" = f32, "f4" = f42)
fit_tbl <- tibble()
for(i in seq_along(all_mbo2)){
  set.seed(321)
  mbo <- all_mbo2[i]
  file_name <- names(mbo)
  form <- str_extract(file_name, "^f\\d")
  var <- str_extract(file_name, "(?<=\\w)[wy](?=\\_)") 
  
  f <- update.formula(formulas[[form]], paste0(var,"~."))
  model_frame <- model.frame(f, df)
  y <- model_frame[,var]
  x <- model_frame[,-1]
  
  opt_pars <- mbo[[1]]$result
  
  print(file_name)
  
  
  rf <- ranger(y = y[train], x = x[train,], oob.error = TRUE,
               keep.inbag = TRUE, min.node.size = opt_pars$min.node.size,
               mtry = opt_pars$mtry, num.trees = 500,
               seed = 123, num.threads = 8, importance = 'permutation')
  
  
  rf1000 <- ranger(y = y[train], x = x[train,], oob.error = TRUE,
                   keep.inbag = TRUE, min.node.size = opt_pars$min.node.size,
                   mtry = opt_pars$mtry, num.trees = 1000,
                   seed = 123, num.threads = 8, importance = 'permutation')
  
  
  rf_hat <- tibble(predict(rf, df_test)$predictions)
  rf_hat1000 <- tibble(predict(rf1000, df_test)$predictions)
  
  
  
  names(rf_hat) <- var
  names(rf_hat1000) <- var
  # fit_list <- list(rf)
  
  target <- df_test[,var]
  
  fit_tbl1000 <- tibble(model = paste("RF", str_extract(form, "\\d")), var = var, target = list(target),
                        fit = list(rf1000), predictions = list(tibble(rf_hat1000)), pars = list(opt_pars),
                        var_imp = list(data.frame(rf1000$variable.importance)), num.trees = 1000)  
  
  
  fit_tbl500 <- tibble(model = paste("RF", str_extract(form, "\\d")), var = var, target = list(target),
                       fit = list(rf), predictions = list(tibble(rf_hat)), pars = list(opt_pars),
                       var_imp = list(data.frame(rf$variable.importance)), num.trees = 500)  
  
  
  fit_tbl_i <- bind_rows(fit_tbl500, fit_tbl1000)
  
  fit_tbl <- bind_rows(fit_tbl, fit_tbl_i)
  
}

rf_out2 <- fit_tbl %>%
  mutate(fitted_values = map(fit,~tibble(predict(.x,df_train)$predictions))
         , fitted_values_oob = map(fit, ~tibble(z=.x$predictions))
  ) %>% 
  group_by(model, num.trees) %>%
  summarise(
    fit = list(map2(fit, var, ~ setNames(list(.x),.y))),
    # fit = map2(fit,var,~map2(fit, var, ~ list(.y = .x))),
    predictions = list(map2(predictions, var, ~ setNames(.x, .y))),
    fitted_values = list(map2(fitted_values, var, ~ setNames(.x, .y))),
    fitted_values_oob = list(map2(fitted_values_oob, var, ~ setNames(.x, .y))),
    var_imp = list(map2(var_imp, var, ~ setNames(.x, .y))),
    target = list(map2(target, var, ~ setNames(.x, .y))), .groups = "drop"
  ) %>%
  ungroup() %>% 
  mutate(predictions = map(predictions, bind_cols), 
         fitted_values = map(fitted_values, bind_cols), 
         fitted_values_oob = map(fitted_values_oob, bind_cols), 
         target = map(target, bind_cols),
         model = str_remove(model, "\\s"),
         name = paste0(model,"v2mbo")) %>% 
  mutate(fit = map(fit, flatten)) %>% 
  mutate(target_train = list(df_train[,c("w","y")]))

saveRDS(rf_out2, paste0(model_path, "rf_fitted_mbo2.rds"))






# # check difference between formula and x,y interface 
# set.seed(321)
# par_set <- all_mbo$f2_w_mlr3mbo_RF.rds$result
# rf2_w_formula <- ranger(formula = update.formula(f2, w~.), data = df_train, oob.error = TRUE, 
#                         keep.inbag = TRUE, min.node.size = par_set$min.node.size,
#                         mtry = par_set$mtry, num.trees = 1000,
#                         seed = 123, num.threads = 8
#                )
# 
# set.seed(321)
# rf2_w_xy <- ranger(x = model.matrix(update.formula(f2, .~.-1), df_train), y = df_train$w,
#                    oob.error = TRUE, 
#                         keep.inbag = TRUE, min.node.size = par_set$min.node.size,
#                         mtry = par_set$mtry, num.trees = 1000,
#                         seed = 123, num.threads = 8
# )
# # both approaches differ..


