## Fit Baseline and grouped MARS models as well as some LM models
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
library(tidyverse)
library(earth)




# Create and set model and plot directories
# model_name <- "lm_test"
model_name <- "new_results"

model_path <- paste0(model_dir,"_",model_name,"/")
fig_path <- paste0(fig_dir,"_",model_name,"/")

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



# library(GGally)
# df %>% distinct(muni_type, muni_popdensity_perkm2_18) %>% ggpairs()




sign_w <- sign(df$w) %>% 
  factor()



# train <- readRDS(paste0(model_dir,"train.rds"))

# set.seed(2023)
# train <- sample_train(df,0.5)

library(purrr)

df <- tibble(df) %>% 
  mutate(sign_w = factor(sign(w)),
         sign_y = factor(sign(y)),
         sign_wy = factor(sign_w:sign_y))


print(mean(train))


df_stage2 <- df %>% mutate(group = sign_w)

train2 <- train


# saveRDS(df_stage2, paste0(data_dir,"stage2.rds"))


# Fit Simple LM Models  -------------------------------

lm0 <- fit_by_group(df_stage2, train2, function(x){lm(f02, data = x)}, grouped = FALSE)
lm1 <- fit_by_group(df_stage2, train2, function(x){lm(f12, data = x)}, grouped = FALSE)
lm2 <- fit_by_group(df_stage2, train2, function(x){lm(f22, data = x)}, grouped = FALSE)
lm3 <- fit_by_group(df_stage2, train2, function(x){lm(f32, data = x)}, grouped = FALSE)
lm4 <- fit_by_group(df_stage2, train2, function(x){lm(f42, data = x)}, grouped = FALSE)


models <- list("LM0.1" = lm0, "LM1.1" = lm1, "LM2.1" = lm2, "LM3.1" = lm3, "LM4.1" = lm4)


saveRDS(models, paste0(model_path,"lm.rds"))




# Fit Simple MARS2 Models of varying degrees -------------------------------

mars1_simple <- fit_by_group(df_stage2, train2,
                             function(x){earth::earth(f2, data = x, degree = 1,
                                                      nk = 500, thresh = 0.00001)},
                             grouped = FALSE)

mars2_simple <- fit_by_group(df_stage2, train2,
                             function(x){earth::earth(f2, data = x, degree = 2,
                                                      nk = 500, thresh = 0.00001)},
                             grouped = FALSE)

mars3_simple <- fit_by_group(df_stage2, train2,
                             function(x){earth::earth(f2, data = x, degree = 3,
                                                      nk = 500, thresh = 0.00001)},
                             grouped = FALSE)

mars4_simple <- fit_by_group(df_stage2, train2,
                             function(x){earth::earth(f2, data = x, degree = 4,
                                                      nk = 500, thresh = 0.00001)},
                             grouped = FALSE)

mars5_simple <- fit_by_group(df_stage2, train2,
                             function(x){earth::earth(f2, data = x, degree = 5,
                                                      nk = 500, thresh = 0.00001)},
                             grouped = FALSE)



models <- list("Simple MARS 2.1" = mars1_simple,
               "Simple MARS 2.2" = mars2_simple,
               "Simple MARS 2.3" = mars3_simple,
               "Simple MARS 2.4" = mars4_simple,
               "Simple MARS 2.5" = mars5_simple)

saveRDS(models, paste0(model_path,"f2_mars_deg.rds"))









# Fit Simple MARS0 Models of varying degrees -------------------------------

mars1_simple <- fit_by_group(df_stage2, train2,
                             function(x){earth::earth(f0, data = x, degree = 1,
                                                      nk = 500, thresh = 0.00001)},
                             grouped = FALSE)

mars2_simple <- fit_by_group(df_stage2, train2,
                             function(x){earth::earth(f0, data = x, degree = 2,
                                                      nk = 500, thresh = 0.00001)},
                             grouped = FALSE)

mars3_simple <- fit_by_group(df_stage2, train2,
                             function(x){earth::earth(f0, data = x, degree = 3,
                                                      nk = 500, thresh = 0.00001)},
                             grouped = FALSE)

mars4_simple <- fit_by_group(df_stage2, train2,
                             function(x){earth::earth(f0, data = x, degree = 4,
                                                      nk = 500, thresh = 0.00001)},
                             grouped = FALSE)

mars5_simple <- fit_by_group(df_stage2, train2,
                             function(x){earth::earth(f0, data = x, degree = 5,
                                                      nk = 500, thresh = 0.00001)},
                             grouped = FALSE)



models <- list("Simple MARS 0.1" = mars1_simple,
               "Simple MARS 0.2" = mars2_simple,
               "Simple MARS 0.3" = mars3_simple,
               "Simple MARS 0.4" = mars4_simple,
               "Simple MARS 0.5" = mars5_simple)

saveRDS(models, paste0(model_path,"f0_mars_deg.rds"))



# Fit Simple MARS1 Models of varying degrees -------------------------------

mars1_simple <- fit_by_group(df_stage2, train2,
                             function(x){earth::earth(f1, data = x, degree = 1,
                                                      nk = 500, thresh = 0.00001)},
                             grouped = FALSE)

mars2_simple <- fit_by_group(df_stage2, train2,
                             function(x){earth::earth(f1, data = x, degree = 2,
                                                      nk = 500, thresh = 0.00001)},
                             grouped = FALSE)

mars3_simple <- fit_by_group(df_stage2, train2,
                             function(x){earth::earth(f1, data = x, degree = 3,
                                                      nk = 500, thresh = 0.00001)},
                             grouped = FALSE)

mars4_simple <- fit_by_group(df_stage2, train2,
                             function(x){earth::earth(f1, data = x, degree = 4,
                                                      nk = 500, thresh = 0.00001)},
                             grouped = FALSE)

mars5_simple <- fit_by_group(df_stage2, train2,
                             function(x){earth::earth(f1, data = x, degree = 5,
                                                      nk = 500, thresh = 0.00001)},
                             grouped = FALSE)



models <- list("Simple MARS 1.1" = mars1_simple,
               "Simple MARS 1.2" = mars2_simple,
               "Simple MARS 1.3" = mars3_simple,
               "Simple MARS 1.4" = mars4_simple,
               "Simple MARS 1.5" = mars5_simple)

saveRDS(models, paste0(model_path,"f1_mars_deg.rds"))






# Fit Simple MARS3 Models of varying degrees -------------------------------

mars1_simple <- fit_by_group(df_stage2, train2,
                             function(x){earth::earth(f3, data = x, degree = 1,
                                                      nk = 500, thresh = 0.00001)},
                             grouped = FALSE)

mars2_simple <- fit_by_group(df_stage2, train2,
                             function(x){earth::earth(f3, data = x, degree = 2,
                                                      nk = 500, thresh = 0.00001)},
                             grouped = FALSE)

mars3_simple <- fit_by_group(df_stage2, train2,
                             function(x){earth::earth(f3, data = x, degree = 3,
                                                      nk = 500, thresh = 0.00001)},
                             grouped = FALSE)

mars4_simple <- fit_by_group(df_stage2, train2,
                             function(x){earth::earth(f3, data = x, degree = 4,
                                                      nk = 500, thresh = 0.00001)},
                             grouped = FALSE)

mars5_simple <- fit_by_group(df_stage2, train2,
                             function(x){earth::earth(f3, data = x, degree = 5,
                                                      nk = 500, thresh = 0.00001)},
                             grouped = FALSE)



models <- list("Simple MARS 3.1" = mars1_simple,
               "Simple MARS 3.2" = mars2_simple,
               "Simple MARS 3.3" = mars3_simple,
               "Simple MARS 3.4" = mars4_simple,
               "Simple MARS 3.5" = mars5_simple)

saveRDS(models, paste0(model_path,"f3_mars_deg.rds"))






# Fit Simple MARS4 Models of varying degrees -------------------------------

mars1_simple <- fit_by_group(df_stage2, train2,
                             function(x){earth::earth(f4, data = x, degree = 1,
                                                      nk = 500, thresh = 0.00001)},
                             grouped = FALSE)

mars2_simple <- fit_by_group(df_stage2, train2,
                             function(x){earth::earth(f4, data = x, degree = 2,
                                                      nk = 500, thresh = 0.00001)},
                             grouped = FALSE)

mars3_simple <- fit_by_group(df_stage2, train2,
                             function(x){earth::earth(f4, data = x, degree = 3,
                                                      nk = 500, thresh = 0.00001)},
                             grouped = FALSE)

mars4_simple <- fit_by_group(df_stage2, train2,
                             function(x){earth::earth(f4, data = x, degree = 4,
                                                      nk = 500, thresh = 0.00001)},
                             grouped = FALSE)

mars5_simple <- fit_by_group(df_stage2, train2,
                             function(x){earth::earth(f4, data = x, degree = 5,
                                                      nk = 500, thresh = 0.00001)},
                             grouped = FALSE)



models <- list("Simple MARS 4.1" = mars1_simple,
               "Simple MARS 4.2" = mars2_simple,
               "Simple MARS 4.3" = mars3_simple,
               "Simple MARS 4.4" = mars4_simple,
               "Simple MARS 4.5" = mars5_simple)

saveRDS(models, paste0(model_path,"f4_mars_deg.rds"))





# Fit grouped models -------------------------------------------------

# By sign(w):sign(y)
df_stage2 <- df %>% mutate(group = sign_wy)


mars0 <- fit_by_group(df_stage2, train2,
                      function(x){earth::earth(f0, data = x, degree = 3,
                                               nk = 500, thresh = 0.00001)})

mars1 <- fit_by_group(df_stage2, train2,
                      function(x){earth::earth(f1, data = x, degree = 3,
                                               nk = 500, thresh = 0.00001)})

mars2 <- fit_by_group(df_stage2, train2,
                      function(x){earth::earth(f2, data = x, degree = 3,
                                               nk = 500, thresh = 0.00001)})

mars3 <- fit_by_group(df_stage2, train2,
                      function(x){earth::earth(f3, data = x, degree = 4,
                                               nk = 500, thresh = 0.00001)})

mars4 <- fit_by_group(df_stage2, train2,
                      function(x){earth::earth(f4, data = x, degree = 4,
                                               nk = 500, thresh = 0.00001)})



models <- list(
  # "grouped lm0" = lm0,"grouped lm1" = lm1,"grouped lm2" = lm2,
  #              "grouped lm3" = lm3, "grouped lm4" = lm4, 
  "grouped MARS0.3" = mars0,
  "grouped MARS1.3" = mars1,"grouped MARS2.3" = mars2,
  "grouped MARS3.4" = mars3, "grouped MARS4.4" = mars4)

saveRDS(models, paste0(model_path,"models_wy.rds"))



# By sign(w)

df_stage2 <- df %>% mutate(group = sign_w)

mars0 <- fit_by_group(df_stage2, train2,
                      function(x){earth::earth(f0, data = x, degree = 3,
                                               nk = 500, thresh = 0.00001)})

mars1 <- fit_by_group(df_stage2, train2,
                      function(x){earth::earth(f1, data = x, degree = 3,
                                               nk = 500, thresh = 0.00001)})

mars2 <- fit_by_group(df_stage2, train2,
                      function(x){earth::earth(f2, data = x, degree = 3,
                                               nk = 500, thresh = 0.00001)})

mars3 <- fit_by_group(df_stage2, train2,
                      function(x){earth::earth(f3, data = x, degree = 4,
                                               nk = 500, thresh = 0.00001)})

mars4 <- fit_by_group(df_stage2, train2,
                      function(x){earth::earth(f4, data = x, degree = 4,
                                               nk = 500, thresh = 0.00001)})



models <- list(
  # "grouped lm0" = lm0,"grouped lm1" = lm1,"grouped lm2" = lm2,
  #              "grouped lm3" = lm3, "grouped lm4" = lm4, 
  "grouped MARS0.3" = mars0,
  "grouped MARS1.3" = mars1,"grouped MARS2.3" = mars2,
  "grouped MARS3.4" = mars3, "grouped MARS4.4" = mars4)

saveRDS(models, paste0(model_path,"models_w.rds"))





