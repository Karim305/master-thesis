## Script that combines data on different models 
## Creates a nested tibble that includes predictions, fitted values, as well as train and test labels, etc. 

library(tidyverse)
library(parallel)
library(earth)
library(ranger)
# Load data & create model formulas----------------------------------------

code_path <- "XXXX"
source(paste0(code_path, "/01_setup.R"))

# Functions ---------------------------------------------------------------

load_models <- function(name){
  if(!str_detect(name,"^\\_")){
    name <- paste0("_",name)
  }
  dir <- model_dir
  path <- paste0(dir,name)
  
  model_names <- list.files(path)
  is_train <- str_detect(model_names, "train")
  
  files <- lapply(list.files(path, full.names = TRUE), readRDS)
  names(files) <- model_names
  out <- list(models = files[!is_train], train = files[is_train])
  
}

# Load Models -------------------------------------------------------------

# LM ----------------------------------------------------------------------
lm_files <- load_models("lm_new")



fits_new <- lm_files$models

train_new <- lm_files$train$train_sel.rds
sum(lm_files$train$train_sel.rds)
rm(lm_files)
gc()

# models were trained ob train_sel
sapply(fits_new, nobs)
sum(train)



# MARS --------------------------------------------------------------------


new_fits <- load_models("new_results")$models

lm_fits <- new_fits$lm.rds %>% bind_rows() %>% mutate(name = names(new_fits$lm.rds))
mars0 <- new_fits$f0_mars_deg.rds %>% bind_rows() %>% mutate(main = names(new_fits$f0_mars_deg.rds))
mars1 <- new_fits$f1_mars_deg.rds %>% bind_rows() %>% mutate(main = names(new_fits$f1_mars_deg.rds))
mars2 <- new_fits$f2_mars_deg.rds %>% bind_rows() %>% mutate(main = names(new_fits$f2_mars_deg.rds))
mars3 <- new_fits$f3_mars_deg.rds %>% bind_rows() %>% mutate(main = names(new_fits$f3_mars_deg.rds))
mars4 <- new_fits$f4_mars_deg.rds %>% bind_rows() %>% mutate(main = names(new_fits$f4_mars_deg.rds))

mars_tbl_new <- bind_rows(mars0,mars1,mars2,mars3,mars4) %>%
  mutate(grouped = str_detect(main, "[a-zA-Z]+$")) %>% 
  mutate(name = str_remove(main, "^.*(?=LM|MARS|lm|mars|RF\\s?)") %>% 
           str_remove_all("\\s")) %>% 
  select(-c(group)) %>% 
  # rename(predictions = predicted) %>%
  mutate(deg_int = str_extract(name, "(?<=\\.)\\d"),
         deg_int = ifelse(is.na(deg_int), 1, deg_int),
         class = str_extract(name, "(?<!\\.)\\d|\\d(?=\\.)")) %>% 
  mutate(fitted_values = map(fit, ~.x$fitted.values)) %>% 
  mutate(target = list(df[!train,c("w","y")])) %>% 
  mutate(target_train = list(df[train,c("w","y")]))

benchmarks_wy <- new_fits$models_wy.rds
benchmarks_w <- new_fits$models_w.rds



# benchmarks w ------------------------------------------------------------

grouped_mars_in_w <- map(benchmarks_w, function(z){
  
  df1 <- tibble(fitted_values = map(z$fit, ~bind_rows(data.frame(.x$fitted.values))),
                target_in = map(z$fit, ~bind_rows(data.frame(.x$fitted.values + .x$residuals)))) 
  
  df1
}) %>% 
  map_df(~{
    tibble(
      fitted_values = list(bind_rows(.x$fitted_values)),
      target_in = list(bind_rows(.x$target_in))
    )
  }) %>% 
  mutate(main = names(benchmarks_w)) 

grouped_mars_w <- tibble(
  predictions = map(benchmarks_w,~bind_rows(.x$predictions)),
  target = map(benchmarks_w,~bind_rows(.x$target))
  # target_in = map2(fitted_values, res_in, ~.x+.y) 
) %>% 
  mutate(main = names(benchmarks_w)) %>% 
  left_join(grouped_mars_in_w, by = "main") %>% 
  mutate(name = str_remove(main,"grouped ") %>% paste0("_w"),
         main = paste0(main, " by w"))


# benchmarks wy -----------------------------------------------------------




grouped_mars_in <- map(benchmarks_wy, function(z){

    df1 <- tibble(fitted_values = map(z$fit, ~bind_rows(data.frame(.x$fitted.values))),
           target_in = map(z$fit, ~bind_rows(data.frame(.x$fitted.values + .x$residuals)))) 

  df1
}) %>% 
  map_df(~{
    tibble(
      fitted_values = list(bind_rows(.x$fitted_values)),
      target_in = list(bind_rows(.x$target_in))
    )
  }) %>% 
  mutate(main = names(benchmarks_wy)) 

grouped_mars <- tibble(
  predictions = map(benchmarks_wy,~bind_rows(.x$predictions)),
  target = map(benchmarks_wy,~bind_rows(.x$target))
  # target_in = map2(fitted_values, res_in, ~.x+.y) 
) %>% 
  mutate(main = names(benchmarks_wy)) %>% 
  left_join(grouped_mars_in, by = "main") %>% 
  mutate(name = str_remove(main,"grouped ") %>% paste0("_wy"),
         main = paste0(main, " by wy")) %>% 
  bind_rows(grouped_mars_w) %>% 
  rename(target_train = target_in) %>% 
  mutate(grouped = TRUE)


# get tbl for LM
fit_tbl <- fits_new %>% {
  tibble(predictions = map(., function(x){predict(x, df[!train,])}),
         fitted_values = map(., ~.x$fitted.values),
         coefficients = map(., ~.x$coefficients))
} %>%
  mutate(target = list(df[!train,c("w","y")])) %>% 
  mutate(target_train = list(df[train,c("w","y")]))


fit_tbl <- fit_tbl %>%
  mutate(R2 = map2(predictions, target, ~diag(caret::R2(.x,.y)))) %>%
  mutate(name = str_remove(names(fits_new), "\\.rds$"),
         deg_int = str_extract(name, "(?<=\\.)\\d$"),
         deg_int = ifelse(is.na(deg_int), 1, deg_int),
         class = str_extract(name, "(?<!\\.)\\d$|\\d(?=\\.)")) 

grouped_mars <- grouped_mars %>% mutate(class = str_extract(name, "\\d(?=\\.)"),
                        deg_int = str_extract(name, "(?<=\\.)\\d"))




# Random Forest -----------------------------------------------------------


rf_names <- list.files(paste0(model_dir,"_rf_reg_oob_new"))
rf_files <- lapply(list.files(paste0(model_dir,"_rf_reg_oob_new"), full.names = TRUE), readRDS)

names(rf_files) <- rf_names  

rf_models <- rf_files[!str_detect(rf_names,"train")]

train_rf <- rf_files[str_detect(rf_names,"train")][[1]]

sum(train_rf)



rf_all_tbl <- tibble(fit = rf_models) %>% 
  mutate(fit_w = map(fit,~.[["w"]]),
         fit_y = map(fit,~.[["y"]])) %>% 
  mutate(name = names(rf_models) %>% 
           str_remove("\\.rds$")) %>% 
  mutate(y_hat = map(fit_y, function(x){predict_rf(x, data = df[!train,])}),
         w_hat = map(fit_w, function(x){predict_rf(x, data = df[!train,])}),
         predictions = map2(w_hat,y_hat,cbind)
  ) %>% 
  mutate(y_hat = map(fit_y, function(x){predict_rf(x, data = df[train,])}),
         w_hat = map(fit_w, function(x){predict_rf(x, data = df[train,])}),
         fitted_values = map2(w_hat,y_hat,cbind)) %>% 
  mutate(predictions = map(predictions, data.frame)) %>% 
  mutate(predictions = map(predictions, ~set_names(.x, c("w", "y")))) %>% 
  mutate(fitted_values = map(fitted_values, data.frame)) %>% 
  mutate(fitted_values = map(fitted_values, ~set_names(.x, c("w", "y")))) %>% 
  mutate(target = list(df[!train,c("w","y")])) %>% 
  mutate(target_train = list(df[train,c("w","y")])) %>% 
  mutate(name = str_remove(name, "\\_reg\\_grid$"))


rf_all_tbl <- rf_all_tbl %>% 
  mutate(grouped = FALSE) %>% 
  mutate(deg_int = str_extract(name, "(?<=\\.)\\d"),
         deg_int = ifelse(is.na(deg_int), 1, deg_int),
         deg_int = as.character(deg_int),
         class = str_extract(name, "(?<!\\.)\\d|\\d(?=\\.)")) %>% 
  mutate(main = paste0("RF",class)) 



# Combine Models ----------------------------------------------------------

all_models <- bind_rows(
  fit_tbl %>% mutate(main = toupper(name), grouped = FALSE) %>% mutate(deg_int = as.character(deg_int)), 
  mars_tbl_new %>% mutate(deg_int = as.character(deg_int)), 
  grouped_mars,
  rf_all_tbl %>% select(-fit,-fit_w, -fit_y) %>% mutate(deg_int = as.character(deg_int))
) %>% 
  mutate(R2 = map2(predictions, target, ~diag(caret::R2(.x,.y)))) %>% 
  mutate(R2_in_sample = map2(fitted_values, target_train, ~diag(caret::R2(.x,.y)))) %>% 
  mutate(residuals = map2(target, predictions, ~.x-.y)) %>% 
  mutate(train_id = map(target, sample_train), version = "new") 


# Add tuned RF MBO -------------------------------------------
rf_mbo <- readRDS(paste0(model_dir,"_rf_reg_mlr3mbo_tuned2/rf_fitted_mbo.rds")) 

rf_mbo <- rf_mbo %>% mutate(name = ifelse(name == "RF3mbo", "RF3v2mbo",name)) %>% 
  mutate(name = paste0(name,num.trees),
         main = paste0(model, " (MBO, num.trees = ", num.trees,")")) %>% 
  mutate(R2_oob = map2(target_train, fitted_values_oob, R2_fun))

rf_mbo <- rf_mbo %>% mutate(class = str_extract(model,"\\d"))

# add tuned RF MBO new
rf_mbo2 <- readRDS(paste0(model_dir,"_rf_reg_mlr3mbo_tuned2/rf_fitted_mbo2.rds")) 

rf_mbo2 <- rf_mbo2 %>% mutate(name = str_remove(name, "v2")) %>% 
  mutate(name = paste0(name,num.trees),
         main = paste0(model, " (MBO updated, num.trees = ", num.trees,")")) %>% 
  mutate(R2_oob = map2(target_train, fitted_values_oob, R2_fun))

rf_mbo2 <- rf_mbo2 %>% mutate(class = str_extract(model,"\\d"))

# Add new LM --------------------------------------------------------------

lm_files <- load_models("lm_new2")


fits <- lm_files$models
train_lm <- lm_files$train$train_sel.rds
sum(lm_files$train$train_sel.rds)
sum(!lm_files$train$train_sel.rds)
rm(lm_files)
gc()

# models were trained ob train_sel
sapply(fits, nobs)
sum(train)

# get tbl for LM
fit_tbl <- fits %>% {
  tibble(predictions = map(., function(x){predict(x, df[!train,])}),
         fitted_values = map(., ~.x$fitted.values),
         coefficients = map(., ~.x$coefficients))
} %>%
  mutate(target = list(df[!train,c("w","y")])) %>% 
  mutate(target_train = list(df[train,c("w","y")]))


fit_tbl <- fit_tbl %>%
  mutate(R2 = map2(predictions, target, ~diag(caret::R2(.x,.y)))) %>%
  mutate(name = str_remove(names(fits), "\\.rds$"),
         deg_int = str_extract(name, "(?<=\\.)\\d$"),
         deg_int = ifelse(is.na(deg_int), 1, deg_int),
         class = str_extract(name, "(?<!\\.)\\d$|\\d(?=\\.)")) #%>% 


fit_tbl <- fit_tbl %>% mutate(main = paste0("LM",class,".",deg_int, " (non-nested)"))


# Add RF MBO and non-nested LM to all_models
all_models <- all_models %>% mutate(version = "old") %>% filter(!str_detect(name, "\\_w$")) %>% 
  mutate(name = toupper(name)) %>% 
  bind_rows(fit_tbl %>% mutate(version = "new"), rf_mbo %>% mutate(version = "new"), rf_mbo2 %>% mutate(version = "new"))

all_models %>% filter(str_detect(name, "500$")) %>% pull(name)


all_models %>% saveRDS(paste0(model_dir,"_combined2.rds"))
