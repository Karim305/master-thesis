## Fit copula to predictions and residuals


library(tidyverse)
library(copula)
library(VC2copula)
library(parallel)
# Load data & models model formulas----------------------------------------
code_path <- "XXXX"
source(paste0(code_path, "/01_setup.R"))
source(paste0(code_path, "/_utils/copula.R"))


all_models <- readRDS(paste0(model_dir,"_combined2.rds"))

# Create and set model and plot directories
# model_name <- "lm_test"
model_name <- "copula_new"

model_path <- paste0(model_dir,"_",model_name,"/")
fig_path <- paste0(fig_dir,"_",model_name,"/")

dir_create(model_path)
dir_create(fig_path)




# Estimate Empirical Copula -----------------------------------------------



pastedf <- function(...) {
  input_string <- paste0(...)
  if(!str_detect(input_string,"\\.pdf$")){
    input_string <- paste0(input_string,".pdf")
  }
  pdf(input_string)
}




# Select Copula -----------------------------------------------------------

# select_models <- c("lm2", "lm3", "MARS2.3", "MARS3.3",
#                    "MARS2Grouped",
#                    "rf2", "rf3", "rf4")
# 
# all_models <- all_models %>% filter(name %in% select_models)

# Get Copulas for all models ----------------------------------------------



# Fit "standard" Copulas
# all_models <- all_models %>%
#   mutate(select_fitCop = map2(predictions, train_id, ~fit_copula(.x[.y,])),
#          select_resCop = map2(residuals, train_id, ~fit_copula(.x[.y,])))

all_models <- all_models %>%
  mutate(select_resCop = map2(residuals, train_id, ~fit_copula(.x[.y,])))

# all_models <- all_models %>%
#   mutate(select_fitCop = map2(predictions, train_id, ~fit_copula(.x[.y,])))



# fit empirical and parametric Vine Copula to predictions
all_models <- all_models %>% 
  mutate(fitEmpCop = map2(predictions, train_id, ~fit_empCopula(.x[.y,])),
         fitVineCop = map2(predictions, train_id, ~fit_vineCopula(.x[.y,])))

# Now, fit empirical and parametric Vine Copula to residuals!
all_models <- all_models %>%
  mutate(resVineCop = map2(residuals, train_id, ~fit_vineCopula(.x[.y,])),
         resEmpCop = map2(residuals, train_id, ~fit_empCopula(.x[.y,])))


# Debugging
# test <- all_models %>% mutate(
#   # test = map2(predictions, train_id, ~.x[.y,]),
#   test <- map2(predictions, train_id, ~fit_copula(.x[.y,]))
# )
###########

# as it takes forever to run BiCopSelect for target for each model, create df ID
# to estimate copula only once per DF
all_models <- all_models %>% 
  group_by(target) %>% nest() %>% 
  ungroup() %>% mutate(df_id = row_number()) %>% 
  unnest(cols = c(data))

targets_tbl <- all_models %>% 
  distinct(target, df_id, train_id) %>% 
  mutate(dataVineCop = map2(target, train_id, ~fit_vineCopula(.x[.y,])),
         # select_dataCop = map2(target, train_id, ~fit_copula(.x[.y,])),
         dataEmpCop = map2(target, train_id, ~fit_empCopula(.x[.y,]))) %>% 
  select(-c(target,train_id))

# extract actual selected copulas
all_models <- all_models %>% 
  left_join(targets_tbl, by = "df_id") 

# # This code crashes due to try()
# all_models <- all_models %>% 
#   mutate(dataCop = map(select_dataCop, ~.x$selected_copula),
#          fitCop = map(select_fitCop, ~.x$selected_copula),
#          resCop = map(select_resCop, ~.x$selected_copula))

## This screws up the code!
## set names for list columns (easier subsetting)
# all_models <- all_models %>% 
#   map(~set_names(.x,all_models$name))

all_models %>% 
  # select(-c(w_hat,y_hat,fit)) %>% 
  saveRDS(paste0(model_path,"fitted_tbl.rds"))