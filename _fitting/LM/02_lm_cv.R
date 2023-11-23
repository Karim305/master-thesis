## CV LM & MARS Models (sensitivity check for sample selection)
library(mlr3verse)
library(tidyverse)
library(sfsmisc)


# Load data & create model formulas----------------------------------------
code_path <- "XXXX"
source(paste0(code_path, "/01_setup.R"))

library(plotmo)
# library(caret)

mlr_tasks

# Create and set model and plot directories
model_name <- "lm_cv"

model_path <- paste0(model_dir,"_",model_name,"/")
fig_path <- paste0(fig_dir,"_",model_name,"/")

dir_create(fig_path)
dir_create(model_path)

# (f1 <- update.formula(f1, .~.-ns(age, df = 3) + age))
# (f2 <- update.formula(f2, .~.-ns(age, df = 3) + age))
# (f3 <- update.formula(f3, .~.-ns(age, df = 3) + age))
# (f4 <- update.formula(f4, .~.-ns(age, df = 3) + age))

# models <- c("f1"=f1, "f2"=f2, "f3"=f3, "f4"=f4)



length(train)
sum(train)
df_test <- df[!train,]
df_train <- df[train,]


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

(v3 <- paste(shares_term, sep = " + ", collapse = " + "))

(v4 <- paste(vars4, collapse = " + "))

# Create formulas
f0 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", v0))
# f02 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", v0))
# f02 <- formula(paste("cbind(w,y) ~ age + ", paste(v1, muni_term, sep = " + ")))

f1 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", paste(v0,v1,sep = " + ")))
# f12 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", paste(v0,v1,sep = " + ")))
# f12 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", paste(v1, muni_term, sep = " + ")))

f2 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", paste(v0, v1, v2, sep = " + ")))
f22 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", paste(v0, v1, v2, sep = " + ")))
# f22 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", paste(v1, v2, muni_term, sep = " + ")))

f3 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", paste(v0, v1, v2, v3, sep = " + ")))
f32 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", paste(v0, v1, v3, sep = " + ")))
# f32 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", paste(v1, v2, v3, muni_term, sep = " + ")))

f4 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", paste(v0, v1, v2, v3, v4, sep = " + ")))
f42 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", paste(v0, v1, v4, sep = " + ")))
# f42 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", paste(v1, v2, v3, v4, muni_term, sep = " + ")))





R2_fun <- function(Y, Y_hat){
  residuals <- Y - Y_hat
  # TSS
  mean <- apply(Y,2,mean)
  TSS <- apply(sweep(Y,2,mean)^2,2,sum)
  # RSS
  RSS <- apply(residuals^2,2,sum)
  1-RSS/TSS
}

k_fold_cv_r_squared <- function(data, k = 10, model_function = function(x){lm(f0, x)}, 
                                seed = 123, return_all = FALSE) {
  set.seed(seed)
  # Split data into k folds
  folds <- split(data, sample(1:k, nrow(data), replace = TRUE))
  
  r_squared_values_w <- numeric(k)
  r_squared_values_y <- numeric(k)
  
  for (i in 1:k) {
    # Create training and test sets
    test_set <- folds[[i]]
    target <- test_set[,c("w","y")]
    train_set <- do.call(rbind, folds[-i])
    
    # Fit the model on the training set
    model <- model_function(train_set)
    
    # Predict on the test set
    predictions <- predict(model, newdata = test_set)

    
    r_squared <- R2_fun(Y = target, Y_hat = predictions)
    
    r_squared_values_w[i] <- r_squared[1]
    r_squared_values_y[i] <- r_squared[2]
  }
  if(!return_all){
    c(w=mean(r_squared_values_w), y = mean(r_squared_values_y)) 
  } else {
    cbind(w=r_squared_values_w, y = r_squared_values_y)
  }
  
  
}

train_test_split_r_squared <- function(train_set = df_train, test_set = df_test, 
                                       model_function) {
  
  target <- test_set[,c("w","y")]
  
  # Fit the model on the training set
  model <- model_function(train_set)
  
  # Predict on the test set
  predictions <- predict(model, newdata = test_set)
  
  r_squared <- R2_fun(Y = target, Y_hat = predictions)
  
  c(w=r_squared[1], y = r_squared[2])
  
}


lm_list <- list(lm0 = function(x){lm(f0, x)},
                lm1 = function(x){lm(f1, x)},
                lm2 = function(x){lm(f2, x)},
                lm3 = function(x){lm(f3, x)},
                lm4 = function(x){lm(f4, x)})

lm_cv <- sapply(lm_list, function(x){k_fold_cv_r_squared(data = df, model_function = x)})

lm_train_test <- sapply(lm_list,
                        function(x){train_test_split_r_squared(train_set = df_train,
                                                               test_set = df_test, model_function = x)})





xtable::xtable(lm_cv, digits = 3, caption = "Out-of-sample R-Squared values obtained via 10-Fold Cross-Validation")

xtable::xtable(lm_train_test, digits = 3, caption = "Out-of-sample R-Squared values obtained via train-test-split")



cv_lm0 <- k_fold_cv_r_squared(data = df, model_function = lm_list[[1]])
cv_lm0_vec <- k_fold_cv_r_squared(data = df, model_function = lm_list[[1]], return_all = TRUE)




# library(parallel)
# 
# k_fold_cv_r_squared_parallel <- function(data, k = 10, model_function = function(x){lm(f0, x)}, 
#                                          seed = 123, return_all = FALSE, cores = NULL) {
#   set.seed(seed)
#   
#   # Split data into k folds
#   folds <- split(data, sample(1:k, nrow(data), replace = TRUE))
#   
#   # Define a function to compute R-squared for each fold
#   compute_fold <- function(i) {
#     # Create training and test sets
#     test_set <- folds[[i]]
#     target <- test_set[,c("w","y")]
#     train_set <- do.call(rbind, folds[-i])
#     
#     # Fit the model on the training set
#     model <- model_function(train_set)
#     
#     # Predict on the test set
#     predictions <- predict(model, newdata = test_set)
#     
#     # Calculate R-squared for this fold
#     r_squared <- R2_fun(Y = target, Y_hat = predictions)
#     
#     return(list(w = r_squared[1], y = r_squared[2]))
#   }
#   
#   # Use mclapply to parallelize the computation over the k folds
#   max_cores <- detectCores()/2
#   if(is.null(cores)){
#     num_cores <- min(max_cores,k)
#   } else {
#     num_cores <- cores
#   }
#   
#   # Check if the system is Windows and use parLapply if true
#   if (.Platform$OS.type == "windows") {
#     cl <- makeCluster(num_cores)
#     results <- parLapply(cl, 1:k, compute_fold)
#     stopCluster(cl)
#   } else {
#     results <- mclapply(1:k, compute_fold, mc.cores = num_cores)
#   }
#   
#   r_squared_values_w <- sapply(results, function(x) x$w)
#   r_squared_values_y <- sapply(results, function(x) x$y)
#   
#   if(!return_all){
#     return(c(w = mean(r_squared_values_w), y = mean(r_squared_values_y)))
#   } else {
#     return(cbind(w = r_squared_values_w, y = r_squared_values_y))
#   }
# }



lm_list2 <- list(LM0 = function(x){lm(f0, x)},
                 LM1 = function(x){lm(f1, x)},
                 LM2 = function(x){lm(f2, x)},
                 LM3 = function(x){lm(f3, x)},
                 LM4 = function(x){lm(f4, x)},
                 LM0.2 = function(x){lm(update.formula(f0,.~(.)^2), x)},
                 LM1.2 = function(x){lm(update.formula(f1,.~(.)^2), x)},
                 LM2.2 = function(x){lm(update.formula(f2,.~(.)^2), x)})



system.time(
  lm_cv <- lapply(lm_list2, function(x){k_fold_cv_r_squared(data = df, model_function = x, return_all = TRUE)})
)

saveRDS(lm_cv, paste0(fig_path,"lm_cv.rds"))

system.time(
  lm_split <- lapply(lm_list2, function(x){train_test_split_r_squared(data = df, model_function = x, return_all = FALSE)})
)

saveRDS(lm_split, paste0(fig_path,"lm_split.rds"))





repeated_cv <- function(data, n = 10, model_function = function(x){lm(f0, x)}, 
                        p_train = 0.5, seed = 123, return_all = TRUE) {
  set.seed(seed)
  # Split data into k folds
  
  
  r_squared_values_w <- numeric(n)
  r_squared_values_y <- numeric(n)
  
  for (i in 1:n) {
    
    # Create training and test sets
    train_id <- sample(c(TRUE,FALSE), size = nrow(data), replace = TRUE, prob = c(p_train,1-p_train))
    test_set <- data[train_id,]
    target <- test_set[,c("w","y")]
    train_set <- data[!train_id,]
    
    # Fit the model on the training set
    model <- model_function(train_set)
    
    # Predict on the test set
    predictions <- predict(model, newdata = test_set)
    
    # Calculate R-squared for this fold
    # ss_total <- sum((test_set$y - mean(test_set$y))^2)
    # ss_res <- sum((test_set$y - predicted_values)^2)
    # r_squared <- 1 - (ss_res / ss_total)
    
    r_squared <- R2_fun(Y = target, Y_hat = predictions)
    
    r_squared_values_w[i] <- r_squared[1]
    r_squared_values_y[i] <- r_squared[2]
  }
  if(!return_all){
    c(w=mean(r_squared_values_w), y = mean(r_squared_values_y)) 
  } else {
    cbind(w=r_squared_values_w, y = r_squared_values_y)
  }
  
  
}




lm_list2 <- list(LM0 = function(x){lm(f0, x)},
                 LM1 = function(x){lm(f1, x)},
                 LM2 = function(x){lm(f2, x)},
                 LM3 = function(x){lm(f3, x)},
                 LM4 = function(x){lm(f4, x)},
                 LMnn3 = function(x){lm(f32, x)},
                 LMnn4 = function(x){lm(f42, x)},
                 LM0.2 = function(x){lm(update.formula(f0,.~(.)^2), x)},
                 LM1.2 = function(x){lm(update.formula(f1,.~(.)^2), x)},
                 LM2.2 = function(x){lm(update.formula(f2,.~(.)^2), x)})

library(earth)

mars_list <- list(
  mars2.3 = function(x){earth(f2, data = x, degree = 3,nk=500, thresh=1e-05)},
  mars2.4 = function(x){earth(f2, data = x, degree = 4,nk=500, thresh=1e-05)},
  mars3.3 = function(x){earth(f3, data = x, degree = 3,nk=500, thresh=1e-05)},
  mars3.4 = function(x){earth(f3, data = x, degree = 4,nk=500, thresh=1e-05)},
  mars4.3 = function(x){earth(f4, data = x, degree = 3,nk=500, thresh=1e-05)},
  mars4.4 = function(x){earth(f4, data = x, degree = 4,nk=500, thresh=1e-05)})



system.time(
  lm_rep_cv <- lapply(lm_list2, function(x){repeated_cv(data = df, model_function = x, return_all = TRUE)})
)

saveRDS(lm_rep_cv, paste0(fig_path,"lm_rep_cv.rds"))


system.time(
  mars_rep_cv <- lapply(mars_list, function(x){repeated_cv(data = df, model_function = x, return_all = TRUE)})
)

saveRDS(mars_rep_cv, paste0(fig_path,"mars_rep_cv.rds"))


