# Script that implements grid search in caret

# Load data & create model formulas----------------------------------------
code_path <- "XXXX"
source(paste0(code_path, "/01_setup.R"))
library(sfsmisc)
library(plgraphics)
library(splines)
library(dplyr)
library(ranger) # faster
library(caret)

# Create formulas
# Create RHS
(v0 <- paste(additional_term, dummy_term, hh_term, sep = " + "))

(v1 <- paste(muni_vars2, collapse = " + "))

(v2 <- paste(sign_term, sep = " + ", collapse = " + "))

(v3 <- paste(shares_term, sep = " + ", collapse = " + "))

(v4 <- paste(vars4, collapse = " + "))


# f0 <- formula(paste("cbind(w,y) ~ age + ", v0))
f0 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", v0))
# f02 <- formula(paste("cbind(w,y) ~ age + ", paste(v1, muni_term, sep = " + ")))

# f1 <- formula(paste("cbind(w,y) ~ age + ", paste(v0,v1,sep = " + ")))
f1 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", paste(v0,v1,sep = " + ")))
# f12 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", paste(v1, muni_term, sep = " + ")))

# f2 <- formula(paste("cbind(w,y) ~ age + ", paste(v0, v1, v2, sep = " + ")))
f2 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", paste(v0, v1, v2, sep = " + ")))
# f22 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", paste(v1, v2, muni_term, sep = " + ")))

# f3 <- formula(paste("cbind(w,y) ~ age + ", paste(v0, v1, v2, v3, sep = " + ")))
f3 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", paste(v0, v1, v2, v3, sep = " + ")))
# f32 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", paste(v1, v2, v3, muni_term, sep = " + ")))

# f4 <- formula(paste("cbind(w,y) ~ age + ", paste(v0, v1, v2, v3, v4, sep = " + ")))
f4 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", paste(v0, v1, v2, v3, v4, sep = " + ")))
# f42 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", paste(v1, v2, v3, v4, muni_term, sep = " + ")))


# Create and set model and plot directories
model_name <- "rf_reg_oob_new"

model_path <- paste0(model_dir,"_",model_name,"/")
fig_path <- paste0(fig_dir,"_",model_name,"/")

dir_create(fig_path)
dir_create(model_path)

# Train-test-split
df_rf <- df
train <- readRDS(paste0(model_dir,"train.rds"))

df_train <- df_rf[train,]
df_test <- df_rf[!train,]

Y_train <- df_train[,1:2]
Y_test <- df_test[,1:2]

# X_train <- model.matrix(update.formula(f2, .~.-1-ns(age, df = 3) + age),df_train)
# Y_train <- sign_w[train]

saveRDS(train, paste0(model_path,"train.rds"))

library(parallel)
library(doParallel)

ncores <- detectCores()/2
cluster <- makePSOCKcluster(ncores)
registerDoParallel(cluster)
clusterSetRNGStream(cluster, 1994)

set.seed(1998)




## hyperparameters:
# - num.trees: number of trees
# - mtry: number of variables considered at each split
# - splitrule: criteria to split tree: Gini or entropy for sign_wy, MSE or MAE for reg
# - min.node.size: minimal size of resulting nodes for split to happen
# - max.depth: of ind. tree, control to avoid overfitting

# Based on rf_reg2 grid plot, refine tuning grid
mns_w <- c(25, 50, 75, 100, 150, 200, 225, 250, 500)
mtry_w <- c(7, 10, 12, 15, 18, 20, 23, 25,30,40, 50)

tuning_grid_w <- expand.grid(
  splitrule = c("variance"),
  mtry = mtry_w,
  min.node.size = mns_w
)

mns_y <- c(10, 15, 20, 23, 25, 30, 40, 50, 100)
mtry_y <- c(7, 10, 12, 15, 18, 20, 23, 25,30, 35, 50)


tuning_grid_y <- expand.grid(
  splitrule = c("variance"),
  mtry = mtry_y,
  min.node.size = mns_y
)

tuning_grid <- list(w = tuning_grid_w, y = tuning_grid_y)

# Set seeds
set.seed(123)

# for CV, length is = (n_repeats*nresampling)+1
# seeds <- vector(mode = "list", length = 6)
seeds <- vector(mode = "list", length = 2) # seeds for OOB

# for(i in 1:5) seeds[[i]]<- sample.int(n=1000, nrow(tuning_grid))
seeds[[1]]<- sample.int(n=1000, nrow(tuning_grid_w))

#for the last model
# seeds[[6]]<-sample.int(1000, 1)
seeds[[2]]<-sample.int(1000, 1)


# Set control parameters for train workflow
ctrl <- trainControl(method = "oob",
                     savePred=TRUE,
                     allowParallel = TRUE,
                     seeds = seeds,
                     search = "grid"
)

# Wrapper function around caret::train()
train2 <- function(x, y, method = "ranger", ctrl = ctrl, type = "Regression",
                   ncores = ncores, tuning_grid = tuning_grid, 
                   ...){
  
  out_w <- caret::train(x = x, y = Y_train$w, method = method, trControl = ctrl,
                        type = type, num.threads = ncores, tuneGrid = tuning_grid$w, ...)
  
  out_y <- caret::train(x = x, y = Y_train$y, method = method, trControl = ctrl,
                        type = type, num.threads = ncores, tuneGrid = tuning_grid$y, ...)
  
  list(w = out_w, y = out_y)
  
}



# RF 0 --------------------------------------------------------------------


system.time(
  rf0 <- train2(
    x = model.matrix(update.formula(f0, .~.-1-ns(age, df = 3) + age),df_train),
    y = Y_train,
    method = "ranger",
    ctrl = ctrl,      
    type = "Regression",
    ncores = ncores,
    tuning_grid = tuning_grid,
    keep.inbag = TRUE

  )           )


# Print the results
print(rf0)

saveRDS(rf0, paste0(model_path,"rf0_reg_grid.rds"))



# RF 1 --------------------------------------------------------------------

#
system.time(
  rf1 <- train2(
    x = model.matrix(update.formula(f1, .~.-1-ns(age, df = 3) + age),df_train),
    y = Y_train,
    method = "ranger",
    ctrl = ctrl,      
    type = "Regression",
    ncores = ncores,
    tuning_grid = tuning_grid,
    keep.inbag = TRUE

  )           )


# Print the results
print(rf1)

saveRDS(rf1, paste0(model_path,"rf1_reg_grid.rds"))



# RF 2 --------------------------------------------------------------------

system.time(
  rf2 <- train2(
    x = model.matrix(update.formula(f2, .~.-1-ns(age, df = 3) + age),df_train),
    y = Y_train,
    method = "ranger",    
    ctrl = ctrl,          
    type = "Regression",
    ncores = ncores,
    tuning_grid = tuning_grid,
    keep.inbag = TRUE

  )           )



# df_X_train <- data.frame(X_train)


# Print the results
print(rf2)

saveRDS(rf2, paste0(model_path,"rf2_reg_grid.rds"))



# RF 3 --------------------------------------------------------------------


system.time(
  rf3 <- train2(
    x = model.matrix(update.formula(f3, .~.-1-ns(age, df = 3) + age),df_train),
    y = Y_train,
    method = "ranger",       
    ctrl = ctrl,             
    type = "Regression",
    ncores = ncores,
    tuning_grid = tuning_grid,
    keep.inbag = TRUE
    
  )           )


# Print the results
print(rf3)

saveRDS(rf3, paste0(model_path,"rf3_reg_grid.rds"))



# RF 4 --------------------------------------------------------------------


system.time(
  rf4 <- train2(
    x = model.matrix(update.formula(f4, .~.-1-ns(age, df = 3) + age),df_train),
    y = Y_train,
    method = "ranger",            
    ctrl = ctrl,              
    type = "Regression",
    ncores = ncores,
    tuning_grid = tuning_grid,
    keep.inbag = TRUE
    
  )           )


# Print the results
print(rf4)

saveRDS(rf4, paste0(model_path,"rf4_reg_grid.rds"))


stopCluster(cluster)
registerDoSEQ()