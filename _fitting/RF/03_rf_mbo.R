# Hyperparameter Tuning via Bayesian Optimization using mlr3mbo

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

# which models do you want to tune?
run_w <- TRUE
run_y <- TRUE


# parameters
cores <- 10

n_trees <- 500
n_iter <- 100
n_design <- 29


# Create RHS
(v0 <- paste(additional_term, dummy_term, hh_term, sep = " + "))

(v1 <- paste(muni_vars2, collapse = " + "))

(v2 <- paste(sign_term, sep = " + ", collapse = " + "))

(v3 <- paste(vars3, sep = " + ", collapse = " + "))

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


# Due to memory limitations, models need to be run sequentially (in different sessions)
# formulas <- list("f2" = f2, "f3" = f3, "f4" = f4)
formulas <- list("f4" = f4)


df_train <- df[train,]




# 
# bayesopt_ego2 <- function (instance, surrogate, acq_function, acq_optimizer, 
#           init_design_size = NULL, random_interleave_iter = 6L) 
# {
#   assert_r6(instance, "OptimInstanceSingleCrit")
#   assert_r6(surrogate, classes = "Surrogate")
#   assert_r6(acq_function, classes = "AcqFunction")
#   assert_r6(acq_optimizer, classes = "AcqOptimizer")
#   assert_int(init_design_size, lower = 1L, null.ok = TRUE)
#   assert_int(random_interleave_iter, lower = 0L)
#   search_space = instance$search_space
#   if (is.null(init_design_size) && instance$archive$n_evals == 
#       0L) {
#     init_design_size = 4L * search_space$length
#   }
#   if (!is.null(init_design_size) && instance$archive$n_evals == 
#       0L) {
#     design = generate_design_sobol(search_space, n = init_design_size)$data
#     instance$eval_batch(design)
#   }
#   surrogate$archive = instance$archive
#   acq_function$surrogate = surrogate
#   acq_optimizer$acq_function = acq_function
#   repeat {
#     xdt = tryCatch({
#       if (isTRUE((instance$archive$n_evals - init_design_size + 
#                   1L)%%random_interleave_iter == 0)) {
#         stop(set_class(list(message = "Random interleaving", 
#                             call = NULL), classes = c("random_interleave", 
#                                                       "mbo_error", "error", "condition")))
#       }
#       acq_function$surrogate$update()
#       acq_function$update()
#       acq_optimizer$optimize()
#     }, mbo_error = function(mbo_error_condition) {
#       lg$info(paste0(class(mbo_error_condition), collapse = " / "))
#       lg$info("Proposing a randomly sampled point")
#       generate_design_random(search_space, n = 1L)$data
#     })
#     instance$eval_batch(xdt)
#     if (instance$is_terminated) 
#       break
#   }
#   return(invisible(instance))
# }
# 
# class(bayesopt_ego2) = "loop_function"
# attr(bayesopt_ego2, "id") = "bayesopt_ego2"
# attr(bayesopt_ego2, "label") = "Efficient Global Optimization"
# attr(bayesopt_ego2, "instance") = "single-crit"
# attr(bayesopt_ego2, "man") = "mlr3mbo::mlr_loop_functions_ego"
# 
# mlr_loop_functions$add("bayesopt_ego2", bayesopt_ego2)



# Create and set model and plot directories
model_name <- "rf_reg_mlr3mbo_parallel3"

model_path <- paste0(model_dir,"_",model_name,"/")
fig_path <- paste0(fig_dir,"_",model_name,"/")

dir_create(fig_path)
dir_create(model_path)

# Load Grid Search results in order to use best parameters as initial design point
cv_grid1 <- readRDS(paste0(model_dir,"_rf_reg_oob_new/rf1_reg_grid.rds"))
cv_grid2 <- readRDS(paste0(model_dir,"_rf_reg_oob_new/rf2_reg_grid.rds"))
cv_grid3 <- readRDS(paste0(model_dir,"_rf_reg_oob_new/rf3_reg_grid.rds"))
cv_grid4 <- readRDS(paste0(model_dir,"_rf_reg_oob_new/rf4_reg_grid.rds"))

opt_pars_grid <- bind_rows(
  bind_rows(cv_grid1$w$bestTune,cv_grid1$y$bestTune) %>% mutate(mod = c("f1", "f1"), var = c("w", "y")),
  bind_rows(cv_grid2$w$bestTune,cv_grid2$y$bestTune) %>% mutate(mod = c("f2", "f2"), var = c("w", "y")),
  bind_rows(cv_grid3$w$bestTune,cv_grid3$y$bestTune) %>% mutate(mod = c("f3", "f3"), var = c("w", "y")),
  bind_rows(cv_grid4$w$bestTune,cv_grid4$y$bestTune) %>% mutate(mod = c("f4", "f4"), var = c("w", "y"))
) %>% select(-splitrule)

saveRDS(opt_pars_grid, paste0(model_path, "opt_pars_grid.rds"))
opt_pars_grid <- readRDS(paste0(model_path, "opt_pars_grid.rds"))


# Set up cluster
cl = makeCluster(cores)

# Set seed
set.seed(123)




# Surrogate GP Model
surrogate = srlrn(lrn("regr.km",
                      # matern kernel
                      covtype = "matern3_2",
                      # rgenoud optimization method
                      optim.method = "gen",
                      # nugget.estim for noisy functions (function is noisy due to test-train split)
                      nugget.estim = TRUE,
                      jitter = 1e-6,
                      control = list(trace = FALSE)))

# EI Acquisition function
acq_function = acqf("ei")
acq_optimizer = acqo(opt("focus_search", n_points = 100L, maxit = 9),
                     terminator = trm("evals", n_evals = 3000))


# Resampling method
# resampling = rsmp("cv", folds = 3)
resampling = rsmp("insample")
# resampling = rsmp("holdout")
# GoF measure
measure = msr("oob_error")


# Loop over models

for(i in seq_along(formulas)){
  model_name <- names(formulas)[i]
  form <- formulas[[i]]
  f_w <- update.formula(form, w ~ .)
  f_y <- update.formula(form, y ~ .)
  
  df_w <- model.frame(f_w, df_train)
  df_y <- model.frame(f_y, df_train)
  
  opt_grid <- opt_pars_grid[opt_pars_grid$mod == model_name,]
  
  # if best previous mtry is out of bound, replace it with maximum possible mtry
  # mm <- model.matrix(f_w, df_w)[1:10,]
  # mtry_max <- ncol(mm)-1
  
  mtry_max <- ncol(df_w) - 1
  opt_grid[opt_grid$mtry > mtry_max,"mtry"] <- mtry_max
  
  
  # BO for wealth
  if(isTRUE(run_w)){
    
    
    df_i <- df_w


    task = TaskRegr$new(id = "w_reg", backend = df_i, target = "w")
    learner = lrn("regr.ranger",
                  num.trees = n_trees,
                  mtry = to_tune(1, mtry_max),
                  min.node.size = to_tune(1, 2000),
                  respect.unordered.factors = "order", # equivalent to TRUE
                  keep.inbag = TRUE, oob.error = TRUE)

    set_threads(learner, n = cores)
    



    instance = TuningInstanceSingleCrit$new(
      task = task,
      learner = learner,
      resampling = resampling,
      measure = measure,
      terminator = trm("evals", n_evals = n_iter),
      store_models = TRUE # necessary for algorithm to work..
    )

    # set initial points to be evaluated (including best previous solution)
    initial_design <- generate_design_lhs(instance$search_space, n = n_design)$data %>% bind_rows(
      opt_grid[opt_grid$var == "w", c("mtry", "min.node.size")]
    )
    run_time <- system.time(
      {
        # evaluate initial design
        instance$eval_batch(initial_design)
        
        
        tuner = tnr("mbo",
                    bayesopt_ego,
                    surrogate = surrogate,
                    acq_function = acq_function,
                    acq_optimizer = acq_optimizer)
        
        # run BO
        tuner$optimize(instance)
        
      }
    )
    
    instance$result
    instance$archive

    result <- list(result = instance$result, archive = instance$archive$data, run_time = run_time)

    # Save instance
    saveRDS(result, paste0(model_path, model_name,"_w_mlr3mbo_RF.rds"))

    # Create plots
    p <- autoplot(instance, type = "surface") + ggplot2::ggtitle(paste0("RF ",model_name," w: Hyperparameter Tuning with Bayesian Optimization"))
    pdf(paste0(fig_path, model_name,"_w_mlr3mbo_RF_surface.pdf"))
    print(p)
    dev.off()


    p <- autoplot(instance, type = "performance") + ggplot2::ggtitle(paste0("RF ",model_name," w: Hyperparameter Tuning with Bayesian Optimization"))
    pdf(paste0(fig_path, model_name,"_w_mlr3mbo_RF_performance.pdf"))
    print(p)
    dev.off()


    p <- autoplot(instance, type = "parallel") + ggplot2::ggtitle(paste0("RF ",model_name," w: Hyperparameter Tuning with Bayesian Optimization"))
    pdf(paste0(fig_path, model_name,"_w_mlr3mbo_RF_parallel.pdf"))
    print(p)
    dev.off()


    p <- autoplot(instance, type = "points") + ggplot2::ggtitle(paste0("RF ",model_name," w: Hyperparameter Tuning with Bayesian Optimization"))
    pdf(paste0(fig_path, model_name,"_w_mlr3mbo_RF_points.pdf"))
    print(p)
    dev.off()


    rm(instance)
    gc()

    
  }
  
  # BO for income
  if(isTRUE(run_y)){
    
    
    df_i <- df_y
    task = TaskRegr$new(id = "y_reg", backend = df_i, target = "y")
    learner = lrn("regr.ranger",
                  num.trees = n_trees,
                  mtry = to_tune(1, ncol(df_i)-1),
                  min.node.size = to_tune(1, 2000),
                  respect.unordered.factors = "order", 
                  keep.inbag = TRUE, oob.error = TRUE)
    
    set_threads(learner, n = cores)
    
    instance = ti(
      task = task,
      learner = learner,
      resampling = resampling,
      measure = measure,
      terminator = trm("evals", n_evals = n_iter),
      store_models = TRUE # necessary for algorithm to work..
    )
    
    initial_design <- generate_design_lhs(instance$search_space, n = n_design)$data %>% bind_rows(
      opt_grid[opt_grid$var == "y", c("mtry", "min.node.size")]  
    )
    
    run_time <- system.time(
    
      {
        
        instance$eval_batch(initial_design)
        
        
        tuner = tnr("mbo",
                    loop_function = bayesopt_ego,
                    surrogate = surrogate,
                    acq_function = acq_function,
                    acq_optimizer = acq_optimizer)
        
        
        tuner$optimize(instance)
        
        
      }
      
      )
    
    instance$result
    run_time

    result <- list(result = instance$result, archive = instance$archive$data, run_time = run_time)
    
    # Save instance
    saveRDS(result, paste0(model_path, model_name,"_y_mlr3mbo_RF.rds"))
    
    # Create plots
    p <- autoplot(instance, type = "surface") + ggplot2::ggtitle(paste0("RF ",model_name," y: Hyperparameter Tuning with Bayesian Optimization"))
    pdf(paste0(fig_path, model_name,"_y_mlr3mbo_RF_surface.pdf"))
    print(p)
    dev.off()
    
    
    p <- autoplot(instance, type = "performance") + ggplot2::ggtitle(paste0("RF ",model_name," y: Hyperparameter Tuning with Bayesian Optimization"))
    pdf(paste0(fig_path, model_name,"_y_mlr3mbo_RF_performance.pdf"))
    print(p)
    dev.off()
    
    
    p <- autoplot(instance, type = "parallel") + ggplot2::ggtitle(paste0("RF ",model_name," y: Hyperparameter Tuning with Bayesian Optimization"))
    pdf(paste0(fig_path, model_name,"_y_mlr3mbo_RF_parallel.pdf"))
    print(p)
    dev.off()
    
    
    p <- autoplot(instance, type = "points") + ggplot2::ggtitle(paste0("RF ",model_name," y: Hyperparameter Tuning with Bayesian Optimization"))
    pdf(paste0(fig_path, model_name,"_y_mlr3mbo_RF_points.pdf"))
    print(p)
    dev.off()
    
    
    rm(instance)
    gc()
  }
}
