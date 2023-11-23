
# Copula Functions

# Estimate Empirical Copula -----------------------------------------------


fit_vineCopula <- function(data){
  pobs <- copula::pobs(data)
  VineCopula::BiCopSelect(pobs[,1], pobs[,2], familyset = NA)
}

fit_empCopula <- function(data){
  copula::empCopula(copula::pobs(data), smoothing = "beta")
}



fit_copula <- function(data,
                       copulas = list(normal = copula::normalCopula(dim = 2, dispstr = "un"),
                                      t = copula::tCopula(dim = 2, dispstr = "un"),
                                      joe = copula::joeCopula(dim = 2),
                                      clayton = copula::claytonCopula(dim = 2),
                                      gumbel = copula::gumbelCopula(dim = 2),
                                      amh = copula::amhCopula(dim = 2L),
                                      frank = copula::frankCopula(dim = 2L),
                                      plackett = copula::plackettCopula(dim = 2L)),
                       num_cores = NULL){
  stopifnot(is.list(copulas))
  # transform data to unit interval
  pobs <- copula::pobs(data)
  
  # if num_cores not provided, defaults to number of models
  if (is.null(num_cores)) num_cores = length(copulas)
  
  # if num_cores == 1, don't parallelize
  if(num_cores == 1){
    # fit models using sapply
    fit <- sapply(copulas,
                  function(x) try(copula::fitCopula(x, data = pobs, method = "mpl")))
  } else {
    # if num_cores > 1, parallelize
    
    fun <- function(cop){
      set.seed(2023)
      try(copula::fitCopula(copula = cop, data = pobs, method = "mpl"))
    }
    # setup cluster and export pobs to each worker
    cl <- parallel::makeCluster(num_cores)
    parallel::clusterExport(cl, varlist=c("pobs")) 
    fit <- parallel::parLapply(cl, copulas, fun)
    parallel::stopCluster(cl)
    
  }
  
  fit
  
}


select_copula <- function(fit, criterion = "AIC"){
  
  # get AIC and BIC for all models
  results <- rbind(
    AIC = sapply(fit, AIC),
    BIC = sapply(fit, BIC)
  )
  
  # select best model based on criterion (AIC or BIC)
  sel_id <- which.min(results[criterion,])
  selected_model <- fit[[sel_id]]
  selected_copula <- selected_model@copula
  
  # print results table to console
  print(results)
  
  # return list with all useful objects
  list(selected_copula = selected_copula, all_models = fit, IC_table = results)
  
}
 

pastedf <- function(...) {
  input_string <- paste0(...)
  if(!str_detect(input_string,"\\.pdf$")){
    input_string <- paste0(input_string,".pdf")
  }
  pdf(input_string)
}



contour_copula <- function(fitCop, empCop, vineCop = NULL, FUN = pCopula, 
                           region = FALSE, main = ""){
  
  
  require(latticeExtra)
  
  contRes <- contourplot2(fitCop, FUN = FUN, region = FALSE, labels = FALSE, col = 2, lty = 2, main = main)
  
  if(!is.null(vineCop)){
    
    if(class(vineCop) == "BiCop") vineCop <- VC2copula::BiCop2copula(vineCop)
    
    contVine <- contourplot2(vineCop, FUN = FUN, region = FALSE, labels = FALSE, col = 3, lty = 3, main = main)
    
    contEmp <- contourplot2(empCop, FUN = FUN, region = FALSE, labels = TRUE, main = main,
                            key = list(corner = c(0.04, 0.04),
                                       lines = list(col = 1:3, lwd = 2, lty = 1:3),
                                       text = list(c("Empirical Copula",
                                                     paste0("Fitted ", class(fitCop)),
                                                     paste0("Fitted ", class(vineCop))))
                            ))
    
    
    
    contEmp + contRes + contVine
    
  } else {
    contEmp <- contourplot2(empCop, FUN = FUN, region = FALSE, labels = TRUE, main = main,
                            key = list(corner = c(0.04, 0.04),
                                       lines = list(col = 1:2, lwd = 2, lty = 1:2),
                                       text = list(c("Empirical Copula",
                                                     paste0("Fitted ", class(fitCop))))))
    contEmp + contRes
    
  }
  
}


# Plotting Functions ------------------------------------------------------


plot_contour <- function(fitCop, dataCop, 
                         predictions = NULL, target = NULL, main = "", add_main,...){
  
  
  if(is.null(predictions)&is.null(target)){
    N <- 4
    plot_pred <- FALSE
  } else {
    N <- 6
    plot_pred <- TRUE
  }
  
  fit_main <- capture.output(print(fitCop))
  data_main <- capture.output(print(dataCop))
  
  sfsmisc::mult.fig(N, main = paste0("Copula ",main), marP = c(-1,-1,-1,0))
  contour(fitCop, margins = "unif", main = fit_main,
          cex.main = 0.8)
  contour(dataCop, col = 2, margins = "unif", main = data_main,
          cex.main = 0.8)
  contour(fitCop,  main = add_main, cex.main = 0.8)
  contour(dataCop, col = 2,  main = "Data", cex.main = 0.8)
  if(plot_pred){
    
    lim <- rbind(
      pmin(apply(predictions,2,min),apply(target,2,min)),
      pmax(apply(predictions,2,max),apply(target,2,max))
    )
    
    
    smoothScatter(predictions, main = add_main, xlim = lim[,"w"], ylim = lim[,"y"], cex.main = 0.8)
    # contour(copLM2,  add = TRUE)
    smoothScatter(target, main = "Data", xlim = lim[,"w"], ylim = lim[,"y"], cex.main = 0.8)
  }
  
}



plot_contour_emp <- function(fitCop, dataCop, 
                             predictions = NULL, target = NULL, main = "", ...){
  
  
  if(is.null(predictions)&is.null(target)){
    N <- 4
    plot_pred <- FALSE
  } else {
    N <- 6
    plot_pred <- TRUE
  }
  
  sfsmisc::mult.fig(N, main = paste0("Copula ",main), marP = c(-1,-1,-1,0))
  contour(fitCop, dCopula, main = "Predictions (dCopula)")
  contour(dataCop, dCopula, col = 2, main = "Data (dCopula)")
  contour(fitCop, pCopula, main = "Predictions (pCopula)")
  contour(dataCop, pCopula, col = 2, main = "Data (pCopula)")
  if(plot_pred){
    
    lim <- rbind(
      pmin(apply(predictions,2,min),apply(target,2,min)),
      pmax(apply(predictions,2,max),apply(target,2,max))
    )
    
    
    smoothScatter(predictions, main = "Predictions", xlim = lim[,"w"], ylim = lim[,"y"])
    # contour(copLM2,  add = TRUE)
    smoothScatter(target, main = "Data", xlim = lim[,"w"], ylim = lim[,"y"])
  }
  
}



pdf2 <- function(dir){
  fig_dir <- get("fig_dir")
  # fig_dir <- "C:/Users/kelouaghlidi/Dropbox/JointWealthIncome/_Karim/_output/_plots/_general/"
  dir2 <- stringr::str_replace(dir, "\\/{2,}", "\\/")
  # path <- stringr::str_extract(dir2,".*(?=\\/\\w+\\.pdf$)")
  path <- stringr::str_extract(dir2,".*(?=\\/)")
  
  if(!dir.exists(path)) dir.create(path, recursive = TRUE)
  pdf(dir2)
}

plot_contour_tbl <- function(fitCop, dataCop, name, main, predictions, target, 
                             file = "contour_plot", path = fig_path, add_main = "Residuals",
                             fun = plot_contour, cex.main = 0.8, ...){
  
  # deparse(substitute((fit_df$fit[[1]]$call)))
  
  
  # fitCop <- all_models$resCop[[1]]
  # dataCop <- all_models$dataCop[[1]]
  # name <- all_models$name[[1]]
  # main <- all_models$main[[1]]
  # predictions <- all_models$predictions[[1]]
  # target <- all_models$target[[1]]
  
  
  file <- paste0(path,name,"_",file,".pdf")
  
  pdf2(file)
  
  main <- paste0(main, " ", add_main)
  
  fun(fitCop = fitCop, dataCop = dataCop, predictions = predictions, 
      target = target, main = main, add_main = add_main, cex.main = cex.main)
  
  
  dev.off()
  
}


# # plot marginal density from nor1mix object
# plot_marginal_density <- function(x, m = 2, bw = "SJ-dpi", main = NULL, n = NULL,
#                                   maxiter = 100, prob = 0.05, seed = 2023,
#                                   marginal = NULL){
#   
#   set.seed(seed)
#   sam_n <- round(prob*length(x))
#   
#   if(is.null(n)) {
#     n <- length(x)
#     
#   } else {
#     x <- sample(x = x, size = n)
#   }
#   
#   if(!is.null(marginal)){
#     stopifnot(nor1mix::is.norMix(marginal))
#     m <- nrow(marginal)
#     mix <- marginal
#   } else {
#     mix <- nor1mix::norMixEM(x,m, maxiter = maxiter)
#   }
#   
#   if(is.null(main)) main <- paste0(m, " Mixture Components")
#   
#   # simulate from mixture model
#   sim <- rnorMix(n, mix)
#   plot(density(x, bw = bw), col = 1, main = main) # plot data density
#   rug2(x)
#   lines(mix, col = 2, lty = 2) # plot mixture
#   lines(density(sim, bw = bw), col = 3, lty = 3) # plot simulations from mixture
#   legend("topleft", legend = c("Data","GMM", paste0("GMM Simulations \n N = ",n)), 
#          col = 1:3, lty = 1:3, bg = "transparent")
# }




plot_marginal_density <- function(x, x_sim = NULL, m = 2, bw = "SJ-dpi", main = NULL, n = NULL,
                                  maxiter = 100, prob = 0.05, seed = 2023, marginal = NULL){
  
  set.seed(seed)
  sam_n <- round(prob*length(x))
  
  
  if(is.null(n)) {
    n <- length(x)
    
  } else {
    x <- sample(x = x, size = n)
    if(!is.null(x_sim)){
      x_sim <- sample(x = x_sim, size = n)  
    }
  }
  if(is.null(x_sim)){
    if(!is.null(marginal)){
      stopifnot(nor1mix::is.norMix(marginal))
      m <- nrow(marginal)
      mix <- marginal
    } else {
      mix <- nor1mix::norMixEM(x,m, maxiter = maxiter)
    }
    if(is.null(main)) main <- paste0(m, " Mixture Components")
    
    # simulate from mixture model
    sim <- rnorMix(n, mix)
    plot(density(x, bw = bw), col = 1, main = main) # plot data density
    rug2(x)
    lines(mix, col = 2, lty = 2) # plot mixture
    lines(density(sim, bw = bw), col = 3, lty = 3) # plot simulations from mixture
    legend("topleft", legend = c("Data","GMM", paste0("GMM Simulations \n N = ",n)), 
           col = 1:3, lty = 1:3, bg = "transparent")
    
  } else {
    
    
    rel_ticks <- c(-10000, 0, 10000, 50000, 100000, 250000, 1000000, 10000000)
    # rel_labs <- paste0("CHF ", c("0","10K", "50K", "100K", "250K", "1M", "10M"))
    rel_labs <- c("-10k", "0","10k", "50k", "100k", "250k", "1M", "10M")
    # rel_loc <- log10(rel_ticks)
    rel_loc <- ihs10(rel_ticks)
    plot(density(x, bw = bw), col = 1, main = main, axes = FALSE, xlab = "CHF")
    axis(1, at = rel_loc, labels = rel_labs)
    axis(2)
    rug2(x)
    lines(density(x_sim, bw = bw), col = 2, lty = 2) # plot simulations from mixture
    legend("topleft", legend = c("Original Data",paste0("Simulated Data \n N = ",n)), 
           col = 1:2, lty = 1:2, bg = "transparent")
    
  }
  
  
  

}




# smoothPairs plots for pseudo-obs ----------------------------------------

plot_pobs <- function(X, main = NULL){
  if(is.null(main)) 
    main <- "Pseudo-Observations"
  else 
    main <- paste0("Pseudo-Observations - ", main)
  
  pobs <- pobs(X)
  smoothPairs(data.frame(pobs), diag.panel = function(x){SciViews::panel.hist(x, breaks = 20)},
              main = main)
}




# Tests -------------------------------------------------------------------


KStest2_n <- function(x, y, n = 10000, seed = 1994, ...){
  stopifnot(dim(x) == dim(y))
  set.seed(seed)
  N <- nrow(x)
  id <- sample(1:N, n)
  
  fasano.franceschini.test::fasano.franceschini.test(x[id,],y[id,], ...)
}

# one-dim KS test with different sample size to check marginals
ks_test <- function(x, mix_fit, n = NULL, seed = 2023, ...){
  # set.seed(seed)
  stopifnot(nor1mix::is.norMix(mix_fit))
  if(is.null(n)){
    # if no n is provided, don't subsample x
    n <- length(x)
  } else {
    # subsample x
    stopifnot(n <= length(x))
    x <- sample(x, size = n)
  }
  x_sim <- rnorMix(n, mix_fit)
  ks.test(x, x_sim, ...)
}



fitCop_apply <- function(cop_list, fun){
  sapply(cop_list, function(x){
    sapply(x, function(y) fun(y@copula))
  })
}


zi_norMix <- function(x, m = 5, zero_sd = 10E-06, maxiter = 500){
  # Fit GMM for non-zero observations
  x_mar <- nor1mix::norMixMLE(x[x!=0], m = m, maxiter = maxiter)
  # Get probability of zeros, renormalize, and add zero component to parameter matrix
  x_p0 <- mean(x==0)
  x_mar[,3] <- x_mar[,3]*(1-x_p0)
  x_m <- rbind(x_mar,c(mu = 0, sigma = zero_sd, w = x_p0))
  # Return norMix object with additional spike component at 0
  nor1mix::norMix(x_m[,1], sigma = x_m[,2], w = x_m[,3])
}


# Function to fit parametric copula with zero-inflated GMM margins
fit_mvdc <- function(x,y,copula, m = 4, zero_sd = 10E-06, method = "Nelder"){
  # Marginal for x
  x_mar <- nor1mix::norMixMLE(x[x!=0], m = m)
  x_p0 <- mean(x==0)
  x_mar[,3] <- x_mar[,3]*(1-x_p0)
  x_m <- rbind(x_mar,c(mu = 0, sigma = zero_sd, w = x_p0))
  x_mix <- nor1mix::norMix(x_m[,1], sigma = x_m[,2], w = x_m[,3])
  # Marginal for y
  y_mar <- nor1mix::norMixMLE(y[y!=0], m = m)
  y_p0 <- mean(y==0)
  y_mar[,3] <- y_mar[,3]*(1-y_p0)
  y_m <- rbind(y_mar,c(mu = 0, sigma = zero_sd, w = y_p0))
  y_mix <- nor1mix::norMix(y_m[,1], sigma = y_m[,2], w = y_m[,3])
  
  
  # fit copula with parametric margins
  mcc <- copula::mvdc(copula = copula, margins = c("norMix", "norMix"), 
                      paramMargins = list(x_mix, y_mix))
  
  start <- c(x_mix, y_mix, copula@parameters)
  
  mvdc <- copula::fitMvdc(cbind(x,y), mvdc = mcc, start = start, method = method)
  
  out <- list(mvdc = mvdc, x_mar = x_mix, y_mar = y_mix, mcc = mcc)
}




