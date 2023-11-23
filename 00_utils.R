## Functions

## Load packages
# library(haven)
# library(MASS)
# library(tidyverse)
library(flexmix)
library(copula)
# library(tictoc)
library(sfsmisc)
# library(relevance)
# library(DescTools)
library(ellipse)
library(splines)

library(data.table)
library(dtplyr)
library(dplyr, warn.conflicts = FALSE)
library(scales)  


# Col <- RColorBrewer::brewer.pal(6, "Set1")
Color <- c("#0000FF", "#00FF00", "#FF0000", "#F0E716", "#FF00E8", "#ABABAB")

load_data <- function(data = "prepared", 
                      data_dir = data_dir){
  readRDS(paste0(data_dir,data,".rds"))
}

## asinh(x) function on log10 scale. Authored by Prof. Mächler!
ihs10 <- function (x) asinh(x/2)/log(10)

inv_ihs10 <- function(y) (exp(2*y*log(10))-1)/exp(y*log(10))

# privacy protection for rugs
rug2 <- function(x, n_remove = 5, n_agg = 30) {
  stopifnot(length(x) > 2 * n_remove)
  sorted <- sort(x)
  trimmed <- sorted[(n_remove + 1):(length(sorted) - n_remove)] %>% data.frame() %>% setnames("x")
  trimmed <- sdcMicro::mafast(trimmed, variables = "x", aggr = n_agg)
  rug(trimmed$x)
}


add_smoother <- function(x,y,  col.smooth = 2, span = 1/1000, iter = 3, ...){
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)) 
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), 
          col = col.smooth, ...)  
}


sample_df <- function(data, n = 2000, seed = 2023){
  set.seed(seed)
  sel <- sample(1:nrow(data), size = n, replace = FALSE)
  data[sel,]
}

plotEll2 <- function(mod, data) plotEll(mod,data[,1:2])

## Plotting function with rugs
density_plot <- function(x, rugs = TRUE, bw = "SJ-dpi", smart_labs = TRUE, ...){
  
  if(class(x)=="density"){
    y <- x
    rugs <- FALSE
  } else {
    y <- density(x, bw = bw)
  }
  
  if(smart_labs){
    
    rel_ticks <- c(-10000, 0, 10000, 50000, 100000, 250000, 1000000, 10000000)
    # rel_labs <- paste0("CHF ", c("0","10K", "50K", "100K", "250K", "1M", "10M"))
    rel_labs <- c("-10k", "0","10k", "50k", "100k", "250k", "1M", "10M")
    # rel_loc <- log10(rel_ticks)
    rel_loc <- ihs10(rel_ticks)
    plot(y, ..., axes = FALSE, xlab = "CHF")
    axis(1, at = rel_loc, labels = rel_labs)
    axis(2)
    
  } else {
    
    plot(y, ...)
    
  }
  
  if(rugs) 
    rug2(x)
  
}

## Winsorize data by value instead of percentile
win <- function(x, low, high) {
  pmin(high, pmax(x, low))
}


# Lines() function that draws all different mixture components in marginal plots
lines_mix <- function(mix_model, xlim = c(0, 8), variable = 1, returns = "lines") {
  n_comp <- length(mix_model@components)
  probs <- table(mix_model@cluster)/sum(table(mix_model@cluster))
  x_ <- seq(xlim[1], xlim[2], length.out = 300)
  y_comb <- numeric(300)
  for (i in 1:n_comp){
    comp_i <- mix_model@components[[i]][[1]]@parameters
    mu <- comp_i$center[variable]
    var_ <- diag(comp_i$cov)[variable]
    if (is.na(var_)){
      # var_ <- comp_i$var[variable]
      var_ <- diag(comp_i$var)[variable]
    }
    y <- probs[i]*dnorm(x_, mean = mu, sd = sqrt(var_))
    if(returns == "lines") lines(x_, y, col = i+1)
    y_comb = y_comb + y
  }
  if(returns == "lines") 
    lines(x_, y_comb, col = 2, lty = "dashed")
  else if (returns == "top")
    max(y_comb)
}

# Plot functions to add components to plots of marginal densities
plot_marginals <- function(x, y = NULL, mix_model = NULL, 
                           xlim = NULL, 
                           # xlim = c(0,8), 
                           title = NULL, bw = "SJ-dpi"){
  
  names <- attributes(x)[[1]]
  
  # if(is.null(y)){
  #   stopifnot(is.data.frame(x))
  #   y <- x[[2]]
  #   x <- x[[1]]
  # }
  
  if(is.null(y)){
    stopifnot(is.data.frame(x)|is.matrix(x))
    ## add possibility to use matrix instead of df..
    if(is.data.frame(x)) {
      y <- x[[2]]
      x <- x[[1]]
    } else if (is.matrix(x)) {
      y <- x[,2]
      x <- x[,1]
    }
  }
  
  if(is.null(xlim)){
    xlim <- c(min(x,y), max(x,y))
  }
  
  
  ymax <- numeric(2)
  if(!is.null(mix_model)){
    n_comp <- length(mix_model@components)
    probs <- table(mix_model@cluster)/sum(table(mix_model@cluster))
    ymax[1] <- max(lines_mix(mix_model, var = 1, returns = "top"), density(x, bw = bw)$y)
    ymax[2] <- max(lines_mix(mix_model, var = 2, returns = "top"), density(y, bw = bw)$y)
  } else {
    ymax[1] <- max(density(x, bw = bw)$y)
    ymax[2] <- max(density(y, bw = bw)$y)
  }
  
  sfsmisc::mult.fig(2, main = title)$old.par -> opar
  density_plot(x, xlim = xlim, main = names[1], 
               # main = attributes(x)[[1]], 
               bw = bw, ylim = c(0, ymax[1]))
  if (!is.null(mix_model)) lines_mix(mix_model, xlim = xlim, var = 1)
  density_plot(y, xlim = xlim, main = names[2], 
               # main = attributes(y)[[1]], 
               bw = bw, ylim = c(0, ymax[2]))
  if (!is.null(mix_model)) lines_mix(mix_model, xlim = xlim, var = 2)
  par(opar)
}



# function that draws ellipses
ellipse_mix <- function(mix_model) {
  n_comp <- length(mix_model@components)
  for (i in 1:n_comp){
    comp_i <- mix_model@components[[i]][[1]]@parameters
    cov_i <- comp_i$cov
    if(is.null(cov_i)){
      # cov_i <- diag(comp_i$var)
      cov_i <- comp_i$var
    }
    
    # ellipse_i <- ellipse(shape = cov_i, center = comp_i$center[1:2])
    ellipse_i <- ellipse::ellipse(x = cov_i, centre = comp_i$center[1:2])
    lines(ellipse_i, col = i+1)
    text(x=comp_i$center[1], y=comp_i$center[2] , col = i+1, labels = as.character(i))
  }
}


# Bivariate density plot with component ellipses 
binorm_plot <- function(x, y = NULL, mix_model = NULL, breaks = 100, scale_k = .25) {
  
  if(is.null(y)){
    stopifnot(is.data.frame(x)|is.matrix(x))
    ## add possibility to use matrix instead of df..
    if(is.data.frame(x)) {
      y <- x[[2]]
      x <- x[[1]]
    } else if (is.matrix(x)) {
      y <- x[,2]
      x <- x[,1]
    }
  }
  
  on.exit(par(mar = par("mar")))
  
  h1 <- hist(x, breaks=breaks, plot=F)
  h2 <- hist(y, breaks=breaks, plot=F)
  top <- max(h1$counts, h2$counts)
  k <- MASS::kde2d(x, y, n=breaks)
  k$z <- k$z^scale_k
  
  par(mar=c(3,3,1,1))
  layout(matrix(c(2,0,1,3),2,2,byrow=T),c(3,1), c(1,3))
  # smoothScatter(x,y,xlab = attributes(x)[[1]], ylab = attributes(y)[[1]])
  image(k, xlab = attributes(x)[[1]], ylab = attributes(y)[[1]]) #plot the image
  # if no flexmix model is provided, don't try to plot ellipses for mixture components
  if (!is.null(mix_model)) ellipse_mix(mix_model)
  par(mar=c(0,2,1,0))
  barplot(h1$counts, axes=F, ylim=c(0, top), space=0, col='red')
  par(mar=c(2,0,0.5,1))
  barplot(h2$counts, axes=F, xlim=c(0, top), space=0, col='red', horiz=T)
  
  
}




# Bivariate density plot with component ellipses 
smoothScatter_ellipse <- function(x, y = NULL, mix_model = NULL, nrpoints = 0, ...) {
  
  if(is.null(y)){
    stopifnot(is.data.frame(x)|is.matrix(x))
    ## add possibility to use matrix instead of df..
    if(is.data.frame(x)) {
      y <- x[[2]]
      x <- x[[1]]
    } else if (is.matrix(x)) {
      y <- x[,2]
      x <- x[,1]
    }
  }
  
  # on.exit(par(mar = par("mar")))
  
  # h1 <- hist(x, breaks=breaks, plot=F)
  # h2 <- hist(y, breaks=breaks, plot=F)
  # top <- max(h1$counts, h2$counts)
  # k <- MASS::kde2d(x, y, n=breaks)
  # k$z <- k$z^scale_k
  
  # par(mar=c(3,3,1,1))
  # layout(matrix(c(2,0,1,3),2,2,byrow=T),c(3,1), c(1,3))
  # smoothScatter(x,y,xlab = attributes(x)[[1]], ylab = attributes(y)[[1]])
  smoothScatter(x,y, xlab = "Wealth", ylab = "Income", nrpoints = nrpoints, ...)
  # image(k, xlab = attributes(x)[[1]], ylab = attributes(y)[[1]]) #plot the image
  # if no flexmix model is provided, don't try to plot ellipses for mixture components
  if (!is.null(mix_model)) ellipse_mix(mix_model)
  # par(mar=c(0,2,1,0))
  # barplot(h1$counts, axes=F, ylim=c(0, top), space=0, col='red')
  # par(mar=c(2,0,0.5,1))
  # barplot(h2$counts, axes=F, xlim=c(0, top), space=0, col='red', horiz=T)
  # 
  
}


## Get variable names by pattern
grep_ch <- function(data, pattern, perl = TRUE) {
  names(data)[grep(pattern,names(data),perl = perl)]
}

## get variable labels by pattern
grep_attr <- function(data, pattern, which.attribute = "label"){
  stopifnot(is.character(pattern))
  cols <- grep_ch(data, pattern)
  row <- data[1, ..cols]
  lapply(row, attr, which = which.attribute)
}



## This helper function is necessary to be able to use the pipe operator on cache_model()
detect_pipe_lhs <- function(x) {
  # Source: https://stackoverflow.com/questions/52066097/get-expression-that-evaluated-to-dot-in-function-called-by-magrittr-pipe/52080518#52080518
  # Answer by Artem Sokolov (retrieved 18.04.2023)
  require(purrr)
  getAST <- function(ee) purrr::map_if(as.list(ee), is.call, getAST)
  sc <- sys.calls()
  ASTs <- purrr::map( as.list(sc), getAST ) %>%
    purrr::keep( ~identical(.[[1]], quote(`%>%`)) )  # Match first element to %>%
  
  if( length(ASTs) == 0 ) return( enexpr(x) )        # Not in a pipe
  dplyr::last( ASTs )[[2]]    # Second element is the left-hand side
}


## Function that fits model only if it does not exist as .rds file already 
cache_model <- function(model){
  
  require(rlang)
  require(stringr)
  lhs <- detect_pipe_lhs()
  # make sure that function works without pipe as well
  if(is_missing(lhs)){
    model_string <- deparse1(substitute(model))
  } else {
    model_string <- deparse1(substitute(lhs))
  }
  
  # create unique model_name string to identify call
  model_name <- model_string %>%
    str_replace_all(" ","") %>%
    str_replace_all("=", "") %>%
    str_split(",") %>%
    unlist() %>%
    str_replace_all("[[:punct:]]", "_") %>%
    str_replace_all("^_*","") %>%
    str_replace_all("_*$", "") %>%
    str_replace_all("~", "") %>%
    sort() %>%
    paste(collapse = "_") %>%
    str_replace_all("^_*", "") %>% 
    abbreviate()
  
  model_path <- paste0("_models/",model_name,".rds")
  
  if(!file.exists(model_path)){
    mod <- model
    saveRDS(mod, model_path)
  } else {
    mod <- readRDS(model_path)
  }
  
  mod
  
}


# Plot functions to add components to plots of marginal densities
plot_marginals_sim <- function(x, y = NULL, mix_model = NULL, xlim = c(0,8), title = NULL, bw = "sj"){
  require(flexmix)
  # if(is.null(y)){
  #   stopifnot(is.data.frame(x))
  #   y <- x[[2]]
  #   x <- x[[1]]
  # }
  
  if(is.null(y)){
    stopifnot(is.data.frame(x)|is.matrix(x))
    ## add possibility to use matrix instead of df..
    if(is.data.frame(x)) {
      y <- x[[2]]
      x <- x[[1]]
    } else if (is.matrix(x)) {
      y <- x[,2]
      x <- x[,1]
    }
  }
  
  if(!is.null(mix_model)) stopifnot(class(mix_model) == "flexmix")
  
  ymax <- numeric(2)
  
  den_x <- density(x, bw = bw)
  den_y <- density(y, bw = bw)
  
  if(!is.null(mix_model)){
    # simulate data from flexmix model
    Y_sim <- flexmix::rflexmix(mix_model)$y[[1]]
    x_sim <- Y_sim[,1]
    y_sim <- Y_sim[,2]
    # estimate density
    den_x_sim <- density(x_sim, bw = bw)
    den_y_sim <- density(y_sim, bw = bw)
    # ymax[1] <- max(lines_mix(mix_model, var = 1, returns = "top"), density(x, bw = bw)$y)
    # ymax[2] <- max(lines_mix(mix_model, var = 2, returns = "top"), density(y, bw = bw)$y)
    ymax[1] <- max(den_x_sim$y, den_x$y)
    ymax[2] <- max(den_y_sim$y, den_y$y)
  } else {
    ymax[1] <- max(den_x$y)
    ymax[2] <- max(den_y$y)
  }
  
  sfsmisc::mult.fig(2, main = title)$old.par -> opar
  density_plot(den_x, xlim = xlim, main = attributes(x)[[1]], bw = bw, ylim = c(0, ymax[1]))
  if (!is.null(mix_model)) 
    lines(den_x_sim, col = 2, lty = 2)
  # lines_mix(mix_model, xlim = xlim, var = 1) 
  density_plot(den_y, xlim = xlim, main = attributes(y)[[1]], bw = bw, ylim = c(0, ymax[2]))
  if (!is.null(mix_model)) 
    lines(den_y_sim, col = 2, lty = 2)
  # lines_mix(mix_model, xlim = xlim, var = 2)
  par(opar)
}



## Adapttion of FLXMCmvcombi driver: with non-diagonal covariance matrix:
setClass("FLXMCmvcombi2",
         representation(binary = "vector"),
         contains = "FLXMC")


FLXMCmvcombi2 <- function(formula=.~.)
{
  z <- new("FLXMCmvcombi2", weighted=TRUE, formula=formula,
           dist = "mvcombi",
           name="model-based binary-Gaussian clustering")
  
  z@defineComponent <- function(para) {
    predict <- function(x, ...){
      matrix(para$center, nrow=nrow(x), ncol=length(para$center),
             byrow=TRUE)
    }
    
    logLik <- function(x, y){
      if(any(para$binary)){
        p <- matrix(para$center[para$binary], nrow=nrow(x),
                    ncol=sum(para$binary), byrow=TRUE)
        z <- rowSums(log(y[,para$binary,drop=FALSE]*p +
                           (1-y[,para$binary,drop=FALSE])*(1-p)))
      } else z <- rep(0, nrow(x))
      if(!all(para$binary)){
        if(sum(!para$binary)==1)
          z <- z + dnorm(y[,!para$binary],
                         mean=para$center[!para$binary], sd=sqrt(para$var),
                         log=TRUE)
        else
          z <- z + mvtnorm::dmvnorm(y[,!para$binary,drop=FALSE],
                                    # mean=para$center[!para$binary], sigma=diag(para$var),
                                    mean=para$center[!para$binary], sigma=para$var,
                                    log=TRUE)
      }
      z
    }
    
    new("FLXcomponent", parameters=list(center=para$center, var=para$var), df=para$df,
        logLik=logLik, predict=predict)
  }
  
  z@fit <- function(x, y, w, binary, ...){
    para <- cov.wt(y, wt=w)[c("center","cov")]
    # para <- list(center = para$center, var = diag(para$cov)[!binary],
    para <- list(center = para$center, var = para$cov[!binary, !binary],
                 df = ncol(y) + sum(!binary),
                 binary = binary)
    z@defineComponent(para)
  }
  z
}

setMethod("FLXgetModelmatrix", signature(model="FLXMCmvcombi2"),
          function(model, data, formula, lhs=TRUE, ...)
          {
            
            model <- callNextMethod(model, data, formula, lhs)
            model@binary <- apply(model@y, 2, function(z) all(unique(z) %in% c(0,1)))
            model
          })

setMethod("FLXmstep", signature(model = "FLXMCmvcombi2"),
          function(model, weights, components)
          {
            return(sapply(seq_len(ncol(weights)),
                          function(k) model@fit(model@x, model@y, weights[,k], model@binary)))
          })





# Residual plots for mlm --------------------------------------------------

# create rstandard method for mlm S3 class (to create standardized residual plots)
# source: https://stackoverflow.com/questions/39562631/obtain-standardised-residuals-and-residual-v-s-fitted-plot-for-mlm-object-f
rstandard.mlm <- function (model) {
  Q <- with(model, qr.qy(qr, diag(1, nrow = nrow(qr$qr), ncol = qr$rank)))  ## Q matrix
  hii <- rowSums(Q ^ 2)  ## diagonal of hat matrix QQ'
  RSS <- colSums(model$residuals ^ 2)  ## residual sums of squares (for each model)
  sigma <- sqrt(RSS / model$df.residual)  ##  ## Pearson estimate of residuals (for each model)
  pointwise_sd <- outer(sqrt(1 - hii), sigma)  ## point-wise residual standard error (for each model)
  model$residuals / pointwise_sd  ## standardised residuals
}


plot_TA <- function(fit.lm, newdata = NULL, title = "TA Plot") {
  
  dimnames <- attributes(fit.lm$coefficients)$dimnames[[2]]
  summary <- summary(fit.lm)
  if(is.null(newdata)){
    fitted_values <-  fitted(fit.lm)
    residuals <- rstandard(fit.lm)
    res <- residuals(fit.lm)
    datastring <- "Train"
    
  } else {
    fitted_values <- predict(fit.lm, newdata)
    residuals <- fitted_values - newdata[,dimnames]
    res <- residuals
    datastring <- "Test"
  }
  
  adj.r.squared <- get_adj_r2(fit.lm, newdata = newdata)
  
  mult.fig(length(dimnames), main = title)$old.par -> opar
  for(i in 1:length(dimnames)){
    RMSE <- round(sqrt(mean(res[,i]^2, na.rm = TRUE)), 3)
    # R2 <- round(summary[[i]]$adj.r.squared, 3)
    R2 <- round(adj.r.squared[i], 3)
    plot(fitted_values[,i], residuals[,i], 
         main = paste0(dimnames[i], " (", datastring," R2 = ", R2, "; ",datastring," RMSE = ", RMSE, ")"), 
         xlab = "Fitted Values", ylab = "Residuals")
    abline(h = 0, lty = 2, col = 2)
  }
  par(opar)
}


plot_fitted <- function(fit.lm, newdata, title = "Fitted values vs. Target") {
  
  dimnames <- attributes(fit.lm$coefficients)$dimnames[[2]]
  summary <- summary(fit.lm)
  
  fitted_values <- predict(fit.lm, newdata)
  y_test <- newdata[,dimnames]
  residuals <- fitted_values - y_test
  
  
  mult.fig(length(dimnames), main = title)$old.par -> opar
  for(i in 1:length(dimnames)){
    RMSE <- round(sqrt(mean(residuals[,i]^2, na.rm = TRUE)), 3)
    R2 <- round(summary[[i]]$adj.r.squared, 3)
    y_i <- y_test[,i][[1]] 
    plot(y_i, fitted_values[,i], 
         main = paste0(dimnames[i], " (R2 = ", R2, "; RMSE = ", RMSE, ")"), 
         ylab = "Fitted Values", xlab = "Actual values")
    abline(a = 0, b = 1, col = 2, lty= 2)
  }
  par(opar)
}

plot_QQ <- function(fit.lm, newdata = NULL, title = "Normal Q-Q Plot", info = TRUE) {
  
  dimnames <- attributes(fit.lm$coefficients)$dimnames[[2]]
  summary <- summary(fit.lm)
  if(is.null(newdata)){
    fitted_values <-  fitted(fit.lm)
    res <- residuals(fit.lm)
  } else {
    fitted_values <- predict(fit.lm, newdata)
    res <- fitted_values - newdata[,dimnames]
  }
  residuals <- rstandard(fit.lm)
  mult.fig(length(dimnames), main = title)$old.par -> opar
  for(i in 1:length(dimnames)){
    RMSE <- round(sqrt(mean(res[,i]^2, na.rm = TRUE)), 3)
    R2 <- round(summary[[i]]$adj.r.squared, 3)
    main_i <- ifelse(info, 
                     paste0(dimnames[i], " (R2 = ", R2, "; RMSE = ", RMSE, ")"),
                     dimnames[i])
    qqnorm(residuals[,i], main = main_i)
    qqline(residuals[,i], col = "steelblue", lwd = 2)
    
  }
  par(opar)
}


## Same but for glmnet object
plot_TA_lasso <- function(fit.lm, newX = X_train, newY = Y_train, 
                          smooth = TRUE, title = "TA Plot", 
                          lambda = "lambda.1se",...) {
  
  if(exists("name")){
    title <- paste0(name,": ", title)
  }
  if(any(class(fit.lm) == "cv.glmnet")){
    fitted_values <- predict(fit.lm, newx = newX, s = lambda)[,,1]
    dimnames <- attributes(fitted_values)$dimnames[[2]]
  } else {
    dimnames <- attributes(fit.lm$beta)$names
    fitted_values <- predict(fit.lm, newx = newX)[,,1]
  }
  residuals <- newY - fitted_values
  
  # X_mat <- as.matrix(newX)
  # summary <- summary(fit.lm)
  
  
  
  mult.fig(length(dimnames), main = title)$old.par -> opar
  for(i in 1:length(dimnames)){
    # RMSE <- round(sqrt(mean(residuals[,i]^2, na.rm = TRUE)), 3)
    # R2 <- round(summary[[i]]$adj.r.squared, 3)
    plot(fitted_values[,i], residuals[,i], 
         main = paste0(dimnames[i]), 
         xlab = "Fitted Values", ylab = "Residuals", ...)
    if(smooth)
      lines(smooth.spline(x = fitted_values[,i], y = residuals[,i]), col = 2)
    #abline(h = 0, lty = 2, col = 3)
  }
  par(opar)
}


plot_QQ_lasso <- function(fit.lm, newX = X_train, newY = Y_train, 
                          title = "Normal Q-Q Plot", info = TRUE, 
                          lambda = "lambda.1se",...) {
  
  if(exists("name")){
    title <- paste0(name,": ", title)
  }
  
  if(any(class(fit.lm) == "cv.glmnet")){
    fitted_values <- predict(fit.lm, newx = newX, s = lambda)[,,1]
    dimnames <- attributes(fitted_values)$dimnames[[2]]
  } else {
    dimnames <- attributes(fit.lm$beta)$names
    fitted_values <- predict(fit.lm, newx = newX)[,,1]
  }
  residuals <- newY - fitted_values
  
  # dimnames <- attributes(fit.lm$beta)$names
  # # summary <- summary(fit.lm)
  # 
  # fitted_values <- predict(fit.lm, newx = newX)[,,1]
  # residuals <- fitted_values - newY
  
  mult.fig(length(dimnames), main = title)$old.par -> opar
  for(i in 1:length(dimnames)){
    RMSE <- round(sqrt(mean(residuals[,i]^2, na.rm = TRUE)), 3)
    # R2 <- round(summary[[i]]$adj.r.squared, 3)
    main_i <- ifelse(info, 
                     paste0(dimnames[i], " (RMSE = ", RMSE, ")"),
                     dimnames[i])
    qqnorm(residuals[,i], main = main_i)
    qqline(residuals[,i], col = "steelblue", lwd = 2)
    
  }
  par(opar)
}


plot_fitted2d <- function(fit, 
                          # newX, newY = NULL, 
                          which = 1, legend = TRUE, ...){
  lm <- any(class(fit) == "mlm")
  glmnet <- any(class(fit) == "glmnet")
  gam <- any(class(fit) == "gam")
  if(lm){
    dimnames <- attributes(fit$coefficients)$dimnames[[2]]  
  } else if (glmnet){
    dimnames <- attributes(fit$beta)$names
  } else if (gam){
    formulas <- summary(fit)$formula
    dimnames <- vapply(formulas, function(y) as.character(y)[2],FUN.VALUE = "character")
  }
  
  if(is.null(newY)){
    sel <- which(names(newX) %in% dimnames)
    newY <- newX[,sel]
    newX <- newX[,-sel]
  }
  
  if(lm){
    fitted_values <- predict(fit, newX)
    residuals <- data.matrix(newY - fitted_values)
  } else if (glmnet){
    fitted_values <- predict(fit, newx = newX)[,,1]
    residuals <- (newY-fitted_values)
  } else if (gam){
    fitted_values <- fit$fitted.values
    # residuals <-  fit$y - fit$fitted.values 
    residuals <- residuals(fit, "response")
  }
  
  par(mfrow=c(1,1))
  
  if(which == 1){
    is_fit <- c(rep(0,nrow(newY)), rep(1, nrow(fitted_values)))
    plot(rbind(newY, fitted_values), col = (is_fit + 1), main = "Fitted Values and Target", ...)
    if(legend)
      legend("bottomleft",bg="transparent", legend = c("Target", "Prediction"), col = 1:2, pch = 1)
    # legend(-5,2, legend = c("Target", "Prediction"), col = 1:2, pch = 1)
  } else if (which == 2){
    plot(x=fitted_values, y=residuals, col = col(fitted_values), main = "TA Plot", ...)
    abline(h = 0, lty=2, col = 2)
    abline(a = 0, b = 1, lty= 2 , col = 3)
    if (legend)
      legend("topleft",bg="transparent", legend = dimnames, col = 1:2, pch = 1)
    # legend(0,10, legend = dimnames, col = 1:2, pch = 1)
  }
  
}




# Regressions script functions --------------------------------------------

sample_train <- function(data, p_train = 0.5, seed = 2023){
  set.seed(seed)
  sample(c(TRUE,FALSE), size = nrow(data), replace = TRUE, prob = c(p_train,1-p_train))
}


# function that extracts the name from model object and is robust for use in lapply()
get_name <- function(x){
  
  name <- deparse(substitute(x))
  if(nchar(name) == 1|name == "fit"|!grepl("\\d+", name)|nchar(name) > 10){
    call <- x$call
    form <- stringr::str_extract(as.character(call$formula), "\\d\\.?\\d?")
    mod <- as.character(call)[1]
    paste0(mod,form)
  } else {
    name
  }
}



R2_to_title <- function(dimnames, newdata = df_test, fit = fit, title = ""){
  if(!is.null(newdata)){
    ind <- which(names(newdata) %in% dimnames)
    Y <- newdata[,ind]
    X <- newdata[,-ind]
    fitted_values <- predict(fit,X)
    residuals <- Y - fitted_values
    
    # compute R2 based on test data
    # TSS
    mean <- apply(Y,2,mean)
    TSS <- apply(sweep(Y,2,mean)^2,2,sum)
    # RSS
    RSS <- apply(residuals^2,2,sum)
    R2 <- round(1-RSS/TSS,4)
    R2string <- paste0("; test R2 = ",R2)
    # title_disc <- " - based on unseen data"
  } else {
    fitted_values <- fit$fitted.values
    residuals <- fit$residuals
    Y <- round(residuals + fitted_values, 4)
    R2string <- paste0("; train R2 = ",get_r2(fit))
    # title_disc <- " - based on training data"
    # R2string <- character(length(dimnames))
  }
  title <- paste0(title, R2string)
  title
}



# plotting function for univariate diagnostics plots
plot_diagnostics_lm <- function(fit, which = c(1:3,5), path, name = NULL, ...){
  # name <- deparse(substitute(fit))
  if(is.null(name))
    name <- get_name(fit)
  Y_names <- attributes(fit$coefficients)$dimnames[[2]]
  # R2 <- get_r2(fit)
  title2 <- R2_to_title(Y_names, fit = fit)
  
  for(var in Y_names){
    i <- which(Y_names == var)
    var_name <- ifelse(var == "w", "Wealth", 
                       ifelse(var == "y", "Income", var))
    # if(length(Y_names) > 1){
    #   fit_i <- update(fit, paste(var,"~."))  
    # } else {
    #   fit_i <- fit
    # }
    
    fit_i <- update(fit, paste(var,"~."))  
    
    dir <- paste0(path, name, "_", var, ".pdf")
    pdf(dir)
    mult.fig(length(Y_names), main = paste0(var_name,title2[i]), ...)$oldpar -> opar
    plot(fit_i, which = which, pch = '.', sub.caption = ''
         # , sub.caption = paste0("Adj. R^2 = ", round(R2[var], 3))
    )
    dev.off()
    par(opar)
  }
}

# Plotting function: residuals vs regressor
plot_xres <- function(fit, x, main = "Residuals vs. Regressor", 
                      smooth = TRUE, pch = '.', path = "_plots/_res/", ...){
  
  
  # model_name <- deparse(substitute(fit))
  model_name <- get_name(fit)
  path <- paste0(path,model_name,"/")
  
  ifelse(!dir.exists(file.path(path)), 
         dir.create(file.path(path), recursive = TRUE), FALSE)
  
  # allow for input vectors instead of variable names (to look at residual plots of age)
  if(is.numeric(x)){
    stopifnot(length(x) == nrow(fit$model))
    X <- data.frame(x)
    x <- deparse(substitute(x))
  } else {
    X <- data.frame(fit$model[,x])
  }
  
  path <- paste0(path, model_name, "_res_", paste(x, collapse = "_"),".pdf")
  
  res <- residuals(fit)
  ny <- ncol(res)
  nx <- length(x)
  y_names <- attributes(fit$coefficients)$dimnames[[2]]
  
  pdf(path)
  mult.fig((ny*nx), main = main)$oldpar -> opar
  for(i in seq_along(y_names)){
    for(j in 1:length(X)){
      x_j <- X[,j]
      if(is.logical(x_j)){
        x_j <- factor(x_j)
      }
      plot(x_j, res[,i], xlab = x[j],
           ylab = paste0("Residuals of ", y_names[i]),
           main = y_names[i], pch = pch)
      
      if(smooth){
        try(add_smoother(x = X[,j],y = res[,i]))
        # try(lines(smooth.spline(x = X[,j],y = res[,i]), col = 2))
      }
    }
  }
  dev.off()
  par(opar)
}

# cut function for kids variable 
cut_kids <- function(x){
  cut(x, c(0,1,2,Inf), include.lowest = TRUE, 
      right = FALSE, ordered_result = TRUE)
}

# identify binary variables
is.dummy <- function(x, na.rm = TRUE, need_both = TRUE){
  if (na.rm) x <- na.omit(x)
  all_x <- unique(x)
  if(need_both){
    all(all_x %in% c(0L,1L)) & all(c(0L,1L) %in% all_x) & !is.logical(x)
  } else {
    all(all_x %in% c(0L,1L)) & !is.logical(x)
  }
}

# get Adjusted R2 from mlm model
get_r2 <- function(fit){
  sum <- summary(fit)
  x <- unlist(sapply(sum,function(x) x["adj.r.squared"]))
  names(x) <- attributes(fit$coefficients)$dimnames[[2]]
  x
  
}


# Function to create ordered, and correctly labelled factors for signs
factor_sign <- function(x){
  x_out <- sign(x)
  labs <- as.character(sort(unique(x_out)))
  factor(x_out, ordered = TRUE, 
         labels = labs)
}

# get all significant coefficients
extract_significant_coefficients <- function(fit, significance_level = 0.05, order = FALSE) {
  # Create a summary object of the linear model
  lm_summary <- summary(fit)
  response_coefficients <- lapply(1:2, function(x) lm_summary[[x]]$coefficients)
  coef_out <- numeric(0)
  for(i in 1:length(response_coefficients)){
    coef_table <- response_coefficients[[i]]
    p_vals <- coef_table[,4]
    significant_coefs <- coef_table[p_vals < significance_level,1]
    
    # coef_names <- names(significant_coefs)
    coef_out <- c(coef_out, significant_coefs)
  }
  if(order){
    unique(names(coef_out[order(abs(coef_out), decreasing  = TRUE)]))  
  } else {
    unique(names(coef_out))
  }
  
}


library(jtools)
# Wraper function for plot_coefs()
plot_coefs_mlm <- function(fit, path = "_plots/_coefs/", 
                           main = NULL, max_n = 30, ...) {
  
  
  if(dim(coefficients(fit))[1] > max_n){
    coefs <- extract_significant_coefficients(fit = fit, order = TRUE)[1:max_n]
  } else {
    coefs <- coefficients(fit)
  }
  
  
  # model_name <- deparse(substitute(fit))
  model_name <- get_name(fit)
  # path <- paste0(path,model_name,"/")
  if(is.null(main)) {
    main <- paste0(model_name, " - Coef. Plot")
  }
  
  ifelse(!dir.exists(file.path(path)), 
         dir.create(file.path(path), recursive = TRUE), FALSE)
  
  path <- paste0(path, model_name, "_coefs.pdf")
  
  y_names <- attributes(fit$coefficients)$dimnames[[2]]
  
  fits <- lapply(y_names, function(y) update(fit, paste0(y,"~.")))
  
  p <- jtools::plot_coefs(fits, model.names = y_names, ...) +
    ggplot2::ggtitle(main)
  
  ggplot2::ggsave(path, plot = p)
  
  p
  
}


get_adj_r2 <- function(fit,newdata = NULL){
  
  dimnames <- attributes(fit$coefficients)$dimnames[[2]] 
  
  if(is.null(newdata)){
    
    fitted_values <- fitted(fit)
    residuals <- residuals(fit)
    Y <- residuals + fitted_values
    
  } else {
    
    ind <- which(names(newdata) %in% dimnames)
    Y <- newdata[,ind]
    X <- newdata[,-ind]
    fitted_values <- predict(fit,X)
    residuals <- Y - fitted_values
  }
  
  
  
  
  
  # compute R2 based on test data
  # TSS
  mean <- apply(Y,2,mean)
  TSS <- apply(sweep(Y,2,mean)^2,2,sum)
  # RSS
  RSS <- apply(residuals^2,2,sum)
  R2 <- 1-RSS/TSS
  
  df_res <- fit$df.residual
  N <- fit$rank+1
  
  round(1-((1-R2)*(df_res+N-1)/(df_res-1)),4)
  
}




bool_NA <- function(x) {
  ifelse(!is.na(x),as.character(x),"NA")
}


binned_percentiles <- function(x, probs = c(0.5, 0.9, 1)){
  
  p <- c(0, probs)*100
  n_groups <- length(probs)
  p1 <- p[2:(n_groups+1)]
  p0 <- p[1:(n_groups)]
  labs <- paste0(paste(p0,p1,sep="-"), "%")
  percentiles <- quantile(x, probs)
  expr <- paste("x <= ", percentiles, " ~ ", 1:n_groups)
  
  factor(case_when(!!!rlang::parse_exprs(expr)), labels = labs)
}



share0 <- function(x, range = c(0,1), replacement = c(-0.1,1.1)){
  case_when(
    # replace with replacement values if out of range
    x > range[2] ~ replacement[2],
    x < range[1] ~ replacement[1],
    # keep if inside range
    TRUE ~ x
  )
}

maxlim <- function(x,y){
  c(min(x[1],y[1]),max(x[2],y[2]))
}

scatter <- function(fun, ...){
  models <- list(...)
  N <- length(models)
  fun_name <- deparse(substitute(fun))
  # Apply function to each model
  predicts <- lapply(models,fun)
  # Check if dataframe "df" exists in workspace and plot this as well
  df_exists <- exists("df_private")
  data <- get("df_private")[,1:2]
  
  if(length(models) > 1){
    mins <- apply(sapply(predicts, function(df) apply(df, 2, min)),1,min)
    # Get the maximum values for each column
    maxs <- apply(sapply(predicts, function(df) apply(df, 2, max)),1,max)
  } else {
    mins <- apply(predicts[[1]], 2, min)
    maxs <- apply(predicts[[1]], 2, max)
  }
  
  xlim <- c(mins[1],maxs[1])
  ylim <- c(mins[2],maxs[2])
  
  if(df_exists){
    
    data_lim <- rbind(apply(data,2,min), apply(data,2,max))
    
    N = N + 1
    w_sign <- factor(sign(df$w))
    y_sign <- factor(sign(df$y))
    col <- w_sign:y_sign
  } else {
    col <- 1
  }
  
  mult.fig(N, main = paste0(fun_name))
  if(df_exists){
    xlim <- maxlim(xlim,data_lim[,1])
    ylim <- maxlim(ylim,data_lim[,2])
  }
  if(fun_name == "predict"){
    plot(data, xlim = xlim, ylim = ylim, pch = '.', main = "Original data", col = col)
  } else{
    plot(data, pch = '.', main = "Original data", col = col)
  } 
  
  
  for(i in seq_along(predicts))
    plot(predicts[[i]], xlim = xlim, ylim = ylim, pch = '.', main = paste0("Model ", i), col = col)
}


# Plot marginal densities by sign of other outcome
density_marginals <- function(df, bw = "nrd0", main = ""){
  mult.fig(2, main = paste0(main, " Marginal Densities - bw = ", bw))
  w <- df$w
  y <- df$y
  y_sign <- sign(y)
  w_sign <- sign(w)
  w_sign_u <- sort(unique(w_sign))
  y_sign_u <- sort(unique(y_sign))
  cols <- c(1,2,3)
  
  ylim_w <- max(sapply(y_sign_u, function(i) max(density(w[y_sign == i], bw = bw)$y)))
  ylim_y <- max(sapply(w_sign_u, function(i) max(density(y[w_sign == i], bw = bw)$y)))
  
  
  
  # Marginal densities of income by wealth
  plot(density(w[y_sign == y_sign_u[1]], bw = bw), col = cols[(y_sign_u+2)[1]], type = 'l',
       xlim = c(min(w),max(w)), main = "Wealth by sign(Income)", ylim = c(0,ylim_w))
  for(i in 2:length(y_sign_u))
    lines(density(w[y_sign == y_sign_u[i]], bw = bw), col = cols[(y_sign_u+2)[i]])
  # legend("topleft", legend = paste0(y_sign_u), lty = 1, col = cols[y_sign_u+2])
  
  # Marginal densities of income by wealth
  plot(density(y[w_sign == w_sign_u[1]], bw = bw), col = cols[(w_sign_u+2)[1]], type = 'l',
       xlim = c(min(y),max(y)), main = "Income by sign(Wealth)", ylim = c(0,ylim_y))
  for(i in 2:length(w_sign_u))
    lines(density(y[w_sign == w_sign_u[i]], bw = bw), col = cols[(w_sign_u+2)[i]])
  
  legend("topright", legend = paste0(w_sign_u), lty = 1, col = cols[w_sign_u+2],
         cex = .6)
}


dir_create <- function(dir){
  
  dir <- stringr::str_replace(dir, "\\/{2,}", "\\/")
  
  if(!dir.exists(dir))
    dir.create(dir, recursive = TRUE)
}

# extension of pdf() that creates directories if they don't exist
pdf2 <- function(dir){
  fig_dir <- get("fig_dir")
  # fig_dir <- "C:/Users/kelouaghlidi/Dropbox/JointWealthIncome/_Karim/_output/_plots/_general/"
  dir2 <- stringr::str_replace(paste0(fig_dir,"/",dir), "\\/{2,}", "\\/")
  path <- stringr::str_extract(dir2,".*(?=\\/\\w+\\.pdf$)")
  if(!dir.exists(path)) dir.create(path, recursive = TRUE)
  pdf(dir2)
}


# Pairs plots -------------------------------------------------------------





library(SciViews)
panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y, method = "spearman"), digits=2)
  txt <- paste0("Rs = ", r)
  cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt
       # , cex = cex.cor * r
  )
}

panel.smoothScatter <- function(x,y, smoother = TRUE, nrpoints = 0,  col.smooth = 3, span = 1/100, iter = 3, ...){
  smoothScatter(x,y,add=T, nrpoints = nrpoints)
  
  if(smoother){
    ok <- is.finite(x) & is.finite(y)
    if (any(ok)) 
      lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), 
            col = col.smooth, ...)  
  }
  
}

# Pairs function with smoothScatter panels
smoothPairs <- function(df, gap = 0.1, 
                        upper.panel = panel.smoothScatter,
                        lower.panel = function(x,y){
                          panel.smoothScatter(x,y); panel.cor(x,y)},
                        diag.panel = function(x){
                          try(SciViews::panel.density(x, bw ="sj", rug = FALSE))},
                        ...){
  pairs(df, gap = gap, upper.panel=upper.panel, lower.panel=lower.panel, diag.panel=diag.panel, ...)
}



plot_TA_earth <- function(fit, title = "", newdata = NULL,
                          # Col = c("#224F63", "#FF0D45", "#F0E716", "#FD2EE3", "#2E51FB", "#38F7B2" ),
                          Col = Color,#c("#0000FF", "#00FF00", "#FF0000", "#F0E716", "#FF00E8", "#ABABAB"),
                          transformX = function(x){x},
                          transformY = function(y){y},
                          transformCol = function(x){scales::alpha(x,0.5)}, ...){
  # get dimension names and GRsq
  dimnames <- attributes(fit$coefficients)$dimnames[[2]]
  # summary <- summary(fit)
  
  if (nchar(title)>0){
    title <- paste0(": ",title)
  }
  title <- paste0("TA Plot", title)
  # obtain labels 
  
  
  if(!is.null(newdata)){
    ind <- which(names(newdata) %in% dimnames)
    Y <- newdata[,ind]
    X <- newdata[,-ind]
    fitted_values <- predict(fit,X)
    residuals <- Y - fitted_values
    
    # compute R2 based on test data
    # TSS
    mean <- apply(Y,2,mean)
    TSS <- apply(sweep(Y,2,mean)^2,2,sum)
    # RSS
    RSS <- apply(residuals^2,2,sum)
    R2 <- round(1-RSS/TSS,4)
    R2string <- paste0("; test R2 = ",R2)
    title_disc <- " - based on unseen data"
  } else {
    fitted_values <- fit$fitted.values
    residuals <- fit$residuals
    Y <- round(residuals + fitted_values, 4)
    title_disc <- " - based on training data"
    R2string <- character(length(dimnames))
  }
  title <- paste0(title, title_disc)
  w <- Y[,1]
  y <- Y[,2]
  # create groups based on sign interaction
  w_sign <- factor(sign(df$w))
  y_sign <- factor(sign(df$y))
  WxY <- w_sign:y_sign
  
  sorted <- sort(table(WxY), decreasing = TRUE)
  o <- order(sorted[WxY])
  

  pal <- transformCol(Col)
  
  # firstpal <- createPalette(6, c("#010101", "#ff0000"), M=1000)
  
  cols <- pal[seq_along(levels(WxY))[WxY]]
  
  # extract CV R2 or GR2 depending on availability
  CVR2 <- fit$cv.rsq.tab["mean",]
  # CVR2 <- round(fit$cv.rsq.tab["mean",],4)
  if(!is.null(CVR2)){
    GR2string <- paste0(" (CV R2 = ", round(CVR2,4),")")
  } else {
    GR2 <- round(summary(fit)$grsq.per.response,4)
    GR2string <- paste0(" (GR2 = ", GR2,")")
  }
  
  # Create plot
  mult.fig(length(dimnames), main = title)$old.par -> opar
  for(i in 1:length(dimnames)){
    X <- transformX(fitted_values[,i])
    Y <- transformY(residuals[,i])
    plot(X[o], Y[o], col = cols[o],
         main = paste0(dimnames[i], GR2string[i], R2string[i]), 
         xlab = "Fitted Values", ylab = "Residuals", pch = '.', ...)
    # abline(h = 0, lty = 2, col = 2)
    ok <- is.finite(X) & is.finite(Y)
    if (any(ok)) 
      lines(stats::lowess(X[ok], Y[ok], f = 1/3, iter = 3), 
            col = 'red')
  }
  legend('right', legend = levels(WxY), col = pal, 
         cex = 0.7, pch = 19, title = "sign(w):sign(y)", bg = "transparent")
  par(opar)
}


# purrr-style fitting and diagnostics functions ---------------------------


predict_with_seen_levels <- function(fitted_model, new_data) {
  # Extract the levels of factor variables from the model
  factor_levels <- sapply(fitted_model$xlevels, as.character)
  
  # Identify rows with unseen levels and affected factor variables
  drop_rows <- logical(nrow(new_data))
  affected_factors <- character(0)
  for (var in names(factor_levels)) {
    if (var %in% names(new_data) && is.factor(new_data[[var]])) {
      test_levels <- levels(new_data[[var]])[which(table(new_data[[var]]) > 0)]
      unseen_levels <- setdiff(test_levels, factor_levels[[var]])
      if (length(unseen_levels) > 0) {
        drop_rows <- drop_rows | (new_data[[var]] %in% unseen_levels)
        affected_factors <- c(affected_factors, var)
      }
    }
  }
  
  # Store row numbers of rows with unseen levels
  dropped_row_numbers <- which(drop_rows)
  
  # Drop rows with unseen levels
  new_data_clean <- new_data[!drop_rows,]
  
  # Predict on the cleaned data
  predictions <- predict(fitted_model, new_data_clean)
  
  list(predictions = predictions, 
       target = new_data_clean,
       dropped_rows = dropped_row_numbers, 
       affected_factors = unique(affected_factors))
}



# Function to identify factor variables with only one level
find_single_level_factors <- function(data) {
  data <- data.frame(lapply(data %>% select(where(is.factor)), droplevels))
  data %>%
    select_if(function(col) is.factor(col) && nlevels(col) == 1) %>%
    names()
}

# # Apply the function to each nested tibble
# result <- df_train2 %>%
#   mutate(single_level_factors = map(train, find_single_level_factors))
# result$single_level_factors
# 
# result <- df_test2 %>%
#   mutate(single_level_factors = map(test, find_single_level_factors))
# 
# with(data, table(shr_w_lifeins_rng))
# with(data, table(shr_w_realest_rng))
# test <- df_train2$train[[2]]
# 
# lm_test <- lm(f32,test)
# f32
# test

fit_by_group <- function(data = df_stage2, train2, model, grouped = TRUE) {
  # create nested datasets
  df_train2 <- data[train2,] %>% {
    if(!grouped)
      mutate(., group = 1)
    else
      .
  } %>% 
    # group_by(!!rlang::sym(by)) %>% 
    group_by(group) %>% 
    nest() %>% 
    ungroup() %>% 
    rename(train = data)
  
  df_test2 <- df_stage2[!train2,] %>% {
    if(!grouped)
      mutate(., group = 1)
    else
      .
  } %>% 
    group_by(group) %>% 
    nest() %>% 
    ungroup() %>% 
    rename(test = data)
  
  df_reg <- df_train2 %>% 
    left_join(df_test2, by = "group") %>% 
    mutate(fit = purrr::map(train, ~ model(.x)))
  
  

  
  # predict only using levels present in training data data
  df_reg <- df_reg %>% 
    mutate(predicted = purrr::map2(fit, test, ~predict_with_seen_levels(.x,.y)),
           predictions = map(predicted,~.x[["predictions"]] %>% 
                               data.frame() %>% set_names(c("w","y"))),
           target = map(predicted, ~data.frame(.x[["target"]][,c("w","y")])),
           dropped_rows = map(predicted, "dropped_rows"),
           affected_factors = map(predicted, "affected_factors")
    ) %>% 
    # sanity check: new_data without dropped rows should be identical with target
    mutate(check = map2(test,predicted, 
                        function(test, predicted){
                          test <- test %>% tibble() %>% select(c("w","y"))
                          dropped_rows <- predicted$dropped_rows
                          test <- if (length(dropped_rows) == 0) test else test[-dropped_rows,]
                          target <- tibble(predicted$target) %>% select(c("w","y"))
                          all.equal(test, target)
                        }
                        
    ))       
  
  # fun <- function(test, predicted){
  #   test <- test %>% tibble() %>% select(c("w","y"))
  #   dropped_rows <- predicted$dropped_rows
  #   test <- test[-dropped_rows,]
  #   target <- tibble(predicted$target) %>% select(c("w","y"))
  #   all_equal(test, target)
  # }
  # 
  # map2(df_reg$test, df_reg$predicted, fun)
  
  # target = purrr::map(test, ~select(., c(w,y)))) 
  
  # df_reg <- df_reg %>% 
  #   mutate(predicted = purrr::map2(fit, test, ~data.frame(predict(.x,.y)) %>% 
  #                                    set_names(c("w","y"))),
  #          
  #          target = purrr::map(test, ~select(., c(w,y)))) 
  
  
  df_reg %>% 
    # select(!!rlang::sym(by), fit, predicted)
    select(group, fit, predictions, target, dropped_rows, affected_factors, check)
  
  
}

combined_R2 <- function(fit_df){
  
  target <- fit_df$target %>% 
    bind_rows()
  
  predicted <- fit_df$predicted %>% 
    bind_rows()
  
  
  stopifnot(identical(dim(target), dim(predicted)))
  
  sep <- diag(caret::R2(predicted,target))
  
  combined <- mean(diag(caret::R2(predicted,target)))
  
  list(R2_combined = combined, R2 = sep)
  
}


plot_diagnostics_grouped <- function(fit_df, main = ""){
  
  # deparse(substitute((fit_df$fit[[1]]$call)))
  
  
  
  call <- fit_df$fit[[1]]$call
  
  model <- paste(toupper(as.character(call)[1]), "with formula", as.character(call)[2])
  
  R2 <- round(combined_R2(fit_df)$R2,3)
  
  main <- paste(main, model, paste0("(Combined R2 = ", round(combined_R2(fit_df)$R2_combined,3), ")"), sep = " ")
  
  target <- fit_df$target %>% 
    bind_rows()
  
  predicted <- fit_df$predicted %>% 
    bind_rows()
  
  residuals <- target - predicted
  
  scaled_res <- scale(residuals)
  
  dimnames <- attributes(fit_df$fit[[1]]$coefficients)$dimnames[[2]]
  sfsmisc::mult.fig(4,main = main)
  
  for(i in 1:length(dimnames)){
    
    plot(predicted[,i], residuals[,i], 
         main = paste0(dimnames[i], " (R2 = ", R2[i], ")"), 
         xlab = "Fitted Values", ylab = "Residuals",pch = '.')
    # abline(h = 0, lty = 2, col = 2)
    try(lines(lowess(x = predicted[,i],y = residuals[,i]), col = 2))
    
    qqnorm(residuals[,i],main = paste0(dimnames[i], " (R2 = ", R2[i], ")"),pch = '.')
    qqline(residuals[,i], col = "steelblue", lwd = 2)
    
  }
  
}


R2_table <- function(model_list, combined = FALSE){
  
  if(combined){
    fun <- function(x){combined_R2(x)$R2_combined}
    rownames <- "w & y"
  } else {
    fun <- function(x){combined_R2(x)$R2}
    rownames <- c("w","y")
  }
  
  map(model_list, ~round(fun(.), 3)) %>% 
    bind_cols() %>% 
    mutate(variable = rownames) %>% 
    column_to_rownames("variable")
}



get_r2 <- function(fit,newdata = NULL){
  
  
  
  if(is.tbl(fit)){
    
    Y <- fit$target %>% 
      bind_rows() %>% 
      as.matrix()
    
    fitted_values <- fit$predicted %>% 
      bind_rows() %>% 
      as.matrix()
    
    residuals <- Y - fitted_values
    
  } else {
    
    dimnames <- attributes(fit$coefficients)$dimnames[[2]] 
    
    if(is.null(newdata)){
      
      fitted_values <- fitted(fit)
      residuals <- residuals(fit)
      Y <- residuals + fitted_values
      
    } else {
      
      ind <- which(names(newdata) %in% dimnames)
      Y <- newdata[,ind]
      X <- newdata[,-ind]
      fitted_values <- predict(fit,X)
      residuals <- Y - fitted_values
    }
    
  }
  
  
  
  # compute R2 based on test data
  # TSS
  mean <- apply(Y,2,mean)
  TSS <- apply(sweep(Y,2,mean)^2,2,sum)
  # RSS
  RSS <- apply(residuals^2,2,sum)
  round(1-RSS/TSS,4)
  
  
}


# Diagnostics plots with tbl framework ------------------------------------

sort_desc2 <- function(x){
  sorted <- sort(table(x), decreasing = TRUE)
  order(-sorted[as.character(x)])
}


plot_private <- function(x, ...){
  x_private <- sdcMicro::mafast(x, var = names(x), aggr = 50)
  plot(x_private, ...)
}

# Diagnostics plots to be used in model tibble (only requires predictions & targets + file metadata)

plot_diagnostics <- function(target, predictions, main = "Diagnostics Plots ", 
                             add_scatter = TRUE, Col = Color){
  
  if(is.numeric(predictions)){
    predictions <- data.frame(predictions)
  }
  
  if(is.numeric(target)){
    target <- data.frame(target)
  }
  
  residuals <- target - predictions
  
  if(is.numeric(residuals)){
    residuals <- data.frame(residuals)
  }
  
  dimnames <- names(predictions)
  
  if(is.null(dimnames))
    dimnames <- names(residuals)
  
  R2 <- caret::R2(predictions, target)
  
  if(length(dimnames)>1){
    R2 <- diag(R2)
  }
  
  # main <- paste(main, paste0("(Combined (mean) R2 = ", round(mean(R2),3), ")"), sep = " ")
  
  
  w_sign <- factor(sign(target$w))
  y_sign <- factor(sign(target$y))
  WxY <- w_sign:y_sign
  
  o <- sort_desc2(WxY)
  # Col <- c("#0000FF", "#00FF00", "#FF0000", "#F0E716", "#FF00E8", "#ABABAB")
  # Col <- RColorBrewer::brewer.pal(6, "Set1")
  pal <- scales::alpha(Col,0.5)
  
  cols <- pal[seq_along(levels(WxY))[WxY]]
  
  predictions <- data.frame(predictions)
  # predictions <- data.frame(predictions)
  
  plus_n <- ifelse(add_scatter, 2, 0)
  
  sfsmisc::mult.fig(plus_n + length(dimnames)*2, main = main, marP = c(-1,-1,-1,0))
  for(i in 1:length(dimnames)){
    
    pred <- predictions[,i]
    res <- residuals[,i]
    
    # plot(pred[o], res[o], col = cols[o],
    plot_private(pred[o], res[o], col = cols[o],
         main = paste0(dimnames[i], " (R2 = ", round(R2[i],3), ")"), 
         xlab = "Fitted Values", ylab = "Residuals",pch = '.')
    # abline(h = 0, lty = 2, col = 2)
    try(lines(lowess(x = predictions[,i],y = residuals[,i], f = 1/100), col = 2))
    legend('topright', legend = levels(WxY), col = pal, 
           cex = 0.6, pch = 19, title = "sign(w):sign(y)", bg = "transparent")
    
    qqnorm(residuals[,i],main = paste0(dimnames[i], " (R2 = ", round(R2[i],3), ")"),pch = '.')
    qqline(residuals[,i], col = "steelblue", lwd = 2)
    # legend('bottomright', legend = levels(WxY), col = pal, 
    #       cex = 0.6, pch = 19, title = "sign(w):sign(y)", bg = "transparent")
    
  }
  
  if(add_scatter){
    mins <- apply(predictions, 2, min)
    maxs <- apply(predictions, 2, max)
    
    xlim <- c(mins[1],maxs[1])
    ylim <- c(mins[2],maxs[2])
    
    data_lim <- rbind(apply(target,2,min), apply(target,2,max))
    
    xlim <- maxlim(xlim,data_lim[,1])
    ylim <- maxlim(ylim,data_lim[,2])
    
    plot_private(target, xlim = xlim, ylim = ylim, pch = '.', main = "Test Data", 
                 col = cols)
    
    # plot(target, xlim = xlim, ylim = ylim, pch = '.', main = "Test Data", 
    #      col = cols)
    
    plot(predictions[o,], xlim = xlim, ylim = ylim, pch = '.', 
         main = "Predictions", 
         col = cols[o])
    legend('left', legend = levels(WxY), col = pal, 
           cex = 0.9, pch = 19, title = "sign(w):sign(y)", bg = "transparent")
    
  }
  
  
}




plot_diagnostics_tbl <- function(predictions, target, name, main, file = "diagnostics_plot"){
  
  # deparse(substitute((fit_df$fit[[1]]$call)))
  
  file <- paste0(fig_path,name,"_",file,".pdf")
  
  pdf(file)
  
  
  plot_diagnostics(target = target, predictions = predictions, main = main)
  
  
  dev.off()
  
}
library(RColorBrewer)

scatter2 <- function(models, main = "", Col = Color, cex.main = 0.7, marP = c(-1,-1,-1,0),R2 = NULL,
                     same_scale = TRUE, data = NULL, targets = vector("list", length(models))){
  stopifnot(is.list(models))
  stopifnot(!is.null(names(models)))
  N <- length(models)
  names <- names(models)
  # fun_name <- deparse(substitute(fun))
  # Apply function to each model
  # predicts <- lapply(models,fun)
  # Check if dataframe "df" exists in workspace and plot this as well
  if(is.null(data)){
    df_exists <- exists("df_test")
    data <- get("df_test")[,1:2]
  } else {
    data <- data[,c("w", "y")]
    df_exists <- TRUE
  }
  stopifnot(all(names(data) %in% c("w","y")))
  
  if(length(models) > 1){
    mins <- apply(sapply(models, function(df) apply(df, 2, min)),1,min)
    # Get the maximum values for each column
    maxs <- apply(sapply(models, function(df) apply(df, 2, max)),1,max)
  } else {
    mins <- apply(models[[1]], 2, min)
    maxs <- apply(models[[1]], 2, max)
  }
  
  xlim <- c(mins[1],maxs[1])
  ylim <- c(mins[2],maxs[2])
  
  if(df_exists){
    
    data_lim <- rbind(apply(data,2,min), apply(data,2,max))
    
    N = N + 1
    # w_sign <- factor(sign(df$w))
    # y_sign <- factor(sign(df$y))
    # col <- w_sign:y_sign
    # 
    # Col <- c("#0000FF", "#00FF00", "#FF0000", "#F0E716", "#FF00E8", "#ABABAB")
    # pal <- scales::alpha(Col,0.5)
    
    
    w_sign <- factor(sign(data$w))
    y_sign <- factor(sign(data$y))
    WxY <- w_sign:y_sign
    
    o <- sort_desc2(WxY)
    
    pal <- scales::alpha(Col,0.5)
    
    lev_wy <- levels(WxY)
    # pal[seq_along(lev_wy)]
    test <- sapply(WxY, function(x) which(lev_wy == x))
    
    
    col <- pal[seq_along(lev_wy)[WxY]]
    
    
  } else {
    col <- rep(1,nrow(models[[1]]))
  }
  
  mult.fig(N, main = main, marP = marP)
  if(df_exists){
    xlim <- maxlim(xlim,data_lim[,1])
    ylim <- maxlim(ylim,data_lim[,2])
  }
  if(same_scale){
    plot_private(data, xlim = xlim, ylim = ylim, pch = '.', main = "Original data", col = col, cex.main = cex.main)
    # plot(data, xlim = xlim, ylim = ylim, pch = '.', main = "Original data", col = col, cex.main = cex.main)
  } else{
    plot_private(data, pch = '.', main = "Original data", col = col, cex.main = cex.main)
    # plot(data, pch = '.', main = "Original data", col = col, cex.main = cex.main)
  } 
  
  
  for(i in seq_along(models)){
    if(!is.null(targets[[i]])){
      d <- targets[[i]]
    } else {
      d <- data
    }
    
    
    w_sign <- factor(sign(d$w))
    y_sign <- factor(sign(d$y))
    WxY <- w_sign:y_sign
    
    o <- sort_desc2(WxY)
    # Col <- c("#0000FF", "#00FF00", "#FF0000", "#F0E716", "#FF00E8", "#ABABAB")
    pal <- scales::alpha(Col,0.5)
    
    if(exists("lev_wy")){
      col_sel <- sapply(WxY, function(x) which(lev_wy == x))
    } else {
      col_sel <- seq_along(lev_wy)[WxY]
    }
    
    col <- pal[col_sel]
    
    
    
    
    if(is.null(R2)) {
      R2_i <- round(diag(caret::R2(models[[i]],d)),3)
    } else {
      R2_i <- round(R2[[i]],3)
    }
    
    R2_string <- paste0(" - R2: w = ",R2_i[1],", y = ",R2_i[2],"")
    plot(models[[i]][o,], xlim = xlim, ylim = ylim, pch = '.', 
         main = paste0(names[i],R2_string), 
         col = col[o], cex.main = cex.main)
  }
  
}


scatter2_res <- function(models, res, main = "", Col = Color,cex.main = 0.7, marP = c(-1,-1,-1,0),R2 = NULL,
                     same_scale = TRUE, data = NULL, targets = vector("list", length(models))){
  stopifnot(is.list(models))
  stopifnot(!is.null(names(models)))
  stopifnot(all(names(models) == names(res)))
  N <- length(models)
  names <- names(models)
  # fun_name <- deparse(substitute(fun))
  # Apply function to each model
  # predicts <- lapply(models,fun)
  # Check if dataframe "df" exists in workspace and plot this as well
  if(is.null(data)){
    df_exists <- exists("df_test")
    data <- get("df_test")[,1:2]
  } else {
    data <- data[,c("w", "y")]
    df_exists <- TRUE
  }
  stopifnot(all(names(data) %in% c("w","y")))
  
  models2 <- map2(models,res,~.x+.y)
  
  if(length(models) > 1){
    mins <- apply(sapply(models, function(df) apply(df, 2, min)),1,min)
    # Get the maximum values for each column
    maxs <- apply(sapply(models, function(df) apply(df, 2, max)),1,max)
  } else {
    mins <- apply(models2[[1]], 2, min)
    maxs <- apply(models2[[1]], 2, max)
  }
  
  xlim <- c(mins[1],maxs[1])
  ylim <- c(mins[2],maxs[2])
  
  if(df_exists){
    
    data_lim <- rbind(apply(data,2,min), apply(data,2,max))
    
    N = 2*N + 1
    # w_sign <- factor(sign(df$w))
    # y_sign <- factor(sign(df$y))
    # col <- w_sign:y_sign
    # 
    # Col <- c("#0000FF", "#00FF00", "#FF0000", "#F0E716", "#FF00E8", "#ABABAB")
    # pal <- scales::alpha(Col,0.5)
    
    
    w_sign <- factor(sign(data$w))
    y_sign <- factor(sign(data$y))
    WxY <- w_sign:y_sign
    
    o <- sort_desc2(WxY)
    # Col <- RColorBrewer::brewer.pal(6, "Set1")
    # Col <- c("#0000FF", "#00FF00", "#FF0000", "#F0E716", "#FF00E8", "#ABABAB")
    pal <- scales::alpha(Col,0.5)
    
    lev_wy <- levels(WxY)
    # pal[seq_along(lev_wy)]
    test <- sapply(WxY, function(x) which(lev_wy == x))
    
    
    col <- pal[seq_along(lev_wy)[WxY]]
    
    
  } else {
    col <- rep(1,nrow(models[[1]]))
  }
  
  mult.fig(N, main = main, marP = marP)
  if(df_exists){
    xlim <- maxlim(xlim,data_lim[,1])
    ylim <- maxlim(ylim,data_lim[,2])
  }
  if(same_scale){
    plot_private(data, xlim = xlim, ylim = ylim, pch = '.', main = "Original data", col = col, cex.main = cex.main)
    # plot(data, xlim = xlim, ylim = ylim, pch = '.', main = "Original data", col = col, cex.main = cex.main)
  } else{
    plot_private(data, pch = '.', main = "Original data", col = col, cex.main = cex.main)
    # plot(data, pch = '.', main = "Original data", col = col, cex.main = cex.main)
  } 
  
  
  for(i in seq_along(models)){
    if(!is.null(targets[[i]])){
      d <- targets[[i]]
    } else {
      d <- data
    }
    
    
    w_sign <- factor(sign(d$w))
    y_sign <- factor(sign(d$y))
    WxY <- w_sign:y_sign
    
    o <- sort_desc2(WxY)
    # Col <- c("#0000FF", "#00FF00", "#FF0000", "#F0E716", "#FF00E8", "#ABABAB")
    pal <- scales::alpha(Col,0.5)
    
    if(exists("lev_wy")){
      col_sel <- sapply(WxY, function(x) which(lev_wy == x))
    } else {
      col_sel <- seq_along(lev_wy)[WxY]
    }
    
    col <- pal[col_sel]
    
    
    
    
    if(is.null(R2)) {
      R2_i <- round(diag(caret::R2(models2[[i]],d)),3)
      R22_i <- round(diag(caret::R2(models[[i]],d)),3)
    } else {
      R2_i <- round(R2[[i]],3)
    }
    
    R2_string <- paste0(" - R2: w = ",R2_i[1],", y = ",R2_i[2],"")
    R22_string <- paste0(" - R2: w = ",R22_i[1],", y = ",R22_i[2],"")
    plot(models2[[i]][o,], xlim = xlim, ylim = ylim, pch = '.', 
         main = paste0("With Simulated Residuals: ",names[i],R2_string), 
         col = col[o], cex.main = cex.main)
    plot(models[[i]][o,], xlim = xlim, ylim = ylim, pch = '.', 
         main = paste0(names[i],R22_string), 
         col = col[o], cex.main = cex.main)
  }
  
}







R2_string <- function(val){
  
  latex2exp::TeX(sprintf(r'($\R^2 = %g$)', val))
  
}

# 
# fit <- mod
# data <- df_train

predict_rf <- function(fit, data = df_test){
  
  if(is.null(fit$finalModel$xNames)){
    
    if(!all(fit$forest$independent.variable.names %in% names(data))){
      data <- model.matrix(update.formula(f4, .~.-1-ns(age, df = 3) + age),data)
    }
    
    
  } else {
    
    if(!all(fit$finalModel$xNames %in% names(data))){
      data <- model.matrix(update.formula(f4, .~.-1-ns(age, df = 3) + age),data)
    }
    
  }
  
  predict(fit, data)
  
}





smoothScatter2 <- function(models, main = "", cex.main = 0.7,R2 = NULL,
                           same_scale = TRUE, data = NULL, targets = vector("list", length(models)),
                           Col = Color){
  stopifnot(is.list(models))
  stopifnot(!is.null(names(models)))
  N <- length(models)
  names <- names(models)
  # fun_name <- deparse(substitute(fun))
  # Apply function to each model
  # predicts <- lapply(models,fun)
  # Check if dataframe "df" exists in workspace and plot this as well
  if(is.null(data)){
    df_exists <- exists("df_test")
    data <- get("df_test")[,1:2]
  } else {
    data <- data[,c("w", "y")]
    df_exists <- TRUE
  }
  stopifnot(all(names(data) %in% c("w","y")))
  
  if(length(models) > 1){
    mins <- apply(sapply(models, function(df) apply(df, 2, min)),1,min)
    # Get the maximum values for each column
    maxs <- apply(sapply(models, function(df) apply(df, 2, max)),1,max)
  } else {
    mins <- apply(models[[1]], 2, min)
    maxs <- apply(models[[1]], 2, max)
  }
  
  xlim <- c(mins[1],maxs[1])
  ylim <- c(mins[2],maxs[2])
  
  if(df_exists){
    
    data_lim <- rbind(apply(data,2,min), apply(data,2,max))
    
    N = N + 1
    
    # w_sign <- factor(sign(df$w))
    # y_sign <- factor(sign(df$y))
    # col <- w_sign:y_sign
    # 
    # Col <- c("#0000FF", "#00FF00", "#FF0000", "#F0E716", "#FF00E8", "#ABABAB")
    # pal <- scales::alpha(Col,0.5)
    
    
    w_sign <- factor(sign(data$w))
    y_sign <- factor(sign(data$y))
    WxY <- w_sign:y_sign
    
    o <- sort_desc2(WxY)
    # Col <- c("#0000FF", "#00FF00", "#FF0000", "#F0E716", "#FF00E8", "#ABABAB")
    pal <- scales::alpha(Col,0.5)
    
    col <- pal[seq_along(levels(WxY))[WxY]]
    
    
  } else {
    col <- rep(1,nrow(models[[1]]))
  }
  
  mult.fig(N, main = main)
  if(df_exists){
    xlim <- maxlim(xlim,data_lim[,1])
    ylim <- maxlim(ylim,data_lim[,2])
  }
  if(same_scale){
    smoothScatter(data, xlim = xlim, ylim = ylim, 
                  # pch = '.', , col = col, cex.main = cex.main,
                  main = "Original data")
  } else{
    smoothScatter(data, main = "Original data",
                  # pch = '.',  col = col, cex.main = cex.main
    )
  } 
  
  
  for(i in seq_along(models)){
    if(!is.null(targets[[i]])){
      d <- targets[[i]]
    } else {
      d <- data
    }
    
    
    w_sign <- factor(sign(d$w))
    y_sign <- factor(sign(d$y))
    WxY <- w_sign:y_sign
    
    o <- sort_desc2(WxY)
    # Col <- c("#0000FF", "#00FF00", "#FF0000", "#F0E716", "#FF00E8", "#ABABAB")
    # pal <- scales::alpha(Col,0.5)
    
    col <- pal[seq_along(levels(WxY))[WxY]]
    
    
    
    
    if(is.null(R2)) {
      R2_i <- round(diag(caret::R2(models[[i]],d)),3)
    } else {
      R2_i <- round(R2[[i]],3)
    }
    
    R2_string <- paste0(" - R2: w = ",R2_i[1],", y = ",R2_i[2],"")
    smoothScatter(models[[i]][o,], xlim = xlim, ylim = ylim,
                  main = paste0(names[i],R2_string), 
                  # col = col[o], pch = '.',  cex.main = cex.main
    )
  }
  
}







# fit <- rf_all_tbl$fit_y$rf1_reg_grid.rds
predict_rf <- function(fit, data = df_test){
  
  if(is.null(fit$finalModel$xNames)){
    
    if(!all(fit$forest$independent.variable.names %in% names(data))){
      data <- model.matrix(update.formula(f42, .~.-1-ns(age, df = 3) + age),data)
    }
    
    
  } else {
    
    if(!all(fit$finalModel$xNames %in% names(data))){
      data <- model.matrix(update.formula(f42, .~.-1-ns(age, df = 3) + age),data)
    }
    
  }
  
  predict(fit, data)
  
}



# Source: https://www.r-bloggers.com/2014/07/implementing-mclapply-on-windows-a-primer-on-embarrassingly-parallel-computation-on-multicore-systems-with-r/
mclapply.hack <- function(...) {
  ## Create a cluster
  ## ... How many workers do you need?
  ## ... N.B. list(...)[[1]] returns the first 
  ##          argument passed to the function. In
  ##          this case it is the list to iterate over
  size.of.list <- length(list(...)[[1]])
  cl <- makeCluster( min(size.of.list, detectCores()) )
  ## Find out the names of the loaded packages 
  loaded.package.names <- c(
    ## Base packages
    sessionInfo()$basePkgs,
    ## Additional packages
    names( sessionInfo()$otherPkgs ))
  ## N.B. tryCatch() allows us to properly shut down the 
  ##      cluster if an error in our code halts execution
  ##      of the function. For details see: help(tryCatch)
  tryCatch( {
    ## Copy over all of the objects within scope to
    ## all clusters. 
    ## 
    ## The approach is as follows: Beginning with the 
    ## current environment, copy over all objects within
    ## the environment to all clusters, and then repeat
    ## the process with the parent environment. 
    ##
    this.env <- environment()
    while( identical( this.env, globalenv() ) == FALSE ) {
      clusterExport(cl,
                    ls(all.names=TRUE, env=this.env),
                    envir=this.env)
      this.env <- parent.env(environment())
    }
    ## repeat for the global environment
    clusterExport(cl,
                  ls(all.names=TRUE, env=globalenv()),
                  envir=globalenv())
    
    ## Load the libraries on all the clusters
    ## N.B. length(cl) returns the number of clusters
    parLapply( cl, 1:length(cl), function(xx){
      lapply(loaded.package.names, function(yy) {
        ## N.B. the character.only option of 
        ##      require() allows you to give the 
        ##      name of a package as a string. 
        require(yy , character.only=TRUE)})
    })
    
    ## Run the lapply in parallel 
    return( parLapply( cl, ...) )
  }, finally = {        
    ## Stop the cluster
    stopCluster(cl)
  })
}






# Start working with S3 classes -------------------------------------------




# Trash -------------------------------------------------------------------

# ## Function to create winsorized kernel density estimates
# estimate_density <- function(x, winsorize_probs = NULL, bw = "sj", ...){
#   if(is.null(winsorize_probs)){
#     density(x,...)
#   } else {
#     stopifnot(is.numeric(winsorize_probs),length(winsorize_probs)==2)
#     x_win <- DescTools::Winsorize(x,probs = winsorize_probs)
#     density(x_win,...)
#   }
# }
# 
# ## Plot function
# plot_density <- function(x, dens_fun = estimate_density, n_rug = 1000, label = NULL, 
#                          winsorize_probs = NULL, ...){
#   # set seed for rng
#   set.seed(31)
#   # if winsorized, express it in sub title
#   if(!is.null(winsorize_probs)) {
#     sub_title <- paste0("Winsorized at (",paste0(winsorize_probs,collapse = ","),")")
#   } else {
#     sub_title <- ""
#   }
#   # extract Stata's variable label
#   if (is.null(label)) {
#       var_label <- attr(x,"label")
#     } else {
#       var_label <- label
#     } 
#   # extract variable name
#   nm <- deparse(substitute(x))
#   var_name <- str_replace(str_replace(nm,"^\\w*(?=\\$)",""),"\\$","")
#   # replace negative values for income with absolute values
#   if(str_detect(var_name,"^y_")){
#     x <- abs(x)
#   }
#   # actual plot
#   plot(dens_fun(x, winsorize_probs = winsorize_probs, ...),
#        main=paste0("NW Density estimator: ",var_name,"\n",var_label),
#        sub = sub_title)
#   rug(sample(x,size=n_rug))
# }
# 
# # custom summary function
# summary <- function(x) {
#   funs <- c(mean, median, sd, mad, IQR)
#   lapply(funs, function(f) f(x, na.rm = TRUE))
# }
# 
# 
# 
# 
# plot_mvnorm <- function(m, k = NULL) {
#   if (inherits(m, "flexmix")) {
#     if (is.null(k)) {
#       k <- m@k0
#     }
#     if (k <= 1) {
#       stop("Specified components must be at least length 2.")
#     }
#     else if (k >= 16) {
#       stop("Are you really searching for 16 or more components? If so, open an issue ticket --> `https://github.com/pdwaggoner/plotmm/issues` \n\n           We will consider updating the package. \n\n           If not, make sure the mixture model is properly specified.")
#     }
#     num_model <- length(m@model)
#     if (num_model == 1) {
#       family <- m@model[[1]]@dist
#       if (family == "poisson" & ncol(m@model[[1]]@x) == 
#           2) {
#       }
#       else if (family == "gaussian" & ncol(m@model[[1]]@x) == 
#                1) {
#         x <- data.frame(m@model[[1]]@y)
#         colnames(x) <- "density"
#         out_plot <- ggplot2::ggplot(x) + ggplot2::geom_density(ggplot2::aes(x = density), 
#                                                                colour = "darkgray", fill = "lightgray") + 
#           ggplot2::theme_minimal()
#         sigma <- flexmix::parameters(m)[2, ]
#         lam <- table(flexmix::clusters(m))
#         mu <- flexmix::parameters(m)[1, ]
#         for (i in 1:k) {
#           out_plot <- out_plot + ggplot2::stat_function(geom = "line", 
#                                                         fun = plot_mix_comps, args = list(mu = mu[i], 
#                                                                                           sigma = sigma[i], lam = lam[i]/sum(lam), 
#                                                                                           normal = TRUE), colour = component_colors[i], 
#                                                         lwd = 1) + ggplot2::ylab("Density") + ggplot2::xlab("x") + 
#             ggplot2::theme_minimal()
#         }
#       }
#       else if (family == "Gamma" & ncol(m@model[[1]]@x) == 
#                1) {
#         x <- data.frame(m@model[[1]]@y)
#         colnames(x) <- "density"
#         out_plot <- ggplot2::ggplot(x) + ggplot2::geom_density(ggplot2::aes(x = density), 
#                                                                colour = "darkgray", fill = "lightgray") + 
#           ggplot2::theme_minimal()
#         shape <- flexmix::parameters(m)[2, ]
#         lam <- table(flexmix::clusters(m))
#         coef <- flexmix::parameters(m)[1, ]
#         for (i in 1:k) {
#           out_plot <- out_plot + ggplot2::stat_function(geom = "line", 
#                                                         fun = plot_mix_comps, args = list(alpha = shape[i], 
#                                                                                           beta = 1/(coef[i] * shape[i]), lam = lam[i]/sum(lam), 
#                                                                                           gamma = TRUE), colour = component_colors[i], 
#                                                         lwd = 1) + ggplot2::ylab("Density") + ggplot2::theme_minimal()
#         }
#       }
#       else if (family == "binomial" & ncol(m@model[[1]]@x) == 
#                2) {
#       }
#     }
#     else if (num_model > 1) {
#       for (j in i:num_model) {
#         family <- m@model[[j]]@family
#         if (family == "poisson" & ncol(m@model[[1]]@x) == 
#             2) {
#         }
#         else if (family == "gaussian" & ncol(m@model[[j]]@x) == 
#                  1) {
#           x <- data.frame(m@model[[j]]@y)
#           colnames(x) <- "density"
#           out_plot <- ggplot2::ggplot(x) + ggplot2::geom_density(ggplot2::aes(x = density), 
#                                                                  colour = "darkgray", fill = "lightgray") + 
#             ggplot2::theme_minimal()
#           sigma <- flexmix::parameters(m)[[j]][2, ]
#           lam <- table(flexmix::clusters(m))
#           mu <- flexmix::parameters(m)[[j]][1, ]
#           for (i in 1:k) {
#             out_plot <- out_plot + ggplot2::stat_function(geom = "line", 
#                                                           fun = plot_mix_comps, args = list(mu = mu[i], 
#                                                                                             sigma = sigma[i], lam = lam[i]/sum(lam), 
#                                                                                             normal = TRUE), colour = component_colors[i], 
#                                                           lwd = 1) + ggplot2::ylab("Density") + 
#               ggplot2::xlab("x") + ggplot2::theme_minimal()
#           }
#         }
#         else if (family == "Gamma" & ncol(m@model[[j]]@x) == 
#                  1) {
#           x <- data.frame(m@model[[j]]@y)
#           colnames(x) <- "density"
#           out_plot <- ggplot2::ggplot(x) + ggplot2::geom_density(ggplot2::aes(x = density), 
#                                                                  colour = "darkgray", fill = "lightgray") + 
#             ggplot2::theme_minimal()
#           shape <- flexmix::parameters(m)[[j]][2, ]
#           lam <- table(flexmix::clusters(m))
#           scale <- flexmix::parameters(m)[[j]][1, ]
#           for (i in 1:k) {
#             out_plot <- out_plot + ggplot2::stat_function(geom = "line", 
#                                                           fun = plot_mix_comps, args = list(alpha = shape[i], 
#                                                                                             beta = 1/(coef[i] * shape[i]), lam = lam[i]/sum(lam), 
#                                                                                             gamma = TRUE), colour = component_colors[i], 
#                                                           lwd = 1) + ggplot2::ylab("Density") + 
#               ggplot2::theme_minimal()
#           }
#         }
#         else if (family == "binomial") {
#         }
#       }
#     }
#   }
# }
# 

