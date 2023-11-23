## Fit copula to income and wealth data and conduct tests

# Load data Packages and Data----------------------------------------
code_path <- "XXXX"
source(paste0(code_path, "/01_setup.R"))
source(paste0(code_path,"/_utils/copula.R"))

library(nor1mix)
library(copula)
library(VC2copula)
library(plotmo)
library(caret)
library(tidyverse)


theme_set(theme_bw())
theme_update(text = element_text(size=12),
             # panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             strip.background = element_blank(),
             legend.background = element_rect(fill='transparent'),
             legend.box.background = element_rect(fill='transparent')
)

# Create and set model and plot directories
# model_name <- "lm_test"
model_name <- "copula3"

model_path <- paste0(model_dir,"_",model_name,"/")
fig_path <- paste0(fig_dir,"_",model_name,"/")

# Load Fitted Copula models
all_models <- readRDS(paste0(model_dir,"_copula_new/fitted_tbl.rds"))
all_models

for(i in 1:ncol(all_models)){
  if(is.list(all_models[,i] %>% pull())){
    names(all_models[,i][[1]]) <- all_models$name
  }
}

# same number of observations
sapply(all_models$predictions,nrow)


# Fit parametric copulas --------------------------------------------------

# as this may take long, restrict model selection
select_models <- c("lm2", "lm3", "MARS2.3", "MARS3.4", "MARS2.3_wy",
                   "rf2", "rf3", "rf4")


models <- all_models %>% 
  filter(name %in% select_models)

## Experience shows that purrr-style fitting takes forever.. 
## Maybe this is due to purrr conflicting with parallel::parLapply
train_list <- list(
  target = map2(models$target, models$train_id, ~.x[.y,]) %>%
    set_names(models$name),
  res = map2(models$residuals, models$train_id, ~.x[.y,]) %>%
    set_names(models$name),
  fit = map2(models$predictions, models$train_id, ~.x[.y,]) %>%
    set_names(models$name)
)

test_list <- list(
  target = map2(models$target, models$train_id, ~.x[!.y,]) %>%
    set_names(models$name),
  res = map2(models$residuals, models$train_id, ~.x[!.y,]) %>%
    set_names(models$name),
  fit = map2(models$predictions, models$train_id, ~.x[!.y,]) %>%
    set_names(models$name)
)

res_table <- sapply(models$select_resCop,function(x)select_copula(x)$IC_table[1,])


fitCop_apply(models$select_resCop, function(x){paste(round(getTheta(x),3), collapse = "; ")}) %>% xtable::xtable()


# Fit parametric copulas with non-parametric margins to data (wealth, income)
target_cop <- fit_copula(data = models$target_train$lm2)

# Get parameters as latex table
fitCop_apply(models$select_resCop, function(x){paste(round(getTheta(x),3), collapse = "; ")}) %>% xtable::xtable()

# AIC table
select_copula(target_cop)$IC_table  %>% xtable::xtable()

# Fit zero-inflated GMM ---------------------------------------------------

w <- df[train,]$w
y <- df[train,]$y

# Fit GMM to get parametric marginal distributions
w_fit <- zi_norMix(w)
# w_sim <- rnorMix(length(w),w_fit)
y_fit <- zi_norMix(y)

# Plot marginals
pdf(paste0(fig_path,"Y_GMM_margins.pdf"))
sfsmisc::mult.fig(2, marP = c(-1,-1,-1,0), main = "Parametric Marginal Distributions (Zero-inflated GMM with 5 components)")
plot_marginal_density(w, marginal = w_fit, main = "Wealth")
plot_marginal_density(y, marginal = y_fit, main = "Income")
dev.off()


# select "best" copula for data (non-parametric margins)
best_cop <- select_copula(target_cop)$selected_copula

system.time(
  data_mvdc <- fit_mvdc(w,y,copula = best_cop)
)


mvdc <- data_mvdc$mvdc@mvdc

xlim <- c(-4.758979,  6.433602 )

# xlim <- c(-6, 6)
ylim <- c(2, 5.3141 )
contourplot2(mvdc, FUN = dMvdc, n.grid = 42, cuts = 33, lwd = 1/2, xlim = xlim, ylim = ylim, ngrid = 5000) # density
wireframe2(mvdc, FUN = pMvdc, delta = 0.025, xlim = xlim, ylim = ylim, xlab = "Wealth", ylab = "Income", n.grid = 100) # density


xlim <- c(-6,  6.433602 )
# xlim <- c(-6, 6)
ylim <- c(-0.001, 6 )
pdf("mvdc_wire_dmvdc.pdf")
wireframe2(mvdc, FUN = dMvdc, delta = 0.025, xlim = xlim, ylim = ylim, xlab = "Wealth", ylab = "Income", n.grid = 100, 
           main = "PDF of Parametric Gumbel Copula for Income and Wealth \n (with zero-inflated Gaussian Mixture Margins)") # density
dev.off()
pdf("mvdc_wire_pmvdc.pdf")
wireframe2(mvdc, FUN = pMvdc, delta = 0.025, xlim = xlim, ylim = ylim, xlab = "Wealth", ylab = "Income", n.grid = 100, 
           main = "CDF of Parametric Gumbel Copula for Income and Wealth \n (with zero-inflated Gaussian Mixture Margins)") # density
dev.off()


contourplot2(mvdc, FUN = dMvdc, n.grid = 100, cuts = 33, lwd = 1/2, xlim = xlim, ylim = ylim) # density
contourplot2(mvdc, FUN = pMvdc, n.grid = 100, cuts = 33, lwd = 1/2, xlim = xlim, ylim = ylim) # density
pdf("mvdc_contour_dmvdc.pdf")
contourplot2(mvdc, FUN = dMvdc, n.grid = 100, cuts = 33, lwd = 1/2, xlim = xlim, ylim = c(3,6), xlab = "Wealth", ylab = "Income", main = "Joint Distribution of Income and Wealth \n Parametric Gumbel Copula with zero-inflated Gaussian Mixture Margins") # density
dev.off()
sim <- rMvdc(100000, mvdc)
smoothScatter(sim)


gc <- mvdc@copula

U <- rCopula(1000, gc)
plot(U, xlab = quote(U[1]), ylab = quote(U[2]))
wireframe2(gc, dCopula, delta = 0.000001, n.grid = 50)


wireframe2(gc, dCopula, delta = .025, n.grid = 50)
wireframe2(gc, dCopula, delta = .005, n.grid = 50)

#####


# Simulate from mvdc model
mvdc_sim <- rMvdc(length(w), mvdc)

xlim <- range(c(mvdc_sim[,1],w))
ylim <- range(c(mvdc_sim[,2],y))

# Plot marginals
pdf(paste0(fig_path,"Y_GMM_joint.pdf"))
sfsmisc::mult.fig(2, marP = c(-1,-1,-1,0), main = "Joint Distributions of Income and Wealth")
smoothScatter(x = w, y = y, xlim = xlim, ylim = ylim, main = "Data", nrpoints = 0)
smoothScatter(mvdc_sim, xlim = xlim, ylim = ylim, xlab = "w", ylab = "y",  main = "Simulations from Parametric Gumbel Copula", nrpoints = 0)
dev.off()


Y_hat <- mvdc_sim
Y_true <- cbind(w,y)

peacock2_n <- function(x, y, n = 10000, seed = 1994){
  stopifnot(dim(x) == dim(y))
  set.seed(seed)
  N <- nrow(x)
  id <- sample(1:N, n)
  
  Peacock.test::peacock2(x[id,],y[id,])
}


time <- system.time(
  test_rf3 <- peacock2_n(Y_true, Y_hat, n = 100)
)

time[3]

n_vec <- c(100, 500, 1000, 5000, 10000, 50000)

p_val <- numeric(0)
time <- numeric(0)
for(i in seq_along(n_vec)){
  
  n_ <- n_vec[i]
  print(paste0("n = ",n_))
  time_i <- system.time(
    p_val[i] <- peacock2_n(Y_true, Y_hat, n = n_)
  )
  
  time[i] <- time_i
  
}

# peacock2 is super slow...

par(mfrow = c(1,1))
plot(n_vec[1:length(time)], time)
plot(n_vec[1:length(time)], p_val)

# however, can already reject at roughly 5% level with only 1000 observations
# 
# library(Peacock.test)
# system.time(
#   test_rf3 <- peacock2(cbind(w_rf3,y_rf3),X)
#   )


# KS Tests (univariate) ---------------------------------------------------


w_sim <- rnorMix(length(w),w_fit)
y_sim <- rnorMix(length(y),y_fit)
ks.test(w,w_sim)
ks.test(y,y_sim)

# Since we have a huge number of observation, it is not unexpected to reject the Null hypothesis
# let's see if that changes with less data
ks.test(w,w_sim)
ks_test(w, w_norm3)

ks_test(w, w_fit, n = 1000)
ks_test(y, y_fit, n = 1000)

pdf(paste0(fig_path,"Margins_KS_rejection.pdf"))
mult.fig(2, main = "KS Test: Marginal Distributions \n Comparing simulations from 5-component Zero Inflated GMM to Data")
curve(sapply(x,function(x)ks_test(w,w_fit,n = x)$p.value), from = 1000,
      to = 138425, n = 200, main = "Wealth: p-values for different sample sizes", ylab = "p-value", xlab = "n")
abline(h = c(0.1, 0.05, 0.01), col = 2, lty = 2)
curve(sapply(x,function(x)ks_test(y,y_fit,n = x)$p.value), from = 1000,
      to = 138425, n = 200, main = "Income: p-values for different sample sizes", ylab = "p-value", xlab = "n")
abline(h = c(0.1, 0.05, 0.01), col = 2, lty = 2)
dev.off()


# Bivariate FS Test -------------------------------------------------------

library(fasano.franceschini.test)

Y <- cbind(w,y)
system.time(
  fs <- fasano.franceschini.test(mvdc_sim, Y, threads = 8)  
)

fs

names(mvdc_sim) <- c("Wealth", "Income")

pdf(paste0(fig_path,"Y_mvdc_parametric_margins.pdf"))
sfsmisc::mult.fig(2, main = "Marginal Distributions: Simulated Data from parametric Gumbel Copula")
plot_marginal_density(w, x_sim = mvdc_sim[,1], main = "Wealth")
plot_marginal_density(y, x_sim = mvdc_sim[,2], main = "Income")
dev.off()

# Now, add more components ------------------------------------------------

# Fit GMM to get parametric marginal distributions
w_fit2 <- zi_norMix(w, m = 6, zero_sd = 10E-08)
# w_sim <- rnorMix(length(w),w_fit)
y_fit2 <- zi_norMix(y, m = 6, zero_sd = 10E-08)

# Plot marginals
pdf(paste0(fig_path,"Y_GMM_margins_7.pdf"))
sfsmisc::mult.fig(2, marP = c(-1,-1,-1,0), main = "Parametric Marginal Distributions (Zero-inflated GMM with 5 components)")
plot_marginal_density(w, marginal = w_fit2, main = "Wealth")
plot_marginal_density(y, marginal = y_fit2, main = "Income")
dev.off()


pdf(paste0(fig_path,"Margins_KS_rejection_7.pdf"))
mult.fig(2, main = "KS Test: Marginal Distributions \n Comparing simulations from 7-component Zero Inflated GMM to Data")
curve(sapply(x,function(x)ks_test(w,w_fit2,n = x)$p.value), from = 1000, 
      to = 138425, n = 200, main = "Wealth: p-values for different sample sizes", ylab = "p-value", xlab = "n")
abline(h = c(0.1, 0.05, 0.01), col = 2, lty = 2)
curve(sapply(x,function(x)ks_test(y,y_fit2,n = x)$p.value), from = 1000, 
      to = 138425, n = 200, main = "Income: p-values for different sample sizes", ylab = "p-value", xlab = "n")
abline(h = c(0.1, 0.05, 0.01), col = 2, lty = 2)
dev.off()


