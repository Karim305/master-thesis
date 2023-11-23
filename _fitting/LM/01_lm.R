# Fit Linear Models (LM)

library(regr)
library(sfsmisc)
library(plgraphics)
library(splines)
library(dplyr)

# Load data & create model formulas----------------------------------------
code_path <- "XXXX"
source(paste0(code_path, "/01_setup.R"))

library(plotmo)
library(caret)



# Create and set model and plot directories
model_name <- "lm_new"

model_path <- paste0(model_dir,"_",model_name,"/")
fig_path <- paste0(fig_dir,"_",model_name,"/")

dir_create(fig_path)
dir_create(model_path)

# (f1 <- update.formula(f1, .~.-ns(age, df = 3) + age))
# (f2 <- update.formula(f2, .~.-ns(age, df = 3) + age))
# (f3 <- update.formula(f3, .~.-ns(age, df = 3) + age))
# (f4 <- update.formula(f4, .~.-ns(age, df = 3) + age))

models <- c("f1"=f1, "f2"=f2, "f3"=f3, "f4"=f4)


# 
# lm2 <- lm(f22, data = df[train,])
# library(plm)
# random <- plm(f22, data=df[train,], index=c("muni"), model="random")
# fixed <- plm(f22, data=df[train,], index=c("muni"), model="random")
# 
# random <- plm(y ~ x1, data=Panel, index=c("country", "year"), model="random")

length(train)

df_test <- df[!train,]
df <- df[train,]


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



# Create RHS variable groups
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


f0 <- f02
f1 <- f12
f2 <- f22
f3 <- f32
f4 <- f42






# LM 0: Only demographics  ------------------------------------------------


f0
lm0 <- lm(f0,data=df)
summary(lm0)

saveRDS(lm0, paste0(model_path, "lm0.rds"))

f0.2 <- update.formula(f0, .~(.)^2)
lm0.2 <- lm(f0.2,data=df)
summary(lm0)

saveRDS(lm0.2, paste0(model_path, "lm0.2.rds"))



 
# plot_diagnostics_lm(lm0, path = fig_path)
# Scale location plot for wealth suggests 3 to 4 different groups..
# QQ-plot: heavy tails, especially left. Maybe this will be mitigated with flexmix
vars_f0 <- strsplit(v0,split = " + ", fixed = TRUE)[[1]]
lapply(vars_f0, function(y) plot_xres(lm0, x = y, path = paste0(fig_path,"_res/")))
plot_coefs_mlm(lm0, path = paste0(fig_path,"_coefs/"))



# LM 1: Demographics + Municipality characteristics -----------------------

lm1 <- lm(f1,data=df)
summary(lm1)

saveRDS(lm1, paste0(model_path, "lm1.rds"))


# diagnostics plots
plot_diagnostics_lm(lm1, path = fig_path)
# coef plots
plot_coefs_mlm(lm1, path = paste0(fig_path,"_coefs/"))
# residual vs. regressors
vars1 <- strsplit(v1,split = " + ", fixed = TRUE)[[1]]
vars_f1 <- c(setdiff(vars_f0, "age"), vars1)
plot_xres(lm1, x = df$age,path = paste0(fig_path,"_res/"))
lapply(vars_f1, function(y) plot_xres(lm1, x = y,path = paste0(fig_path,"_res/")))

## Model 1.2: like model 1, but all two-way interactions
# include all 2-way interactions
f1.2 <- update.formula(f1, .~(.)^2)
lm1.2 <- lm(f1.2, data = df)

saveRDS(lm1.2, paste0(model_path, "lm1.2.rds"))

summary(lm1.2)
get_r2(lm1.2)
plot_diagnostics_lm(lm1.2, path = fig_path)
# coef plots
plot_coefs_mlm(lm1.2, coefs = extract_significant_coefficients(lm1.2, order = TRUE)[1:20],
               path = paste0(fig_path,"_coefs/"))
plot_xres(lm1.2, x = df$age,path = paste0(fig_path,"_res/"))
lapply(vars_f1, function(y) plot_xres(lm1.2, x = y,path = paste0(fig_path,"_res/")))



## Adjusted R-squared
# R2_lm1 <- data.frame(
#   "only demographics" = get_r2(lm0),
#   "with ns(age,3)" = get_r2(lm1),
#   "with two-way interactions" = get_r2(lm1.2)
# )
# 
# R2_lm1
# 
# 
# 
# aov1 <- anova(lm0, lm1.1, lm1, lm1.2)
# attr(aov1,"heading") <- NULL
# aov1
# 




# LM 2: With signs ---------------------------------------------

lm2 <- lm(f2,data=df)
summary(lm2)

saveRDS(lm2, paste0(model_path, "lm2.rds"))

plot_diagnostics_lm(lm2, path = fig_path)

vars_f2 <- c(vars_f1, vars2)
lapply(vars_f2, function(y) plot_xres(lm2, x = y, path = paste0(fig_path, "_res/")))
plot_xres(lm2,df$age, path = paste0(fig_path, "_res/"))
plot_coefs_mlm(lm2, path = paste0(fig_path, "_coefs/"))


f2.2 <- update.formula(f1, paste(".~(.)^2 + ",v2))

f2.2 <- update.formula(f2, .~(.)^2)
lm2.2 <- lm(f2.2, data = df)

saveRDS(lm2.2, paste0(model_path, "lm2.2.rds"))

get_r2(lm2.2)

plot_diagnostics_lm(lm2.2, path = fig_path)

plot_coefs_mlm(lm2.2, coefs = extract_significant_coefficients(lm2.2, order = TRUE)[1:20],
               path = paste0(fig_path, "_coefs/"))

# coef plots
# residual plots
plot_xres(lm2.2, x = df$age, path =paste0(fig_path, "_res/"))
lapply(vars_f2, function(y) plot_xres(lm2.2, x = y, path =paste0(fig_path, "_res/")))



# LM 3: With shares -----------------------
f3
lm3 <- lm(f3, df)
summary(lm3)

saveRDS(lm3, paste0(model_path, "lm3.rds"))

# diagnostics plots
plot_diagnostics_lm(lm3, path = fig_path)
plot_coefs_mlm(lm3, path = paste0(fig_path,"_coefs/"))
plot_xres(lm3, x = df$age, path = paste0(fig_path, "_res/"))
vars_f3 <- c(vars_f1, vars2, vars3)
lapply(vars_f3, function(y) plot_xres(lm3, x = y, path = paste0(fig_path, "_res/")))

f3.2 <- update.formula(f3, .~(.)^2)
lm3.2 <- lm(f3.2, data = df)

saveRDS(lm3.2, paste0(model_path, "lm3.2.rds"))

get_r2(lm3.2)
plot_coefs_mlm(lm3.2, path = paste0(fig_path,"_coefs/"),
               coefs = extract_significant_coefficients(lm3.2, order = TRUE)[1:20])
plot_xres(lm3.2, x = df$age, path = paste0(fig_path, "_res/"))
lapply(vars_f3, function(y) plot_xres(lm3.2, x = y, path = paste0(fig_path, "_res/")))


# LM 4: With lagged income and wealth -----------------------
f4
lm4 <- lm(f4, df)
summary(lm4)

saveRDS(lm4, paste0(model_path, "lm4.rds"))


# diagnostics plots
# plot_diagnostics_lm(lm4, path = fig_path)
plot_coefs_mlm(lm4, path = paste0(fig_path,"_coefs/"))
plot_xres(lm4, x = df$age, path = paste0(fig_path, "_res/"))
vars_f4 <- c(vars_f1, vars2, vars3, vars4)
lapply(vars_f4, function(y) plot_xres(lm4, x = y, path = paste0(fig_path, "_res/")))


f4.2 <- update.formula(f4, .~(.)^2)
lm4.2 <- lm(f4.2, data = df)

saveRDS(lm4.2, paste0(model_path, "lm4.2.rds"))

get_r2(lm4.2)
plot_coefs_mlm(lm4.2, path = paste0(fig_path,"_coefs/"), 
               coefs = extract_significant_coefficients(lm4.2, order = TRUE)[1:20])
plot_xres(lm4.2, x = df$age, path = paste0(fig_path, "_res/"))
lapply(vars_f4, function(y) plot_xres(lm4.2, x = y, path = paste0(fig_path, "_res/")))





