## Fit non-nested Linear Models (LM)
library(regr)
library(sfsmisc)
library(plgraphics)
library(splines)
library(dplyr)
library(plotmo)

# Load data & create model formulas----------------------------------------
code_path <- "XXXX"
source(paste0(code_path, "/01_setup.R"))


# Create and set model and plot directories
model_name <- "lm_new2"

model_path <- paste0(model_dir,"_",model_name,"/")
fig_path <- paste0(fig_dir,"_",model_name,"/")

dir_create(fig_path)
dir_create(model_path)

df_test <- df[!train,]
df_train <- df[train,]


muni_vars2 <- c("muni_pop_18", "muni_dpop_pct_10to18", "muni_popdensity_perkm2_18","muni_hh_avgsize_18",
                "muni_popshareforeigners_18", "muni_ageshare20to64_18", "muni_ageshare65plus_18",
                "muni_promil_births_18", "muni_promil_deaths_18", "muni_socialbenefits_receiver_share_18",
                "muni_share_1st_sector_17","muni_share_2nd_sector_17", 
                "district", "lab_region")



# Create RHS
(v0 <- paste(additional_term, dummy_term, hh_term, sep = " + "))

(v1 <- paste(muni_vars2, collapse = " + "))

(v2 <- paste(sign_term, sep = " + ", collapse = " + "))

(v3 <- paste(shares_term, sep = " + ", collapse = " + "))

(v4 <- paste(vars4, collapse = " + "))

# Create formulas
f3 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", paste(v0, v1, v3, sep = " + ")))

f4 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", paste(v0, v1, v4, sep = " + ")))

vars_f0 <- strsplit(v0,split = " + ", fixed = TRUE)[[1]]
vars1 <- strsplit(v1,split = " + ", fixed = TRUE)[[1]]
vars_f1 <- c(setdiff(vars_f0, "age"), vars1)
vars_f2 <- c(vars_f1, vars2)
vars_f3 <- c(vars_f1, vars3)
vars_f4 <- c(vars_f1, vars4)

# LM 3: With shares -----------------------
f3
lm3 <- lm(f3, df_train)
summary(lm3)
 
saveRDS(lm3, paste0(model_path, "LMnn3.rds"))

# diagnostics plots
plot_diagnostics_lm(lm3, path = fig_path)
plot_coefs_mlm(lm3, path = paste0(fig_path,"_coefs/"))
plot_xres(lm3, x = df_train$age, path = paste0(fig_path, "_res/"))
lapply(vars_f3, function(y) plot_xres(lm3, x = y, path = paste0(fig_path, "_res/")))

# With all 2-way interactions
f3.2 <- update.formula(f3, .~(.)^2)
lm3.2 <- lm(f3.2, data = df_train)

saveRDS(lm3.2, paste0(model_path, "LMnn3.2.rds"))

# diagnostics plots
plot_coefs_mlm(lm3.2, path = paste0(fig_path,"_coefs/"),
               coefs = extract_significant_coefficients(lm3.2, order = TRUE)[1:20])
plot_xres(lm3.2, x = df$age, path = paste0(fig_path, "_res/"))
lapply(vars_f3, function(y) plot_xres(lm3.2, x = y, path = paste0(fig_path, "_res/")))

# LM 4: With lagged income and wealth -----------------------
f4
lm4 <- lm(f4, df_train)
summary(lm4)

saveRDS(lm4, paste0(model_path, "LMnn4.rds"))

# diagnostics plots
plot_diagnostics_lm(lm4, path = fig_path)
plot_coefs_mlm(lm4, path = paste0(fig_path,"_coefs/"))
plot_xres(lm4, x = df$age, path = paste0(fig_path, "_res/"))
lapply(vars_f4, function(y) plot_xres(lm4, x = y, path = paste0(fig_path, "_res/")))

# With all 2-way interactions
f4.2 <- update.formula(f4, .~(.)^2)
lm4.2 <- lm(f4.2, data = df_train)

saveRDS(lm4.2, paste0(model_path, "LMnn4.2.rds"))

# diagnostics plots
plot_coefs_mlm(lm4.2, path = paste0(fig_path,"_coefs/"), 
               coefs = extract_significant_coefficients(lm4.2, order = TRUE)[1:20])
plot_xres(lm4.2, x = df_train$age, path = paste0(fig_path, "_res/"))
lapply(vars_f4, function(y) plot_xres(lm4.2, x = y, path = paste0(fig_path, "_res/")))

