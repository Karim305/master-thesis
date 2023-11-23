## Script that creates most of the figures presented in the Results section of my Thesis 
# MBO loss surface and optimization path plots are created in "fit_mbo_solution.R"

# Load data & create model formulas----------------------------------------

code_path <- "XXXX"
source(paste0(code_path, "/01_setup.R"))

# install.packages("regr", repos="http://R-forge.R-project.org")
library(regr)
library(glmnet)
library(sfsmisc)
# install.packages("plgraphics", repos="http://R-forge.R-project.org")
library(plgraphics)
library(splines)
library(dplyr)
library(ranger) # faster
library(caret)
library(tidyverse)
library(RColorBrewer)
library(ggnewscale)

# Set default plotting theme
theme_set(theme_bw())
theme_update(text = element_text(size=12),
             # panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             strip.background = element_blank(),
             legend.background = element_rect(fill='transparent'),
             legend.box.background = element_rect(fill='transparent')
)



# Custom functions --------------------------------------------------------

R2_fun <- function(Y, Y_hat){
  residuals <- Y - Y_hat
  # TSS
  mean <- apply(Y,2,mean)
  TSS <- apply(sweep(Y,2,mean)^2,2,sum)
  # RSS
  RSS <- apply(residuals^2,2,sum)
  1-RSS/TSS
}


plot_r2_from_table <- function(table, lab_size = 6, y_lab = "R-Squared",
                               main = "Comparison of different Models of class 2",
                               submain = "Includes Demographics + sign(Income and Wealth components)"){
  
  df <- table %>% t() %>% data.frame() %>% tibble::rownames_to_column("model") %>% 
    # filter(w > 0.05, y > 0.05) %>% 
    filter(w > 0.05, y > 0.05) %>%
    filter(w < 1, y < 1) %>% 
    pivot_longer(cols = c("w","y")) %>% 
    arrange(desc(value)) 
  
  order <- df %>% 
    group_by(model) %>% 
    summarize(mean_r2 = mean(value)) %>% 
    arrange(mean_r2) %>% 
    mutate(order = row_number())  
  
  df %>% 
    left_join(order, by = "model") %>% 
    mutate(model = factor(model)) %>% 
    mutate(model = fct_reorder(model, order)) %>% 
    arrange(name, order) %>% 
    group_by(name) %>% 
    mutate(upward = value - lag(value) >= 0,
           upward = ifelse(is.na(upward), TRUE, upward)) %>%
    rename(improvement = upward,
           variable = name) %>% 
    ggplot(aes(x = model, y = value, col = variable, group = variable)) +
    geom_point(aes(shape = improvement, size = improvement)) + theme_bw() + geom_line() +
    scale_shape_manual(values = c(2,1)) +
    scale_size_manual(values = c(4,2)) +
    ylab(y_lab) +
    ggtitle(main, 
            subtitle = submain) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) + theme_bw() +
    theme(legend.position = "bottom") + theme(axis.text=element_text(size=lab_size)
                                              # , axis.title=element_text(size=10,face="bold")
    )
  
}


plot_r2 <- function(df, main = "", submain = "", lab_size = 6){
  table <- df %>% 
    pull(R2) %>% 
    set_names(df$name) %>% 
    bind_cols() %>% 
    mutate(rowname = c("w","y")) %>% 
    tibble::column_to_rownames()
  
  table %>% 
    plot_r2_from_table(main = main, submain = submain, lab_size = lab_size)
  
}



# Load models
all_models <- readRDS(paste0(model_dir,"_combined2.rds")) 

# Create and set model and plot directories
model_name <- "combined_results8"

model_path <- paste0(model_dir,"_",model_name,"/")
fig_path <- paste0(fig_dir,"_",model_name,"/")

dir_create(model_path)
dir_create(fig_path)

sum(train)

# Train-test-split
df_train <- df[train,]
df_test <- df[!train,]




all_models <- all_models %>%
  mutate(R2 = map2(target, predictions, R2_fun),
         R2_in_sample = map2(target_train, fitted_values, R2_fun))

for(i in 1:ncol(all_models)){
  if(is.list(all_models[,i] %>% pull())){
    names(all_models[,i][[1]]) <- all_models$name
  }
}




R2_tbl <- all_models %>% select(name, R2, class, grouped) %>% unnest_wider(R2) %>% 
  filter(w > 0.05, y > 0.05) %>%
  filter(w < 1, y < 1) %>% 
  mutate(model_name = toupper(str_extract(name,"^\\w+(?=\\d)")))

# Select models with unreasonable R^2 values
issue_models <- all_models %>% select(name, R2, class, grouped) %>% unnest_wider(R2) %>% 
  filter(w < 0.05 | y < 0.05 | w > 1 | y > 1) %>% select(name) %>% pull()

issues <- all_models %>% filter(name %in% issue_models)


# Plot predictions of models with issues
pdf(paste0(fig_path,"issues_predictions.pdf"))
issues  %>% 
  {
    scatter2(.$predictions, R2 = .$R2, targets = .$target, main = "Predictions from Models with weird R2", same_scale = FALSE)  
  }
dev.off()


issues$R2_in_sample


library(caTools)
library(car)
# names(all_models$fit) <- all_models$name
# vif(all_models$fit$MARS4.3)

lm4 <- readRDS(paste0(model_dir,"_lm_new/lm4.rds"))
lm3 <- readRDS(paste0(model_dir,"_lm_new/lm3.rds"))
lm2 <- readRDS(paste0(model_dir,"_lm_new/lm2.rds"))
lm1 <- readRDS(paste0(model_dir,"_lm_new/lm1.rds"))
# vif_lm4 <- vif(lm4, type="predictor")
# vif_lm3 <- vif(lm3, type="predictor")
# vif_lm2 <- vif(lm2, type="predictor")
# vif_lm1 <- vif(lm1, type="predictor")

lm3.2 <- readRDS(paste0(model_dir,"_lm_new/lm3.2.rds"))
lm4.2 <- readRDS(paste0(model_dir,"_lm_new/lm4.2.rds"))

# c("Pillai", "Wilks", "Hotelling-Lawley", "Roy",
#   "Spherical")

# ANOVA for LM models
anova(lm4, fits$LMnn4.rds, fits$LMnn3.rds, lm3, lm2, test = "Wilks")
anova(lm4, fits$LMnn4.rds, fits$LMnn3.rds, lm3, lm2, test = "Pillai")
anova(fits$LMnn3.rds, lm3, lm2, test = "Pillai")



# Correlation plots of LM and MARS models ---------------------------------
library(corrplot)

# Source for the following two functions: https://towardsdatascience.com/how-to-create-a-correlation-matrix-with-too-many-variables-309cc0c0a57
corr_tab_simple <- function(data=df,sig=0.5){
  #convert data to numeric in order to run correlations
  #convert to factor first to keep the integrity of the data - each value will become a number rather than turn into NA
  df_cor <- data %>% mutate_if(is.character, as.factor)
  df_cor <- df_cor %>% mutate_if(is.factor, as.numeric)
  #run a correlation and drop the insignificant ones
  corr <- cor(df_cor)
  #prepare to drop duplicates and correlations of 1     
  corr[lower.tri(corr,diag=TRUE)] <- NA 
  #drop perfect correlations
  # corr[corr == 1] <- NA 
  #turn into a 3-column table
  corr <- as.data.frame(as.table(corr))
  #remove the NA values from above 
  corr <- na.omit(corr) 
  #select significant values  
  corr <- subset(corr, abs(Freq) > sig) 
  #sort by highest correlation
  corr <- corr[order(-abs(corr$Freq)),] 
  #print table
  print(corr)
  corr
  #turn corr back into matrix in order to plot with corrplot
  # reshape2::acast(corr, Var1~Var2, value.var="Freq")
}

corr_simple <- function(data=df,sig=0.5, ...){
  #convert data to numeric in order to run correlations
  #convert to factor first to keep the integrity of the data - each value will become a number rather than turn into NA
  df_cor <- data %>% mutate_if(is.character, as.factor)
  df_cor <- df_cor %>% mutate_if(is.factor, as.numeric)
  #run a correlation and drop the insignificant ones
  corr <- cor(df_cor)
  #prepare to drop duplicates and correlations of 1     
  corr[lower.tri(corr,diag=TRUE)] <- NA 
  #drop perfect correlations
  # corr[corr == 1] <- NA 
  #turn into a 3-column table
  corr <- as.data.frame(as.table(corr))
  #remove the NA values from above 
  corr <- na.omit(corr) 
  #select significant values  
  corr <- subset(corr, abs(Freq) > sig) 
  #sort by highest correlation
  corr <- corr[order(-abs(corr$Freq)),] 
  #print table
  print(corr)
  #turn corr back into matrix in order to plot with corrplot
  mtx_corr <- reshape2::acast(corr, Var1~Var2, value.var="Freq")
  
  #plot correlations visually
  corrplot(mtx_corr, is.corr=FALSE, tl.col="black", na.label=" ", ...)
}

mars3 <- all_models$fit$MARS3.3
mars4 <- all_models$fit$MARS4.4

# Create plots
pdf(paste0(fig_path,"correlations_lm4.pdf"))
corr_simple(lm4$model, sig = .5 #%>% select(where(is.numeric))
            , method = "pie", tl.cex=.6)
dev.off()

mars_df <- data.frame(mars3$bx)
pdf(paste0(fig_path,"correlations_mars3.3.pdf"))
corr_simple(data.frame(mars3$bx), sig = .5, #%>% select(where(is.numeric))
             method = "pie", tl.pos='n')
dev.off()


pdf(paste0(fig_path,"correlations_mars4.4.pdf"))
corr_simple(data.frame(mars4$bx), sig = .5, #%>% select(where(is.numeric))
            method = "pie", tl.pos='n')
dev.off()

mod_df <- model.matrix(lm3.2) %>% data.frame()
pdf(paste0(fig_path,"correlations_lm3.2.pdf"))
corr_simple(mod_df, sig = .5, #%>% select(where(is.numeric))
            method = "pie", tl.pos='n')
dev.off()


pdf(paste0(fig_path,"correlations_lm3.2_new.pdf"))
corr_simple(model.matrix(lm3.2, newdata = df_test), sig = .5, #%>% select(where(is.numeric))
            method = "pie", tl.pos='n')
dev.off()


mars_cor <- corr_tab_simple(mars_df)


# R-Squared Scatter plots -------------------------------------------------

# set colors
col <- RColorBrewer::brewer.pal(5, "Set1")
names(col) <- 0:4

# create plotting data for out-of-sample and in-sample R^2 (Only "old"/main models)
R2_tbl <- all_models %>% filter(version == "old") %>% select(name, R2, class, grouped) %>% unnest_wider(R2) %>% 
  # filter(w > 0.05, y > 0.05) %>%
  filter(w > 0.05, y > 0.05) %>%
  filter(w < 1, y < 1) %>% 
  # mutate(model_name = toupper(str_extract(name,"^\\w+(?=\\d)")))
  mutate(model_name = str_extract(name,"^(LM|lm|MARS|RF|rf)"))
  
R2_tbl_in <- all_models %>% filter(version == "old") %>% select(name, R2 = R2_in_sample, class, grouped) %>% unnest_wider(R2) %>% 
  # filter(w > 0.05, y > 0.05) %>%
  filter(w > 0.05, y > 0.05) %>%
  filter(w < 1, y < 1) %>% 
  # mutate(model_name = toupper(str_extract(name,"^\\w+(?=\\d)")))
  mutate(model_name = str_extract(name,"^(LM|lm|MARS|RF|rf)"))


pdf(paste0(fig_path,"R2_all.pdf"))
R2_tbl %>%
  mutate(class = factor(class, ordered = TRUE)) %>%
  ggplot(aes(x = w, y = y, color = class, shape = model_name, label = name)) +
  geom_abline(slope = 1, intercept = -0.6, linetype = "dashed", color = "gray70") +
  geom_abline(slope = 1, intercept = -0.4, linetype = "dashed", color = "gray70") +
  geom_abline(slope = 1, intercept = -0.2, linetype = "dashed", color = "gray70") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray70") +
  geom_abline(slope = 1, intercept = 0.2, linetype = "dashed", color = "gray70") +
  geom_abline(slope = 1, intercept = 0.4, linetype = "dashed", color = "gray70") +
  geom_abline(slope = 1, intercept = 0.6, linetype = "dashed", color = "gray70") +
  geom_abline(slope = 1, intercept = 0.8, linetype = "dashed", color = "gray70") +
  geom_point() +
  scale_color_manual(values = col) +
  ggrepel::geom_text_repel(min.segment.length = 0, max.overlaps = 20, size = 3) +
  theme_bw() +
  theme(legend.position = "below") +
  ggtitle("R-Squared on unseen Test Data (with dashed 45°-lines)") +
  xlim(0.19, 1) +
  ylim(0.5, 1) 
dev.off()


pdf(paste0(fig_path,"R2_all_in.pdf"))
R2_tbl_in %>%
  mutate(class = factor(class, ordered = TRUE)) %>%
  ggplot(aes(x = w, y = y, color = class, shape = model_name, label = name)) +
  geom_abline(slope = 1, intercept = -0.6, linetype = "dashed", color = "gray70") +
  geom_abline(slope = 1, intercept = -0.4, linetype = "dashed", color = "gray70") +
  geom_abline(slope = 1, intercept = -0.2, linetype = "dashed", color = "gray70") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray70") +
  geom_abline(slope = 1, intercept = 0.2, linetype = "dashed", color = "gray70") +
  geom_abline(slope = 1, intercept = 0.4, linetype = "dashed", color = "gray70") +
  geom_abline(slope = 1, intercept = 0.6, linetype = "dashed", color = "gray70") +
  geom_abline(slope = 1, intercept = 0.8, linetype = "dashed", color = "gray70") +
  geom_point() +
  scale_color_manual(values = col) +
  ggrepel::geom_text_repel(min.segment.length = 0, max.overlaps = 20, size = 3) +
  theme_bw() +
  theme(legend.position = "below") +
  ggtitle("R-Squared on Training Data (with dashed 45°-lines)") +
  xlim(0.19, 1) +
  ylim(0.5, 1) 
dev.off()


# now for selected models (with new models such as RF MBO)
R2_tbl <- all_models %>% select(name, R2, class, grouped) %>% unnest_wider(R2) %>% 
  # filter(w > 0.05, y > 0.05) %>%
  filter(w > 0.05, y > 0.05) %>%
  filter(w < 1, y < 1) %>% 
  # mutate(model_name = toupper(str_extract(name,"^\\w+(?=\\d)")))
  mutate(model_name = str_extract(name,"^(LM|lm|MARS|RF|rf)"))



R2_tbl_in <- all_models %>% select(name, R2 = R2_in_sample, class, grouped) %>% unnest_wider(R2) %>% 
  # filter(w > 0.05, y > 0.05) %>%
  filter(w > 0.05, y > 0.05) %>%
  filter(w < 1, y < 1) %>% 
  # mutate(model_name = toupper(str_extract(name,"^\\w+(?=\\d)")))
  mutate(model_name = str_extract(name,"^(LM|lm|MARS|RF|rf)"))

# select MARS models to plot
selected_mars <- c("MARS2.3","MARS2.3_WY", "MARS3.4", "MARS3.4_WY", "MARS4.4", "MARS4.4_WY")

R2_mars <- R2_tbl %>% filter(name %in% selected_mars)
R2_LM <- R2_tbl %>% filter(model_name == "LM") %>% filter(w > 0.01, y > 0.01)
R2_complete <- R2_tbl %>% filter(model_name == "RF") %>% bind_rows(R2_mars, R2_LM)

R2_mars <- R2_tbl_in %>% filter(name %in% selected_mars)
R2_LM <- R2_tbl_in %>% filter(model_name == "LM") %>% filter(w > 0.01, y > 0.01)
R2_complete_in <- R2_tbl_in %>% filter(model_name == "RF") %>% bind_rows(R2_mars, R2_LM)

# Scatter plots for selected models
pdf(paste0(fig_path,"R2_sel_all2.pdf"))
R2_complete %>% 
  mutate(class = factor(class, ordered = TRUE)) %>% 
  ggplot(aes(x = w, y = y, color = class, shape = model_name, 
             label = name)) +
  geom_abline(slope = 1, intercept = -0.6, linetype = "dashed", color = "gray70") +
  geom_abline(slope = 1, intercept = -0.4, linetype = "dashed", color = "gray70") +
  geom_abline(slope = 1, intercept = -0.2, linetype = "dashed", color = "gray70") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray70") +
  geom_abline(slope = 1, intercept = 0.2, linetype = "dashed", color = "gray70") +
  geom_abline(slope = 1, intercept = 0.4, linetype = "dashed", color = "gray70") +
  geom_abline(slope = 1, intercept = 0.6, linetype = "dashed", color = "gray70") +
  geom_abline(slope = 1, intercept = 0.8, linetype = "dashed", color = "gray70") +
  geom_point() +
  geom_point() +
  # geom_text(aes(label = model_name)) +
  scale_color_manual(values = col) +
  ggrepel::geom_text_repel(min.segment.length = 0,max.overlaps = 20) +
  theme_bw() +
  theme(legend.position = "below") +
  ggtitle("R-Squared on unseen Test Data (with dashed 45°-lines)") + xlim(0.19,1) + ylim(0.5,1)
dev.off()


pdf(paste0(fig_path,"R2_sel_all_in2.pdf"))
R2_complete_in %>% 
  mutate(class = factor(class, ordered = TRUE)) %>% 
  ggplot(aes(x = w, y = y, color = class, shape = model_name, 
             label = name)) +
  geom_abline(slope = 1, intercept = -0.6, linetype = "dashed", color = "gray70") +
  geom_abline(slope = 1, intercept = -0.4, linetype = "dashed", color = "gray70") +
  geom_abline(slope = 1, intercept = -0.2, linetype = "dashed", color = "gray70") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray70") +
  geom_abline(slope = 1, intercept = 0.2, linetype = "dashed", color = "gray70") +
  geom_abline(slope = 1, intercept = 0.4, linetype = "dashed", color = "gray70") +
  geom_abline(slope = 1, intercept = 0.6, linetype = "dashed", color = "gray70") +
  geom_abline(slope = 1, intercept = 0.8, linetype = "dashed", color = "gray70") +
  geom_point() +
  # geom_text(aes(label = model_name)) +
  scale_color_manual(values = col) +
  ggrepel::geom_text_repel(min.segment.length = 0,max.overlaps = 20) +
  theme_bw() +
  theme(legend.position = "below") +
  ggtitle("R-Squared on Training Data (with dashed 45°-lines)") + xlim(0.19,1) + ylim(0.5,1)
dev.off()



# Plot R2 for LM ----------------------------------------------------------

lm_tbl <- all_models %>% filter(str_detect(name,"^LM")) %>% unnest_wider(R2) %>% 
  # filter(w > 0.05, y > 0.05) %>% 
  filter(w > 0.05, y > 0.05) %>%
  filter(w < 1, y < 1) %>% 
  mutate(model_name = str_extract(name,"^\\w+(?=\\d)")) 

pdf(paste0(fig_path,"R2_LM_all.pdf"))
lm_tbl %>% 
  mutate(class = factor(class, ordered = TRUE)) %>% 
  ggplot(aes(x = w, y = y, color = class, 
             label = name)) +
  geom_point() +
  # geom_text(aes(label = model_name)) +
  scale_color_manual(values = col) +
  ggrepel::geom_text_repel(min.segment.length = 0,max.overlaps = 10) +
  theme_bw() +
  theme(legend.position = "below") +
  ggtitle("R-Squared on unseen Test Data")+ xlim(0.19,1) + ylim(0.5,1)
dev.off()




# Plot R2 for RF ----------------------------------------------------------

rf_tbl <- all_models %>% filter(str_detect(name,"^RF"), class == 3) %>% unnest_wider(R2) %>% 
  # filter(w > 0.05, y > 0.05) %>% 
  filter(w > 0.05, y > 0.05) %>%
  filter(w < 1, y < 1) %>% 
  mutate(model_name = str_extract(name,"^\\w+(?=\\d)")) 

pdf(paste0(fig_path,"R2_RF3_all.pdf"))
rf_tbl %>% 
  mutate(class = factor(class, ordered = TRUE)) %>% 
  ggplot(aes(x = w, y = y, color = class, 
             label = name)) +
  geom_point() +
  # geom_text(aes(label = model_name)) +
  scale_color_manual(values = col) +
  ggrepel::geom_text_repel(min.segment.length = 0,max.overlaps = 10) +
  theme_bw() +
  theme(legend.position = "below") +
  ggtitle("R-Squared on unseen Test Data")#+ xlim(0.707,0.709) + ylim(0.769,0.789)
dev.off()

rf_tbl <- all_models %>% filter(str_detect(name,"^RF"), class >= 2) %>% unnest_wider(R2) %>% 
  # filter(w > 0.05, y > 0.05) %>% 
  filter(w > 0.05, y > 0.05) %>%
  filter(w < 1, y < 1) %>% 
  mutate(model_name = str_extract(name,"^\\w+(?=\\d)")) 

pdf(paste0(fig_path,"R2_RF_all.pdf"))
rf_tbl %>% 
  mutate(class = factor(class, ordered = TRUE)) %>% 
  ggplot(aes(x = w, y = y, color = class, 
             label = name)) +
  geom_point() +
  # geom_text(aes(label = model_name)) +
  scale_color_manual(values = col) +
  ggrepel::geom_text_repel(min.segment.length = 0,max.overlaps = 10) +
  theme_bw() +
  theme(legend.position = "below") +
  ggtitle("R-Squared on unseen Test Data")#+ xlim(0.707,0.709) + ylim(0.769,0.789)
dev.off()




# R^2 Line plots ----------------------------------------------------------

all_models <- all_models %>% 
  mutate(model_name = str_extract(name,"^(MARS|LM|RF)"))


pdf(paste0(fig_path,"MARS_R2_line.pdf"))
all_models %>% filter(model_name == "MARS") %>% 
  plot_r2(main = "MARS: R-Squared on unseen test data",
          # submain = "Includes Demographics + sign(Income and Wealth components)",
          lab_size = 6)
dev.off()


pdf(paste0(fig_path,"LM_R2_line.pdf"))
all_models %>% filter(model_name == "LM") %>% 
  plot_r2(main = "LM: R-Squared on unseen test data", 
          # submain = "Includes Demographics + sign(Income and Wealth components)",
          lab_size = 10)
dev.off()


pdf(paste0(fig_path,"LM_R2_line_in.pdf"))
all_models %>% filter(model_name == "LM") %>% mutate(R2 = R2_in_sample) %>% 
  plot_r2(main = "LM: R-Squared on training data", 
          # submain = "Includes Demographics + sign(Income and Wealth components)",
          lab_size = 10)
dev.off()


pdf(paste0(fig_path,"f2_R2_line.pdf"))
all_models %>% filter(class == 2) %>% 
  plot_r2(main = "Model 2: R-Squared on unseen test data", 
          submain = "Includes Demographics + sign(Income and Wealth components)",
          lab_size = 10)
dev.off()


pdf(paste0(fig_path,"f3_R2_line.pdf"))
all_models %>% filter(class == 3) %>% 
  plot_r2(main = "Model 3: R-Squared on unseen test data",
          submain = "Includes Demographics + signs and shares of Income and Wealth components",
          lab_size = 10)
dev.off()


pdf(paste0(fig_path,"f4_R2_line.pdf"))
all_models %>% filter(class == 4) %>% 
  plot_r2(main = "Model 4: R-Squared on unseen test data", 
          submain = "Includes Demographics + signs, shares and levels of Income and Wealth components",
          lab_size = 10)
dev.off()





# RF Tuning Grid Plots ----------------------------------------------------


selected_models <- c("lm2.2", "MARS2.3")

all_models$name

f2_models <- all_models %>% filter(class == 2)

f2_table <- f2_models %>% 
  pull(R2) %>%
  set_names(f2_models$name) %>%
  bind_rows(.id = "model") 

R2_f2 <- f2_table %>% filter(model %in% selected_models) %>% 
  pivot_longer(cols = c(w,y))

all_models$fit

cv_grid <- readRDS(paste0(model_dir, "_rf_reg2_new/tuning_grid_f2.rds")) 

node <- list(min.node.size =cv_grid$min.node.size %>% unique())
mtry <- list(mtry =cv_grid$mtry %>% unique())
num.trees <- list(num.trees =cv_grid$num.trees %>% unique())


pdf(paste0(fig_path,"RF2_CV_path_R2.pdf"))
cv_grid %>% 
  select(mtry, min.node.size, num.trees, y = r2_y, w = r2_w) %>% 
  pivot_longer(c(y,w)) %>% 
  mutate(across(c(min.node.size, num.trees), ~factor(.x, ordered = TRUE))) %>% 
  ggplot(aes(x = mtry, y = value, color = min.node.size, group = min.node.size)) +
  geom_line() +
  geom_point() +
  ggnewscale::new_scale_color() +
  geom_hline(data = R2_f2, aes(yintercept = value, color = model), linetype = "dashed") +
  facet_grid(name~num.trees, scales = "free_y") +
  theme_bw() + ylab("R-Squared") + ggtitle("Random Forest f2: OOB R-Squared") +
  theme(legend.position = "bottom")
dev.off()  


pdf(paste0(fig_path,"RF2_CV_path_R2_models.pdf"))
cv_grid %>% 
  filter(num.trees == 500) %>% 
  select(mtry, min.node.size, num.trees, y = r2_y, w = r2_w) %>% 
  pivot_longer(c(y,w)) %>% 
  mutate(across(c(min.node.size, num.trees), ~factor(.x, ordered = TRUE))) %>% 
  ggplot(aes(x = mtry, y = value, color = min.node.size, group = min.node.size)) +
  geom_line() +
  geom_point() +
  # add horizontal lines for different competing models
  ggnewscale::new_scale_color() +
  geom_hline(data = R2_f2, aes(yintercept = value, color = model), linetype = "dashed") +
  facet_wrap(~name, scales = "free_y", nrow = 2) +
  theme_bw() + ylab("R-Squared") + ggtitle("Random Forest f2: OOB R-Squared") +
  theme(legend.position = "bottom") + guides(col = guide_legend(nrow = 2))
dev.off()  

pdf(paste0(fig_path,"RF2_CV_path_MSE.pdf"))
cv_grid %>% 
  select(mtry, min.node.size, num.trees, y = mse_y, w = mse_w) %>% 
  pivot_longer(c(y,w)) %>% 
  mutate(across(c(min.node.size, num.trees), ~factor(.x, ordered = TRUE))) %>% 
  ggplot(aes(x = mtry, y = value, color = min.node.size, group = min.node.size)) +
  geom_line() +
  geom_point() +
  facet_grid(name~num.trees, scales = "free_y") +
  theme_bw() + ylab("MSE") + ggtitle("Random Forest f2: OOB Mean Squared Error") +
  theme(legend.position = "bottom")
dev.off()

# Difference in R2 between num.trees
pdf(paste0(fig_path,"RF2_path_R2_diff.pdf"))
cv_grid %>% 
  select(mtry, min.node.size, num.trees, y = r2_y, w = r2_w) %>% 
  pivot_longer(c(y,w)) %>% 
  group_by(mtry, min.node.size, name) %>% 
  arrange(mtry, min.node.size, name, num.trees) %>% 
  mutate(diff = value - lag(value)) %>% 
  ungroup() %>% filter(!is.na(diff)) %>% 
  mutate(across(c(min.node.size, num.trees), ~factor(.x, ordered = TRUE))) %>% 
  ggplot(aes(x = mtry, y = diff, color = min.node.size, group = min.node.size)) +
  geom_line() +
  geom_point() +
  facet_wrap(~name, nrow = 2) +
  theme_bw() + ylab("R-Squared") + ggtitle("Random Forest f2: OOB R-Squared difference between 1000 and 500 trees") +
  theme(legend.position = "bottom")
dev.off()  


# Difference in R2 between num.trees
pdf(paste0(fig_path,"RF2_path_MSE_diff.pdf"))
cv_grid %>% 
  select(mtry, min.node.size, num.trees, y = mse_y, w = mse_w) %>% 
  pivot_longer(c(y,w)) %>% 
  group_by(mtry, min.node.size, name) %>% 
  arrange(mtry, min.node.size, name, num.trees) %>% 
  mutate(diff = value - lag(value)) %>% 
  ungroup() %>% filter(!is.na(diff)) %>% 
  mutate(across(c(min.node.size, num.trees), ~factor(.x, ordered = TRUE))) %>% 
  ggplot(aes(x = mtry, y = diff, color = min.node.size, group = min.node.size)) +
  geom_line() +
  geom_point() +
  facet_wrap(~name, nrow = 2) +
  theme_bw() + ylab("MSE") + ggtitle("Random Forest f2: OOB Mean Squared Error difference between 1000 and 500 trees") +
  theme(legend.position = "bottom")
dev.off()  



## check improved grid search
cv_grid2 <- readRDS(paste0(model_dir,"_rf_reg_oob_new/rf2_reg_grid.rds"))
cv_grid3 <- readRDS(paste0(model_dir,"_rf_reg_oob_new/rf3_reg_grid.rds"))
cv_grid4 <- readRDS(paste0(model_dir,"_rf_reg_oob_new/rf4_reg_grid.rds"))


df_grid <- bind_rows(
  cv_grid2$w$results %>% mutate(variable = "w"),
  cv_grid2$y$results %>% mutate(variable = "y")
) %>% mutate(num.trees = 500)


df_grid3 <- bind_rows(
  cv_grid3$w$results %>% mutate(variable = "w"),
  cv_grid3$y$results %>% mutate(variable = "y")
) %>% mutate(num.trees = 500)


df_grid4 <- bind_rows(
  cv_grid4$w$results %>% mutate(variable = "w"),
  cv_grid4$y$results %>% mutate(variable = "y")
) %>% mutate(num.trees = 500)


pdf(paste0(fig_path,"RF2_R2_caret.pdf"))
df_grid %>%
  mutate(across(c(min.node.size, num.trees), ~factor(.x, ordered = TRUE))) %>%
  rename(value = Rsquared, name = variable) %>%
  ggplot(aes(x = mtry, y = value, color = min.node.size, group = min.node.size)) +
  geom_line() +
  geom_point() +
  # add horizontal lines for different competing models
  # ggnewscale::new_scale_color() +
  # geom_hline(data = R2_f2, aes(yintercept = value, color = model), linetype = "dashed") +
  facet_wrap(~name, scales = "free_y", nrow = 2) +
  theme_bw() + ylab("R-Squared") + ggtitle("Random Forest f2: OOB R-Squared") +
  theme(legend.position = "bottom") + guides(col = guide_legend(nrow = 2))
dev.off()


pdf(paste0(fig_path,"RF3_R2_caret.pdf"))
df_grid3 %>%
  mutate(across(c(min.node.size, num.trees), ~factor(.x, ordered = TRUE))) %>%
  rename(value = Rsquared, name = variable) %>%
  ggplot(aes(x = mtry, y = value, color = min.node.size, group = min.node.size)) +
  geom_line() +
  geom_point() +
  # add horizontal lines for different competing models
  # ggnewscale::new_scale_color() +
  # geom_hline(data = R2_f2, aes(yintercept = value, color = model), linetype = "dashed") +
  facet_wrap(~name, scales = "free_y", nrow = 2) +
  theme_bw() + ylab("R-Squared") + ggtitle("Random Forest f3: OOB R-Squared") +
  theme(legend.position = "bottom") + guides(col = guide_legend(nrow = 2))
dev.off()


pdf(paste0(fig_path,"RF4_R2_caret.pdf"))
df_grid4 %>%
  mutate(across(c(min.node.size, num.trees), ~factor(.x, ordered = TRUE))) %>%
  rename(value = Rsquared, name = variable) %>%
  ggplot(aes(x = mtry, y = value, color = min.node.size, group = min.node.size)) +
  geom_line() +
  geom_point() +
  # add horizontal lines for different competing models
  # ggnewscale::new_scale_color() +
  # geom_hline(data = R2_f2, aes(yintercept = value, color = model), linetype = "dashed") +
  facet_wrap(~name, scales = "free_y", nrow = 2) +
  theme_bw() + ylab("R-Squared") + ggtitle("Random Forest f4: OOB R-Squared") +
  theme(legend.position = "bottom") + guides(col = guide_legend(nrow = 2))
dev.off()


R2_f222 <- cv_grid %>% filter(num.trees == 500) %>% mutate(model = paste0("Best 1st Model: mtry = ",mtry,"; m.n.s. = ", min.node.size)) %>% 
  select(model, y = r2_y, w=r2_w) %>% pivot_longer(c(w,y)) %>% 
  group_by(name) %>% filter(value == max(value))


df_grid <- df_grid %>% group_by(variable) %>% 
  mutate(best_r2 = Rsquared == max(Rsquared),
         best_rmse = RMSE == min(RMSE)) %>% ungroup()

R2_f22 <- df_grid %>% filter(best_r2) %>% mutate(model = paste0("Best 2nd Model: mtry = ",mtry,"; m.n.s. = ", min.node.size)) %>% 
  select(model, name = variable, value = Rsquared) %>%  
  bind_rows(R2_f222)



pdf(paste0(fig_path,"grid2_RF2_CV_path_R2_models_refined_caret.pdf"))
df_grid %>% {
  filter(.,num.trees == 500) %>% 
    mutate(value = Rsquared, name = variable) %>% 
    # select(mtry, min.node.size, num.trees, y = r2_y, w = r2_w) %>% 
    # pivot_longer(c(y,w)) %>% 
    mutate(across(c(min.node.size, num.trees), ~factor(.x, ordered = TRUE))) %>% 
    ggplot(aes(x = mtry, y = value, color = min.node.size, group = min.node.size)) +
    geom_line() +
    geom_point() +
    guides(col = guide_legend(nrow = 4)) + theme(legend.key.size = unit(.5, 'cm')) +
    # add horizontal lines for different competing models
    ggnewscale::new_scale_color() +
    geom_hline(data = R2_f22, aes(yintercept = value, color = model), linetype = "dashed") +
    geom_vline(data = filter(., best_r2) %>% mutate(name = variable,
                                                    model = paste0("Best 2nd Model: mtry = ",mtry,"; m.n.s. = ", min.node.size)), 
               aes(xintercept = mtry, color = model), linetype = "dashed") +
    facet_wrap(~name, scales = "free_y", nrow = 2) +
    theme_bw() + ylab("R-Squared") + ggtitle("Refined Grid for Random Forest f2: OOB R-Squared") +
    theme(legend.position = "bottom", legend.title = element_text(size = 8)) + guides(col = guide_legend(nrow = 4)) #+ theme(legend.key.size = unit(.4, 'cm'))
}
dev.off()  


pdf(paste0(fig_path,"grid2_RF2_CV_path_MSE_refined_caret.pdf"))
df_grid %>% 
  filter(num.trees == 500) %>% 
  mutate(value = RMSE, name = variable) %>% 
  mutate(across(c(min.node.size, num.trees), ~factor(.x, ordered = TRUE))) %>% 
  ggplot(aes(x = mtry, y = value, color = min.node.size, group = min.node.size)) +
  geom_line() +
  geom_point() +
  facet_grid(name~num.trees, scales = "free_y") +
  theme_bw() + ylab("MSE") + ggtitle("Refined Grid for Random Forest f2: OOB Root Mean Squared Error") +
  theme(legend.position = "bottom")
dev.off()



f3_models <- all_models %>% filter(class == 3)

f3_table <- f3_models %>% 
  pull(R2) %>%
  set_names(f3_models$name) %>%
  transpose() %>%
  bind_rows() %>%
  mutate(rowname = c("w","y")) %>%
  tibble::column_to_rownames()


R2_f3 <- data.frame(f3_table) %>% 
  t() %>% data.frame() %>% 
  rownames_to_column("model") %>% 
  pivot_longer(cols = c(w,y)) %>% 
  filter(model %in% c("RF3"))


pdf(paste0(fig_path,"grid2_RF3_CV_path_R2_models_caret.pdf"))
df_grid3 %>% 
  filter(num.trees == 500) %>% 
  mutate(value = Rsquared, name = variable) %>% 
  # select(mtry, min.node.size, num.trees, y = r2_y, w = r2_w) %>% 
  # pivot_longer(c(y,w)) %>% 
  mutate(across(c(min.node.size, num.trees), ~factor(.x, ordered = TRUE))) %>% 
  ggplot(aes(x = mtry, y = value, color = min.node.size, group = min.node.size)) +
  geom_line() +
  geom_point() +
  # add horizontal lines for different competing models
  ggnewscale::new_scale_color() +
  geom_hline(data = R2_f3, aes(yintercept = value, color = model), linetype = "dashed") +
  facet_wrap(~name, scales = "free_y", nrow = 2) +
  theme_bw() + ylab("R-Squared") + ggtitle("Random Forest f3: OOB R-Squared") +
  theme(legend.position = "bottom")
dev.off()  

pdf(paste0(fig_path,"grid2_RF3_CV_path_MSE.pdf"))
df_grid3 %>% 
  filter(num.trees == 500) %>% 
  mutate(value = RMSE, name = variable) %>% 
  mutate(across(c(min.node.size, num.trees), ~factor(.x, ordered = TRUE))) %>% 
  ggplot(aes(x = mtry, y = value, color = min.node.size, group = min.node.size)) +
  geom_line() +
  geom_point() +
  facet_grid(name~num.trees, scales = "free_y") +
  theme_bw() + ylab("MSE") + ggtitle("Random Forest f3: OOB Root Mean Squared Error") +
  theme(legend.position = "bottom")
dev.off()


 
# # mtry min.node.size num.trees
# c(40,200,500)
# c(30,25,500)


# Prediction and Residual Scatter Plots -------------------------------------------------------

rf_all_tbl <- all_models %>% filter(model_name == "RF") %>% filter(!str_detect(name, "mbo"))

pdf(paste0(fig_path,"scatter/RF_predictions_scatter.pdf"))
rf_all_tbl %>% 
  mutate(target = map(target, data.frame)) %>% {
    scatter2(.$predictions, data = df[!train,], main = "RF Predictions") 
  }
dev.off()

rf_all_tbl <- rf_all_tbl %>% mutate(
  target = map(target, data.frame),
  residuals = map2(target,predictions,~.x-.y))

names(rf_all_tbl$residuals) <- names(rf_all_tbl$predictions)

# rf_all_tbl <- rf_all_tbl %>% mutate(R2 = map2(target, predictions, ~diag(caret::R2(.y,.x))))

pdf(paste0(fig_path,"RF_residuals_scatter.pdf"))
rf_all_tbl %>% {
  scatter2(.$residuals, R2 = .$R2, data = df[!train,], main = "Residuals from Random Forest Models", same_scale = FALSE) 
}
dev.off()



rf_all_tbl <- all_models %>% filter(model_name == "RF") %>% filter(!str_detect(name, "500")) %>% 
  filter(!str_detect(name,"RF\\dmbo"))

pdf(paste0(fig_path,"scatter/RF_predictions_scatter_mbo.pdf"))
rf_all_tbl %>% 
  mutate(target = map(target, data.frame)) %>% {
    scatter2(.$predictions, data = df[!train,], main = "Predictions from Random Forest Models") 
  }
dev.off()



## 

# LM Predictions ----------------------------------------------------------

sel_lm <- c("LM0", "LM1", "LM1.2", "LM2", "LM2.2", "LM3", "LM4")
lm_all <- all_models %>% filter(model_name == "LM")  %>% filter(name %in% sel_lm)

pdf(paste0(fig_path,"scatter/LM_predictions_scatter2.pdf"))
lm_all %>% 
  mutate(target = map(target, data.frame)) %>% {
    scatter2(.$predictions, data = df[!train,], main = "LM Predictions", diff_scale =  TRUE) 
  }
dev.off()



pdf(paste0(fig_path,"scatter/LM_predictions_scatter22.pdf"))
lm_all %>% #filter(name != "LM2.2", name != "LM1.2") %>% 
  mutate(target = map(target, data.frame)) %>% {
    scatter3(.$predictions, targets = .$target, main = "LM Predictions") 
  }
dev.off()



rf_all_tbl <- rf_all_tbl %>% mutate(
  target = map(target, data.frame),
  residuals = map2(target,predictions,~.x-.y))

names(rf_all_tbl$residuals) <- names(rf_all_tbl$predictions)

# rf_all_tbl <- rf_all_tbl %>% mutate(R2 = map2(target, predictions, ~diag(caret::R2(.y,.x))))

pdf(paste0(fig_path,"RF_residuals_scatter.pdf"))
rf_all_tbl %>% {
  scatter2(.$residuals, R2 = .$R2, data = df[!train,], main = "Residuals from Random Forest Models", same_scale = FALSE) 
}
dev.off()











# MARS predictions --------------------------------------------------------

all_models %>% filter(model_name == "MARS") %>% select(name) %>% pull()

sel_mars <- c("MARS0.3", "MARS1.3", "MARS2.3", "MARS2.5", "MARS3.3", "MARS3.5", 
              "MARS4.3", "MARS4.4", "MARS4.5", "MARS2.3_WY", "MARS3.4_WY")
mars_all <- all_models %>% filter(model_name == "MARS")  %>% filter(name %in% sel_mars)

pdf(paste0(fig_path,"scatter/MARS_predictions_scatter222.pdf"))
mars_all %>% 
  mutate(target = map(target, data.frame)) %>% {
    scatter3(.$predictions, targets = .$target, main = "MARS Predictions", same_scale = TRUE) 
  }
dev.off()




pdf(paste0(fig_path,"scatter/MARS3_predictions_scatter22.pdf"))
all_models %>% filter(model_name == "MARS", class == 3) %>%  
  mutate(target = map(target, data.frame)) %>% {
    scatter3(.$predictions, targets = .$target, main = "MARS3 Predictions") 
  }
dev.off()






install.packages("Peacock.test")
library(Peacock.test)

names(rf_files)


list.files(path = paste0(model_dir,"_benchmark_split2"))
f2_mars <- readRDS("D:/WealthInequality/Thesis_Karim/_models/_benchmark_split2/f2_models_wy.rds")
simple_lm <- readRDS("D:/WealthInequality/Thesis_Karim/_models/_stage2/simple_lm.rds")



all_mods <- bind_rows(
  rf_all_tbl %>% mutate(formula = 1:4,model = "RF"),
  models_f2 %>% mutate(formula = 2),
  simple_lm %>% mutate(formula = 1:4, model = "LM")
)



models_f2 <- bind_rows(
  rf_tbl,
  mult_fits %>% filter(name == "LMf2_wy"),
  fits %>% filter(name == "LMf2sep_wy"),
  lm2 %>% filter(name == "lm_f2.2"),
  simple_lm %>% filter(name == "lm_f2"),
  simple_mars %>% filter(name == "mars_f2")
)





# MARS Regression Surfaces -------------------------------------------------------

mars2 <- all_models$fit$MARS2.3
mars3 <- all_models$fit$MARS3.3



library(earth)
library(plotmo)

## plotmo searches global env for object named x as this was the alias for data object used in function.
x <- df_train %>% droplevels()

pdf(paste0(fig_path,"plotmo_mars2.3_W.pdf"))
plotmo(mars2, nresponse = 1)
dev.off()

pdf(paste0(fig_path,"plotmo_mars2.3_Y.pdf"))
plotmo(mars2, nresponse = 2)
dev.off()


# plotmo for MARS 3.4
pdf(paste0(fig_path,"plotmo_mars3.3_W.pdf"))
plotmo(mars3, nresponse = 1)
dev.off()

pdf(paste0(fig_path,"plotmo_mars3.3_Y.pdf"))
plotmo(mars3, nresponse = 2)
dev.off()




# RF Variable Importance Plots --------------------------------------------


library(randomForestExplainer)
randomForestExplainer::important_variables(rf)
plot_predict_interaction(rf, variable1 = )


pdf(paste0(fig_path,"RF2mbo500_W_varimp.pdf"))
ggplot(
  enframe(
    all_models$fit$RF2mbo500$w$variable.importance,
    name = "variable",
    value = "importance"
  ),
  aes(
    x = reorder(variable, importance),
    y = importance,
    fill = importance
  )
) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  ylab("Variable Importance") +
  xlab("") +
  ggtitle("RF2mbo500 Wealth: Variable Importance (permutation test)") +
  guides(fill = "none") +
  scale_fill_gradient(low = "blue", high = "red")
dev.off()


pdf(paste0(fig_path,"RF2mbo500_Y_varimp.pdf"))
ggplot(
  enframe(
    all_models$fit$RF2mbo500$y$variable.importance,
    name = "variable",
    value = "importance"
  ),
  aes(
    x = reorder(variable, importance),
    y = importance,
    fill = importance
  )
) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  ylab("Variable Importance") +
  xlab("") +
  ggtitle("RF2mbo500 Income: Variable Importance (permutation test)") +
  guides(fill = "none") +
  scale_fill_gradient(low = "blue", high = "red")
dev.off()




pdf(paste0(fig_path,"RF3v2mbo500_W_varimp.pdf"))
ggplot(
  enframe(
    all_models$fit$RF3v2mbo500$w$variable.importance,
    name = "variable",
    value = "importance"
  ),
  aes(
    x = reorder(variable, importance),
    y = importance,
    fill = importance
  )
) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  ylab("Variable Importance") +
  xlab("") +
  ggtitle("RF3v2mbo500 Wealth: Variable Importance (permutation test)") +
  guides(fill = "none") +
  scale_fill_gradient(low = "blue", high = "red")
dev.off()


pdf(paste0(fig_path,"RF3v2mbo500_Y_varimp.pdf"))
ggplot(
  enframe(
    all_models$fit$RF3v2mbo500$y$variable.importance,
    name = "variable",
    value = "importance"
  ),
  aes(
    x = reorder(variable, importance),
    y = importance,
    fill = importance
  )
) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  ylab("Variable Importance") +
  xlab("") +
  ggtitle("RF3v2mbo500 Income: Variable Importance (permutation test)") +
  guides(fill = "none") +
  scale_fill_gradient(low = "blue", high = "red")
dev.off()




pdf(paste0(fig_path,"RF4v2mbo500_W_varimp.pdf"))
ggplot(
  enframe(
    all_models$fit$RF4v2mbo500$w$variable.importance,
    name = "variable",
    value = "importance"
  ),
  aes(
    x = reorder(variable, importance),
    y = importance,
    fill = importance
  )
) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  ylab("Variable Importance") +
  xlab("") +
  ggtitle("RF4v2mbo500 Wealth: Variable Importance (permutation test)") +
  guides(fill = "none") +
  scale_fill_gradient(low = "blue", high = "red")
dev.off()


pdf(paste0(fig_path,"RF4v2mbo500_Y_varimp.pdf"))
ggplot(
  enframe(
    all_models$fit$RF4v2mbo500$y$variable.importance,
    name = "variable",
    value = "importance"
  ),
  aes(
    x = reorder(variable, importance),
    y = importance,
    fill = importance
  )
) +
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  ylab("Variable Importance") +
  xlab("") +
  ggtitle("RF4v2mbo500 Income: Variable Importance (permutation test)") +
  guides(fill = "none") +
  scale_fill_gradient(low = "blue", high = "red")
dev.off()





# scatter2 ----------------------------------------------------------------

# Plots for models f2
all_models %>% filter(is.na(deg_int)) %>% select(name) %>% pull()

models_f2 <- all_models %>% filter(class == 2, deg_int <= 3)
models_f2 %>% select(name, main)

names(models_f2$predictions) <- models_f2$name

models_f2 <- models_f2 %>% 
  mutate(predictions = map(predictions, data.frame),
         target = map(target, data.frame))

pdf(paste0(fig_path,"all_no22_predictions_f2.pdf"))
models_f2 %>% filter(name != "LM2.2") %>% 
  {
      scatter2(.$predictions, R2 = .$R2, targets = .$target, main = "Predictions from Class 2 Models")  
  }
dev.off()


pdf(paste0(fig_path,"all_predictions_f2.pdf"))
models_f2 %>% {
  scatter2(.$predictions, R2 = .$R2, targets = .$target, main = "Predictions from Class 2 Models")    
}
dev.off()



models_f2


# Diagnostics Plots -------------------------------------------------------



# apply plotting function to all models
models_f2 %>% {
  pmap(list(predictions = .$predictions, target = .$target, name = .$name, main = .$main),
       plot_diagnostics_tbl)
  
}

models_f3 %>% {
  pmap(list(predictions = .$predictions, target = .$target, name = .$name, main = .$main),
       plot_diagnostics_tbl)
  
}


models_f4 %>% {
  pmap(list(predictions = .$predictions, target = .$target, name = .$name, main = .$main),
       plot_diagnostics_tbl)
  
}

models_f3 <- all_models %>% filter(class == 3) 
models_f3 %>% select(name, title_string)

names(models_f3$predictions) <- models_f3$name

# names(models_f2$predictions) <- c("RF2_Grid", "RF2_BO", "LM2_wy", "LM2sep_wy", "LM2.2", "LM2",
#                                   paste0("MARS2_deg",1:5), "MARS2_wy")


mods3 <- c("LM3", "LMnn3", "MARS3.1", "MARS3.2", "MARS3.4", "MARS3.5", "MARS3.4_WY", "RF3")


models_f3 <- models_f3 %>% 
  mutate(predictions = map(predictions, data.frame),
         target = map(target, data.frame)) %>% filter(name %in% mods3)



pdf(paste0(fig_path,"/scatter/all_predictions_f3.pdf"))
models_f3 %>% filter(name != "LM3.2") %>%  {
  scatter2(.$predictions, R2 = .$R2, targets = .$target, main = "Predictions from Models of Class 3")  
}
dev.off()


pdf(paste0(fig_path,"/scatter/all_predictions_f3_noWY.pdf"))
models_f3 %>% filter(name != "MARS3.4_WY") %>%  {
  scatter2(.$predictions, R2 = .$R2, targets = .$target, main = "Predictions from Models of Class 3")  
}
dev.off()



## F4
all_models %>% filter(class == 4) %>% select(name) %>% pull()
mods4 <- c("LM4", "LMnn4", "LMnn4.2", "MARS4.1", "MARS4.2", "MARS4.4", "MARS4.5", "MARS4.4_WY", "RF4", "RF4v2mbo500")
models_f4 <- all_models %>% filter(class == 4) 

models_f4 <- models_f4 %>% 
  mutate(predictions = map(predictions, data.frame),
         target = map(target, data.frame)) %>% filter(name %in% mods4)



pdf(paste0(fig_path,"/scatter/all_predictions_f4.pdf"))
models_f4 %>% filter(name != "LM3.2") %>%  {
  scatter2(.$predictions, R2 = .$R2, targets = .$target, main = "Predictions from Models of Class 4")  
}
dev.off()

sel_mods 

pdf(paste0(fig_path,"/scatter/all_predictions_f3_sel.pdf"))
models_f3 %>% filter(name != "MARS3.4_WY") %>%  {
  scatter2(.$predictions, R2 = .$R2, targets = .$target, main = "Predictions from Models of Class 3")  
}
dev.off()





# Plots for models f2
models_f3 <- all_models %>% filter(class == 3, deg_int <= 4, muni == TRUE)
models_f3 %>% select(name, title_string)

names(models_f2$predictions) <- models_f2$name


models_f3 <- models_f3 %>% 
  mutate(predictions = map(predictions, data.frame),
         target = map(target, data.frame))

pdf(paste0(fig_path,"all_no22_predictions_f2.pdf"))
models_f2 %>% filter(name != "lm2.2") %>% 
  {
    scatter2(.$predictions, R2 = .$R2, targets = .$target)  
  }
dev.off()


pdf(paste0(fig_path,"all_predictions_f2.pdf"))
models_f2 %>% {
  scatter2(.$predictions, R2 = .$R2)  
}
dev.off()



models_f2

# apply plotting function to all models
models_f2 %>% {
  pmap(list(predictions = .$predictions, target = .$target, name = .$name, main = .$main),
       plot_diagnostics_tbl)
  
}

