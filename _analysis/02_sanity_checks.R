## Scale data and select year 2018
library(tidyverse)
library(haven)

data_dir <- "X"
fig_dir <- "XX"
code_path <- "XXXX"
source(paste0(code_path, "/00_utils.R"))




muni_data <- read_dta(paste0(data_dir,"/municipality_characteristics2019.dta")) %>% 
  mutate_all(labelled::to_factor)

theme_set(theme_bw())
theme_update(text = element_text(size=12),
             # panel.grid.major = element_blank(),
             panel.grid.minor = element_blank(),
             strip.background = element_blank(),
             legend.background = element_rect(fill='transparent'),
             legend.box.background = element_rect(fill='transparent')
)


# Load data ---------------------------------------------------------------

wealth_granular <- c("w_securities_avg","w_lifeinsurance_avg", "w_debt_total_avg",
                     "w_businessass_avg","w_otherass_avg","w_realestate_adj_avg")

income_granular <- c("y_emp_avg","y_selfemp_avg","y_securities_avg",
                     "y_realestate_avg","y_pensions_avg", "y_transfers_avg",
                     "y_other_avg")

wealth_special <- c("w_lotgains_avg","w_inheritance_avg","w_gift_avg",
                    "w_giftaway_avg","w_inheritance_declared_avg",
                    "w_gift_declared_avg","w_giftaway_declared_avg",
                    "w_received_avg")



df_full <- load_data() %>% 
  relocate(c(w,y,everything()))



# Check Municipality ------------------------------------------------------

n_muni <- df_full %>% filter(year == 2018) %>% distinct(pid_hh,muni) %>%  count(muni) 




df <- df_full %>%
  filter(year == 2018) %>%
  mutate(O_age = age) %>% 
  mutate(age = pmin(age,99)) %>% 
  mutate(age_cat = cut(age, seq(min(age), max(age), by = 4))) %>% 
  select(-c(pid_hh, n_hh))



df_cat <- df %>% 
  select(-where(is.numeric))


df_long <- df_cat %>% 
  dplyr::select(starts_with("sign")) %>%
  mutate_all(as.character) %>% 
  tidyr::pivot_longer(cols = everything())

cat_counts <- df_long %>% 
  group_by(name) %>% 
  count(value)

minority_classes <- cat_counts %>% 
  filter(n<=25)

minority_classes


df_raw <- load_data("full_data") %>% 
  filter(year <= 2019) %>% select(id, year, muni, starts_with("y_"), starts_with("w_")) %>% 
  select(id, year, muni, ends_with("_avg"))

df_raw %>% select(all_of(wealth_special)) %>% names()

full_vars <- df_raw %>% 
  filter(year >= 2016) %>% 
  distinct() %>% 
  mutate(w_lifeinsurance_avg = w_fin_avg - w_securities_avg,
         w_received_avg = w_inheritance_avg + w_gift_avg) %>% 
  mutate(y_transfers_avg = y_trans_avg - y_tr_pensions_total_avg) %>% 
  rename(y_pensions_avg = y_tr_pensions_total_avg) %>% 
  select(id, year, muni, all_of(income_granular), all_of(wealth_granular), all_of(wealth_special),
         w_tot_avg, y_tot_avg, w_net_avg) %>% group_by(id) %>% 
  arrange(year) %>% mutate(across(-c(year,muni,w_tot_avg,w_net_avg,y_tot_avg),lag)) %>% 
  mutate(muni_l1 = lag(muni)) %>% ungroup()



# Link to municipality Data -----------------------------------------------

test_muni <- muni_data %>% select(muni = gdenr2019,
                                  gde_amgrossreg2018, gde_amreg2018, bezirk, grossregion, 
                                  gde_type9_2012, gde_degurba2011, gde_citycharacter2012,
                                  gde_size2015) %>% mutate(muni = as.numeric(muni))






Y <- full_vars %>% filter(year == 2018) %>% ungroup() %>% select(c(w_net_avg, y_tot_avg)) 

df_raw <- df_raw %>% left_join(test_muni, by = "muni")

df18 <- df_raw %>% filter(year == 2018) %>%
  mutate(w = ihs10(w_net_avg), y = ihs10(y_tot_avg))




table(df18$gde_amgrossreg2018)
table(df18$gde_amreg2018)

with(df18, table(gde_amgrossreg2018, gde_degurba2011))


df18 %>% count(bezirk)


df18 %>% filter(gde_amgrossreg2018 == "Region Lausanne") %>% select(muni) %>% pull() %>% unique()




df18 <- df18 %>% 
  filter(!(bezirk %in% c(600,700)), !is.na(bezirk)) 

library(ggplot2)
library(viridis)

pdf(paste0(fig_dir,"/test.pdf"))
df18 %>% 
  ggplot(aes(x = w, y = y, group = gde_amgrossreg2018, color = gde_amgrossreg2018)) +
  geom_density2d(alpha = .4)
dev.off()


var <- "gde_amgrossreg2018"
levels <- df18[[var]] %>% unique()

xlim <- range(df18$w)
ylim <- range(df18$y)

pdf(paste0(fig_dir,"_general/_sanity_checks/granular/wy_muni_",var,"_scatter.pdf"))
sfsmisc::mult.fig(length(levels) + 1, marP = c(-1,-1,-1,0), main = "Labor Market regions")
with(df18, smoothScatter(w,y,main = "All", xlim = xlim, ylim = ylim, nrpoints = 0))
for(lev in levels){
  sel <- df18[[var]] == lev
  N <- sum(sel, na.rm = TRUE)
  with(df18[sel,], smoothScatter(w,y,main = paste0(lev, " (N=", N, ")"), xlim = xlim, ylim = ylim, nrpoints = 0))
  
}
dev.off()




var <- "gde_type9_2012"
levels <- df18[[var]] %>% unique()

xlim <- range(df18$w)
ylim <- range(df18$y)

pdf(paste0(fig_dir,"_general/_sanity_checks/granular/wy_muni_",var,"_scatter.pdf"))
sfsmisc::mult.fig(length(levels) + 1, marP = c(-1,-1,-1,0), main = "Labor Market regions")
with(df18, smoothScatter(w,y,main = "All", xlim = xlim, ylim = ylim, nrpoints = 0))
for(lev in levels){
  sel <- df18[[var]] == lev
  N <- sum(sel, na.rm = TRUE)
  with(df18[sel,], smoothScatter(w,y,main = paste0(lev, " (N=", N, ")"), xlim = xlim, ylim = ylim, nrpoints = 0))
  
}
dev.off()






pdf(paste0(fig_dir,"_general/_sanity_checks/f_raw_smoothScatter.pdf"))
with(Y, smoothScatter(w_net_avg, y_tot_avg, main = "Joint Distribution of Raw Net Wealth and Total Income", nrpoints = 0))
dev.off()





muni_count <- df_raw %>% filter(year == 2018) %>% count(muni)

muni18 <- df_raw %>% filter(year == 2018) %>% select(starts_with("gde"), bezirk)#, cant)

muni18 %>% summarize_all(n_distinct)

muni_data %>% filter(gdenr2019 == 351)
# Bern = Canton 2
muni_data <- muni_data %>% filter(cant == 2)

test_muni <- muni_data %>% select(muni = gdenr2019,
                     gde_amgrossreg2018, gde_amreg2018, bezirk, grossregion, 
                     gde_type9_2012, gde_degurba2011, gde_citycharacter2012,
                     gde_size2015)

test_muni %>% mutate_all(labelled::to_factor)



with(test_muni, table(bezirk, gde_type9_2012))

install.packages("expss")

library(expss)
cross_cases(test_muni, bezirk, gde_type9_2012)
cross_cases(test_muni, gde_degurba2011, gde_type9_2012)


expss::cross_cases(test_muni, bezirk, gde_type9_2012)

expss::cross_cases(test_muni, bezirk, gde_type9_2012)


muni18 %>% select(-gde_name2019) %>% 
# GGally::ggpairs()

Y <- full_vars %>% filter(year == 2018) %>% ungroup() %>% select(c(w_net_avg, y_tot_avg)) 

pdf(paste0(fig_dir,"_general/_sanity_checks/f_raw_smoothScatter.pdf"))
with(Y, smoothScatter(w_net_avg, y_tot_avg, nrpoints = 0,
                      main = "Joint Distribution of Raw Net Wealth and Total Income"))
dev.off()

w <- ihs10(Y$w_net_avg)
y <- ihs10(Y$y_tot_avg)

pdf(paste0(fig_dir,"_general/_sanity_checks/f_ihs10_smoothScatter.pdf"))
smoothScatter(w, y, main = "Joint Distribution of Transformed Income and Wealth", nrpoints = 0)
dev.off()





rug2 <- function(x, n_remove = 5, n_agg = 30) {
  stopifnot(length(x) > 2 * n_remove)
  sorted <- sort(x)
  trimmed <- sorted[(n_remove + 1):(length(sorted) - n_remove)] %>% data.frame() %>% setnames("x")
  trimmed <- sdcMicro::mafast(trimmed, variables = "x", aggr = n_agg)
  rug(trimmed$x)
}


pdf(paste0(fig_dir,"_general/_sanity_checks/f_raw_marginals.pdf"))
mult.fig(2, main = "Untransformed Marginal Densities", marP = c(-1,-1,-1,0))
plot(density(Y$w_net_avg, bw = "sj"), main = "Wealth")
rug2(Y$w_net_avg)
plot(density(Y$y_tot_avg, bw = "sj"), main = "Income")
rug2(Y$y_tot_avg)
dev.off()

t_wy <- prop.table(table(sign_w = sign(w), sign_y = sign(y)))


t_w <- table(sign(w)) / length(w)
t_y <- table(sign(y)) / length(y)

tab <- rbind(t_wy,'sum y' = t_y) %>% cbind('sum w' = c(t_w,1)) *100 

dimnames(tab) <- list("Wealth" = c("negative", "zero", "positive", ""),
                      "Income" = c("negative", "zero", "positive", ""))


round(tab,3)

tab <- as.table(tab)

xtable::xtable(tab)

pdf(paste0(fig_dir,"_general/_sanity_checks/f_ihs10_marginals.pdf"))
mult.fig(2, main = "Transformed Marginal Densities", marP = c(-1,-1,-1,0))
plot(density(w, bw = "sj"), main = "Wealth")
rug2(w)
plot(density(y, bw = "sj"), main = "Income")
rug2(y)
dev.off()




# pdf(paste0(fig_dir,"_general/_sanity_checks/f_ihs10_marginals.pdf"))
# mult.fig(2, main = "Transformed Marginal Densities", marP = c(-1,-1,-1,0))
# density_plot(w, bw = "sj", main = "Wealth")
# # rug(w)
# density_plot(y, bw = "sj", main = "Income")
# # rug(y)
# dev.off()


require(latex2exp)

range <- c(1,max(abs(range(Y))))
xyaxis <- function() abline(h=0, v=0, col = "gray20", lty = 3)

pdf(paste0(fig_dir,"_general/_sanity_checks/t_abs_diff.pdf"))
curve(abs(ihs10(x)-log10(1+x)), from = range[1], to = range[2],n=1000, log="x", xaxt="n",
      # main = "Absolute Difference |t(x)-log10(x)|"
      main=TeX(r'(Absolute Difference: $|t(x)-log_{10}(1+x)|$)', bold = TRUE),
      ); eaxis(1)
abline(h = 0, col = 2)
curve(abs(ihs10(x)-log10(1+x)), from = range[1], to = range[2],n=1000, log="x", add = TRUE)
dev.off()

pdf(paste0(fig_dir,"_general/_sanity_checks/t_rel_error.pdf"))
curve(1 - ihs10(x) / log10(1+x), .1, 1000, col=2, log = "xy",
      # main="Relative Error: (t(x) - log10(1+x)) / log10(1+x)"
      main=TeX(r'(Relative Error: $\frac{t(x)-log_{10}(1+x)}{log_{10}(1+x)}$)', bold = TRUE),
      xaxt="n", yaxt="n", ylab = ""); eaxis(1); eaxis(2)
dev.off()



log10_plus <- function(x) sign(x)*log10(abs(x)+1)

# plot t(x) 
pdf(paste0(fig_dir,"_general/_sanity_checks/t_curves.pdf"))
curve(asinh(x/2)/log(10), from = -40, to = 40, n=1000, ylab = "f(x)",
      main = "Curve of Data Transformation t(x)"); xyaxis()
curve(log10_plus, col = 2, add = TRUE)#, lwd = 2)
curve(log10(1+x), col=4, lty = 2, add=TRUE, lwd = 1)
curve(log10(x), col=3, lty = 2, add=TRUE, lwd = 1)
legend("bottomright", 
       legend = c(
         TeX(r'($t(x) = \frac{asinh(x/2)}{ln(10)}$)'),
         
         # TeX(r'($t(x)$)'), 
         TeX(r'($sign(x)*log_{10}(1+|x|)$)'),
         TeX(r'($log_{10}(1+x)$)'),TeX(r'($log_{10}(x)$)')),
         
         # "t(x)", "sign(x)*log10(abs(x)+1)", "log10(1+x)", "log10(x)"),
       col = c(1,2,4,3), lty = c(1,1,2,2))
dev.off()

library(tidyverse)
# For all sign variables, get the number of negative observations ---------
# get category counts for all wealth and income variables

df18 <- full_vars %>% filter(year == 2018) %>% 
  mutate(w = ihs10(w_net_avg), y = ihs10(y_tot_avg)) 


df_sign <- df18 %>% select(all_of(wealth_granular), all_of(income_granular), all_of(wealth_special), 
                         y_tot_avg, w_tot_avg, w_net_avg) %>% 
  mutate(across(everything(),sign)) %>% 
  mutate_all(as.character) %>% 
  tidyr::pivot_longer(cols = everything()) %>% 
  group_by(name) %>% count(value)

df_sign <- df_sign %>% group_by(name) %>% mutate(share = n/sum(n)) %>% ungroup() %>% 
  mutate(label = ifelse(share < 0.003, "Rare (<0.3%)", "Common (>=0.3%)"),
         var_type = ifelse(str_detect(name,"^w"), "Wealth", "Income")) 
  
# plot for income
pdf(paste0(fig_dir,"_general/_sanity_checks/y_share_negative.pdf"))
df_sign %>% filter(value == -1, var_type == "Income") %>% 
  mutate(share = share * 100, n = as.character(n)) %>% {
    
    max_share <- max(filter(., label == "Rare (<0.3%)") %>% select(share) %>%  pull())
    
    ggplot(data = ., aes(x = reorder(name, share), y = share)) +
      geom_bar(stat = "identity", fill = "grey69") + 
      coord_flip() +
      facet_wrap(~label, nrow = 2, scales = "free") +
      # add vertical line for comparison
      geom_hline(yintercept = max_share, col = 2) +
      # add text
      geom_text(aes(label = n), color = "blue", hjust = 0.5) +
      ylab("%") + xlab("Variable") +
      ggtitle("Income: Share of Negative Observations") +
      # scale_color_manual(values = "blue", name = "Count") + 
      theme(legend.position = c(.85, .15))
    
  }
dev.off()
  
pdf(paste0(fig_dir,"_general/_sanity_checks/w_share_negative.pdf"))
df_sign %>% filter(value == -1, var_type == "Wealth") %>% 
  mutate(share = share * 100, n = as.character(n)) %>% {
    
    max_share <- max(filter(., label == "Rare (<0.3%)") %>% select(share) %>%  pull())
    
    ggplot(data = ., aes(x = reorder(name, share), y = share)) +
      geom_bar(stat = "identity", fill = "grey69") + 
      coord_flip() +
      facet_wrap(~label, nrow = 2, scales = "free") +
      # add vertical line for comparison
      geom_hline(yintercept = max_share, shape = 2, col = 2) +
      # add text
      geom_text(aes(label = n), colour = "blue", hjust = 0.5) +
      ylab("%") + xlab("Variable") +
      ggtitle("Wealth: Share of Negative Observations") +
      # scale_color_manual(values = "blue", name = "Count") + 
      theme(legend.position = c(.85, .15), axis.text.y = element_text(size = 7))
    
  }
dev.off()



# For each variable, plot worrisome observations on top of smoothS --------




w_scatter <- df_sign %>% filter(value == -1, var_type == "Wealth") 


w_issue <- df_sign %>% filter(value == -1, var_type == "Wealth") %>% pull(name)

# w_issue<- c("OG_w_securities_avg", "OG_w_otherass_avg", "OG_w_debt_total_avg", "OG_w_inheritance_avg",
#             "OG_w_inheritance_declared_avg", "OG_w_giftaway_avg", "OG_w_giftaway_declared_avg", 
#             "OG_w_gift_avg", "OG_w_gift_declared_avg")

y_issue <- df_sign %>% filter(value == -1, var_type == "Income") %>% pull(name)



Y_log <- ihs10(Y)
Y_private <- sdcMicro::mafast(Y_log, variables = names(Y_log), aggr = 50)

w_vars <- setdiff(w_issue, c("w_net_avg")) 

w_scatter <- w_vars[!str_detect(w_vars,"declared")]
# n_agg <- 30
# sfsmisc::mult.fig(length(w_issue), marP = c(-1,-1,0,0),
pdf(paste0(fig_dir,"_general/_sanity_checks/w_negative_scatter_privacy.pdf"))
sfsmisc::mult.fig(length(w_scatter), marP = c(-1,-1,-1,0),
                  main = "Wealth and Income values of Observations with negative Wealth")
for (var in w_scatter){
  sel <- df18[[var]] < 0
  p_issue <- Y_private[sel,]
  N <- sum(sel, na.rm = TRUE)
  col <- if(N > 1000) scales::alpha(2,0.5) else 2
  smoothScatter(Y_log, main = paste0(var," (N=",N,")"), nrpoints = 0)
  with(p_issue, points(w_net_avg, y_tot_avg, col = col, pch = '.'))
  
}
dev.off()




# sfsmisc::mult.fig(length(w_issue), marP = c(-1,-1,0,0),
pdf(paste0(fig_dir,"_general/_sanity_checks/y_negative_scatter_privacy.pdf"))
sfsmisc::mult.fig(length(y_issue), marP = c(-1,-1,-1,0),
                  main = "Wealth and Income values of Observations with negative Income")
for (var in y_issue){
  sel <- df18[[var]] < 0
  p_issue <- Y_private[sel,]
  N <- sum(sel, na.rm = TRUE)
  col <- if(N > 1000) scales::alpha(2,0.5) else 2
  smoothScatter(Y_log, main = paste0(var," (N=",N,")"), nrpoints = 0)
  with(p_issue, points(w_net_avg, y_tot_avg, col = col, pch = '.'))
  
}
dev.off()


## For w_otherass_avg, do smoothPairs + Points

# Pairs function with smoothScatter panels
smoothPairs_points <- function(df, gap = 0.1, sel, col = 2, 
                        upper.panel = function(x,y){
                          panel.smoothScatter(x,y, smoother = FALSE); points(x[sel],y[sel], col = col, pch = '.')},
                        lower.panel = function(x,y){
                          panel.smoothScatter(x,y, smoother = FALSE); points(x[sel],y[sel], col = col, pch = '.')},
                        diag.panel = function(x){
                          
                          try({tryCatch( 
                            {
                              SciViews::panel.density(na.omit(x), bw ="sj", rug = FALSE)
                            },
                            error = function(e) {
                              SciViews::panel.density(na.omit(x), bw ="nrd0", rug = FALSE)
                            }
                          ); rug(x[sel], col = 2)})},
                          
                          # try({SciViews::panel.density(na.omit(x), bw ="sj", rug = FALSE); rug(x[sel], col = 2)})},
                        ...){
  pairs(df, gap = gap, upper.panel=upper.panel, lower.panel=lower.panel, diag.panel=diag.panel, ...)
}

df_wy <- df18 %>% select(w_net_avg, y_tot_avg, any_of(wealth_granular), any_of(income_granular)) %>% 
  mutate_all(ihs10)

# NA_sums <- df_wy %>% rowwise() %>%
#   mutate(all = rowSums(across(where(is.numeric)))) %>%
#   ungroup() %>%
#   mutate_all(is.na) %>% 
#   summarize_all(sum)
  
# microaggregate per variable (to keep signs of variables)
df_wy_private <- lapply(df_wy, function(x){
  df_x <- data.frame(var = x, sign_x = sign(x))
  sdcMicro::mafast(df_x, variables = "var", by = "sign_x", aggr = 50)$var
  }) %>% data.frame() %>% tibble()


# df_wy_private <- sdcMicro::mafast(tibble(df_wy), 
#                                   variables = names(df_wy),
#                                   aggr = 50)


for(var in c(w_scatter, y_issue)){
  sel <- df18[[var]] < 0
  N <- sum(sel, na.rm = TRUE)
  col <- if(N > 1000) scales::alpha(2,0.5) else 2
  pdf(paste0(fig_dir,"_general/_sanity_checks/granular/p_Y_granular_negative_",var,"_scatter.pdf"))
  df_wy_private %>% select(w_net_avg, y_tot_avg, all_of(income_granular)) %>% 
    # mutate_all(ihs10) %>% 
    smoothPairs_points(sel = sel, main = paste0(var," (N=",N,"): Granular Income"), col = col)
  dev.off()
  
  pdf(paste0(fig_dir,"_general/_sanity_checks/granular/p_W_granular_negative_",var,"_scatter.pdf"))
  df_wy_private %>% select(w_net_avg, y_tot_avg, all_of(wealth_granular)) %>% 
    # mutate_all(ihs10) %>% 
    smoothPairs_points(sel = sel, main = paste0(var," (N=",N,"): Granular Wealth"), col = col)
  dev.off()
  
}
sel <- df18$w_otherass_avg < 0
df18 %>% select(w_net_avg, y_tot_avg, all_of(wealth_granular)) %>% 
  mutate_all(ihs10) %>% 
  smoothPairs_points(sel = sel)

# y_issue <- paste0("OG_", c("y_other_avg", "y_transfers_avg", "y_emp_avg", "y_pensions_avg"))


# For W & Y variables, get the share of negatives and positives th --------

# Define the function to compute the share
compute_share <- function(variable, threshold){
  negative_share <- sum(variable < 0 & variable > -threshold) / sum(variable < 0)
  positive_share <- sum(variable > 0 & variable < threshold) / sum(variable > 0)
  negative_total <- sum(variable < 0)
  positive_total <- sum(variable > 0)
  return(list(negative_share = negative_share, positive_share = positive_share,
              negative_total = negative_total, positive_total = positive_total))
}

# Loop through variables and thresholds
thresholds <- seq(0, 1000, by = 5)



results <- data.frame(variable = character(), threshold = numeric(), 
                      share_type = character(), share = numeric(), total = numeric())

# n0 <- nrow(df18)
# df18 <- df18 %>% na.omit()
# n1 <- nrow(df18)

# n0 - n1

for (var in y_issue){
  x <- df18[[var]] %>% na.omit()
  for (th in thresholds){
    shares <- compute_share(x, th)
    results <- rbind(results, data.frame(variable = var, threshold = th, 
                                         share_type = "negative", share = shares$negative_share,
                                         total = shares$negative_total))
    results <- rbind(results, data.frame(variable = var, threshold = th, 
                                         share_type = "positive", share = shares$positive_share,
                                         total = shares$positive_total))
  }
}


results_y <- results %>% mutate(var_type = "income")

# for Wealth
results <- data.frame(variable = character(), threshold = numeric(), 
                      share_type = character(), share = numeric())


for (var in w_issue){
  x <- df18[[var]] %>% na.omit()
  for (th in thresholds){
    shares <- compute_share(x, th)
    results <- rbind(results, data.frame(variable = var, threshold = th, 
                                         share_type = "negative", share = shares$negative_share,
                                         total = shares$negative_total))
    results <- rbind(results, data.frame(variable = var, threshold = th, 
                                         share_type = "positive", share = shares$positive_share,
                                         total = shares$positive_total))
  }
}

results_w <- results %>% mutate(var_type = "wealth")

results <- rbind(results_w, results_y) %>% 
  # mutate(variable = str_remove(variable, "^OG\\_")) %>% 
  mutate(label = paste0(variable, " (N = ", total,")"))

library(ggplot2)

# # Calculate the threshold values for vertical lines for each variable and share_type
# calc_vline_thresholds <- function(data, shares){
#   vlines <- data.frame(variable = character(), share_type = character(), vthreshold = numeric(), share = numeric())
#   
#   for (var in unique(data$variable)){
#     for (st in unique(data$share_type)){
#       subset_data <- subset(data, variable == var & share_type == st)
#       for (sh in shares){
#         first_over_share <- subset_data$threshold[which.max(subset_data$share > sh)]
#         if (length(first_over_share) > 0){
#           vlines <- rbind(vlines, data.frame(variable = var, share_type = st, vthreshold = first_over_share, share = sh))
#         }
#       }
#     }
#   }
#   return(vlines)
# }
# 
# vline_data <- calc_vline_thresholds(results, c(0.01, 0.05, 0.1))
# 
# # Visualize the results with the vertical lines and respective threshold texts
# vline_data %>% filter(share_type == "positive") %>%  {
#   results %>% mutate(share = ifelse(share == 0, NA, share)) %>% 
#     filter(share_type == "positive") %>% 
#     ggplot(aes(x = threshold, y = share, color = share_type)) +
#     geom_line() +
#     facet_grid(variable ~ share_type, scales = "free_y") +
#     labs(title = "Share of Observations Closer to Zero than Threshold",
#          y = "Share", x = "Threshold") +
#     theme_minimal()  +
#     geom_vline(data = ., aes(xintercept = vthreshold), linetype = "dashed", color = "red") +
#     geom_text(data = ., aes(x = vthreshold, y = -0,
#                             label = paste0(share*100, "%")),
#               vjust = 1.5, color = "red", size = 3) + theme_minimal() 
#   
# }





# Plots for Income
pdf(paste0(fig_dir,"_general/_sanity_checks/y_neg_share_marginal.pdf"))
results %>% 
  # mutate(share = ifelse(share == 0, NA, share)) %>% 
  filter(share_type == "negative", var_type == "income") %>% 
  ggplot(aes(x = threshold, y = share, color = share_type)) +
  geom_line() +
  # facet_grid(variable ~ share_type, scales = "free_y") +
  facet_wrap(~ label, ncol = 1, scales = "free_y")+#, scales = "free_y") +
  labs(title = "Income: Share of negative Observations Closer to Zero than Threshold",
       y = "Share", x = "Threshold") + theme(legend.position = "none") + expand_limits(y=0)
dev.off()
# theme_minimal()   + theme_minimal()

pdf(paste0(fig_dir,"_general/_sanity_checks/y_pos_share_marginal.pdf"))
results %>% 
  filter(share_type == "positive", var_type == "income") %>% 
  
  ggplot(aes(x = threshold, y = share, color = share_type)) +
  geom_line() +
  # facet_grid(variable ~ share_type, scales = "free_y") +
  facet_wrap(~ label, scales = "free_y", ncol = 1)+#, scales = "free_y") +
  labs(title = "Income: Share of positive Observations Closer to Zero than Threshold",
       y = "Share", x = "Threshold") + theme(legend.position = "none") + expand_limits(y=0)
dev.off()

# Plots for Wealth
pdf(paste0(fig_dir,"_general/_sanity_checks/w_neg_share_marginal.pdf"))
results %>% 
  # mutate(share = ifelse(share == 0, NA, share)) %>%
  filter(share_type == "negative", var_type == "wealth") %>% 
  ggplot(aes(x = threshold, y = share, color = share_type)) +
  geom_line() +
  facet_wrap(~ label, scales = "free_y", ncol = 2)+#, scales = "free_y") +
  labs(title = "Wealth: Share of negative Observations Closer to Zero than Threshold",
       y = "Share", x = "Threshold") + theme(legend.position = "none") + expand_limits(y=0)
dev.off()
# theme_minimal()   + theme_minimal()

pdf(paste0(fig_dir,"_general/_sanity_checks/w_pos_share_marginal.pdf"))
results %>% 
  filter(share_type == "positive", var_type == "wealth") %>% 
  ggplot(aes(x = threshold, y = share, color = share_type)) +
  geom_line() +
  facet_wrap(~ label, scales = "free_y", ncol = 2)+#, scales = "free_y") +
  labs(title = "Wealth: Share of positive Observations Closer to Zero than Threshold",
       y = "Share", x = "Threshold") + theme(legend.position = "none") + expand_limits(y=0)
dev.off()




## Plots with everything in one graph
pdf(paste0(fig_dir,"_general/_sanity_checks/y_share_marginal.pdf"))
results %>% 
  # mutate(share = ifelse(share == 0, NA, share)) %>%
  filter(var_type == "income") %>% 
  ggplot(aes(x = threshold, y = share, color = share_type)) +
  geom_line() +
  # facet_grid(variable ~ share_type, scales = "free_y") +
  facet_wrap(~ variable, scales = "free_y", ncol = 2)+#, scales = "free_y") +
  labs(title = "Income: Share of Observations Closer to Zero than Threshold",
       y = "Share", x = "Threshold") +theme(legend.position = c(.9, .9))#+ theme(legend.position = "bottom") + expand_limits(y=0)
dev.off()

pdf(paste0(fig_dir,"_general/_sanity_checks/w_share_marginal.pdf"))
results %>% 
  # mutate(share = ifelse(share == 0, NA, share)) %>%
  # filter(share_type == "positive", var_type == "income") %>% 
  filter(var_type == "wealth") %>% 
  ggplot(aes(x = threshold, y = share, color = share_type)) +
  geom_line() +
  # facet_grid(variable ~ share_type, scales = "free_y") +
  facet_wrap(~ variable, scales = "free_y", ncol = 3)+#, scales = "free_y") +
  labs(title = "Wealth: Share of Observations Closer to Zero than Threshold",
       y = "Share", x = "Threshold") +theme(legend.position = c(.85, .07)) + expand_limits(y=0)
dev.off()



ggplot(results, aes(x = threshold, y = share, color = share_type)) +
  geom_line() +
  facet_grid(variable ~ share_type, scales = "free_y") +
  labs(title = "Share of Observations Closer to Zero than Threshold",
       y = "Share", x = "Threshold") +
  theme_minimal() +
  geom_hline(yintercept = c(0.01)) +
  geom_vline(data = vline_data, aes(xintercept = vthreshold), linetype = "dashed", color = "red")


# Visualize the results
library(ggplot2)

ggplot(results, aes(x = threshold, y = share, color = share_type)) +
  geom_line() +
  # facet_wrap(~ variable, scales = "free_y") +
  facet_grid(variable ~ share_type, scales = "free_y") +
  labs(title = "Share of Observations Closer to Zero than Threshold",
       y = "Share", x = "Threshold") +
  theme_minimal() +
  geom_hline(yintercept = c(0.01))



test <- df18 %>% 
  rowwise() %>% 
  mutate(w_net_avg_l1 = rowSums(across(all_of(wealth_granular)))-2*w_debt_total_avg, 
         y_tot_avg_l1 = rowSums(across(all_of(income_granular))))

test <- full_vars %>% group_by(id) %>% arrange(year) %>% mutate(w_net_avg_l1 = lag(w_net_avg),
                                                        y_tot_avg_l1 = lag(y_tot_avg),
                                                        delta = year - lag(year))


test %>% filter(year == 2018) %>% 
  with(plot(ihs10(y_tot_avg_l1), ihs10(y_tot_avg), col = delta, pch = '.'))


test18 <- test %>% ungroup() %>%  filter(year == 2018) %>% select(w_net_avg, w_net_avg_l1, y_tot_avg, y_tot_avg_l1) 
pdf(paste0(fig_dir,"_general/_sanity_checks/smoothPairs_lag.pdf"))
test18 %>% mutate_all(ihs10) %>% na.omit() %>% smoothPairs(main = "Joint Distributions of Income, Wealth, and their lags (2018)")
dev.off()

test18 <- test18 %>% mutate(y_diff = y_tot_avg - y_tot_avg_l1, w_diff = w_net_avg - w_net_avg_l1) 

sfsmisc::mult.fig(2, "Difference of Current Value")





with(test18, plot(ihs10(y_tot_avg), y_diff))

full_ts <- df_raw %>% 
  # filter(year >= 2016) %>% 
  distinct() %>% 
  mutate(w_lifeinsurance_avg = w_fin_avg - w_securities_avg,
         w_received_avg = w_inheritance_avg + w_gift_avg) %>% 
  mutate(y_transfers_avg = y_trans_avg - y_tr_pensions_total_avg) %>% 
  rename(y_pensions_avg = y_tr_pensions_total_avg) %>% 
  select(id, year, all_of(income_granular), all_of(wealth_granular), all_of(wealth_special),
         w_tot_avg, y_tot_avg, w_net_avg) %>% group_by(id) %>% 
  arrange(year) %>% mutate(w_net_avg_l1 = lag(w_net_avg),
                           y_tot_avg_l1 = lag(y_tot_avg),
                           delta = year - lag(year)) %>% 
  select(year, delta, id, starts_with("w_net_"), starts_with("y_tot_")) %>% 
  # mutate(across(c(starts_with("w_"), starts_with("y_")), ihs10)) %>% 
  mutate(w_change = ihs10(w_net_avg) - ihs10(w_net_avg_l1), 
         y_change = ihs10(y_tot_avg) - ihs10(y_tot_avg_l1))



full_ts <- full_ts %>% na.omit()


pdf(paste0(fig_dir,"_general/_sanity_checks/smoothPairs_lag.pdf"))
full_ts %>% filter(year == 2018) %>% with(smoothScatter(ihs10(w_net_avg), w_change, nrpoints = 0))
dev.off()
