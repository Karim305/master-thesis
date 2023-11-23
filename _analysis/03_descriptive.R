## descriptiive analysis of full data

# Check distribution of wealth variables over time 


# Load functions
source("D:/WealthInequality/Thesis_Karim/_code/00_utils.R")

fig_dir <- "C:/Users/kelouaghlidi/Dropbox/JointWealthIncome/_Karim/_output/_plots/_general"
# fig_dir <- "D:/WealthInequality/Thesis_Karim/_plots/_general"

# load data
df <- load_data("prepared")

# Different wealth variable types



# Different Income variable types

# income
income_agg <- c("y_labor_avg","y_capital_avg","y_trans_avg","y_other_avg" )

income_semi <- c("y_prim_avg", "y_trans_avg", "y_other_avg")

income_granular <- c("y_emp_avg","y_selfemp_avg","y_securities_avg",
                     "y_realestate_avg","y_pensions_avg", "y_transfers_avg",
                     "y_other_avg")

# wealth
wealth_agg <- c("w_movableass_avg","w_realestate_adj_avg","w_debt_total_avg")

wealth_semi <- c("w_fin_avg","w_businessass_avg","w_otherass_avg",
                 "w_movableass_avg","w_realestate_adj_avg","w_debt_total_avg")

wealth_granular <- c("w_securities_avg","w_lifeinsurance_avg", "w_debt_total_avg",
                     "w_businessass_avg","w_otherass_avg","w_realestate_adj_avg")

wealth_special <- c("w_lotgains_avg","w_inheritance_avg","w_gift_avg",
                    "w_giftaway_avg","w_inheritance_declared_avg",
                    "w_gift_declared_avg","w_giftaway_declared_avg",
                    "w_received_avg")

# Loop over all years
years <- 2003:2019

for(ye in years){
  
  t <- which(years == ye)
  
  df_t <- df %>% 
    filter(year == !!ye)
  
  gc() # drop previous df_t from memory
  
  N <- nrow(df_t)
  
  # pairs plots for outcome + aggregate income variables
  pdf2(paste0("pairs_income_agg","/",ye,"_y_agg.pdf"))
  df_t %>% 
    select(w,y,all_of(income_agg)) %>% 
    smoothPairs(main = paste0(as.character(ye)," (N = ", N, ")"))
  dev.off()
  
  # pairs plots for outcome + aggregate wealth variables
  pdf2(paste0("pairs_wealth_agg","/",ye,"_w_agg.pdf"))
  df_t %>% 
    select(w,y,all_of(wealth_agg)) %>% 
    smoothPairs(main = paste0(as.character(ye)," (N = ", N, ")"))
  dev.off()
  
  # pairs plots for aggregate income and wealth variables
  pdf2(paste0("pairs_agg","/",ye,"_pairs_agg.pdf"))
  df_t %>% 
    select(all_of(wealth_agg), all_of(income_agg)) %>% 
    smoothPairs(main = paste0(as.character(ye)," (N = ", N, ")"))
  dev.off()
  
  # marginal distribution of outcomes by sign(other outcome)
  pdf2(paste0("marginals_by_sign","/",ye,"_marginals_by_sign.pdf"))
  density_marginals(df_t, main = paste0(as.character(ye)," (N = ", N, ")"))
  dev.off()
  
}



df_raw <- load_data("prepared") %>% filter(year %in% c(2017,2018, 2019))

df18 <- df_raw %>% filter(year == 2018) 


pdf2(paste0("marginals_by_sign","/",ye,"_marginals_by_sign.pdf"))
df18 %>% select(all_of())

density_marginals(main = paste0(as.character(ye)," (N = ", N, ")"))
dev.off()


rng_long <- df18 %>% select(ends_with("_rng"), w, y) %>%
  mutate(across(ends_with("_rng"), as.character)) %>% 
  pivot_longer(cols = -c(w,y))


rng_means <- rng_long %>% 
  group_by(name, value) %>% 
  summarize(n = n(), mean_w = mean(w), mean_y = mean(y)) %>% 
  ungroup() %>% 
  mutate(variable = ifelse(str_detect(name, "^shr_y"), "Income", "Wealth")) %>% 
  mutate(col = ifelse(value == "in", 1, ifelse(value == "neg", 2, 3)))

mult.fig(2, main = "Abnormal Observations")
smoothScatter(df18$w, df18$y, main = "Wealth Components", xlab = "w", ylab = "y")
rng_means %>% filter(variable == "Wealth") %>% 
  with(points(mean_w, mean_y, col = col))
smoothScatter(df18$w, df18$y, main = "Income Components", xlab = "w", ylab = "y")
rng_means %>% filter(variable == "Income") %>% 
  with(points(mean_w, mean_y, col = col))

y <- df18$OG_y_tot_avg
y_inv <- inv_ihs10(df18$y) 

all.equal(y, y_inv)



## Check whether rounding around 0 gets rid of minority classes

grep_ch(df_raw, "^OG_")


w_issue<- c("OG_w_securities_avg", "OG_w_otherass_avg", "OG_w_debt_total_avg", "OG_w_inheritance_avg",
            "OG_w_inheritance_declared_avg", "OG_w_giftaway_avg", "OG_w_giftaway_declared_avg", 
            "OG_w_gift_avg", "OG_w_gift_declared_avg")



y_issue <- paste0("OG_", c("y_other_avg", "y_transfers_avg", "y_emp_avg", "y_pensions_avg"))


# Check marginal groups for wealth

w_issue<- "OG_w_securities_avg"

for (var in w_issue){
  
  x <- df18 %>% filter(!!rlang::sym(var) < 0) %>% select(all_of(var), OG_w_tot_avg)
  pairs(x)
  
  
}

df18 %>% select(all_of(w_issue)) %>% filter(OG_w_gift_avg < 0) %>%  pairs()

df18 %>% filter(OG_w_gift_avg < 0) %>% summary()






with(df_raw, table(married = married01, female = female_p1))

with(df_raw, table(married = married01, female = female_p2))


Y <- df_raw %>% filter(year == 2018) %>% distinct(id, w_net_avg, y_tot_avg) %>% select(-id)

smoothScatter(Y, main = "Untransformed Income and Wealth Data", xlab = "Wealth", ylab = "Income", nrpoints = 0)

binorm_plot(Y, breaks = 200)


df18 <- df_raw %>% filter(year %in% c(2017, 2018))

df_check <- df18 %>% select(-contains("decile"),-contains("pctile"), - contains("group")) %>%  
  select(year, id, starts_with("y_"), starts_with("w_")) %>% 
  select(ends_with("_avg"), year, id) %>% distinct()

df_signs <- df_check %>% mutate(across(-c(year,id), function(x) factor(sign(x))))
  
table <- df_signs %>% group_by(year) %>% pivot_longer(-c(year,id)) %>% select(-id) %>% 
  # gather("var", "value") %>% 
  count(year,name,value) %>% 
  group_by(year, name) %>% mutate(prop = prop.table(n))


table




