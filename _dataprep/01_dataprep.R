# This script prepares the full data set for analyses


# Load functions ----------------------------------------------------------
data_dir <- "YYYY"
code_path <- "XXXX"
source(paste0(code_path, "/00_utils.R"))

library(haven)
library(data.table)
library(dtplyr)
library(dplyr)

# Load raw data (Stata format) and save it as .rds
system.time(
  data <- read_dta(pathdata) %>%
    data.table()
)
# Save full data object as .rds file
system.time(
  saveRDS(data, paste0(data_dir, "/full_data.rds"))
)
rm(data)
gc()



# Load data ---------------------------------------------------------------

data <- readRDS(paste0(data_dir, "/full_data.rds"))
  # data.table()


# check distribution of negatives around zero -----------------------------
# 
# df18 <- data %>% 
#   lazy_dt() %>% filter(year %in% c(2017,2018)) %>% 
#   as.data.table()
# 
# # df18 %>% 
# #   select(starts_with("y_")) %>% smoothPairs()
# 
# 
# df18 %>% filter()

ncol0 <- ncol(data)
ncol0

# Re-create wealth variables (+ debt) -------------------------------------

## granular wealth components:

# 1) w_fin_avg 
#   (wsv - Wertschriften, Bankguthaben )
#   + (rkw_lebens - Lebensversicherungen)

# 2) w_securities_avg (only wsv)


# Thus, can recreate rkw_lebens by w_lifeinsurance_avg = w_fin_avg - w_securities_avg

# 3) w_businessass_avg: -verm_betr + verm_kk + verm_bg + ek_s + ek_l 
#   (verm_betr is negative for some reason in raw data)

# 4) w_otherass_avg: wvw (übrige Vermögenswerte zB Autos)

# w_movableass_avg = w_fin_avg + w_businessass_avg + w_otherass_avg
# w_tot_avg = w_movableass_avg + w_realestate


## Change negative values for assett classes to debt:

# step1: recover w_lifeinsurance_avg = w_fin_avg - w_securities_avg

# step2: for all wealth classes, create new variable capturing negative values as debt

data <- data %>% 
  mutate(w_lifeinsurance_avg = w_fin_avg - w_securities_avg) %>% 
  mutate(y_tot_p2 = ifelse(married01 == 0, 0, y_tot_p2),
         y_emp_p2 = ifelse(married01 == 0, 0, y_emp_p2))

raw_ass <- c("securities", "lifeinsurance", "businessass", "otherass", "realestate_adj")

name0 <- paste("w", raw_ass, "avg", sep = "_")
name1 <- paste("debt", raw_ass, "avg", sep = "_")
name2 <- paste("wealth", raw_ass, "avg", sep = "_")

data[, (name1) := lapply(.SD, function(x) {abs(pmin(x,0))}), .SDcols = name0]
data[, (name2) := lapply(.SD, function(x) pmax(x,0)), .SDcols = name0]

rm(raw_ass, name0, name1, name2)


data <- data %>% 
  mutate(# re-create financial, movable, and total wealth
    wealth_fin_avg = wealth_lifeinsurance_avg + wealth_securities_avg,
    wealth_movableass_avg = wealth_fin_avg + wealth_businessass_avg + wealth_otherass_avg,
    wealth_tot_avg = wealth_movableass_avg + wealth_realestate_adj_avg,
    # same for debt
    debt_fin_avg = debt_lifeinsurance_avg + debt_securities_avg,
    debt_movableass_avg = debt_fin_avg + debt_businessass_avg + debt_otherass_avg,
    debt_tot_avg = debt_movableass_avg + debt_realestate_adj_avg)


## To Do: Recover eg_ertrag

# gen EK_tot_avg	= EK_erwerb_avg + EK_verm_avg + EK_trans_avg + EK_other_avg
# gen EK_erwerb_hh 			= EK_emp_hh + EK_selfemp_hh*0.7
# gen EK_verm_hh				= EK_lieg_hh + EK_wertschr_hh + eg_ertrag + EK_selfemp_hh*0.3

data <- data %>% 
  mutate(y_inheritance_avg = y_capital_avg - y_securities_avg - 0.3*y_selfemp_avg)


# Investigate negative values for different incomes -----------------------

inc <- c("y_tot_avg", "y_emp_avg", "y_selfemp_avg" , "y_securities_avg", 
         "y_capital_avg", "y_tr_pensions_total_avg", "y_realestate_avg", 
         "y_trans_avg", "y_other_avg") 

wealth <- c("w_tot_avg","w_rein_avg", "w_net_avg", "w_businessass_avg",
            "w_realestate_avg", "w_securities_avg", "w_fin_avg")

# data %>% 
#   select(all_of(inc)) %>% 
#   summary()
# 
# data %>% 
#   select(all_of(inc)) %>% 
#   # summarize_all(.funs = list(neg =function(x) mean(x<0)))
#   summarize_all(function(x) mean(x<0)*100) 
# 
# nrow(data)
# 
# 
# data %>% 
#   select(all_of(wealth)) %>% 
#   # summarize_all(.funs = list(neg =function(x) mean(x<0)))
#   summarize_all(function(x) mean(x<0)*100) 

nrow(data)

# data %>% 
#   filter(w_debt_total_avg < 0) %>% 
#   nrow()
# # 82 cases with negative debt.. drop them!
# data %>% 
#   filter(w_debt_total_avg < 0) %>% 
#   pull(w_debt_total_avg) %>% 
#   hist()

## 82/1.5M have negative total debt (original variable): for now, drop them.


data <- data %>% 
  # drop 82 cases with negative total debt
  filter(w_debt_total_avg >= 0) %>% 
  # create net wealth and total debt
  mutate(debt_total_avg = debt_tot_avg + w_debt_total_avg,
         wealth_net_avg = wealth_tot_avg - debt_total_avg) %>% 
  # Leverage and diversification definitions from Schularick & Kuhn
  mutate(leverage = 100*debt_total_avg/(wealth_tot_avg+1),
         diversification = 100*wealth_realestate_adj_avg/(wealth_movableass_avg+1)) 



# # Different wealth variable types
# wealth_agg <- c("w_movableass_avg","w_realestate_adj_avg","w_debt_total_avg")
# 
# wealth_granular <- c("w_securities_avg","w_lifeinsurance_avg", "w_debt_total_avg",
#                      "w_businessass_avg","w_otherass_avg","w_realestate_adj_avg")
# 
# wealth_semi <- c("w_fin_avg","w_businessass_avg","w_otherass_avg",
#                  "w_movableass_avg","w_realestate_adj_avg","w_debt_total_avg")
# 
# wealth_special <- c("w_lotgains_avg","w_inheritance_avg","w_gift_avg",
#                     "w_giftaway_avg","w_inheritance_declared_avg",
#                     "w_gift_declared_avg","w_giftaway_declared_avg",
#                     "w_received_avg")
# 
# # Different Income variable types
# 
# 
# income_agg <- c("y_labor_avg","y_capital_avg","y_trans_avg","y_other_avg" )
# 
# income_semi <- c("y_prim_avg", "y_trans_avg", "y_other_avg")
# 
# income_granular <- c("y_emp_avg","y_selfemp_avg","y_securities_avg",
#                      "y_realestate_avg","y_tr_pensions_total_avg",
#                      "y_other_avg")


# #    - egen EK_trans_p1	= rowtotal(s1_p1 s2_p1 suva_p1 rente_p1 alv_p1 eo_p1 tagg_p1 alim_p1 leibr_p1)
# #     - gen EK_tr_renten_p1 	= rente_p1 + EK_tr_BVG_p1 + suva_p1 + leibr_p1
# 
# # gen EK_tr_AHV_p1 		= s1_p1
# # gen EK_tr_BVG_p1		= s2_p1
# # gen EK_tr_renten_p1 	= rente_p1 + EK_tr_BVG_p1 + suva_p1 + leibr_p1 


data <- data %>% 
  mutate(y_transfers_avg = y_trans_avg - y_tr_pensions_total_avg) %>% 
  rename(y_pensions_avg = y_tr_pensions_total_avg)


income_granular <- c("y_emp_avg","y_selfemp_avg","y_securities_avg",
                     "y_realestate_avg","y_pensions_avg", "y_transfers_avg",
                     "y_other_avg")

wealth_granular <- c("w_securities_avg","w_lifeinsurance_avg", "w_debt_total_avg",
                     "w_businessass_avg","w_otherass_avg","w_realestate_adj_avg")


wealth_special <- c("w_lotgains_avg","w_inheritance_avg","w_gift_avg",
                    "w_giftaway_avg","w_inheritance_declared_avg",
                    "w_gift_declared_avg","w_giftaway_declared_avg",
                    "w_received_avg")


# Round Wealth and Income around 0 ----------------------------------------
variables_of_interest <- setdiff(
  c(income_granular,wealth_granular,wealth_special,"w_tot_avg", "w_net_avg", "y_tot_avg"), "w_received_avg")


# backup original income and wealth data
data <- data %>% 
  mutate(across(all_of(variables_of_interest),
                .fns = function(x){x},
                .names = "OG_{.col}"))


round_zero <- function(x, threshold = 100){
  ifelse(abs(x) < threshold, 0, x)
}

x <- -1000:1000
y <- round_zero(x)

plot(x,y,type = "l")

data <- data %>% 
  mutate(across(all_of(variables_of_interest), round_zero))



# Create dummy and sum variables capturing past inheritances --------

data <- data %>%
  group_by(id) %>%
  arrange(id, year) %>%
  mutate(delta = year - lag(year)) %>%
  mutate(got_married = (married01 == 1 & lag(married01) == 0),
         got_divorced = (divorced01 == 1 & lag(divorced01)==0)
  ) %>%
  arrange(id, desc(year)) %>%
  mutate(received = w_inheritance_avg + w_gift_avg,
         # received = abs(w_inheritance_avg) + abs(w_gift_avg),
         received01 = (received > 0),
         lottery = abs(w_lotgains_avg),
         lottery01 = (lottery > 0),
         sum_received_lead = lag(cumsum(received)),
         num_received_lead = lag(cumsum(received01))
  ) %>%
  arrange(id, year) %>%
  mutate(sum_received_lag = lag(cumsum(received)),
         num_received_lag = lag(cumsum(received01)),
         num_lottery_lag = lag(cumsum(lottery01)),
         sum_lottery_lag = lag(cumsum(lottery))) 

nrow(data)


# Aggregate dummy variables to households ---------------------------------

## Investigate 4th group in nonworking_hh: 
df_unnmarried <- data %>% 
  filter(married01 == 0) 

with(df_unnmarried, table(nonworkingX01_p1,nonworkingX01_p2))
with(df_unnmarried, table(id_p2==0))

data <- data %>% 
  # filters:
  filter(estimtax01 == 0) %>%
  # select(-estimtax01) %>%
  # this one drops 1/3 of all observations.. Problematic
  # filter(!(married01 == 0 & y_tot_p2 > 0)) %>%
  filter(y_tot_avg >= 0,
         d_pill3a_avg >= 0,
         w_realestate_avg >= 0,
         d_interest_avg >= 0) %>%

  rename(O_selfempX01_p1 = selfempX01_p1,
         O_selfempX01_p2 = selfempX01_p2,
         O_empX01_p1 = empX01_p1,
         O_empX01_p2 = empX01_p2) %>% 
  mutate(empX01_p1 = as.integer(y_emp_p1 > 0),
         empX01_p2 = as.integer(y_emp_p2 > 0),
         pill3aX01_p1 = as.integer(d_pill3a_p1 > 0),
         pill3aX01_p2 = as.integer(d_pill3a_p2 > 0),
         # constructing selfemployment dummy for individuals is problematic.. 
         # selfempX01_p1 = as.integer(y_selfemp_p1 > 0),
         # selfempX01_p2 = as.integer(y_selfemp_hh)
         selfemp_hh = as.integer(y_selfemp_hh > 0)
  ) %>% 
  mutate(
    n_hh = 1+as.numeric(id_p2 != 0),
    # create dummy aggregates for household
    emp_hh = as.factor(2*(empX01_p1 + empX01_p2)/n_hh),
    nonworking_hh = as.factor(2*(nonworkingX01_p1 + nonworkingX01_p2)/n_hh),
    retiree_hh = as.factor(2*(retireeXX01_p1 + retireeXX01_p2)/n_hh),
    pill3a_hh = as.factor(2*(pill3aX01_p1 + pill3aX01_p2)/n_hh)
  ) %>% 
  
  # create mean(age), age diff
  rowwise() %>% 
  mutate(age = mean(c(age_p1, age_p2), na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(age_diff = (age_p1 - age_p2)) %>% 
  mutate(age_diff = ifelse(is.na(age_diff), 0, age_diff)) %>% 
  ungroup()


with(data,table(nonworking_hh,married01))


(new_vars <- names(data)[(ncol0+1):ncol(data)])


income_granular <- c("y_emp_avg","y_selfemp_avg","y_securities_avg",
                     "y_realestate_avg","y_pensions_avg", "y_transfers_avg",
                     "y_other_avg")

wealth_granular <- c("w_securities_avg","w_lifeinsurance_avg", "w_debt_total_avg",
                     "w_businessass_avg","w_otherass_avg","w_realestate_adj_avg")


wealth_special <- c("w_lotgains_avg","w_inheritance_avg","w_gift_avg",
                    "w_giftaway_avg","w_inheritance_declared_avg",
                    "w_gift_declared_avg","w_giftaway_declared_avg",
                    "w_received_avg")

# select new variables that should be dropped
drop_vars <- c("y_inv_hh", "delta", "empX01_p1", "empX01_p2", 
               "pill3aX01_p1", "pill3aX01_p2") 

inc_vars <- grep_ch(data, "^y_(?!(decile|pctile|groups)).*_avg$")

wealth_vars <- grep_ch(data, "^w_(?!(decile|pctile|groups)).*_avg$")


keep_vars <- c("pid_hh", "year", "muni", "married01", "divorced01", 
               "kids01", "nkids", "singleparent01", "adultkids01", 
               "nadultkids", "widow01", "farmerX01_hh", "cpi",
               setdiff(new_vars, drop_vars), inc_vars, "y_tot_p1", "y_tot_p2", "female_p1",
               "age_p1", wealth_vars)

keep_vars


# 1) create share variables

data <- data %>% 
  select(all_of(keep_vars)) %>%
  mutate(has_received01 = num_received_lag > 0, 
         won_lottery = num_lottery_lag > 0) %>% 
  mutate(across(all_of(income_granular),
                .fns = function(x){share0(x/(y_tot_avg+1))},
                .names = "shr_{.col}")) %>% 
  rename_with(.fn = ~ stringr::str_replace(.x, "_avg$", ""),
              .cols = matches("shr_y_.*avg$")) %>% 
  mutate(across(all_of(wealth_granular),
                .fns = function(x){share0(x/(w_tot_avg+1))},
                .names = "shr_{.col}")) %>% 
  rename_with(.fn = ~ stringr::str_replace(.x, "_avg$", ""),
              .cols = matches("shr_w_.*avg$")) %>% 
  rename(shr_y_self = shr_y_selfemp, 
         shr_y_sec = shr_y_securities,
         shr_y_pens = shr_y_pensions,
         shr_y_tr = shr_y_transfers,
         shr_y_oth = shr_y_other,
         shr_y_realest = shr_y_realestate,
         # Wealth
         shr_w_sec = shr_w_securities,
         shr_w_lifeins = shr_w_lifeinsurance,
         shr_w_d = shr_w_debt_total,
         shr_w_bus = shr_w_businessass,
         shr_w_oth = shr_w_otherass,
         shr_w_realest = shr_w_realestate_adj)


share_vars <- grep_ch(data, "^shr_")

# 2) create sign variables
data <- data %>% 
  mutate(across(all_of(income_granular),
                .fns = factor_sign,
                .names = "sign_{.col}")) %>% 
  rename_with(.fn = ~ stringr::str_replace(.x, "_avg$", ""),
              .cols = matches("sign_y_.*avg$")) %>% 
  mutate(across(all_of(wealth_granular),
                .fns = factor_sign,
                .names = "sign_{.col}")) %>% 
  rename_with(.fn = ~ stringr::str_replace(.x, "_avg$", ""),
              .cols = matches("sign_w_.*avg$")) %>% 
  rename(sign_y_self = sign_y_selfemp, 
         sign_y_sec = sign_y_securities,
         sign_y_pens = sign_y_pensions,
         sign_y_tr = sign_y_transfers,
         sign_y_oth = sign_y_other,
         sign_y_realest = sign_y_realestate,
         # Wealth
         sign_w_sec = sign_w_securities,
         sign_w_lifeins = sign_w_lifeinsurance,
         sign_w_d = sign_w_debt_total,
         sign_w_bus = sign_w_businessass,
         sign_w_oth = sign_w_otherass,
         sign_w_realest = sign_w_realestate_adj)

sign_vars <- grep_ch(data,"^sign_")


# 3) create indicator for range (for use as interaction with share variable)
data <- data %>% 
  mutate(across(starts_with("shr_"),.fns = list(rng = function(x) case_when(
    x < 0 ~ "neg",
    x > 1 ~ "out", 
    TRUE ~ "in"
  ) %>% 
    factor()
  ))) 


data <- data %>% 
  mutate(across(c(starts_with("wealth_"), starts_with("debt_"), starts_with("y_"), 
                  starts_with("w_"), received, lottery, starts_with("sum_")),
                function(x) 100*x/cpi)) %>% 
  filter(year <= 2019) %>% 
  group_by(pid_hh, year) %>%
  arrange(female_p1, desc(age_p1), desc(y_tot_p1)) %>%
  filter(row_number() == 1) %>% 
  # mutate()
  # select(-c(y_tot_p1, female_p1, age_p1)) %>% 
  distinct() %>% 
  ungroup()  %>% 
  mutate(w_net = ihs10(wealth_net_avg),
         y = ihs10(y_tot_avg), 
         w = ihs10(wealth_tot_avg)) %>% 
  mutate(muni = factor(muni)) %>% 
  data.table() 


## Want to use lag of X variables:
data %>% 
  select(starts_with("wealth_"), starts_with("debt"), 
         starts_with("w_"), starts_with("y_")) %>% 
  names()


data %>% 
  select(starts_with("shr_"), starts_with("sign_")) %>% 
  names()


# backup of original income and wealth data
data <- data %>%
  mutate(across(c(starts_with("wealth_"), starts_with("debt"),
                  starts_with("w_"), starts_with("y_"), w, w_net),
                .fns = function(x){x},
                .names = "OG2_{.col}"))

data <- data %>% 
  group_by(pid_hh) %>% 
  arrange(year) %>% 
  select(-w) %>% 
  rename(w = w_net) %>% 
  mutate(across(c(starts_with("wealth_"), starts_with("debt"), 
                  starts_with("w_"), starts_with("y_")),
                function(x) lag(ihs10(x)))) %>%
  mutate(across(c(starts_with("shr_"), starts_with("sign_")),
                lag)) %>%
  mutate(delta = year - lag(year)) %>% 
  ungroup() %>% 
  na.omit() 


O_dummy <- grep_ch(data, "01")

# Check datatypes
data %>% 
  select(-ends_with("_rng")) %>% 
  names()


# Convert dummy variables to boolean
dummy_vars <- data %>% 
  select(-starts_with("sign_")) %>% 
  select(where(is.dummy)) %>% 
  names() 

dummy_vars 

# Transform dummy variables to booleans and discretize kids
data <- data %>% 
  mutate(across(all_of(dummy_vars), as.logical)) %>% 
  mutate(adultkids01 = nadultkids > 0) %>% 
  mutate(O_nkids = nkids, 
         O_nadultkids = nadultkids) %>% 
  mutate(across(c(nkids, nadultkids), cut_kids))

# table(df$nkids, df$O_nkids)


data <- data %>% 
  rename(w_received_avg = received) %>%
  rename(farmer_hh = farmerX01_hh) %>%
  rename(w_lottery_avg = lottery) %>% 
  rename_with(function(x) gsub("01","",x),
              .cols = contains("01")) 


data <- data %>% 
  group_by(pid_hh) %>% 
  arrange(year) %>% 
  mutate(had_kids = factor(bool_NA(lag(cumsum(kids)) > 0)),
         had_adultkids = factor(bool_NA(lag(cumsum(adultkids)) > 0))) %>% 
  ungroup()


saveRDS(data, paste0(data_dir, "/prepared.rds"))
