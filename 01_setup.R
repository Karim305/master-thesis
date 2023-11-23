## Set-up

# Set up directories ------------------------------------------------------

data_dir <- "XX"
raw_data_dir <- "XXX"

# fig_dir <- "D:/WealthInequality/Thesis_Karim/_plots/"



code_dir <- "XXXXXXXXX"
model_dir <- "XXXXXX"

fig_dir <- "XXXX"


# Load functions
source(paste0(code_dir, "/00_utils.R"))



df <- load_data("prepared18") %>% 
  mutate(sign_w = factor(sign(w))
         # , sign_y = factor(sign(y)), sign_wy = sign_w:sign_y
         ) %>% 
  filter(!(sign_w == "-1" & sign_w_oth == "-1")) %>% 
  select(-sign_w)

# Create Model Formulas ---------------------------------------------------

share_vars <- df %>% 
  select(starts_with("shr_")) %>% 
  names() %>% 
  stringr::str_replace("_rng","") %>% 
  unique()

sign_vars <- grep_ch(df, "^sign_")

vars_y <- c("w", "y")





muni_vars <- c("district", "lab_region", "muni_type", "muni_density")


muni_vars2 <- c("muni_pop_18", "muni_dpop_pct_10to18", "muni_popdensity_perkm2_18","muni_hh_avgsize_18",
               "muni_popshareforeigners_18", "muni_ageshare20to64_18", "muni_ageshare65plus_18",
               "muni_promil_births_18", "muni_promil_deaths_18", "muni_socialbenefits_receiver_share_18",
               "muni_share_1st_sector_17","muni_share_2nd_sector_17", 
               "district", "lab_region", "muni_type"
               # Voting behavior
               # "muni_voting_share_19_fdp", "muni_voting_share_19_svp", "muni_voting_share_19_glp",
               # "muni_voting_share_19_gps","muni_voting_share_19_sp", "muni_voting_share_19_cvp"
)


# vars1 <- c("married", "divorced", "nkids", "singleparent", 
#            "nadultkids", "widow", "farmer_hh", "got_married",
#            "got_divorced", "received", "num_received_lag", "invest", 
#            "has_received", "selfemp_hh", "emp_hh", "nonworking_hh", 
#            "retiree_hh", "pill3a_hh", "age", "age_diff", 
#            "had_adultkids", "had_kids")

# recreate these variables as lags:
# vars2 <- c("share_y_emp", "share_y_selfemp", "share_income_inv", 
#            "share_y_realestate", "share_y_prim", "share_y_labor", 
#            "share_w_realestate_return", "share_w_realestate", 
#            "sign_y_realestate", "sign_y_selfemp", "realestate")


wealth_granular <- c("w_securities_avg","w_lifeinsurance_avg", "w_debt_total_avg",
                     "w_businessass_avg","w_otherass_avg","w_realestate_adj_avg")

income_granular <- c("y_emp_avg","y_selfemp_avg","y_securities_avg",
                     "y_realestate_avg","y_pensions_avg", "y_transfers_avg",
                     "y_other_avg")


vars2 <- sign_vars

vars3 <- share_vars


# vars 3 should include values of previous years for income, and wealth variables
# vars3 <- c("y_labor_avg", "y_capital_avg", "y_trans_avg", "y_other_avg", "y_realestate_avg",
#            "wealth_fin_avg", "wealth_realestate_adj_avg", "wealth_otherass_avg", "debt_tot_avg" )

vars4 <- c(income_granular, wealth_granular)


# Create interaction terms of shares and share_rng to treat different "regions"
# of share variable differently (continuous within plausible range, dummy outside)
# Only create interaction for variables where observations fall out of range
drop_shares <- c("share_w_realestate")
drop_shares <- c()
share_vars <- setdiff(share_vars, drop_shares)

shares_term <- paste(paste(paste(share_vars, paste0(share_vars,"_rng"), sep = ":"), 
                           paste(drop_shares, collapse = " + "), 
                           # sep = " + "))
                          collapse = " + "))

sign_term <- paste(grep_ch(df, "^sign_"), collapse = " + ")

logical_vars <- df %>% 
  select(where(is.logical), -any_of(c("drop"))) %>% 
  names()

dummy_term <- paste(setdiff(logical_vars, 
                            c("kids", "adultkids", "selfemp_hh", "farmer_hh")), 
                    collapse = " + ")

additional_term <- paste(c("nkids", "nadultkids", "age_diff"), 
                         collapse = " + ")

muni_term <- paste(c("district", "lab_region", "muni_type", "muni_density"), collapse = " + ")

hh_term <- paste(grep_ch(df, "_hh$"), collapse = " + ")

# Create RHS
(v1 <- paste(additional_term, dummy_term, hh_term, sep = " + "))

(v2 <- paste(sign_term, sep = " + ", collapse = " + "))

(v3 <- paste(sign_term, shares_term, sep = " + ", collapse = " + "))

(v4 <- paste(vars4, collapse = " + "))

# Create formulas
f0 <- formula(paste("cbind(w,y) ~ age + ", v1))
f02 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", paste(v1, muni_term, sep = " + ")))

f1 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", v1))
f12 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", paste(v1, muni_term, sep = " + ")))

f2 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", paste(v1, v2, sep = " + ")))
f22 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", paste(v1, v2, muni_term, sep = " + ")))

f3 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", paste(v1, v2, v3, sep = " + ")))
f32 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", paste(v1, v2, v3, muni_term, sep = " + ")))

f4 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", paste(v1, v2, v3, v4, sep = " + ")))
f42 <- formula(paste("cbind(w,y) ~ ns(age, df = 3) + ", paste(v1, v2, v3, v4, muni_term, sep = " + ")))



df <- df %>% filter(!drop) %>% select(-age_cat) %>% na.omit()


set.seed(2023)
train <- sample_train(df,0.5)
saveRDS(train,paste0(model_dir,"train.rds"))


df_private <- df %>% select(w,y) %>% mutate(sign_w=factor(sign(w)),
                              sign_y = factor(sign(y)),
                              sign_wy = sign_w:sign_y) %>% 
  mutate(sign_wy = as.character(sign_wy)) %>% 
  select(w,y,sign_wy) %>% 
  sdcMicro::mafast(variables = c("w","y"), by = "sign_wy", aggr = 50) %>% 
  select(w,y)


