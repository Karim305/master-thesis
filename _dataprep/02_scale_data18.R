## Scale data and select year 2018
code_path <- "XXX"
source(paste0(code_path, "/00_utils.R"))
library(tidyverse)
data_dir <- "XXXX"
fig_dir <- "YYYY"
model_dir <- "ZZZZ"
# Load data ---------------------------------------------------------------

muni_data <- haven::read_dta(paste0(data_dir,"/municipality_characteristics2019.dta")) %>% 
  mutate_all(labelled::to_factor)


muni <- muni_data %>% select(muni = gdenr2019,
                             gde_amgrossreg2018, gde_amreg2018, bezirk, grossregion, 
                             gde_type9_2012, gde_degurba2011, gde_citycharacter2012,
                             gde_size2015) #%>% mutate(muni = as.numeric(muni))


df_full <- load_data() %>% 
  relocate(c(w,y,everything()))


df_full <- df_full %>% 
  filter(w_lifeinsurance_avg >= 0, w_realestate_adj_avg >= 0, w_debt_total_avg >= 0,
         y_emp_avg >= 0, y_pensions_avg >= 0, y_transfers_avg >= 0, y_other_avg >= 0,
         y_tot_avg >= 0)
# filter(w_lifeinsurance_avg >= 0, w_realestate_avg >= 0, w_debt_avg >= 0, 
#        Y_emp, y_pensions, y_transfers, y_other, y_tot_avg)

df <- df_full %>%
  filter(year == 2018) %>%
  mutate(O_age = age) %>% 
  mutate(age = pmin(age,99)) %>% 
  mutate(age_cat = cut(age, seq(min(age), max(age), by = 4))) %>% 
  select(-c(pid_hh, n_hh)) %>% 
  left_join(muni, by = "muni")



bezirk_df <- data.frame(bezirk = 241:250, name = c("Jura", "Biel","Seeland", "Oberaargau",
                                                   "Emmental", "Bern-Mittelland", "Thun", "Obersimmental-Saanen",
                                                   "Frutigen-Niedersimmental","Interlaken-Oberhasli"))



df <- df %>% left_join(bezirk_df %>% mutate(bezirk = factor(bezirk)), by = "bezirk") 

df <- df %>% mutate(district = factor(name),
                    lab_region = gde_amreg2018,
                    muni_type = gde_type9_2012,
                    muni_density = gde_degurba2011) 
  
table(df$district)

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



# # For W & Y variables, get the share of negatives and positives th --------
# 
# # Define the function to compute the share
# compute_share <- function(variable, threshold){
#   negative_share <- sum(variable < 0 & variable > -threshold) / sum(variable < 0)
#   positive_share <- sum(variable > 0 & variable < threshold) / sum(variable > 0)
#   
#   return(list(negative_share = negative_share, positive_share = positive_share))
# }
# 
# # Loop through variables and thresholds
# thresholds <- seq(0, 1000, by = 5)
# results <- data.frame(variable = character(), threshold = numeric(), 
#                       share_type = character(), share = numeric())
# 
# w_issue<- c("OG_w_securities_avg", "OG_w_otherass_avg", "OG_w_debt_total_avg", "OG_w_inheritance_avg",
#             "OG_w_inheritance_declared_avg", "OG_w_giftaway_avg", "OG_w_giftaway_declared_avg", 
#             "OG_w_gift_avg", "OG_w_gift_declared_avg")
# 
# 
# 
# y_issue <- paste0("OG_", c("y_other_avg", "y_transfers_avg", "y_emp_avg", "y_pensions_avg"))
# 
# 
# for (var in y_issue){
#   x <- df[[var]]
#   for (th in thresholds){
#     shares <- compute_share(x, th)
#     results <- rbind(results, data.frame(variable = var, threshold = th, 
#                                          share_type = "negative", share = shares$negative_share))
#     results <- rbind(results, data.frame(variable = var, threshold = th, 
#                                          share_type = "positive", share = shares$positive_share))
#   }
# }
# 
# 
# 
# 
# 
# library(ggplot2)
# 
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
# 
# 
# # Visualize the results with the vertical lines and respective threshold texts
# vline_data %>% filter(share_type == "negative") %>%  {
#   results %>% mutate(share = ifelse(share == 0, NA, share)) %>% 
#     filter(share_type == "negative") %>% 
#     ggplot(aes(x = threshold, y = share, color = share_type)) +
#     geom_line() +
#     facet_grid(variable ~ share_type, scales = "free_y") +
#     labs(title = "Income: Share of Observations Closer to Zero than Threshold",
#          y = "Share", x = "Threshold") +
#     theme_minimal()  +
#     geom_vline(data = ., aes(xintercept = vthreshold), linetype = "dashed", color = "red") +
#     geom_text(data = ., aes(x = vthreshold, y = -0,
#                             label = paste0(share*100, "%")),
#               vjust = 1.5, color = "red", size = 3) + theme_minimal() 
#   
# }
# 
# 
# 
# results %>% 
#   # mutate(share = ifelse(share == 0, NA, share)) %>% 
#   filter(share_type == "negative") %>% 
#   ggplot(aes(x = threshold, y = share, color = share_type)) +
#   geom_line() +
#   facet_grid(variable ~ share_type, scales = "free_y") +
#   labs(title = "Income: Share of Observations Closer to Zero than Threshold",
#        y = "Share", x = "Threshold") #+
#   # theme_minimal()   + theme_minimal()
# 
# 
# 
# 
# ggplot(results, aes(x = threshold, y = share, color = share_type)) +
#   geom_line() +
#   facet_grid(variable ~ share_type, scales = "free_y") +
#   labs(title = "Share of Observations Closer to Zero than Threshold",
#        y = "Share", x = "Threshold") +
#   theme_minimal() +
#   geom_hline(yintercept = c(0.01)) +
#   geom_vline(data = vline_data, aes(xintercept = vthreshold), linetype = "dashed", color = "red")
# 
# 
# # Visualize the results
# library(ggplot2)
# 
# ggplot(results, aes(x = threshold, y = share, color = share_type)) +
#   geom_line() +
#   # facet_wrap(~ variable, scales = "free_y") +
#   facet_grid(variable ~ share_type, scales = "free_y") +
#   labs(title = "Share of Observations Closer to Zero than Threshold",
#        y = "Share", x = "Threshold") +
#   theme_minimal() +
#   geom_hline(yintercept = c(0.01))
# 




# Drop minority groups (LM Crashes) ---------------------------------------

## Debug benchmark_split2.R
sign_vars <- df %>% select(starts_with("sign_")) %>% names()


df <- df %>% mutate(sign_w = factor(sign(w)), sign_y = factor(sign(y)),
                    sign_wy = sign_w:sign_y)

# get all combinations between sign_wy and sign_vars
table <- lapply(sign_vars, function(x){table(sign_wy = df$sign_wy, var = df[,x] %>% pull())}) %>% 
  set_names(sign_vars)



# Function to get rownumbers of observations to be dropped ----------------

drop_minority_groups <- function(df, var = "sign_wy", var_list = sign_vars, nmin = 50){
  table <- lapply(var_list, function(x){table(var = df[,var] %>% pull(), x = df[,x] %>% pull())}) %>% 
    set_names(var_list)
  drop_list <- lapply(table, function(x){subset(data.frame(x), Freq > 0 & Freq <= nmin)})
  
  nrows <- sapply(drop_list, nrow)
  
  drop_list <- drop_list[nrows > 0]
  
  ids <- numeric()
  names <- character()
  for(i in seq_along(drop_list)){
    name <- names(drop_list)[i]
    drop_df <- drop_list[[i]]
    base_col <- df[,var] %>% pull() %>% as.character()
    issue_col <- pull(df[,name]) %>% as.character()
    for(j in nrow(drop_df)){
      id <- which(base_col == drop_df$var[j] & issue_col == drop_df$x[j])
      names <- c(names, name)
      ids <- c(ids, id)
    }
  }
  to_drop <- ids %>% unique()
  
  to_drop
  
}


count_discarded <- function(df, var, var_list = sign_vars, nmin = 10){
  table <- lapply(var_list, function(x){table(var = df[,var] %>% pull(), x = df[,x] %>% pull())}) %>% 
    set_names(var_list)
  drop_list <- lapply(table, function(x){subset(data.frame(x), Freq > 0 & Freq <= nmin)})
  
  nrows <- sapply(drop_list, nrow)
  
  drop_list <- drop_list[nrows > 0]
  
  ids <- numeric()
  names <- character()
  for(i in seq_along(drop_list)){
    name <- names(drop_list)[i]
    drop_df <- drop_list[[i]]
    base_col <- df[,var] %>% pull() %>% as.character()
    issue_col <- pull(df[,name]) %>% as.character()
    for(j in nrow(drop_df)){
      id <- which(base_col == drop_df$var[j] & issue_col == drop_df$x[j])
      names <- c(names, name)
      ids <- c(ids, id)
    }
  }
  to_drop <- ids %>% unique()
  
  length(to_drop)
}

count_discarded(df, var = "sign_wy")

x <- 1:200
y <- sapply(x, function(y){count_discarded(df, var = "sign_wy", nmin = y)})
thresh <- c(20,50,100)
yticks <- sapply(thresh,function(t)y[which(x >= t)[1]])

pdf(paste0(fig_dir,"_general/drop_minority_groups.pdf"))
plot(x,y,type = "l", main = "Number of Discarded Observations per minimum group Size of factor combinations  \n in Benchmark models (by sign_wy)",
     xlab = "Minimum Group Size of sign_var:sign_wy", ylab = "# Lost Observations", cex.main = 0.9)
abline(h = yticks, col = (1:length(thresh))+1, lty = rep(2,length(thresh)))
legend("right", legend = paste("Group Size >= ",thresh), col = (1:length(thresh))+1, lty = rep(2,length(thresh)))
dev.off()

# Given the huge number of observations, even dropping ~250 observations shouldn't change the results
# As we sometimes do 5-fold CV, let's require minimum 
250/nrow(df)*100


get_discarded_levels <- function(df, var = "sign_wy", var_list = sign_vars, nmin = 10){
  table <- lapply(var_list, function(x){table(var = df[,var] %>% pull(), x = df[,x] %>% pull())}) %>% 
    set_names(var_list)
  drop_list <- lapply(table, function(x){subset(data.frame(x), Freq > 0 & Freq <= nmin)})
  
  nrows <- sapply(drop_list, nrow)
  
  drop_list <- drop_list[nrows > 0]
  
  
  drop_df <- purrr::map2(drop_list, names(drop_list), ~mutate(.x, base_name = !!var, var_name = .y)) %>% 
    bind_rows() %>% rename(base_level=var, var_level = x)
  
  drop_df
}




pdf(paste0(fig_dir, "_general/dropped_levels50.pdf"))
p <- get_discarded_levels(df = df, nmin = 50) %>% 
  # mutate(var_name = str_remove(var_name, "^sign\\_")) %>% 
  ggplot2::ggplot() +
  geom_text(aes(x = var_name, y = var_level, col = var_name, label = Freq, size = Freq)) +
  facet_wrap(~base_level, ncol = 1) +
  scale_x_discrete(guide = guide_axis(angle = 45))+
  scale_size_continuous(range = c(2, 5)) +
  theme_bw() +
  ggtitle("Number of discarded observations per group for threshold 50",
          subtitle = "sign_wy")

g <- ggplot_build(p)
cols <- g$data[[1]] %>% distinct(colour,x) %>% arrange(x) %>% pull(colour)
p + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = cols),
          legend.position = "none")
dev.off()




pdf(paste0(fig_dir, "_general/dropped_levels25.pdf"))
p <- get_discarded_levels(df = df, nmin = 25) %>% 
  # mutate(var_name = str_remove(var_name, "^sign\\_")) %>% 
  ggplot2::ggplot() +
  geom_text(aes(x = var_name, y = var_level, col = var_name, label = Freq, size = Freq)) +
  facet_wrap(~base_level, ncol = 1) +
  scale_x_discrete(guide = guide_axis(angle = 45))+
  scale_size_continuous(range = c(2, 5)) +
  theme_bw() +
  ggtitle("Number of discarded observations per group for threshold 25",
          subtitle = "sign_wy")

g <- ggplot_build(p)
cols <- g$data[[1]] %>% distinct(colour,x) %>% arrange(x) %>% pull(colour)
p + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = cols),
          legend.position = "none")
dev.off()


pdf(paste0(fig_dir, "_general/dropped_levels100.pdf"))
p <- get_discarded_levels(df = df, nmin = 100) %>% 
  # mutate(var_name = str_remove(var_name, "^sign\\_")) %>% 
  ggplot2::ggplot() +
  geom_text(aes(x = var_name, y = var_level, col = var_name, label = Freq, size = Freq)) +
  facet_wrap(~base_level, ncol = 1) +
  scale_x_discrete(guide = guide_axis(angle = 45))+
  scale_size_continuous(range = c(2, 5)) +
  theme_bw() +
  ggtitle("Number of discarded observations per group for threshold 100",
          subtitle = "sign_wy")

g <- ggplot_build(p)
cols <- g$data[[1]] %>% distinct(colour,x) %>% arrange(x) %>% pull(colour)
p + theme(axis.text.x = element_text(angle = 45, hjust = 1, colour = cols),
          legend.position = "none")
dev.off()


dropped <- get_discarded_levels(df = df, nmin = 100)
dropped

# Before dropping minority groups
(n0 <- nrow(df))

drop_id <- drop_minority_groups(df, nmin = 50)
df$drop <- FALSE
df$drop[drop_id] <- TRUE

df$drop %>% sum()


# df <- drop_minority_groups(df, nmin = 100)
# dropped 245 observations
(n1 <- nrow(df))
n0 - n1

# dropped 0.04% of observations
100*(n0 - n1)/n0

df <- df %>% 
  filter(sign_w_lifeins != "-1",
         sign_y_pens != "-1") %>% 
  filter(!(sign(w) == 0 & sign_y_tr == -1)) %>% 
  filter(!(bezirk %in% c(600,700)), !is.na(bezirk)) 

n2 <- nrow(df)
n0 - n2

df <- df %>% select(-c(sign_w, sign_y, sign_wy))


## Prepare municipality data
# library(tidyverse)
data_muni <- readxl::read_xlsx(paste0(data_dir,"/bfs_muni_18.xlsx"), 
                               sheet = 2,range = "A5:AQ2208", col_names = TRUE) %>% 
  select(-starts_with("..."))



data_muni[data_muni == "X"] <- NA
data_muni[data_muni == "*"] <- NA

data_muni <- data_muni %>% mutate(across(-c("muni_name"), as.numeric)) 

data_muni <- data_muni %>% mutate(muni_share_1st_sector_17 = muni_workforce_17_primarysector / muni_workforce_17_total,
                                  muni_share_2nd_sector_17 = muni_workforce_17_secondarysector / muni_workforce_17_total,
                                  muni_share_3rd_sector_17 = muni_workforce_17_tertiarysector / muni_workforce_17_total)

is.na(data_muni$muni_share_1st_sector_17 + data_muni$muni_share_2nd_sector_17 + data_muni$muni_share_3rd_sector_17) %>% sum()


muni_vars <- c("muni_pop_18", "muni_dpop_pct_10to18", "muni_popdensity_perkm2_18","muni_hh_avgsize_18",
               "muni_popshareforeigners_18", "muni_ageshare20to64_18", "muni_ageshare65plus_18",
               "muni_promil_births_18", "muni_promil_deaths_18", "muni_socialbenefits_receiver_share_18",
               "muni_share_1st_sector_17","muni_share_2nd_sector_17", "muni_share_3rd_sector_17", 
               # Voting behavior
               "muni_voting_share_19_fdp", "muni_voting_share_19_svp", "muni_voting_share_19_glp",
               "muni_voting_share_19_gps","muni_voting_share_19_sp", "muni_voting_share_19_cvp"
               )


all_munis <- df$muni %>% unique()

present_munis <- data_muni %>% filter(muni %in% all_munis) %>% 
  select(muni, muni_name, all_of(muni_vars))

with(present_munis, sum(is.na(muni_share_1st_sector_17 + muni_share_2nd_sector_17 + muni_share_3rd_sector_17)))

missing_munis <- setdiff(all_munis, present_munis$muni)

# for 1310 observations, no municipality details were available 
df %>% filter(muni %in% missing_munis) %>% nrow()

final_muni <- present_munis %>% na.omit()

missing_munis <- setdiff(present_munis$muni, final_muni$muni)


# How many observations would be lost by using all new muni variables (through NAs)?
n0 <- nrow(df)
n1 <- df %>% filter(muni %in% final_muni$muni) %>% nrow()
# Using all muni variables drops 20k obs.
n0-n1
# or 3.5%
(n0-n1)/n0

# How many municipalities are lost when using all muni variables?
map(present_munis, ~sum(is.na(.x)))

# sector shares drops 9k obs, share of social benefits receivers around 12k, voting around 1k
sapply(present_munis, function(x){
  isNA <- is.na(x)
  dropped_munis <- present_munis[isNA, ]$muni
  nrow(df[df$muni %in% dropped_munis,]) 
})


# df %>% count(gde_amgrossreg2018)
# df %>% count(gde_amreg2018)
# df %>% count(bezirk) %>% arrange(n)
# df %>% count(gde_amreg2018) %>% arrange(n)
# 
# df %>% count(gde_amreg2018) %>% arrange(n)
# df %>% count(muni) %>% arrange(n) %>% filter(n<100)
# 
# df %>% count(gde_degurba2011)
# 
# df %>% count(bezirk, gde_amreg2018)
# with(df, table(bezirk, gde_amreg2018))
# 
# test <- df %>% count(bezirk, gde_amreg2018)
# 
# 
# with(df, table(gde_degurba2011, gde_type9_2012))


df <- df %>% left_join(present_munis %>% mutate(muni = factor(muni)), by = "muni")

df %>% filter(is.na(muni_name)) %>% nrow()

# dropping all rows with at least one missing variable means dropping 21282 observations
nrow(df)-nrow(df %>% na.omit())
xlim <- c(min(df$w), max(df$w))
ylim <- c(min(df$y), max(df$y))

pdf(paste0(fig_dir,"_general/wy_scatter_naomit.pdf"))
sfsmisc::mult.fig(2, main = "Joint Distribution of Income and Wealth",marP = c(-1,-1,-1,0))
with(df %>% na.omit(),smoothScatter(w,y,main = "Observations without any Missing Values",xlim = xlim, ylim = ylim, nrpoints = 0))
with(df,smoothScatter(w,y,main = "All Observations",xlim = xlim, ylim = ylim, nrpoints = 0))
dev.off()
# df <- df %>% filter(!drop)
nrow(df)

df <- df %>% ungroup() %>% mutate(row_id = row_number()) %>% select(-starts_with("muni_voting")) 

# save final data
saveRDS(df, paste0(data_dir, "prepared18.rds"))

df_na <- df[rowSums(is.na(df)) > 0,]
df_nona <- df[rowSums(is.na(df)) == 0,]


pdf(paste0(fig_dir,"_general/wy_scatter_naomit2.pdf"))
sfsmisc::mult.fig(2, main = "Joint Distribution of Income and Wealth",marP = c(-1,-1,-1,0))
with(df_na,smoothScatter(w,y,main = "Observations with Missing Values for any variable",xlim = xlim, ylim = ylim, nrpoints = 0))
with(df_nona,smoothScatter(w,y,main = "Observations without any Missing Values",xlim = xlim, ylim = ylim, nrpoints = 0))
dev.off()

