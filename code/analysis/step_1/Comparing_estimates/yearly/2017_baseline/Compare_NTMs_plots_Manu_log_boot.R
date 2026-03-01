



################################################################################
#                    Gravity regression analysis: residual approach


################################################################################

rm(list=ls())

library(readr)
library(tidyr)
library(dplyr)
library(data.table)
library(stringi)
library(fixest)
library(countrycode)
library(tidyverse)
library(vroom)
library(countrycode)
library(Hmisc)
library(haven)
library(sfaR)
library(frontier)

################################################################################
# directory: 
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git")
exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/Compare_values/yearly/plot/2017/"

################################################################################
# 1) Load data 
################################################################################

US <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/output/Compare_values/yearly/robust/US_ln_NTMs_base_2017_FE_boot.csv")

################################################################################

################################################################################
# drop extreme values first 
################################################################################
names(US)
summary(US)

# drop first and 99th percentile
quant <- 0.01

# Compute quantiles
FE_q  <- quantile(US$diff_ln_AVE_FE, quant, na.rm = TRUE)
FE_qH <- quantile(US$diff_ln_AVE_FE, 1 - quant, na.rm = TRUE)

FE_b_q  <- quantile(US$diff_ln_AVE_FE_bench, quant, na.rm = TRUE)
FE_b_qH <- quantile(US$diff_ln_AVE_FE_bench, 1 - quant, na.rm = TRUE)

FE_w_q  <- quantile(US$diff_ln_AVE_FE_wmean, quant, na.rm = TRUE)
FE_w_qH <- quantile(US$diff_ln_AVE_FE_wmean, 1 - quant, na.rm = TRUE)


cat("FE [1%,99%]:", FE_q, FE_qH,"| Bench [1%,99%]:", FE_b_q, FE_b_qH,
    "| Wmean [1%,99%]:", FE_w_q, FE_w_qH, "\n")


# For FE
US1 <- US %>%
  mutate(diff_ln_AVE_FE = ifelse(diff_ln_AVE_FE < FE_q | diff_ln_AVE_FE > FE_qH, NA, diff_ln_AVE_FE),
         diff_ln_AVE_FE_bench = ifelse(diff_ln_AVE_FE_bench < FE_b_q | diff_ln_AVE_FE_bench > FE_b_qH, NA, diff_ln_AVE_FE_bench),
         diff_ln_AVE_FE_wmean = ifelse(diff_ln_AVE_FE_wmean < FE_w_q | diff_ln_AVE_FE_wmean > FE_w_qH, NA, diff_ln_AVE_FE_wmean))

summary(US)
summary(US1)



################################################################################

# For US ag: 

# create simple average change and weighted average change in 
US_manu <- US1 %>% filter(sector == "Manu")

################################################################################
# Construct weights 
################################################################################

# 1) Base 2017 (draw==1) totals at hs6 (this is the "atom" you use everywhere)
base2017_hs6 <- US_manu %>% filter(year == 2017, draw == 1) %>% group_by(hs6_H5, hs_section, hs2, hs4) %>%
  summarise(Trade_value_USD_2017 = sum(Trade_value_USD, na.rm = TRUE), .groups = "drop")

# 2) Build weights for each aggregation level, then bind into ONE table

# sector level weight: HS6 to sector 
w_sector <- base2017_hs6 %>%  group_by(hs6_H5) %>%
  summarise(Trade_value_USD_2017 = sum(Trade_value_USD_2017), .groups = "drop") %>%
  mutate(tot_Trade_value_USD_2017 = sum(Trade_value_USD_2017),
         weight_sector = if_else(tot_Trade_value_USD_2017 > 0, Trade_value_USD_2017 / tot_Trade_value_USD_2017, 0)) %>%
  select(hs6_H5, weight_sector)

# HS section level weight: HS6 to HS section 
w_hs_sect <- base2017_hs6 %>%  group_by(hs_section, hs6_H5) %>%
  summarise(Trade_value_USD_2017 = sum(Trade_value_USD_2017), .groups = "drop_last") %>%
  mutate(hs_sect_tot_Trade_value_USD_2017 = sum(Trade_value_USD_2017),
         weight_hs_sect = if_else(hs_sect_tot_Trade_value_USD_2017 > 0,
                                  Trade_value_USD_2017 / hs_sect_tot_Trade_value_USD_2017, 0) ) %>%
  ungroup() %>% select(hs_section, hs6_H5, weight_hs_sect)

# HS2 level weight: HS6 to HS2 
w_hs2 <- base2017_hs6 %>%  group_by(hs2, hs6_H5) %>%
  summarise(Trade_value_USD_2017 = sum(Trade_value_USD_2017), .groups = "drop_last") %>%
  mutate(hs2_tot_Trade_value_USD_2017 = sum(Trade_value_USD_2017),
         weight_hs2 = if_else(hs2_tot_Trade_value_USD_2017 > 0,
                              Trade_value_USD_2017 / hs2_tot_Trade_value_USD_2017, 0)) %>%
  ungroup() %>% select(hs2, hs6_H5, weight_hs2)

# HS4 level weight: HS6 to HS4
w_hs4 <- base2017_hs6 %>% group_by(hs4, hs6_H5) %>%
  summarise(Trade_value_USD_2017 = sum(Trade_value_USD_2017), .groups = "drop_last") %>%
  mutate(hs4_tot_Trade_value_USD_2017 = sum(Trade_value_USD_2017),
         weight_hs4 = if_else(hs4_tot_Trade_value_USD_2017 > 0,
                              Trade_value_USD_2017 / hs4_tot_Trade_value_USD_2017, 0)) %>%
  ungroup() %>% select(hs4, hs6_H5, weight_hs4)

# 3) Single join (instead of 4 joins)
US_manu <- US_manu %>% left_join(w_sector,  by = "hs6_H5") %>%
  left_join(w_hs_sect, by = c("hs_section", "hs6_H5")) %>%
  left_join(w_hs2,     by = c("hs2", "hs6_H5")) %>%
  left_join(w_hs4,     by = c("hs4", "hs6_H5"))

################################################################################
# 3) Add Chen et al estimates
################################################################################
# add Chen et al. estimations 
Chen <- read_csv("data/chen_NTB_tariff/hs2_agriculture_manufacturing_clean.csv")

Chen <- Chen %>% select(-Country, - ISO3_Code) %>% 
  rename(hs2 = HS2 , Chen_US_import_share = US_import_share, diff_log_tariff_Chen = tau_tariff_CHN, diff_ln_AVE_chen = tau_NTB)

US_manu$hs2 <- as.numeric(US_manu$hs2)
US_manu <- left_join(US_manu,Chen)
names(US_manu)

# create weights for Chen et al at sector level and HS section

# Manu sector level: create weight in trade (hs6/total US export)
# weights for hs6 to aggregate to sector level
US_manu_2017_hs2_chen <- US_manu %>%
  filter(year == 2017 & draw == 1 ) %>%  group_by(hs2) %>%
  summarise(Trade_value_USD_2017 = sum(Trade_value_USD, na.rm = TRUE),.groups = "drop"  ) %>%
  mutate(  chen_tot_Trade_value_USD_2017 = sum(Trade_value_USD_2017, na.rm = TRUE),
           weight_sector_chen = if_else( chen_tot_Trade_value_USD_2017 > 0,Trade_value_USD_2017 / chen_tot_Trade_value_USD_2017,
                                         NA_real_    )  ) %>% select(-Trade_value_USD_2017 )
US_manu <-left_join(US_manu,US_manu_2017_hs2_chen )


# HS section level : weights for hs2 to aggregate to HS-section
US_manu_2017_hs_sect_chen <- US_manu %>%  
  filter(year == 2017 & draw == 1 ) %>%
  group_by(hs_section, hs2) %>%
  summarise(Trade_value_USD_2017 = sum(Trade_value_USD, na.rm = TRUE),
            .groups = "drop_last"  ) %>%
  group_by(hs_section) %>%
  mutate( Chen_hs_sect_tot_Trade_value_USD_2017 = sum(Trade_value_USD_2017, na.rm = TRUE),
          weight_hs_sect_Chen = if_else(Chen_hs_sect_tot_Trade_value_USD_2017 > 0,Trade_value_USD_2017 / Chen_hs_sect_tot_Trade_value_USD_2017,
                                        NA_real_    )  ) %>%  ungroup() %>% select(-Trade_value_USD_2017 )
US_manu <-left_join(US_manu, US_manu_2017_hs_sect_chen )


################################################################################

US_manu <- US_manu %>% filter(year>2017)




################################################################################
# 4) Plot change in ln(1+AVE)
################################################################################


# create a theme for ggplot 
theme_trade <- theme_minimal(base_size = 14) +
  theme(    panel.spacing.x = unit(1.2, "lines"),
            plot.title = element_text(size = 11, hjust = 0.5),
            panel.background = element_rect(fill = "white", color = NA),
            plot.background  = element_rect(fill = "white", color = NA),
            axis.text.x = element_text(size = 9),
            axis.text.y = element_text(size = 9),
            axis.title.x = element_text(size = 11),
            axis.title.y = element_text(size = 11),
            legend.text  = element_text(size = 10),
            legend.title = element_text(size = 10)  )


################################################################################
# a) sector level 
################################################################################

# trade weighted and simple average trade costs :
names(US_manu)
summary(US_manu)

US_manu_w <- US_manu %>%  group_by(year, draw) %>%
  summarise( w_FE        = weighted.mean(diff_ln_AVE_FE,           w = weight_sector, na.rm = TRUE),
             w_FE_bench  = weighted.mean(diff_ln_AVE_FE_bench,     w = weight_sector, na.rm = TRUE), 
             w_FE_mean   = weighted.mean(diff_ln_AVE_FE_wmean,       w = weight_sector, na.rm = TRUE),
             
             # --- added: log FE versions ---
             w_FE_log        = weighted.mean(diff_ln_AVE_FE_log,       w = weight_sector, na.rm = TRUE),
             w_FE_log_bench  = weighted.mean(diff_ln_AVE_FE_log_bench, w = weight_sector, na.rm = TRUE),
             w_FE_log_mean   = weighted.mean(diff_ln_AVE_FE_log_wmean,       w = weight_sector, na.rm = TRUE),
             
             w_chen      = weighted.mean(diff_ln_AVE_chen,         w = weight_sector_chen, na.rm = TRUE),
             w_tariff    = weighted.mean(diff_log_tariff_2017,     w = weight_sector, na.rm = TRUE),
             
             s_FE        = mean(diff_ln_AVE_FE,        na.rm = TRUE),
             s_FE_bench  = mean(diff_ln_AVE_FE_bench,  na.rm = TRUE),
             s_FE_mean   = mean(diff_ln_AVE_FE_wmean,  na.rm = TRUE),
             
             # --- added: log FE versions ---
             s_FE_log        = mean(diff_ln_AVE_FE_log,       na.rm = TRUE),
             s_FE_log_bench  = mean(diff_ln_AVE_FE_log_bench, na.rm = TRUE),
             s_FE_log_mean   = mean(diff_ln_AVE_FE_log_wmean, na.rm = TRUE),
             
             s_chen      = mean(diff_ln_AVE_chen,       na.rm = TRUE), # chen
             s_tariff    = mean(diff_log_tariff_2017,   na.rm = TRUE)  ) %>% 
  ungroup() %>%  mutate(W_chen = mean(w_chen, na.rm = TRUE), s_chen = mean(s_chen, na.rm = TRUE)  )


US_manu_q <- US_manu_w %>%  group_by(year) %>%  summarise(
  # ---- FE (levels) ----
  FE_mean = mean(w_FE, na.rm = TRUE),
  FE_lo   = quantile(w_FE, 0.025, na.rm = TRUE),
  FE_hi   = quantile(w_FE, 0.975, na.rm = TRUE),
  
  # ---- FE benchmark (levels) ----
  FEb_mean = mean(w_FE_bench, na.rm = TRUE),
  FEb_lo   = quantile(w_FE_bench, 0.025, na.rm = TRUE),
  FEb_hi   = quantile(w_FE_bench, 0.975, na.rm = TRUE),
  
  # ---- FE demeaned (levels) ----
  FEm_mean = mean(w_FE_mean, na.rm = TRUE),
  FEm_lo   = quantile(w_FE_mean, 0.025, na.rm = TRUE),
  FEm_hi   = quantile(w_FE_mean, 0.975, na.rm = TRUE),
  
  # ---- FE (log version) ----
  FE_log_mean = mean(w_FE_log, na.rm = TRUE),
  FE_log_lo   = quantile(w_FE_log, 0.025, na.rm = TRUE),
  FE_log_hi   = quantile(w_FE_log, 0.975, na.rm = TRUE),
  
  # ---- FE benchmark (log version) ----
  FEb_log_mean = mean(w_FE_log_bench, na.rm = TRUE),
  FEb_log_lo   = quantile(w_FE_log_bench, 0.025, na.rm = TRUE),
  FEb_log_hi   = quantile(w_FE_log_bench, 0.975, na.rm = TRUE),
  
  # ---- FE demeaned (log version) ----
  FEm_log_mean = mean(w_FE_log_mean, na.rm = TRUE),
  FEm_log_lo   = quantile(w_FE_log_mean, 0.025, na.rm = TRUE),
  FEm_log_hi   = quantile(w_FE_log_mean, 0.975, na.rm = TRUE),
  
  # ---- other series ----
  chen_mean   = mean(w_chen, na.rm = TRUE),
  tariff_mean = mean(w_tariff, na.rm = TRUE),
  
  .groups = "drop"  )


US_manu_q <- US_manu_q %>% mutate(chen_mean = if_else(year %in% c(2018:2019), chen_mean, NA))
US_manu_q <- US_manu_q %>% bind_rows(as_tibble( setNames(as.list(c(2017, rep(0, ncol(.) - 1))), names(.)))) %>%
  arrange(year)

names(US_manu_q)
plot <- ggplot(US_manu_q, aes(x = year)) +
  ## FE 
  geom_line(aes(y = FE_mean, color = "FE"),    linewidth = 1  ) +
 ## demeaned
  geom_line(  aes(y = FEm_mean, color = "FE_demeaned"),  linetype = "dashed",  linewidth = 1  ) +
  # log_FE 
  geom_line(  aes(y = FEm_log_mean, color = "FE_demeaned_log"),  linetype = "dashed",  linewidth = 1  ) +
  ## Tariff
  geom_line(aes(y = tariff_mean, color = "tariff"),   linewidth = 1  ) +
  
  geom_line(aes(y = chen_mean, color = "Chen_et_al"), linewidth = 1) +
  
  scale_color_manual(  values = c( "FE" = "blue","tariff" = "darkgreen", "FE_demeaned_log" = "purple", 
                                   "FE_demeaned" = "orange", "Chen_et_al" = "red" ),    name = "Variables"  ) +
  labs(  title = "Weighted average Δ ln(1+AVE), Manufacturing (relative to 2017)",
         x = "Year",    y = "Weighted Δ ln(1+AVE)"  ) +
  theme_trade

plot
ggsave(filename = file.path(exp, "Compare_ln_AVE_manu_weight_log_boot.png"),plot = plot, width = 8, height = 5, dpi = 300)


################################################################################
# a) HS section level
################################################################################
# get trade weighted values 
names(US_manu)
US_manu_w_sect <- US_manu %>%   
  group_by(year, draw, hs_section) %>%
  summarise(  w_FE        = weighted.mean(diff_ln_AVE_FE,           w = weight_hs_sect, na.rm = TRUE),
              w_FE_bench  = weighted.mean(diff_ln_AVE_FE_bench,     w = weight_hs_sect, na.rm = TRUE),
              w_FE_mean   = weighted.mean(diff_ln_AVE_FE_wmean,       w = weight_hs_sect, na.rm = TRUE),
              
              # added (log)
              w_FE_log        = weighted.mean(diff_ln_AVE_FE_log,       w = weight_hs_sect, na.rm = TRUE),
              w_FE_log_bench  = weighted.mean(diff_ln_AVE_FE_log_bench, w = weight_hs_sect, na.rm = TRUE),
              w_FE_log_mean   = weighted.mean(diff_ln_AVE_FE_log_wmean,       w = weight_hs_sect, na.rm = TRUE),
              
              w_chen      = weighted.mean(diff_ln_AVE_chen,         w = weight_hs_sect_Chen, na.rm = TRUE),
              w_tariff    = weighted.mean(diff_log_tariff_2017,     w = weight_hs_sect, na.rm = TRUE),
              
              s_FE        = mean(diff_ln_AVE_FE,        na.rm = TRUE),
              s_FE_bench  = mean(diff_ln_AVE_FE_bench,  na.rm = TRUE),
              s_FE_mean   = mean(diff_ln_AVE_FE_wmean,  na.rm = TRUE),
              
              # added (log)
              s_FE_log        = mean(diff_ln_AVE_FE_log,       na.rm = TRUE),
              s_FE_log_bench  = mean(diff_ln_AVE_FE_log_bench, na.rm = TRUE),
              s_FE_log_mean   = mean(diff_ln_AVE_FE_log_wmean, na.rm = TRUE),
              
              s_chen      = mean(diff_ln_AVE_chen,      na.rm = TRUE),
              s_tariff    = mean(diff_log_tariff_2017,  na.rm = TRUE),    .groups = "drop"  ) %>%  
  group_by(hs_section) %>% mutate(w_chen = mean(w_chen, na.rm = TRUE),s_chen = mean(s_chen, na.rm = TRUE)  ) %>%
  ungroup()

# aggregate at hs section
US_manu_q_sect <- US_manu_w_sect %>%    group_by(year, hs_section) %>%
  summarise( # FE (non-benchmark)
    FE_mean = mean(w_FE, na.rm = TRUE),
    FE_lo   = quantile(w_FE, 0.025, na.rm = TRUE),
    FE_hi   = quantile(w_FE, 0.975, na.rm = TRUE),
    
    # FE (benchmark) + CI
    FEb_mean = mean(w_FE_bench, na.rm = TRUE),
    FEb_lo   = quantile(w_FE_bench, 0.025, na.rm = TRUE),
    FEb_hi   = quantile(w_FE_bench, 0.975, na.rm = TRUE),
    
    # ---- FE demeaned (levels) ----
    FEm_mean = mean(w_FE_mean, na.rm = TRUE),
    FEm_lo   = quantile(w_FE_mean, 0.025, na.rm = TRUE),
    FEm_hi   = quantile(w_FE_mean, 0.975, na.rm = TRUE),
    
    # FE_log (non-benchmark)
    FE_log_mean = mean(w_FE_log, na.rm = TRUE),
    FE_log_lo   = quantile(w_FE_log, 0.025, na.rm = TRUE),
    FE_log_hi   = quantile(w_FE_log, 0.975, na.rm = TRUE),
    
    # FE_log (benchmark) + CI
    FEb_log_mean = mean(w_FE_log_bench, na.rm = TRUE),
    FEb_log_lo   = quantile(w_FE_log_bench, 0.025, na.rm = TRUE),
    FEb_log_hi   = quantile(w_FE_log_bench, 0.975, na.rm = TRUE),
    
    # ---- FE demeaned (log version) ----
    FEm_log_mean = mean(w_FE_log_mean, na.rm = TRUE),
    FEm_log_lo   = quantile(w_FE_log_mean, 0.025, na.rm = TRUE),
    FEm_log_hi   = quantile(w_FE_log_mean, 0.975, na.rm = TRUE),
    
    tariff_mean = mean(w_tariff, na.rm = TRUE),
    
    # Chen (optional CI)
    chen_mean = mean(w_chen, na.rm = TRUE),
    chen_lo   = quantile(w_chen, 0.025, na.rm = TRUE),
    chen_hi   = quantile(w_chen, 0.975, na.rm = TRUE),    .groups = "drop"  )

US_manu_q_sect <- US_manu_q_sect %>% mutate(chen_mean = if_else(year %in% c(2018:2019), chen_mean, NA))
# columns you want to set to 0 (everything except keys)
cols0 <- setdiff(names(US_manu_q_sect), c("year", "hs_section"))
add_2017 <- US_manu_q_sect %>%  distinct(hs_section) %>%  mutate(year = 2017) %>%
  mutate(!!!setNames(rep(list(0), length(cols0)), cols0))  # create cols as 0
US_manu_q_sect <- bind_rows(US_manu_q_sect, add_2017) %>%  arrange(hs_section, year)


# plot poisson model 
plot <- ggplot(US_manu_q_sect, aes(x = year)) +
  # FE CI band
  geom_line(aes(y = FE_mean, color = "FE"), linewidth = 1) +
  # Mean FE (NEW)
  geom_line(aes(y = FEm_mean, color = "FE_mean"),       linewidth = 1) +
  # Tariff
  geom_line(aes(y = tariff_mean, color = "tariff"), linetype = "dashed", linewidth = 1) +
  # log_FE 
  geom_line(  aes(y = FEm_log_mean, color = "FE_demeaned_log"),  linetype = "dashed",  linewidth = 1  ) +
  # Chen
  geom_line(aes(y = chen_mean, color = "Chen_et_al"), linewidth = 1) +
  scale_color_manual(
    values = c("FE" = "blue",   "FE_mean" = "orange","tariff" = "darkgreen",   
               "FE_demeaned_log" = "purple","Chen_et_al" = "red" ),
    labels = c("FE" = "FE","FE_mean" = "FE (demean)","tariff" = "Tariff",  
               "FE_bench" = "FE (benchmark)","Chen_et_al" = "Chen et al.","FE_demeaned_log" = "FE log (demean)" ),
    name = "Variables"  ) +
  facet_wrap(~ hs_section, scales = "free_y") +
  labs(  title = "Weighted average Δ ln(1+AVE), Manufacturing by HS section (relative to 2017)",
         x = "Year",    y = "Weighted Δ ln(1+AVE)"  ) +
  theme_trade
plot
ggsave(filename = file.path(exp, "Compare_ln_AVE_manu_hs_sect_weight_log_boot.png"),plot = plot, width = 9, height = 5, dpi = 300)


 
# ################################################################################
# # a) HS 2 level level
# ################################################################################


names(US_manu)
US_manu_w_hs2 <- US_manu %>%   group_by(year,draw, hs2) %>% 
  summarise(  w_FE        = weighted.mean(diff_ln_AVE_FE,           w = weight_hs2, na.rm = TRUE),
              w_FE_bench  = weighted.mean(diff_ln_AVE_FE_bench,     w = weight_hs2, na.rm = TRUE),
              w_FE_mean   = weighted.mean(diff_ln_AVE_FE_wmean,       w = weight_hs2, na.rm = TRUE),
              
              # added (log)
              w_FE_log        = weighted.mean(diff_ln_AVE_FE_log,       w = weight_hs2, na.rm = TRUE),
              w_FE_log_bench  = weighted.mean(diff_ln_AVE_FE_log_bench, w = weight_hs2, na.rm = TRUE),
              w_FE_log_mean   = weighted.mean(diff_ln_AVE_FE_log_wmean, w = weight_hs2, na.rm = TRUE),
              
              w_chen      = weighted.mean(diff_ln_AVE_chen,         w = weight_hs2, na.rm = TRUE),
              w_tariff    = weighted.mean(diff_log_tariff_2017,     w = weight_hs2, na.rm = TRUE),
              
              
              s_FE        = mean(diff_ln_AVE_FE,        na.rm = TRUE),
              s_FE_bench  = mean(diff_ln_AVE_FE_bench,  na.rm = TRUE),
              s_FE_mean   = mean(diff_ln_AVE_FE_wmean,  na.rm = TRUE),
              
              # added (log)
              s_FE_log        = mean(diff_ln_AVE_FE_log,       na.rm = TRUE),
              s_FE_log_bench  = mean(diff_ln_AVE_FE_log_bench, na.rm = TRUE),
              s_FE_log_mean   = mean(diff_ln_AVE_FE_log_wmean, na.rm = TRUE),
              
              s_chen      = mean(diff_ln_AVE_chen,      na.rm = TRUE),
              s_tariff    = mean(diff_log_tariff_2017,  na.rm = TRUE),.groups = "drop")  %>%  group_by(hs2) %>%
  mutate(w_chen = mean(w_chen, na.rm = TRUE), s_chen = mean(s_chen, na.rm = TRUE))


# aggregate at hs section
US_manu_q_hs2 <- US_manu_w_hs2 %>%  group_by(hs2) %>%
  summarise( # FE (non-benchmark)
    FE_mean = mean(w_FE, na.rm = TRUE),
    FE_lo   = quantile(w_FE, 0.025, na.rm = TRUE),
    FE_hi   = quantile(w_FE, 0.975, na.rm = TRUE),
    
    # FE (benchmark) + CI
    FEb_mean = mean(w_FE_bench, na.rm = TRUE),
    FEb_lo   = quantile(w_FE_bench, 0.025, na.rm = TRUE),
    FEb_hi   = quantile(w_FE_bench, 0.975, na.rm = TRUE),
    
    # ---- FE demeaned (levels) ----
    FEm_mean = mean(w_FE_mean, na.rm = TRUE),
    FEm_lo   = quantile(w_FE_mean, 0.025, na.rm = TRUE),
    FEm_hi   = quantile(w_FE_mean, 0.975, na.rm = TRUE),
    
    # FE_log (non-benchmark)
    FE_log_mean = mean(w_FE_log, na.rm = TRUE),
    FE_log_lo   = quantile(w_FE_log, 0.025, na.rm = TRUE),
    FE_log_hi   = quantile(w_FE_log, 0.975, na.rm = TRUE),
    
    # FE_log (benchmark) + CI
    FEb_log_mean = mean(w_FE_log_bench, na.rm = TRUE),
    FEb_log_lo   = quantile(w_FE_log_bench, 0.025, na.rm = TRUE),
    FEb_log_hi   = quantile(w_FE_log_bench, 0.975, na.rm = TRUE),
    
    # ---- FE demeaned (log version) ----
    FEm_log_mean = mean(w_FE_log_mean, na.rm = TRUE),
    FEm_log_lo   = quantile(w_FE_log_mean, 0.025, na.rm = TRUE),
    FEm_log_hi   = quantile(w_FE_log_mean, 0.975, na.rm = TRUE),
    
    tariff_mean = mean(w_tariff, na.rm = TRUE),
    
    # Chen (optional CI)
    chen_mean = mean(w_chen, na.rm = TRUE))



hs2_desc <- data.frame(
  hs2 = sprintf("%02d", 1:24),
  description = c(
    "Live animals",
    "Meat and edible meat offal",
    "Fish and seafood",
    "Dairy, eggs, honey",
    "Other animal products",
    "Live trees and plants",
    "Vegetables",
    "Fruits and nuts",
    "Coffee, tea, spices",
    "Cereals",
    "Milling products; malt; starch",
    "Oil seeds and oleaginous fruits",
    "Gums, resins, vegetable saps",
    "Other vegetable materials",
    "Animal & vegetable fats and oils",
    "Prepared meat, fish",
    "Sugar and sugar confectionery",
    "Cocoa and cocoa preparations",
    "Preparations of cereals & bakery",
    "Processed vegetables, fruit, nuts",
    "Misc. edible preparations",
    "Beverages",
    "Animal feed, residues",
    "Tobacco and substitutes"  ),  stringsAsFactors = FALSE)
hs2_desc$hs2 <- as.numeric(hs2_desc$hs2)
US_manu_q_hs2 <- US_manu_q_hs2 %>%  left_join(hs2_desc, by = "hs2")


library(writexl)
# write_xlsx( list("hs2_summary" = summary),  path = file.path(exp, "hs2_summary_manu.xlsx"))








