
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
library(ggplot2)

################################################################################
# directory: 
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git")
dir_dta <-  "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/Compare_values/yearly/robust"


################################################################################
# 1) Load data 
################################################################################

# Load FE estimates 
trade_dta <- read_csv(paste0(dir_dta, "/US_ln_NTMs_base_2015.csv"))

dta <- read_csv(paste0(dir_dta, "/US_ln_NTMs_base_2015_FE_bench_windsorised.csv"))
colSums(is.na(dta))
unique(dta$year)
names(dta)


################################################################################
# Load SOE data
SOE <- read_csv("data/SOE_dta/SOE_share_2010.csv")


################################################################################
# select data of interest 

names(dta)
dta <- dta %>% select(year, sector, hs_section, hs2,  hs4,  
                      diff_ln_AVE_FE_mean, diff_ln_AVE_FE_lo, diff_ln_AVE_FE_hi, sig_FE,
                      diff_ln_AVE_FE_bench_mean, diff_ln_AVE_FE_lo_bench,diff_ln_AVE_FE_hi_bench, sig_bench)


names(SOE)
SOE <- SOE %>% select(-Year)

################################################################################

# keep 2015 trade values to  aggregate tariff data at hs4 level 
trade_2015_weights <- trade_dta %>%  filter(year == 2015) %>% 
  select(hs2, hs4, hs6_H5, Trade_value_USD) %>%
  group_by(hs4) %>%
  mutate( tot_Trade_value_USD_2015 = sum(Trade_value_USD, na.rm = TRUE),
    weight_hs4 = if_else(tot_Trade_value_USD_2015 == 0, 0, Trade_value_USD / tot_Trade_value_USD_2015)  ) %>%  ungroup()


tariff <- trade_dta %>% select(year,hs2, hs4, hs6_H5,log_tariff, diff_log_tariff_2015) %>% 
  filter(year %in% c(2018, 2019))
# merge back with trade data 
tariff <- left_join(tariff, trade_2015_weights)
# calcualte weighted tariffs 
tariff <- tariff %>%
  group_by(year, hs2, hs4) %>%
  summarise(
    log_tariff_weighted = weighted.mean(log_tariff, w = weight_hs4, na.rm = TRUE),
    diff_log_tariff_2015_weighted = weighted.mean(diff_log_tariff_2015, w = weight_hs4, na.rm = TRUE),
    .groups = "drop"  )

################################################################################

# keep 2018 to 2019 for change in NTMs
dta <- dta %>% filter(year %in% c(2018, 2019))

# include product codes at HS sectiona nd sector level

dta$hs2 <- as.numeric(dta$hs2)
dta <- dta %>% mutate(
  hs_section = case_when(
    hs2 %in% 1:5 ~ 1,
    hs2 %in% 6:14 ~ 2,
    hs2 %in% 15 ~ 3,
    hs2 %in% 16:24 ~ 4,
    hs2 %in% 25:27 ~ 5,
    hs2 %in% 28:38 ~ 6,
    hs2 %in% 39:40 ~ 7,
    hs2 %in% 41:43 ~ 8,
    hs2 %in% 44:46 ~ 9,
    hs2 %in% 47:49 ~ 10,
    hs2 %in% 50:63 ~ 11,
    hs2 %in% 64:67 ~ 12,
    hs2 %in% 68:70 ~ 13,
    hs2 %in% 71 ~ 14,
    hs2 %in% 72:83 ~ 15,
    hs2 %in% 84:85 ~ 16,
    hs2 %in% 86:89 ~ 17,
    hs2 %in% 90:92 ~ 18,
    hs2 %in% 93 ~ 19,
    hs2 %in% 94:96 ~ 20,
    hs2 %in% 97 ~ 21  ),
  sector = case_when(
    hs_section %in% 1:4   ~ "Ag",
    hs_section %in% 5:20  ~ "Manu",
    TRUE                  ~ "Other"))


################################################################################

dta <- left_join(dta, tariff)


names(dta)
################################################################################
# aggregate at HS 4 level 
################################################################################

# For SOE data

# get product level 
SOE <- SOE %>% mutate(hs4 =  str_sub(hs6_H5, 1,4), hs2 =  str_sub(hs6_H5, 1,2))

# for SOE:
SOE_hs4 <- SOE %>% group_by(hs4) %>%
  summarise(Trade_value_USD_SOE = sum(Trade_value_USD_SOE, na.rm=TRUE),
            tot_Trade_value_USD = sum(tot_Trade_value_USD, na.rm=TRUE)  ) %>%
  ungroup()
SOE_hs4 <- SOE_hs4 %>% mutate(share_value_SOE = Trade_value_USD_SOE / tot_Trade_value_USD ) %>%
  select(hs4, share_value_SOE)

test <- SOE_hs4 %>% filter(share_value_SOE ==0)

################################################################################

# FOR AVE data 

names(dta)

dta_hs4 <- dta %>% group_by(year, hs4, hs2, hs_section, sector) %>%
  summarise(
    diff_log_tariff_2015_weighted = mean(diff_log_tariff_2015_weighted, na.rm=TRUE),
    diff_ln_AVE_FE_mean = mean(diff_ln_AVE_FE_mean, na.rm=TRUE),
    diff_ln_AVE_FE_lo = mean(diff_ln_AVE_FE_lo, na.rm=TRUE),
    sig_FE = first(sig_FE),
    diff_ln_AVE_FE_bench_mean = mean(diff_ln_AVE_FE_bench_mean, na.rm=TRUE),
    diff_ln_AVE_FE_lo_bench = mean(diff_ln_AVE_FE_lo_bench, na.rm=TRUE)  ,
    sig_bench = first(sig_bench)) %>%
  ungroup()


# drop NaN
dta_hs4 <- dta_hs4 %>% filter(!is.nan(diff_ln_AVE_FE_mean) & !is.nan(diff_ln_AVE_FE_bench_mean) )

################################################################################

# merge with SOE data 

class(dta_hs4$hs4)
class(SOE_hs4$hs4)

merged_hs4 <- full_join(dta_hs4, SOE_hs4)
colSums(is.na(merged_hs4))
# drop NA
merged_hs4 <- merged_hs4 %>% filter(!is.na(share_value_SOE) & !is.na(diff_ln_AVE_FE_mean) & !is.na(diff_ln_AVE_FE_bench_mean) )

################################################################################
# PLot 
################################################################################

names(merged_hs4)

# plot 
ggplot( subset(merged_hs4, sig_FE == "TRUE"), aes(x = share_value_SOE, y = diff_ln_AVE_FE_mean)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~year)+
  geom_smooth(method = "loess", formula = y ~ x, color = "blue", se = FALSE) +
  labs(
    title = "Change in AVE vs. SOE share",
    x = "SOE share in 2010",
    y = "Change in AVE (FE approach)"  ) +
  theme_minimal()


# plot 
ggplot( subset(merged_hs4, sig_FE == "TRUE"), aes(x = share_value_SOE, y = diff_ln_AVE_FE_lo)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~year)+
  geom_smooth(method = "loess", formula = y ~ x, color = "blue", se = FALSE) +
  labs(
    title = "Change in AVE vs. SOE share",
    x = "SOE share in 2010",
    y = "Change in AVE (FE approach)"  ) +
  theme_minimal()


########################
# with benchmark

# plot 
ggplot( subset(merged_hs4, sig_bench == "TRUE"), aes(x = share_value_SOE, y = diff_ln_AVE_FE_bench_mean)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~year)+
  geom_smooth(method = "loess", formula = y ~ x, color = "blue", se = FALSE) +
  labs(
    title = "Change in AVE vs. SOE share",
    x = "SOE share in 2010",
    y = "Change in AVE (FE approach)"  ) +
  theme_minimal()


# plot 
ggplot( subset(merged_hs4, sig_bench == "TRUE" ), aes(x = share_value_SOE, y = diff_ln_AVE_FE_lo_bench)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~year)+
  geom_smooth(method = "loess", formula = y ~ x, color = "blue", se = FALSE) +
  labs(
    title = "Change in AVE vs. SOE share",
    x = "SOE share in 2010",
    y = "Change in AVE (FE approach)"  ) +
  theme_minimal()

################################################################################

# Model 1 – full sample
library(fixest)
length(unique(merged_hs4$hs4))
names(merged_hs4)

# Full sample
FE_all <- feols(diff_ln_AVE_FE_mean ~ share_value_SOE + diff_log_tariff_2015_weighted,
  data = merged_hs4)

FE_bench_all <- feols(  diff_ln_AVE_FE_bench_mean ~ share_value_SOE + diff_log_tariff_2015_weighted,
  data = merged_hs4)

# Significant FE subsets
FE_sig <- feols(  diff_ln_AVE_FE_mean ~ share_value_SOE + diff_log_tariff_2015_weighted,
  data = subset(merged_hs4, sig_FE == TRUE))

# If sig_FE is character "TRUE", use this instead:
FE_bench_sig <- feols(  diff_ln_AVE_FE_bench_mean ~ share_value_SOE + diff_log_tariff_2015_weighted,
                        data = subset(merged_hs4, sig_bench == TRUE))

# Table
etable(FE_all, FE_bench_all, FE_sig, FE_bench_sig,
  headers = c("All FE", "All FE bench", "Sig FE", "Sig FE bench"),
  digits = 4,
  fitstat = ~ n + r2 + ar2 + f + f.p + rmse)
  



