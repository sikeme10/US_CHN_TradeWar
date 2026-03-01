

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
exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/Compare_values/yearly/robust/"

################################################################################


################################################################################
# 1) Load data 
################################################################################

# load trade data
trade <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_gravity/dta_CHN_gravity_yearly.csv")
names(trade)
table(trade$year)
table(trade$ExporterISO3)

# Load FE estimates 
# fe <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/output/FE/yearly/gravity_pois_FE.csv")

#LOAD POISSON WITH BOOSTRAP
fe_boot <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/output/FE/yearly/boot/gravity_pois_FE_boot_fixef.csv")
#LOAD POISSON WITH LOG LINEAR
fe_log_boot <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/output/FE/yearly/boot/gravity_logOLS_FE_boot_fixef_drop0.csv")
names(fe_boot)
unique(fe_boot$draw)


################################################################################
# 1) merge the data from different estimation methods
################################################################################

# balance trade and aggregate at hs4 level 

# create fe_id value for FE 
names(trade)
trade <- trade %>%  mutate(hs2 = substr( hs6_H5 ,1,2), hs4 = substr( hs6_H5 ,1,4))
trade <- trade %>% mutate(fe_id = interaction(year, ExporterISO3, hs4, drop = TRUE))


# aggregate trade data at hs4: (by doing weighted average of )
trade_hs4 <- trade %>%  group_by(year, hs2, hs4, ExporterISO3, ImporterISO3, fe_id) %>%
  summarise(Trade_value_USD = sum(Trade_value_USD, na.rm = TRUE),  .groups = "drop" )

# balance trade data to get 50 draws on each (repeat each of the values  50 times for draw to appear to get balanced data )
D <- length(unique(fe_boot$draw))
D
trade_with_draws <- trade_hs4 %>%  tidyr::crossing(draw = 1:D)
colSums(is.na(trade_with_draws))
table(trade_with_draws$draw)
summary(trade_hs4)






################################################################################
# merge trade data and NTMs \

# select data we want 
names(fe_boot)
names(fe_log_boot)

# rename vars
fe_log_boot <- fe_log_boot %>% rename(FE_log = FE)

# merge the two 
dta <- full_join(fe_boot, fe_log_boot)

names(trade_with_draws)
dta <- full_join(trade_with_draws, dta)
table(dta$year)
names(dta)
colSums(is.na(dta))


################################################################################
# add product level variables 
################################################################################

# check values:
summary(dta$FE)
summary(dta$FE_log)

# add HS section for each HS6 product code 
# class(dta$hs6_H5)
# unique(nchar(dta$hs6_H5))
# unique(dta$hs2)
# table(dta$hs2)

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
# unique(dta$sector)
# unique(dta$hs_section)
# unique(dta$hs2)
# table(dta$hs2, dta$hs_section)


################################################################################
# create weights for trade from other countries 

# at hs 4 level 
trade_dta_hs4 <- dta %>%
  filter(draw == 1 & ExporterISO3 != "USA") %>%
  group_by(year, hs4) %>%
  summarise(tot_hs4_trade = sum(Trade_value_USD, na.rm = TRUE), .groups = "drop")
colSums(is.na(trade_dta_hs4))


# 2) Merge back to original data
dta <- left_join(dta, trade_dta_hs4)

dta <- dta %>% mutate(share_hs4_trade = if_else(tot_hs4_trade !=0, Trade_value_USD / tot_hs4_trade,0))
colSums(is.na(dta))
summary(dta$share_hs4_trade)

test <- dta %>% filter(is.na(share_hs4_trade))
  
################################################################################
# Changes in AVEs
################################################################################
# add FE country bechmarks:

# If we want to create a benchmark for each values 
# we take the max value of the FE and u among all exporter for a specific product, month, year
dta <- dta %>% group_by(year, draw, hs4) %>%
  mutate(
    FE_bench = {
      m <- max(FE[ExporterISO3 != "USA"], na.rm = TRUE)
      ifelse(is.infinite(m), NA_real_, m)
    },
    FE_log_bench = {
      m <- max(FE_log[ExporterISO3 != "USA"], na.rm = TRUE)
      ifelse(is.infinite(m), NA_real_, m)
    },
    # Weighted mean excluding USA
    FE_wmean = {
      m <- weighted.mean(FE[ExporterISO3 != "USA"], w = share_hs4_trade[ExporterISO3 != "USA"], na.rm = TRUE)
      ifelse(is.nan(m), NA_real_, m)
    },
    
    FE_log_wmean = {
      m <- weighted.mean(FE_log[ExporterISO3 != "USA"],  w = share_hs4_trade[ExporterISO3 != "USA"], na.rm = TRUE)
      ifelse(is.nan(m), NA_real_, m)
    }
  ) %>%   ungroup()
summary(dta$FE_bench)
summary(dta$FE_log_bench)
summary(dta$FE_wmean)


# we want to look at changes in FE and u relative to 2015 levels


# save values of FE in pre trade war period, but also for the benchmark part 
dta <- dta %>%  group_by(ExporterISO3, draw, hs4) %>%
  mutate( FE_pre_2015 = mean(FE[year %in% c(2015)], na.rm = TRUE),
          FE_pre_2015_bench = mean(FE_bench[year %in% c(2015)], na.rm = TRUE),
          FE_pre_2015_wmean = mean(FE_wmean[year %in% c(2015)], na.rm = TRUE),
          
          FE_log_pre_2015 = mean(FE_log[year == 2015], na.rm = TRUE),
          FE_log_pre_2015_bench = mean(FE_log_bench[year == 2015], na.rm = TRUE),
          FE_log_pre_2015_wmean = mean(FE_log_wmean[year == 2015], na.rm = TRUE)) %>%  ungroup()




################################################################################
# Add elasticities
################################################################################

# add elasticities from chen et al.

dta <- dta %>% mutate(elasticities = case_when(sector == "Ag" ~  3 ,
                                               sector == "Manu" ~ 1.97,
                                               sector == "Other" ~ 5 ))

dta <- dta %>% arrange(year, hs4)

################################################################################
# create differences in AVEs
################################################################################


# create adjusted values of efficiency and FE estimates
# do the difference with 2015 baseline 
dta <- dta %>%
  mutate(
    # Difference in FE relative to 2015 baseline
    diff_FE_2015 = if_else( year > 2015 & !is.na(FE) & !is.na(FE_pre_2015),
                            FE - FE_pre_2015,      NA_real_    ),
    
    # Difference in FE with benchmarks relative to 2015 baseline
    diff_FE_2015_bench = if_else( year > 2015 & !is.na(FE) & !is.na(FE_pre_2015),
                                  (FE - FE_bench) - (FE_pre_2015 - FE_pre_2015_bench), NA_real_    ),
    
   # Difference in FE with de meaned relative to 2015 baseline
    diff_FE_2015_wmean = if_else( year > 2015 & !is.na(FE) & !is.na(FE_pre_2015),
                                  (FE - FE_wmean) - (FE_pre_2015 - FE_pre_2015_wmean), NA_real_    ),
    
    # Difference in FE_log relative to 2015 baseline
    diff_FE_log_2015 = if_else(year > 2015 & !is.na(FE_log) & !is.na(FE_log_pre_2015),
                               FE_log - FE_log_pre_2015, NA_real_    ),
    
    # Difference in FE_log with benchmarks relative to 2015 baseline
    diff_FE_log_2015_bench = if_else( year > 2015 & !is.na(FE_log) & !is.na(FE_log_pre_2015),
                                      (FE_log - FE_log_bench) - (FE_log_pre_2015 - FE_log_pre_2015_bench), NA_real_ ),
   
   # Difference in FE_log with de meaned relative to 2015 baseline
   diff_FE_log_2015_wmean = if_else( year > 2015 & !is.na(FE_log) & !is.na(FE_log_pre_2015),
                                     (FE_log - FE_log_wmean) - (FE_log_pre_2015 - FE_log_pre_2015_wmean), NA_real_ ) )

test  <- dta %>% filter(year >2015)
unique(test$year)
colSums(is.na(test))
summary(dta$diff_FE_2015)
summary(dta$diff_FE_2015_bench)

################################################################################

dta <- dta %>%
  mutate(
    ln_AVE_FE = (1/(1-elasticities))*FE,
    diff_ln_AVE_FE =if_else(year %in% c(2016:2020), (1/(1-elasticities))*diff_FE_2015, NA) ,
    diff_ln_AVE_FE_log = if_else(year %in% 2016:2020, (1 / (1 - elasticities)) * diff_FE_log_2015, NA_real_),
    # with benchmarks:
    diff_ln_AVE_FE_bench =if_else(year %in% c(2016:2020), (1/(1-elasticities))*diff_FE_2015_bench, NA),
    diff_ln_AVE_FE_log_bench = if_else(year %in% 2016:2020, (1 / (1 - elasticities)) * diff_FE_log_2015_bench, NA_real_) ,
    # with demeaned:
    diff_ln_AVE_FE_wmean =if_else(year %in% c(2016:2020), (1/(1-elasticities))*diff_FE_2015_wmean, NA),
    diff_ln_AVE_FE_log_wmean= if_else(year %in% 2016:2020, (1 / (1 - elasticities)) * diff_FE_log_2015_wmean, NA_real_)     )
summary(dta$ln_AVE_FE)
names(dta)
colSums(is.na(dta))

################################################################################
# export results
write_csv(dta , paste0(exp, "estimates_reduced_form_base_2015_FE_boot_100.csv"))

################################################################################


dta <- read_csv( paste0(exp, "estimates_reduced_form_base_2015_FE_boot_100.csv"))
names(dta)


# merge back with trade data (have to aggregate tariff data at HS 4 )
tariffs <- read_csv(paste0(exp, "estimates_log_tariff_FE_boot.csv"))
names(tariffs)

tariffs <- tariffs %>% select(year, hs4, hs6_H5, ExporterISO3, ImporterISO3, Trade_value_USD,
                              Applied_tariff, log_tariff, 
                              log_tariff_pre_2015, diff_log_tariff_2015,
                              log_tariff_pre_2017, diff_log_tariff_2017)




# HS4 weights
w_hs4_2015 <- tariffs %>% filter(year == 2015) %>%  group_by(hs4, hs6_H5) %>%
  summarise(Trade_value_USD = sum(Trade_value_USD), .groups = "drop_last") %>%
  mutate(tot = sum(Trade_value_USD),
         weight_hs4_2015 = if_else(tot > 0, Trade_value_USD / tot, 0)) %>%
  ungroup() %>%  select(hs4, hs6_H5, weight_hs4_2015)
w_hs4_2017 <- tariffs %>% filter(year == 2017) %>%  group_by(hs4, hs6_H5) %>%
  summarise(Trade_value_USD = sum(Trade_value_USD), .groups = "drop_last") %>%
  mutate(tot = sum(Trade_value_USD),
         weight_hs4_2017 = if_else(tot > 0, Trade_value_USD / tot, 0)) %>%
  ungroup() %>%  select(hs4, hs6_H5, weight_hs4_2017)

tariffs <- tariffs %>% left_join(w_hs4_2015,     by = c("hs4", "hs6_H5")) %>%
  left_join(w_hs4_2017,     by = c("hs4", "hs6_H5"))

tariffs_hs4 <- tariffs %>%
  group_by(year, hs4, ExporterISO3, ImporterISO3) %>%
  summarise(
    Trade_value_USD = sum(Trade_value_USD, na.rm = TRUE),
    
    diff_log_tariff_2015 = if (first(year) > 2015)
      weighted.mean(diff_log_tariff_2015, w = weight_hs4_2015, na.rm = TRUE)
    else NA_real_,
    
    diff_log_tariff_2017 = if (first(year) > 2017)
      weighted.mean(diff_log_tariff_2017, w = weight_hs4_2017, na.rm = TRUE)
    else NA_real_, .groups = "drop"  )
colSums(is.na(tariffs_hs4))
any(duplicated(tariffs_hs4[, c("year", "hs4",  "ExporterISO3", "ImporterISO3")]))


dta1 <- full_join(dta, tariffs_hs4)
colSums(is.na(dta))
colSums(is.na(dta1))


################################################################################
# get US data
################################################################################


US <- dta1  %>% filter(ExporterISO3 == "USA")

summary(US)
names(US)
unique(US$hs2)
unique(US$hs_section)
names(US)
# US1 <- US %>% filter(year %in% c(2018,2019))
write_csv(US, paste0(exp, "US_ln_NTMs_base_2015_FE_boot_hs4.csv"))


US <- read_csv(paste0(exp, "US_ln_NTMs_base_2015_FE_boot_hs4csv"))









