

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
exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/Compare_values/yearly/robust/elast/"

################################################################################


################################################################################
# 1) Load data
################################################################################

# load trade data
trade <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_gravity/dta_CHN_gravity_yearly.csv")
# names(trade)
# table(trade$year)
# table(trade$ExporterISO3)

# Load FE estimates
# fe <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/output/FE/yearly/gravity_pois_FE.csv")

#LOAD POISSON WITH BOOSTRAP
fe_boot <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/output/FE/yearly/gravity_pois_FE.csv")
#LOAD POISSON WITH LOG LINEAR
fe_log_boot <-  read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/output/FE/yearly/gravity_logOLS_FE.csv")


elast <- read_csv("data/elast/clean_Elasticities_Soderbery2018.csv")
names(elast)
class(elast$hs4)


################################################################################
# SET BASE YEAR HERE - only thing you need to change
base_year <- 2017


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


################################################################################
# merge trade data and NTMs

# select data we want
names(fe_boot)
names(fe_log_boot)

# rename vars
fe_log_boot <- fe_log_boot %>% rename(FE_log = FE)

# select variable of interest
fe_boot <- fe_boot %>% select(year, hs4,fe_id, FE )
fe_log_boot <- fe_log_boot %>% select(year, hs4,fe_id, FE_log )
length(unique(fe_boot$fe_id))
length(unique(fe_log_boot$fe_id))


# merge the two
dta <- full_join(fe_boot, fe_log_boot)
names(dta)
colSums(is.na(dta))
# drop duplicates:
dta <- dta[!duplicated(dta[, c("year", "hs4", "fe_id",  "FE" , "FE_log" )]), ]

names(trade_hs4)
# dta <- full_join(trade, dta)
dta <- full_join(trade_hs4, dta)
# table(dta$year)
# names(dta)
# colSums(is.na(dta))


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



################################################################################
# create weights for trade from other countries

# at hs 4 level
trade_dta_hs4 <- dta %>%  filter(ExporterISO3 != "USA") %>%
  group_by(year, hs4) %>%  summarise(tot_hs4_trade = sum(Trade_value_USD, na.rm = TRUE), .groups = "drop")
# colSums(is.na(trade_dta_hs4))


# 2) Merge back to original data
dta <- left_join(dta, trade_dta_hs4)

dta <- dta %>% mutate(share_hs4_trade = if_else(tot_hs4_trade !=0, Trade_value_USD / tot_hs4_trade,0))
# colSums(is.na(dta))
summary(dta$share_hs4_trade)
#
# test <- dta %>% filter(is.na(share_hs4_trade))

################################################################################
# Changes in AVEs
################################################################################
# add FE country bechmarks:

# If we want to create a benchmark for each values
# we take the max value of the FE and u among all exporter for a specific product, month, year
dta <- dta %>% group_by(year, hs4) %>%
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
# summary(dta$FE_bench)
# summary(dta$FE_log_bench)
# summary(dta$FE_wmean)


################################################################################
# 6) Save base year values
################################################################################

dta <- dta %>% group_by(ExporterISO3, hs4) %>%
  mutate(
    FE_pre_base          = mean(FE[year == base_year],              na.rm = TRUE),
    FE_pre_base_bench    = mean(FE_bench[year == base_year],        na.rm = TRUE),
    FE_pre_base_wmean    = mean(FE_wmean[year == base_year],        na.rm = TRUE),
    FE_log_pre_base      = mean(FE_log[year == base_year],          na.rm = TRUE),
    FE_log_pre_base_bench= mean(FE_log_bench[year == base_year],    na.rm = TRUE),
    FE_log_pre_base_wmean= mean(FE_log_wmean[year == base_year],    na.rm = TRUE)  ) %>% ungroup()





################################################################################
# Add elasticities
################################################################################

# add elasticities from chen et al.
dta <- dta %>% mutate(CHEN_elasticities = case_when(sector == "Ag" ~  3 ,
                                               sector == "Manu" ~ 1.97,
                                               sector == "Other" ~ 5 ))

# and from Soderberry
elast <- read_csv("data/elast/clean_Elasticities_Soderbery2018.csv")
names(elast)
class(elast$hs4)

dta$hs4 <- as.numeric(dta$hs4)
elast$hs4 <- as.numeric(elast$hs4)
# merge back with data 
dta <- left_join(dta,elast)
# if exporting country is not in take the averga eof the elastciities:
dta <- dta %>%  group_by(hs4) %>%  mutate(
  elasticities = ifelse(is.na(elasticities), mean(elasticities, na.rm = TRUE),  elasticities) ) %>%  ungroup()
colSums(is.na(dta))
dta <- dta %>% arrange(year, hs4)


################################################################################
# 7) Differences relative to base year
################################################################################

dta <- dta %>%
  mutate(
    diff_FE_base = if_else(year > base_year & !is.na(FE) & !is.na(FE_pre_base),
                            FE - FE_pre_base, NA_real_),
    diff_FE_base_bench = if_else(year > base_year & !is.na(FE) & !is.na(FE_pre_base),
                                 (FE - FE_bench) - (FE_pre_base - FE_pre_base_bench), NA_real_),
    diff_FE_base_wmean = if_else(year > base_year & !is.na(FE) & !is.na(FE_pre_base),
                                 (FE - FE_wmean) - (FE_pre_base - FE_pre_base_wmean), NA_real_),
    diff_FE_log_base = if_else(year > base_year & !is.na(FE_log) & !is.na(FE_log_pre_base),
                               FE_log - FE_log_pre_base, NA_real_),
    diff_FE_log_base_bench = if_else(year > base_year & !is.na(FE_log) & !is.na(FE_log_pre_base),
                                     (FE_log - FE_log_bench) - (FE_log_pre_base - FE_log_pre_base_bench), NA_real_),
    diff_FE_log_base_wmean = if_else(year > base_year & !is.na(FE_log) & !is.na(FE_log_pre_base),
                                     (FE_log - FE_log_wmean) - (FE_log_pre_base - FE_log_pre_base_wmean), NA_real_)  )

test  <- dta %>% filter(year >base_year)
# unique(test$year)
# colSums(is.na(test))
# summary(dta$diff_FE_2015)
# summary(dta$diff_FE_2015_bench)

################################################################################
# 9) AVE calculations
################################################################################

dta <- dta %>%
  mutate(
    # Soderberry  elasticities
    ln_AVE_FE              = (1/(1-elasticities)) * FE,
    diff_ln_AVE_FE         = if_else(year %in% (base_year+1):2020, (1/(1-elasticities)) * diff_FE_base,        NA_real_),
    diff_ln_AVE_FE_log     = if_else(year %in% (base_year+1):2020, (1/(1-elasticities)) * diff_FE_log_base,    NA_real_),
    diff_ln_AVE_FE_bench   = if_else(year %in% (base_year+1):2020, (1/(1-elasticities)) * diff_FE_base_bench,  NA_real_),
    diff_ln_AVE_FE_log_bench = if_else(year %in% (base_year+1):2020, (1/(1-elasticities)) * diff_FE_log_base_bench, NA_real_),
    diff_ln_AVE_FE_wmean   = if_else(year %in% (base_year+1):2020, (1/(1-elasticities)) * diff_FE_base_wmean,  NA_real_),
    diff_ln_AVE_FE_log_wmean = if_else(year %in% (base_year+1):2020, (1/(1-elasticities)) * diff_FE_log_base_wmean, NA_real_),
    
    # CHEN elasticities
    ln_AVE_FE_Chen              = (1/(1-CHEN_elasticities)) * FE,
    diff_ln_AVE_FE_Chen         = if_else(year %in% (base_year+1):2020, (1/(1-CHEN_elasticities)) * diff_FE_base,        NA_real_),
    diff_ln_AVE_FE_log_Chen     = if_else(year %in% (base_year+1):2020, (1/(1-CHEN_elasticities)) * diff_FE_log_base,    NA_real_),
    diff_ln_AVE_FE_bench_Chen   = if_else(year %in% (base_year+1):2020, (1/(1-CHEN_elasticities)) * diff_FE_base_bench,  NA_real_),
    diff_ln_AVE_FE_log_bench_Chen = if_else(year %in% (base_year+1):2020, (1/(1-CHEN_elasticities)) * diff_FE_log_base_bench, NA_real_),
    diff_ln_AVE_FE_wmean_Chen   = if_else(year %in% (base_year+1):2020, (1/(1-CHEN_elasticities)) * diff_FE_base_wmean,  NA_real_),
    diff_ln_AVE_FE_log_wmean_Chen = if_else(year %in% (base_year+1):2020, (1/(1-CHEN_elasticities)) * diff_FE_log_base_wmean, NA_real_)
  )

################################################################################
# export results
write_csv(dta , paste0(exp, "estimates_reduced_form_base_",base_year, "_FE_elast.csv"))


################################################################################


dta <- read_csv( paste0(exp,"estimates_reduced_form_base_",base_year, "_FE_elast.csv"))
names(dta)


# merge back with trade data (have to aggregate tariff data at HS 4 )
tariffs <- read_csv(paste0("/data/sikeme/TRADE/US_CHN_TradeWar_git/output/Compare_values/yearly/robust/estimates_log_tariff_FE_boot.csv"))
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

tariffs_hs4$hs4 <- as.numeric(tariffs_hs4$hs4)
dta$hs4 <- as.numeric(dta$hs4)
dta1 <- full_join(dta, tariffs_hs4)
colSums(is.na(dta))
colSums(is.na(dta1))
test <- dta1 %>% filter(ExporterISO3 == "USA")


write_csv(dta1 , paste0(exp,"estimates_reduced_form_base_",base_year, "_FE_elast.csv"))


################################################################################


dta1 <- read_csv( paste0(exp, "estimates_reduced_form_base_",base_year, "_FE_elast.csv"))
names(dta)

################################################################################
# get US data
################################################################################


US <- dta1  %>% filter(ExporterISO3 == "USA")

# summary(US)
# names(US)
# unique(US$hs2)
# unique(US$hs_section)
# names(US)
# US1 <- US %>% filter(year %in% c(2018,2019))
write_csv(US, paste0(exp, "US_ln_NTMs_", base_year ,"_FE_hs4_elast.csv"))


# US <- read_csv(paste0(exp, "US_ln_NTMs_base_2015_FE_boot_hs4_elast.csv"))









