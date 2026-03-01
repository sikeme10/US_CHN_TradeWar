

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


################################################################################
# 1) merge the data from different estimation methods
################################################################################

# balance trade and aggregate at hs4 level 

# create fe_id value for FE 
names(trade)
trade <- trade %>%  mutate(hs2 = substr( hs6_H5 ,1,2), hs4 = substr( hs6_H5 ,1,4))
trade<- trade %>% mutate(log_tariff = log(1+Applied_tariff/100))
trade <- trade %>% mutate(fe_id = interaction(year, ExporterISO3, hs4, drop = TRUE))
colSums(is.na(trade))


################################################################################


trade <- trade %>% select(year,hs2, hs4, hs6_H5,fe_id, ExporterISO3, ImporterISO3, Trade_value_USD, 
                      Applied_tariff,log_tariff)
colSums(is.na(trade))


# merge all data 


# create a log of tariff 
summary(trade$log_tariff)
summary(trade$Applied_tariff)
names(trade)


################################################################################
# add product level variables 
################################################################################

# add HS section for each HS6 product code 
# class(trade$hs6_H5)
# unique(nchar(trade$hs6_H5))
# unique(trade$hs2)
# table(trade$hs2)

trade$hs2 <- as.numeric(trade$hs2)
trade <- trade %>% mutate(
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
# Changes in AVEs
################################################################################

# save values of FE in pre trade war period, but also for the benchmark part 
trade <- trade %>%  group_by(ExporterISO3,  hs6_H5) %>%
  mutate(log_tariff_pre_2015 = mean(log_tariff[year %in% c(2015)], na.rm = TRUE),
         log_tariff_pre_2017 = mean(log_tariff[year %in% c(2017)], na.rm = TRUE)) %>%  ungroup()

summary(trade$log_tariff)
summary(trade$log_tariff_pre_2015)
summary(trade$log_tariff_pre_2017)

test  <- trade %>% filter(is.na(log_tariff_pre_2015))


################################################################################
# Add elasticities
################################################################################

# add elasticities from chen et al.

trade <- trade %>% mutate(elasticities = case_when(sector == "Ag" ~  3 ,
                                               sector == "Manu" ~ 1.97,
                                               sector == "Other" ~ 5 ))

trade <- trade %>% arrange(year, hs6_H5)

################################################################################
# create differences in AVEs
################################################################################


# create adjusted values of efficiency and FE estimates
# do the difference with 2015 baseline 
trade <- trade %>% mutate( 
    # Difference in log tariffs relative to 2015 baseline
    diff_log_tariff_2015 = if_else(year > 2015 & !is.na(log_tariff) & !is.na(log_tariff_pre_2015),
                                     log_tariff - log_tariff_pre_2015, NA_real_),
    diff_log_tariff_2017 = if_else(year > 2017 & !is.na(log_tariff) & !is.na(log_tariff_pre_2017),
                                   log_tariff - log_tariff_pre_2017, NA_real_))

test  <- trade %>% filter(year >2015)
unique(test$year)
colSums(is.na(test))
summary(trade$diff_log_tariff_2015)
summary(trade$diff_log_tariff_2017)


################################################################################

# export results

write_csv(trade , paste0(exp, "estimates_log_tariff_FE_boot.csv"))






