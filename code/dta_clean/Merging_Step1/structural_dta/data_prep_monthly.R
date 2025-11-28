
################################################################################
#                     Structural model analysis 

# get data ready for structural model
# log differentiation of variable sof interest 
# aggregate at the yearly levle?

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

exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/reg/structural"


################################################################################
# 1) Load data 
################################################################################


dta <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_gravity/dta_CHN_gravity_3.csv")
names(dta)
colSums(is.na(dta))


################################################################################
table(dta$year)

# create a time variabke from month and year variable 
dta <- dta %>% mutate( time = (year - 2015) * 12 + month )
table(dta$time)


################################################################################



################################################################################

# log differentiating the variables of interest 

# outcome variable and independnt variable (such as tariff) 
names(dta)
# log variable of interest first
dta <- dta %>% mutate(log_Trade_value_USD = log(Trade_value_USD +1),
                      log_Unit_Price = log(Unit_Price +1),
                      log_Applied_tariff = log( Applied_tariff +1) )
summary(dta$log_Trade_value_USD)
summary(dta$log_Unit_Price)
summary(dta$log_Applied_tariff)
# then do the log differentiation by exporter - hs6 - time
dta <- dta %>%
  group_by( ExporterISO3, hs6_H5 ) %>%
  arrange( ExporterISO3, hs6_H5, time ) %>%
  mutate( d_log_Trade_value_USD = log_Trade_value_USD - lag(log_Trade_value_USD),
          d_log_Unit_Price = log_Unit_Price - lag(log_Unit_Price),
          d_log_Applied_tariff = log_Applied_tariff - lag(log_Applied_tariff) ) %>%
  ungroup()

test <- dta %>% group_by(ExporterISO3, hs6_H5) %>% 
  filter(any(log_Trade_value_USD != 0))


################################################################################






