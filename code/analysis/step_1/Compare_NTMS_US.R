

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
exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/Compare_values/"

################################################################################
# 1) Load data 
################################################################################

trade <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/output/summary/prelim_reg/gravity_pois_FE.csv")
names(trade)
US_NTMs <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/output/Compare_values/US_ln_NTMs.csv")
names(US_NTMs)

################################################################################
# get share of export from US

trade <- trade %>% select(year,month,hs2, hs4, hs6_H5, ExporterISO3, ImporterISO3,Applied_tariff,Trade_value_USD)

total_exp <- trade %>% group_by(year,month,hs2, hs4, hs6_H5, ImporterISO3,) %>% 
  summarise( tot_Trade_value_USD = sum(Trade_value_USD, na.rm = TRUE))


US_trade <- trade %>% filter(ExporterISO3 == "USA") 
US_trade <- left_join(US_trade, total_exp)
names(US_trade)
colSums(is.na(US_trade))

# share of export <- 
US_trade <- US_trade %>% mutate(share_US_export = Trade_value_USD *100 / tot_Trade_value_USD )

# clean ntm data
US_NTMs <- US_NTMs %>% select(year,month,hs2, hs4, hs6_H5, ExporterISO3, ImporterISO3,
                              u, teJLMS , FE_benchmark_exporter , u_benchmark_exporter,
                              elastcities , ln_AVE_FE , ln_AVE_u)

# join NTm and trade data 
US <- left_join(US_trade, US_NTMs)
colSums(is.na(US))


# create a log of tariff variable 
summary(US$Applied_tariff)
US <- US %>% mutate(ln_tariff  = log(1+ Applied_tariff/100))
summary(US$ln_tariff)


##################################log()################################################################################
# aggregate at the yearly level 

# average of tariffs and NTMs AVE 

US_yearly <- US %>% group_by(year,hs2, hs4, hs6_H5, ExporterISO3, ImporterISO3) %>% 
  summarise(Trade_value_USD =  sum(Trade_value_USD, na.rm = TRUE),
            tot_Trade_value_USD =  sum(tot_Trade_value_USD, na.rm = TRUE),
            ln_tariff =  mean(ln_tariff, na.rm = TRUE),
            ln_AVE_FE =  mean(ln_AVE_FE, na.rm = TRUE),
            ln_AVE_u =  mean(ln_AVE_u, na.rm = TRUE)    )

colSums(is.na(US_yearly))





