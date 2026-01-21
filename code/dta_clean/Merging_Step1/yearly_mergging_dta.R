
################################################################################
#                      Aggregating Gravity data at the yearly level


# aggregation of data 
# for trade value can sum it up
# for tariff can take the average 
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

################################################################################
# directory: 
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git")

exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_gravity/"

################################################################################
# 1) Load data 
################################################################################

dta <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_gravity/dta_CHN_gravity_3.csv")

################################################################################

names(dta)
summary(dta$Applied_tariff)
table(dta$year)
table(dta$month)
################################################################################
# aggregate at yearly level
################################################################################
# take sum of trade value
#take avergae of tariffs 
# gravity characteristics that won't change by month:
gravity <- c(  "contig","dist" ,"comlang_off",  "Colonial_ties",  "Importer_GDP",
  "Exporter_GDP",  "Exporter_wto","Exporter_eu",  "Exporter_GDP_current_USD",
  "Exporter_GDPperCap_current_USD",  "Exporter_Gross_Cap_formation_current_USD",
  "Exporter_Ag_land_K2",  "Exporter_Exchange_rate_LCU_per_USD", "rta", "fta_and_eia")



# dta_yearly <- dta %>% group_by(hs6_H5, year,ImporterISO3, ExporterISO3, `HS6 Description`, hs6_H4) %>%
#   summarise( Trade_value_USD = sum(Trade_value_USD, na.rm = TRUE),
#              Unit_Price      = mean(Unit_Price, na.rm = TRUE),
#              Applied_tariff = mean(Applied_tariff, na.rm=TRUE),
#              # For all variables in vars_take_any:
#              across( all_of(gravity),   ~ first(na.omit(.x))    ),
#     .groups = "drop"  )

dta_yearly <- dta %>% group_by(hs6_H5, year,ImporterISO3, ExporterISO3, `HS6 Description`, hs6_H4) %>%
  summarise( Trade_value_USD = sum(Trade_value_USD, na.rm = TRUE),
             Unit_Price      = mean(Unit_Price, na.rm = TRUE),
             Applied_tariff = mean(Applied_tariff, na.rm=TRUE),
             # For all variables in vars_take_any:
              across(all_of(gravity), ~ dplyr::first(.x[!is.na(.x)])),
             .groups = "drop" )

# checks
table(dta_yearly$year)

################################################################################
# export data
################################################################################
# save final data
write_csv(dta_yearly, paste0(exp, "dta_CHN_gravity_yearly.csv"))

