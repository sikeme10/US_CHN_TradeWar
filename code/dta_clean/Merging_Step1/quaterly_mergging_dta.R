
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
# aggregate at quaterly level
################################################################################

# create a quaterly variable
names(dta)
table(dta$year)
table(dta$month)

dta <- dta %>% mutate(quarter = case_when(
  month %in% c(1, 2, 3)   ~ 1,
  month %in% c(4, 5, 6)   ~ 2,
  month %in% c(7, 8, 9)   ~ 3,
  month %in% c(10, 11, 12) ~ 4
))

table(dta$quarter,dta$month)


# take sum of trade value
#take avergae of tariffs 
# gravity characteristics that won't change by month:
gravity <- c(  "contig","dist" ,"comlang_off",  "Colonial_ties",  "Importer_GDP",
  "Exporter_GDP",  "Exporter_wto","Exporter_eu",  "Exporter_GDP_current_USD",
  "Exporter_GDPperCap_current_USD",  "Exporter_Gross_Cap_formation_current_USD",
  "Exporter_Ag_land_K2",  "Exporter_Exchange_rate_LCU_per_USD", "rta", "fta_and_eia")



dta_quaterly <- dta %>% group_by(hs6_H5, year,quarter, ImporterISO3, ExporterISO3, `HS6 Description`, hs6_H4) %>%
  summarise( Trade_value_USD = sum(Trade_value_USD, na.rm = TRUE),
             Unit_Price      = mean(Unit_Price, na.rm = TRUE),
             Applied_tariff = mean(Applied_tariff, na.rm=TRUE),
             # For all variables in vars_take_any:
              across(all_of(gravity), ~ dplyr::first(.x[!is.na(.x)])),
             .groups = "drop" )

# checks
table(dta_quaterly$year)

################################################################################
# export data
################################################################################
# save final data
write_csv(dta_quaterly, paste0(exp, "dta_CHN_gravity_quaterly.csv"))
test <- read_csv( paste0(exp, "dta_CHN_gravity_quaterly.csv"))
