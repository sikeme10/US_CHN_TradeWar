




################################################################################
#                      Fagelbaum tariff data


#Data provide US export data and tariffs on US export (provide retaliatory tariffs)

################################################################################


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
rm(list=ls())

################################################################################

setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git")
################################################################################

# we can get US export data and associated tariff data faced by US export data 
# we can get Chinese tariffs  from this data 



# Load data 
library(haven)
US_export <- read_dta("data/tariff_dta/x_flow_hs10_fm_new.dta")

unique(US_export$cty_name)
label(US_export)

CHN_US_export <- US_export %>% filter(cty_name == "CHINA")

################################################################################

# Select variable of interest: tariffs

# variables of interest: "Statutory Tariff Rate"/"WTO MFN Tariff Rate" /   "Trade War Export Tariff Increase
summary(CHN_US_export$x_increase)
summary(CHN_US_export$x_stattariff1)
summary(CHN_US_export$x_stattariff2)
summary(CHN_US_export$x_mfn_tariff)

names(CHN_US_export)
CHN_US_export1 <- CHN_US_export %>% select(cty_name, hs10, hs6,x_stattariff1, x_stattariff2, x_mfn_tariff,x_increase )

################################################################################

# HS Porduct Aggregation

# tariffs are at HS10 level
# need to get at HS6 level












