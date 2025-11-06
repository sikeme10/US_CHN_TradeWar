




################################################################################
#                      Cepii bilateral gravity charteristics


# get worldbank data for gravity models


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

# variables of interest: "Statutory Tariff Rate"/"WTO MFN Tariff Rate" /   "Trade War Export Tariff Increase
summary(CHN_US_export$x_increase)

CHN_US_export <- CHN_US_export %>%




