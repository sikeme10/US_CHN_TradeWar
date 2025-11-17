




################################################################################
#                      Merging data for gravity model


# merge trade dta with tariffs and gravity model 
# at HS6 product level 
# chinese imports from exporting source countries 
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
################################################################################

# 1) Load data 

# trade data 
trade <- read_csv("data/trade/GTA_CHN_import/CHN_import_2015_2023.csv")

# gravity data
gravity <- read_csv("data/gravity/clean_Gravity.csv")

# worldbank data
worldbank <- read_csv("data/gravity/Worldbank_dta.csv")

# tariff data 
tariff <- read_csv("data/tariff_dta/trade_war_tariffs.csv")
MFN <- read_csv("data/tariff_dta/CHN_import_tariffs/CHN_WITS_tariff_clean.csv")

################################################################################







