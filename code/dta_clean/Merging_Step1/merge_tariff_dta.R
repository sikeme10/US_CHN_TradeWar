





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



# Load data 
fagelbaum <- read_csv("data/tariff_dta/CHN_tariff_HS6_Fagel.csv")
teti <- read_csv("data/tariff_dta/CHN_tariff_HS6_Teti.csv")
MFN_WITS <- read_csv( "data/tariff_dta/CHN_import_tariffs/CHN_WITS_tariff_clean.csv")




