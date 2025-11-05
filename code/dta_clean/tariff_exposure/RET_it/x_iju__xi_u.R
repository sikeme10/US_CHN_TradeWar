
















################################################################################
# we create two variables here: fraction of US industry i sold domestically

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




rm(list=ls())
# Set directory
setwd("/data/sikeme/TRADE/NTM_trade_war/data")
getwd()

################################################################################
# 1) load data:


# US tot export at HS6 level H4:
US_tot_export <- read_csv("trade/US_tot_export_2012.csv")

# US industry output NAICS 3 digit level
output_NAICS3 <-  read_csv("/data/sikeme/TRADE/NTM_trade_war/data/Census_output/output_NAICS_3.csv")

# crosswalk: industry NAICS to HS product code 

HS_NAICS <- read_csv("crosswalk/HS6_NAICS_Diane/NAICS_HS_2012.csv")
