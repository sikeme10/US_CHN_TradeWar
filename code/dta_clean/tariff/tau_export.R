

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
library(labelled)
library(haven)



rm(list=ls())
# Set directory
setwd("/data/sikeme/TRADE/NTM_trade_war/data")
getwd()

exp <- "/data/sikeme/TRADE/NTM_trade_war/data/tariff_dta"



# load data
export <- read_dta("/data/sikeme/TRADE/NTM_trade_war/data/tariff_dta/x_flow_hs10_fm_new.dta")



################################################################################

################################################################################


# check labels of variables 
names(export)
var_label(export)

unique(export$cty_name)
summary(export$m_applied_tariff)
unique(export$Year)
unique(export$Month)

# get tariffs applied to U.S.








