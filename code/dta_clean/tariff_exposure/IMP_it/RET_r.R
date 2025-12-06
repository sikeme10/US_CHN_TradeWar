

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
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git")
getwd()

exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure"

################################################################################
# 1) load data:
################################################################################

labor <- read_csv("/data/sikeme/TRADE/NTM_trade_war/data/QCEW/clean_labor_share_2012.csv")

write_csv(merged_data2,  "/data/sikeme/TRADE/NTM_trade_war/data/QCEW/clean_labor_share_2012.csv")







