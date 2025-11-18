




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
names(trade)

# gravity data
gravity <- read_csv("data/gravity/clean_Gravity.csv")
names(gravity)


# worldbank data
worldbank <- read_csv("data/gravity/Worldbank_dta.csv")
names(worldbank)

# tariff data 
tariff <- read_csv("data/tariff_dta/trade_war_tariffs.csv")
MFN <- read_csv("data/tariff_dta/CHN_import_tariffs/CHN_WITS_tariff_clean.csv")
names(MFN)

################################################################################

# check product codes 

length(unique(trade$`HS6 Code`))
table(trade$Year)
length(unique(trade$PartnerISO3))
unique(trade$`Trade Partner`)
unique(MFN$`Partner Name`)

# change some of the variables names to harmonize
trae <- trade %>% rename()

################################################################################

# select year of analysis 

years <- c(2015:2020)

trade <- trade %>% 










