
################################################################################
#                      Merging data for gravity model


# merge trade dta with tariffs and gravity model 
# at HS6 product level 
# Chinese imports from exporting source countries 
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
################################################################################

# trade data 
trade <- read_csv("data/trade/GTA_CHN_import/CHN_import_2015_2020.csv")
names(trade)

# gravity data
gravity <- read_csv("data/gravity/clean_Gravity.csv")
names(gravity)

# worldbank data
worldbank <- read_csv("data/gravity/Worldbank_dta.csv")
names(worldbank)

# tariff data 
tariff <- read_csv("data/tariff_dta/trade_war_tariffs.csv")
names(tariff)
MFN <- read_csv("data/tariff_dta/CHN_import_tariffs/CHN_WITS_tariff_clean.csv")
names(MFN)


################################################################################
# 2) Check each data to merge them: get combination of product and partner to balance data
################################################################################

# a) check product codes 

length(unique(trade$`HS6 Code`))
length(unique(trade$hs6_H4))
length(unique(trade$hs6_H5))
length(unique(tariff$hs6_H4))
length(unique(tariff$hs6))
length(unique(MFN$hs6_H5))
length(unique(MFN$hs6_H4))

# get unique variation in productCode using HS 2017 revision 
ProductCode_H5 <- unique(c(trade$hs6_H5, MFN$hs6_H5))



# b) trade partners 

length(unique(trade$ExporterISO3))
unique(trade$`Trade Partner`)
length(unique(MFN$`Partner Name`))
unique(MFN$`Partner Name`)

# order partner based on traded volume 
partner_order <- trade %>%  group_by(ExporterISO3) %>%
  summarise(total_usd = sum(Trade_value_USD, na.rm = TRUE)) %>%
  arrange(desc(total_usd)) %>%
  pull(ExporterISO3)
partner_order

Partners <- partner_order[1:100]
# Check if present in MFN data 
in_mfn <- Partners %in% MFN$ExporterISO3
# Missing partners
missing <- Partners[!in_mfn]
missing
# get name of those countries 
test <- trade %>% filter(ExporterISO3 %in% missing)
unique(test$`Trade Partner`)
# Remove missing partners from Partners vector
Partners <- Partners[Partners %in% MFN$ExporterISO3]


# c) time 
table(trade$year)
table(trade$month)
trade$month <- as.numeric(trade$month)

################################################################################

# Balanced data 

# change some of the variables names to harmonize
# get data of unique combination of Product-code, Partners, Year and month 
#  have to re balance the data so that unique observation for hs6", "year", "month", and exporter 

panel_balanced <- expand_grid(
  hs6_H5       = ProductCode_H5,
  year         = 2015:2020,
  month        = 1:12,
  ExporterISO3 = Partners,
  ImporterISO3 = "CHN")
table(panel_balanced$year)
table(panel_balanced$month)


# merge everything:
dups <- trade %>%  group_by(hs6_H5, year, month, ExporterISO3, ImporterISO3) %>%
  filter(n() > 1)
unique(dups$`Trade Partner`)
# drop duplicates for spain 
trade <- trade %>%  filter(!`Trade Partner` %in% c(  "Melilla",  "Canary Islands",
    "French Polynesia","Society Islands", "Tuamotu Islands","Gambier Islands",
    "Marquesas Islands", "Tubuai Islands","Ceuta","NL Antilles (Bonaire)",  "NL Antilles (Saba)"  ))
dta <- left_join(panel_balanced, trade)


################################################################################









