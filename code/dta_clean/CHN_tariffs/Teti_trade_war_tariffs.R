


################################################################################
#                      Teti tariff data

 
# The data provide tariff rates imposed and faced by the United States vis-à-vis all
# trading partners at the 6-digit product level. Tariff rates are observed on each date
# when changes occurred between 2018 and 2025.
# Each variable t_YYYYMMDD reports the effective applied tariff rate (in percent) on that specific date.
# All product codes are harmonized to the HS2017 nomenclature.

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
dta <- read_csv("data/tariff_dta/teti/GTD-tradeWar_hs6.csv")

################################################################################

# checks
names(dta)
unique(dta$importer)
unique(dta$exporter)


# select Chinese import
CHN_import <- dta %>% filter(importer == "CHN")
names(CHN_import)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

# drop the extra cols (including v2017)
CHN_import <- CHN_import %>% select(-eu1, -eu2, -v2017)

# pivot only t_* columns to long, parse dates, tidy result
CHN_import2 <- CHN_import %>%
  select(importer, exporter, hs6, starts_with("t_")) %>%
  pivot_longer(
    cols = starts_with("t_"),
    names_to  = "var",
    values_to = "tariff",
    values_drop_na = TRUE
  ) %>%
  mutate(
    date = ymd(str_remove(var, "^t_"))
  ) %>%
  arrange(importer, exporter, hs6, date) %>%
  select(importer, exporter, hs6, date, tariff)


colSums(is.na(CHN_import2))
unique(CHN_import2$date)

# create a month year variabke:
library(lubridate)

CHN_import2 <- CHN_import2 %>%
  mutate(
    year  = year(date),
    month = month(date)  )


# add nomenclature:

CHN_import2 <- CHN_import2 %>% mutate(nomenclature = "HS2017")

write_csv(CHN_import2, "data/tariff_dta/CHN_tariff_HS6_Teti.csv")















