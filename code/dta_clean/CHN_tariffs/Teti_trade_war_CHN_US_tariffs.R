


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
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)


################################################################################
# directory: 
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git")
################################################################################

# 1) Load data 
dta <- read_csv("data/tariff_dta/teti/GTD-tradeWar_hs6.csv")

################################################################################
# 1) we get chinese retaliatory tariffs on US 
################################################################################

# checks
names(dta)
unique(dta$importer)
unique(dta$exporter)


# select Chinese import
CHN_import <- dta %>% filter(importer == "CHN")
unique(CHN_import$exporter)
names(CHN_import)

# drop the extra cols (including v2017)
CHN_import <- CHN_import %>% select(-eu1, -eu2, -v2017)

# pivot only t_* columns to long, parse dates, tidy result
CHN_import2 <- CHN_import %>%
  select(importer, exporter, hs6, starts_with("t_")) %>%
  pivot_longer(
    cols = starts_with("t_"),
    names_to  = "var",
    values_to = "tariff",
    values_drop_na = TRUE  ) %>%
  mutate( date = ymd(str_remove(var, "^t_"))) %>%
  arrange(importer, exporter, hs6, date) %>%select(importer, exporter, hs6, date, tariff)


colSums(is.na(CHN_import2))
unique(CHN_import2$date)

# create a month year variable:
library(lubridate)

CHN_import2 <- CHN_import2 %>% mutate(year  = year(date),  month = month(date))
names(CHN_import2)

# duplicates in data
any(duplicated(CHN_import2[c("importer", "exporter" , "hs6", "year", "month", "date")]))
any(duplicated(CHN_import2[c("hs6", "year", "month")]))
# need to aggregate at the month level: by taking the max of the value
CHN_import2 <- CHN_import2 %>% group_by(importer, exporter, hs6, year, month) %>%
  summarise(tariff = max(tariff, na.rm = TRUE), .groups = "drop"  )
summary(CHN_import2)

# add nomenclature:
CHN_import2 <- CHN_import2 %>% mutate(nomenclature = "HS2017")

summary(CHN_import2$tariff)

write_csv(CHN_import2, "data/tariff_dta/teti/CHN_tariff_HS6_Teti.csv")

################################################################################




