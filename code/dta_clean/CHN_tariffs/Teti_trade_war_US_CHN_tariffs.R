


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
names(dta)


###############################################################################
# 2) we get ROW (drop CHN) retaliatory tariffs on US 
################################################################################

# checks
names(dta)
unique(dta$importer)
unique(dta$exporter)


# select Chinese import
ROW_import <- dta %>% filter(!importer %in% c("USA", "CHN"))
unique(ROW_import$exporter)
names(ROW_import)

# drop the extra cols (including v2017)
ROW_import <- ROW_import %>% select(-eu1, -eu2, -v2017)

# pivot only t_* columns to long, parse dates, tidy result
ROW_import2 <- ROW_import %>%
  select(importer, exporter, hs6, starts_with("t_")) %>%
  pivot_longer(  cols = starts_with("t_"),
    names_to  = "var",  values_to = "tariff",  values_drop_na = TRUE  ) %>%
  mutate(   date = ymd(str_remove(var, "^t_"))  ) %>%
  arrange(importer, exporter, hs6, date) %>%
  select(importer, exporter, hs6, date, tariff)


colSums(is.na(ROW_import2))
unique(ROW_import2$date)

# create a month year variabke:
library(lubridate)
ROW_import2 <- ROW_import2 %>% mutate( year  = year(date),  month = month(date))

# need to aggregate at the month level: by taking the max of the value 
ROW_import2 <- ROW_import2 %>% group_by(importer, exporter, hs6, year, month) %>%
  summarise(tariff = max(tariff, na.rm = TRUE), .groups = "drop"  )
summary(ROW_import2)


# add nomenclature:
ROW_import2 <- ROW_import2 %>% mutate(nomenclature = "HS2017")

summary(ROW_import2$tariff)
test <- ROW_import2 %>% filter(tariff > 1000)

# export the data
write_csv(ROW_import2, "data/tariff_dta/teti/ROW_US_tariff_HS6_Teti.csv")



###############################################################################
# 2) we get USA import tariffs on CHN and ROW 
################################################################################

# select US import
USA_import <- dta %>% filter(importer == "USA")
unique(USA_import$exporter)
names(USA_import)

# drop the extra cols (including v2017)
USA_import <- USA_import %>% select(-eu1, -eu2, -v2017)

# pivot only t_* columns to long, parse dates, tidy result
USA_import2 <- USA_import %>%
  select(importer, exporter, hs6, starts_with("t_")) %>%
  pivot_longer(
    cols = starts_with("t_"),
    names_to  = "var",
    values_to = "tariff",
    values_drop_na = TRUE) %>%
  mutate(  date = ymd(str_remove(var, "^t_"))  ) %>%
  arrange(importer, exporter, hs6, date) %>%
  select(importer, exporter, hs6, date, tariff)


colSums(is.na(USA_import2))
unique(USA_import2$date)

# create a month year variable:
USA_import2 <- USA_import2 %>%  mutate( year  = year(date),  month = month(date)  )


# need to aggregate at the month level: by taking the max of the value 
USA_import2 <- USA_import2 %>% group_by(importer, exporter, hs6, year, month) %>%
  summarise(tariff = max(tariff, na.rm = TRUE), .groups = "drop"  )
summary(USA_import2)

# add nomenclature:

USA_import2 <- USA_import2 %>% mutate(nomenclature = "HS2017")
names(USA_import2)



# merge back with MFN data 

write_csv(USA_import2, "data/tariff_dta/teti/USA_tariff_HS6_Teti.csv")



###############################################################################
# 2) we get USA import tariffs on CHN (add MFN data)
################################################################################

# load MFN data 
MFN <- read_csv("data/tariff_dta/US_CHN_MFN_tariff.csv")
names(MFN)
table(MFN$`Tariff Year`)
names(MFN) <- gsub(" ", "_", names(MFN))

MFN <- MFN %>% filter(Reporter_Name == "United States")

# get year of interest 2017
MFN <- MFN %>% filter(Tariff_Year %in% c(2015:2020))

MFN_duplicates <- MFN %>% group_by(Product, Tariff_Year,DutyType) %>% filter(n() > 1) %>%
  arrange(Product, Tariff_Year,DutyType) %>%  ungroup()


# select variables of interest 
MFN <- MFN %>% select(Native_Nomen,Product, Reporter, Partner_Name, Tariff_Year, DutyType,
                      Weighted_Average,Simple_Average) %>%  rename(year = Tariff_Year)
# get duty types 
unique(MFN$DutyType)
MFN <- MFN %>% filter(DutyType %in% c("AHS", "MFN"))
MFN1 <- MFN %>% pivot_wider( names_from = DutyType, 
                            values_from = c(Weighted_Average, Simple_Average), names_sep = "_")
MFN_duplicates <- MFN1 %>% group_by(Product, year) %>% filter(n() > 1) %>%
  arrange(Product, year) %>%  ungroup()


# concord product code to HS2017
table(MFN1$Native_Nomen, MFN1$year)
class(MFN1$Product)
unique(nchar(MFN1$Product))
MFN1 <- MFN1 %>% mutate(
  hs6_H5 = case_when(Native_Nomen == "H5" ~ Product,
                     Native_Nomen == "H4" ~ concord_hs(Product,  origin = "HS4", destination = "HS5",
                                                       dest.digit = 6, all = FALSE),    TRUE ~ NA_character_))
colSums(is.na(MFN1))
MFN1 <- MFN1 %>% filter(!is.na(hs6_H5))

MFN_duplicates <- MFN1 %>% group_by(hs6_H5, year) %>% filter(n() > 1) %>%
  arrange(hs6_H5, year) %>%  ungroup()
names(MFN1)

# get min for some of the duplicates (happen when concordance is not perfect)
safe_min <- function(x) {
  if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
}

MFN1 <- MFN1 %>%group_by(hs6_H5, year) %>% summarise(across( c(Weighted_Average_AHS, Weighted_Average_MFN,
                      Simple_Average_AHS,   Simple_Average_MFN),  safe_min),.groups = "drop"  )


# balance the panel data
n_prod  <- n_distinct(MFN1$hs6_H5)
n_years <- n_distinct(MFN1$year)
n_prod * n_years

MFN1_bal_year <- MFN1 %>%
  mutate(year = as.integer(year)) %>%
  distinct(hs6_H5, year, .keep_all = TRUE) %>%   # in case of duplicates
  complete( hs6_H5 = unique(hs6_H5),year   = sort(unique(year))) %>%
  arrange(hs6_H5, year)
nrow(MFN1_bal_year) == n_prod * n_years

# add monthly variables 
MFN1_bal_month <- MFN1_bal_year %>%  crossing(month = 1:12) %>%  arrange(hs6_H5, year, month)

# do a downup for missing variables
colSums(is.na(MFN1_bal_month))
MFN1_bal_month <- MFN1_bal_month %>% arrange(hs6_H5, year, month) %>%
  group_by(hs6_H5) %>%  fill(  Weighted_Average_AHS, Weighted_Average_MFN,
                               Simple_Average_AHS,   Simple_Average_MFN,.direction = "downup" ) %>%  ungroup()
colSums(is.na(MFN1_bal_month))
test <- MFN1_bal_month %>% filter(is.na(Weighted_Average_AHS))

class(c$hs6_H5)

################################################################################


# aggregate at yearly level
USA_import_CHN  <- USA_import2 %>% filter(exporter == "CHN" )


USA_import_CHN$hs6 <- as.character(USA_import_CHN$hs6)
unique(nchar(USA_import_CHN$hs6))
USA_import_CHN <- USA_import_CHN %>%  mutate(hs6 = str_pad(hs6, width = 6, side = "left", pad = "0"))



######

# join with MFN data 
length(unique(MFN1_bal_month$hs6_H5))
length(unique(USA_import_CHN$hs6))

# get the unique product codes 
product  <- read_csv("data/HS_codes/Concordance_HS_to_H5.CSV")
length(unique(product$`HS 2017 Product Code`))
class(product$`HS 2017 Product Code`)
HS5 <- unique(product$`HS 2017 Product Code`)

# check missing product codes 
missing_in_MFN <- setdiff(HS5, unique(MFN1_bal_month$hs6_H5))
missing_in_MFN <- setdiff(unique(MFN1_bal_month$hs6_H5), HS5)

missing_in_teti <- setdiff(HS5, unique(USA_import_CHN$hs6))
missing_in_teti <- setdiff(unique(USA_import_CHN$hs6), HS5)


# create banlance data 
month <- c(1:12)
year <- c(2015:2020)
hs6 <- unique(product$`HS 2017 Product Code`)
balanced_panel <- crossing(hs6  = hs6, year = year, month = month) %>% arrange(hs6, year, month)


# no merge with MFN and the rest of the data 
names(balanced_panel)
names(MFN1_bal_month)
names(USA_import_CHN)
US_tariff <- left_join(balanced_panel, MFN1_bal_month, by = c("hs6" = "hs6_H5",
                                                              "year" = "year",
                                                              "month" = "month"))
colSums(is.na(US_tariff))
US_tariff <- left_join(US_tariff, USA_import_CHN)
names(US_tariff)

# fill values 
US_tariff <- US_tariff %>% arrange(hs6, year, month) %>% group_by(hs6) %>% 
  fill(tariff, .direction = "down" ) %>%  ungroup()
names(US_tariff)

US_tariff <- US_tariff %>% 
  mutate(tariff2 = case_when(is.na(Weighted_Average_AHS) & !is.na(tariff) ~ tariff,
                             !is.na(Weighted_Average_AHS) & is.na(tariff) ~ Weighted_Average_AHS,
                             !is.na(Weighted_Average_AHS) & !is.na(tariff) ~ pmax(Weighted_Average_AHS, tariff),
                             TRUE ~ NA_real_))
colSums(is.na(US_tariff))
US_tariff <- US_tariff %>% arrange(hs6, year, month) %>% group_by(hs6) %>% 
  fill(tariff2, .direction = "up" ) %>%  ungroup()

US_tariff <- US_tariff %>% select(hs6,year,month,tariff2 ) %>% rename(US_tariff_onCHN = tariff2)

# create baseline variables 
US_baseline <- US_tariff %>%   filter(year %in% c(2015, 2017)) %>%
  pivot_wider(names_from = year, values_from = US_tariff_onCHN, 
              names_prefix = "US_tariff_onCHN_" )
US_tariff <- US_tariff %>% filter(year %in% c(2018:2020))

US_tariff <- left_join(US_tariff, US_baseline)

write_csv(US_tariff, "data/tariff_dta/teti/US_tariff_on_CHN_monthly.csv")

# at the yearly level 
US_tariff_yearly <- US_tariff %>% group_by(hs6,year) %>% 
  summarise(US_tariff_onCHN = mean(US_tariff_onCHN, na.rm = TRUE),
            US_tariff_onCHN_2015 = mean(US_tariff_onCHN_2015, na.rm = TRUE),
            US_tariff_onCHN_2017 = mean(US_tariff_onCHN_2017, na.rm = TRUE))

write_csv(US_tariff_yearly, "data/tariff_dta/teti/US_tariff_on_CHN_yearly.csv")







