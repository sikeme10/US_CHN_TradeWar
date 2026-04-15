


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
library(concordance)

################################################################################
# directory: 
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git")
################################################################################
# 1) Load data 
################################################################################

# teti trade data 
dta <- read_csv("data/tariff_dta/teti/GTD-tradeWar_hs6.csv")
names(dta)



###############################################################################
# 2) we get Teti USA import tariffs on CHN and ROW 
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
unique(USA_import2$exporter)


# merge back with MFN data 

write_csv(USA_import2, "data/tariff_dta/teti/USA_tariff_HS6_Teti.csv")

USA_import2 <- read_csv("data/tariff_dta/teti/USA_tariff_HS6_Teti.csv")

###############################################################################
# 2) We clean MFN data to create baselines 
################################################################################

# load MFN data 
MFN <- read_csv("data/tariff_dta/WITS_MFN/US_ROW_MFN_tariff.csv")
names(MFN)
names(MFN) <- gsub(" ", "_", names(MFN))
table(MFN$Tariff_Year)
unique(MFN$Partner_Name)


MFN <- MFN %>% filter(Reporter_Name == "United States")

# get year of interest 2017
MFN <- MFN %>% filter(Tariff_Year %in% c(2015:2020))

MFN_duplicates <- MFN %>% group_by(Partner_Name, Product, Tariff_Year,DutyType) %>% filter(n() > 1) %>%
  arrange(Partner_Name, Product, Tariff_Year,DutyType) %>%  ungroup()


# select variables of interest 
MFN <- MFN %>% select(Native_Nomen,Product, Reporter, Partner_Name, Tariff_Year, DutyType,
                      Weighted_Average,Simple_Average) %>%  rename(year = Tariff_Year)


# get duty types 
unique(MFN$DutyType)
MFN <- MFN %>% filter(DutyType %in% c("AHS", "MFN"))
MFN1 <- MFN %>% pivot_wider( names_from = DutyType, 
                            values_from = c(Weighted_Average, Simple_Average), names_sep = "_")
MFN_duplicates <- MFN1 %>% group_by(Partner_Name , Product, year) %>% filter(n() > 1) %>%
  arrange(Partner_Name, Product, year) %>%  ungroup()


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

MFN_duplicates <- MFN1 %>% group_by(Partner_Name, hs6_H5, year) %>% filter(n() > 1) %>%
  arrange(Partner_Name,hs6_H5, year) %>%  ungroup()
names(MFN1)

# # get min for some of the duplicates (happen when concordance is not perfect)
# safe_min <- function(x) {
#   if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)
# }
# 
# MFN1 <- MFN1 %>%group_by(hs6_H5, year) %>% summarise(across( c(Weighted_Average_AHS, Weighted_Average_MFN,
#                       Simple_Average_AHS,   Simple_Average_MFN),  safe_min),.groups = "drop"  )


# balance the panel data
n_prod  <- n_distinct(MFN1$hs6_H5)
n_partners  <- n_distinct(MFN1$Partner_Name)
n_years <- n_distinct(MFN1$year)
n_prod * n_years *n_partners

MFN1_bal_year <- MFN1 %>%
  mutate(year = as.integer(year)) %>%
  distinct(Partner_Name, hs6_H5, year, .keep_all = TRUE) %>%   # in case of duplicates
  complete( Partner_Name = unique(Partner_Name), hs6_H5 = unique(hs6_H5), year   = sort(unique(year))) %>%
  arrange(Partner_Name, hs6_H5, year)
nrow(MFN1_bal_year) == n_prod * n_years*n_partners



# add monthly variables 
MFN1_bal_month <- MFN1_bal_year %>%  crossing(month = 1:12) %>%  arrange(hs6_H5, year, month)

# do a downup for missing variables
colSums(is.na(MFN1_bal_month))
MFN1_bal_month <- MFN1_bal_month %>% arrange(Partner_Name, hs6_H5, year, month) %>%
  group_by(Partner_Name, hs6_H5) %>%  fill(Weighted_Average_AHS, Weighted_Average_MFN,
                               Simple_Average_AHS,   Simple_Average_MFN,.direction = "downup" ) %>%  ungroup()
colSums(is.na(MFN1_bal_month))
test <- MFN1_bal_month %>% filter(is.na(Weighted_Average_AHS))



# get isocodes 
unique(MFN1_bal_month$Partner_Name)
colSums(is.na(MFN1_bal_month))
MFN1_bal_month$ExporterISO3 <- countrycode(MFN1_bal_month$Partner_Name,
                       origin = "country.name",  destination = "iso3c")
test <-  MFN1_bal_month %>% filter(is.na(ExporterISO3))
unique(test$Partner_Name)
MFN1_bal_month <- MFN1_bal_month %>% filter(!is.na(ExporterISO3))
length(unique(MFN1_bal_month$ExporterISO3))


################################################################################
# Merge teti and MFN data 
################################################################################


# chosose exporter ROW countries (drop CHN)
unique(USA_import2$exporter)
unique(USA_import2$year)
USA_import_ROW  <- USA_import2 %>% filter(exporter != "CHN" )
USA_import_ROW  <- USA_import_ROW  %>% filter(year %in% c(2018:2020))
colSums(is.na(USA_import_ROW))

# check which countries has a tariff of 0 across all products and year 
zero_tariff_countries <- USA_import_ROW %>%  group_by(exporter) %>%
  summarise(all_zero = all(tariff == 0, na.rm = TRUE)) %>% filter(all_zero == TRUE)

# choose exporters that are provided by teti 
exporters_teti <- unique(USA_import_ROW$exporter)
exporters_MFN <- unique(MFN1_bal_month$ExporterISO3)
missing_in_countries <- setdiff(exporters_MFN, exporters_teti)




# get harmonized product characters
class(MFN1_bal_month$hs6_H5)
class(USA_import_ROW$hs6)
USA_import_ROW$hs6 <- as.character(USA_import_ROW$hs6)
unique(nchar(USA_import_ROW$hs6))
USA_import_ROW <- USA_import_ROW %>%  mutate(hs6 = str_pad(hs6, width = 6, side = "left", pad = "0"))

length(unique(MFN1_bal_month$hs6_H5))
length(unique(USA_import_ROW$hs6))

# get the unique product codes 
product  <- read_csv("data/HS_codes/Concordance_HS_to_H5.CSV")
length(unique(product$`HS 2017 Product Code`))
class(product$`HS 2017 Product Code`)
HS5 <- unique(product$`HS 2017 Product Code`)

# check missing product codes 
missing_in_MFN <- setdiff(HS5, unique(MFN1_bal_month$hs6_H5))
missing_in_MFN <- setdiff(unique(MFN1_bal_month$hs6_H5), HS5)

missing_in_teti <- setdiff(HS5, unique(USA_import_ROW$hs6))
missing_in_teti <- setdiff(unique(USA_import_ROW$hs6), HS5)


################################################################################

# create banlance data 
month <- c(1:12)
year <- c(2015:2020)
hs6 <- unique(product$`HS 2017 Product Code`)
exporters_teti  <- unique(USA_import_ROW$exporter)
balanced_panel <- crossing(hs6  = hs6, year = year, month = month, ExporterISO3 = exporters_teti) %>% arrange(hs6, year, month)


# no merge with MFN and the rest of the data 
names(balanced_panel)
names(MFN1_bal_month)
names(USA_import_ROW)
US_tariff <- left_join(balanced_panel, MFN1_bal_month, 
                       by = c("hs6" = "hs6_H5","year" = "year", "month" = "month", "ExporterISO3" = "ExporterISO3"))

colSums(is.na(US_tariff))
names(USA_import_ROW)
USA_import_ROW <- USA_import_ROW %>% rename(ImporterISO3 = importer, ExporterISO3 = exporter)
US_tariff <- left_join(US_tariff, USA_import_ROW)
names(US_tariff)

################################################################################

# fill values 
US_tariff <- US_tariff %>% arrange(ExporterISO3, hs6, year, month) %>% group_by(ExporterISO3, hs6) %>% 
  fill(tariff, .direction = "down" ) %>%  ungroup()
names(US_tariff)


# we create a variable tariff2 that takes values of teti tariffs and 

US_tariff <- US_tariff %>% 
  group_by(hs6, ExporterISO3) %>%
  mutate(
    # Reference values at 2018 month 1
    ref_AHS    = Weighted_Average_AHS[year == 2018 & month == 1],
    ref_tariff = tariff[year == 2018 & month == 1],
    # Start tariff2 as tariff
    tariff2 = tariff,
    # For observations before 2018m1, apply the rule
    tariff2 = case_when(
      year < 2018 & ref_AHS < ref_tariff ~ Weighted_Average_AHS,  # AHS was lower -> use AHS pre-2018
      year < 2018 & ref_AHS >= ref_tariff ~ tariff,               # AHS was higher -> keep tariff pre-2018
      year == 2018 & month == 1 ~ tariff,                         # 2018m1 itself always tariff
      TRUE ~ tariff2                                               # 2018m1 onward stays as tariff
    )  ) %>%  ungroup() %>%  select(-ref_AHS, -ref_tariff) 


# fill up tariff pre2018
US_tariff <- US_tariff %>%
  arrange(hs6, year, month) %>%
  group_by(hs6, ExporterISO3) %>%
  mutate(
    tariff2 = if_else(
      year < 2018 | (year == 2018 & month == 1),
      # within pre-2018m2 window: fill upward using the first non-NA value in the group
      zoo::na.locf(tariff2, fromLast = TRUE, na.rm = FALSE),
      tariff2  # post-2018m1: leave untouched
    )  ) %>%  ungroup()



US_tariff <- US_tariff %>% select(ExporterISO3, hs6,year,month,tariff2 ) %>% rename(US_tariff_onROW = tariff2)

################################################################################

# create baseline variables 
US_baseline <- US_tariff %>%   filter(year %in% c(2015, 2017)) %>%
  pivot_wider(names_from = year, values_from = US_tariff_onROW, 
              names_prefix = "US_tariff_onROW_" )
US_tariff <- US_tariff %>% filter(year %in% c(2018:2020))

US_tariff <- left_join(US_tariff, US_baseline)


test <- US_tariff %>% filter(ExporterISO3 == "CAN")
test <- US_tariff %>% filter(US_tariff_onROW %in% c(10, 25))


write_csv(US_tariff, "data/tariff_dta/teti/US_tariff_on_ROW_monthly.csv")

US_tariff <- read_csv( "data/tariff_dta/teti/US_tariff_on_ROW_monthly.csv")
names(US_tariff)



# at the yearly level 
US_tariff_yearly <- US_tariff %>% group_by(ExporterISO3, hs6,year) %>% 
  summarise(US_tariff_onROW = mean(US_tariff_onROW, na.rm = TRUE),
            US_tariff_onROW_2015 = mean(US_tariff_onROW_2015, na.rm = TRUE),
            US_tariff_onROW_2017 = mean(US_tariff_onROW_2017, na.rm = TRUE))

write_csv(US_tariff_yearly, "data/tariff_dta/teti/US_tariff_on_ROW_yearly.csv")







