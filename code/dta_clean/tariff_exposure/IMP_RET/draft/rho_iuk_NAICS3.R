

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
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git/data")
getwd()

exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure"


################################################################################
# 1) load data:
################################################################################

# US tot import at HS6 level H4:
US_import <- read_csv("trade/US_import_schott_2012.csv")
colSums(is.na(US_import))
test<- US_import %>% filter(is.na(ISO_Code))
unique(test$Country)

# US tot export 
US_tot_export <- read_csv("trade/US_tot_export_schott_2012.csv")


# US industry output NAICS 3 digit level
output_NAICS3 <-  read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/Census_output/output_NAICS_3.csv")

# crosswalk: industry NAICS to HS product code 
HS_NAICS <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/crosswalk/clean_HS6_naics3_2012.csv")


################################################################################
# 2) Get import at naics3 digit
################################################################################

names(US_import)
names(HS_NAICS)
names(output_NAICS3)
class(US_import$HS6)
unique(US_import$year)
names(US_tot_export)

# drop naics codes to use our concordance 
US_import <- US_import %>% select(-naics4, -naics3, -naics6, -sic)

# convert product codes in numeric vairable
HS_NAICS$HS6 <- as.numeric(HS_NAICS$HS6)


# Merge HS naics concordance to import data:
Merge_trade <- left_join(US_import, HS_NAICS)
colSums(is.na(Merge_trade))
names(Merge_trade)
test <- Merge_trade %>% filter(is.na(ISO_Code))
unique(test$Country)
test <- Merge_trade %>% filter(is.na(Country))
test <- Merge_trade %>% filter(is.na(naics3))


# aggregate at naics level by summing import value 
Merge_trade_import <- Merge_trade %>% group_by(year, naics3,Country,ISO3_Code ) %>%
  summarise(US_import_USD = sum(import_val_USD, na.rm = TRUE))

################################################################################
# 3) Get tot export at naics3 digit
################################################################################


# Merge HS naics concordance to import data:
Merge_trade2 <- left_join(US_tot_export, HS_NAICS)
colSums(is.na(Merge_trade))
names(Merge_trade)
test <- Merge_trade %>% filter(is.na(ISO_Code))
unique(test$Country)
test <- Merge_trade %>% filter(is.na(Country))
test <- Merge_trade %>% filter(is.na(naics3))


US_tot_export


################################################################################
# 2) Aggregate at naics 3 digit level 
################################################################################

names(Merge_trade_tot_export)
colSums(is.na(Merge_trade_tot_export))

# b) aggregate import at NAICS level of interest

names(Merge_trade_tot_export)

# convert output and import in USD 
# sum import at naics3 digit level
Merge <- Merge_trade_tot_export %>% group_by(year, naics3,Country,ISO3_Code ) %>%
  summarise(US_import_USD = sum(import_val_USD, na.rm = TRUE),
            Tot_export_val_USD =  sum(export_val_USD), na.rm = TRUE )

# Merge output to import data 
Merge_trade_tot_export <- left_join(Merge_trade, US_tot_export)

################################################################################
# 3) Create Shares:
################################################################################


# x_iju = US import to country j
# gamma_iju = share of US industry i output sold to country j =  x_iju /x_iu

Merge <- Merge %>% mutate(
  gamma_iju = US_import_USD /Tot_output_USD)

summary(Merge$gamma_iju)
test <- Merge %>% filter(is.na(gamma_iju))


################################################################################
# 4) import
################################################################################


write_csv(Merge, paste0(exp, "/gamma_iju.csv"))




