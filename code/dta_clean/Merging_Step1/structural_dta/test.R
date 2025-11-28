library(readr)
library(dplyr)
library(data.table)




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
library(sfaR)
library(frontier)

################################################################################
# directory: 
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git")

exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_structural/"


################################################################################
# 1) Load data 
################################################################################

dta <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_gravity/dta_CHN_gravity_3_quant.csv")

# test<- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_gravity/dta_CHN_gravity_3.csv")

names(dta)
colSums(is.na(dta))


################################################################################
# 1) Aggregate at the yearly level
################################################################################

# for price if the price is 0 put NA so that when taking average of prices it s not counted 
# dta <- dta %>% mutate( Unit_Price = ifelse( Unit_Price == 0 , NA, Unit_Price ) )
dta <- dta %>% mutate( Quantity = ifelse( is.na(Quantity) , 0, Quantity ) )
head(dta)
colSums(is.na(dta))

split_dta_15_17 <- dta %>% filter(year %in% c(2015, 2016, 2017)) 
dta_yearly_15_17  <- split_dta_15_17 %>% group_by( year, ImporterISO3, ExporterISO3, hs6_H5 ) %>%
  summarise( Trade_value_USD = sum( Trade_value_USD , na.rm=TRUE ),
             Quantity = sum( Quantity , na.rm=TRUE ),
             Unit_Price = mean( Unit_Price , na.rm=TRUE ),
             Applied_tariff = mean( Applied_tariff , na.rm=TRUE ) ) %>%
  ungroup() 
write_csv(dta_yearly_15_17, file.path( exp, "dta_CHN_structural_quant_yearly_15_17.csv" ) )


split_dta_18_20 <- dta %>% filter(year %in% c(2018, 2019, 2020)) 
dta_yearly_18_20  <- split_dta_18_20 %>% group_by( year, ImporterISO3, ExporterISO3, hs6_H5 ) %>%
  summarise( Trade_value_USD = sum( Trade_value_USD , na.rm=TRUE ),
             Quantity = sum( Quantity , na.rm=TRUE ),
             Unit_Price = mean( Unit_Price , na.rm=TRUE ),
             Applied_tariff = mean( Applied_tariff , na.rm=TRUE ) ) %>%
  ungroup() 
write_csv(dta_yearly_18_20, file.path( exp, "dta_CHN_structural_quant_yearly_18_20.csv" ) )










# 
# 
# 
# 
# 
# 
# setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git")
# exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_structural/"
# 
# # 1) Read data (as you do)
# dta <- read_csv("data/created_gravity/dta_CHN_gravity_3_quant.csv")
# 
# # 2) Convert Unit_Price from character -> numeric safely
# #    - strip non-numeric characters if needed
# #    - readr::parse_number is robust if there are weird symbols
# dta <- dta %>%
#   mutate(
#     Unit_Price = readr::parse_number(Unit_Price)
#   )
# 
# # 3) Apply your cleaning rules
# #    - set Unit_Price == 0 to NA
# #    - if Quantity is NA, set to 0
# dta <- dta %>%
#   mutate(
#     Unit_Price = ifelse(Unit_Price == 0, NA_real_, Unit_Price),
#     Quantity   = ifelse(is.na(Quantity), 0, Quantity)
#   )
# 
# # 4) Switch to data.table (in-place, no copy)
# setDT(dta)
# 
# # 5) Fast aggregation with base::mean() (avoids GForce type error)
# dta_yearly <- dta[
#   ,
#   .(
#     Trade_value_USD = sum(Trade_value_USD, na.rm = TRUE),
#     Quantity        = sum(Quantity,        na.rm = TRUE),
#     Unit_Price      = base::mean(Unit_Price,     na.rm = TRUE),
#     Applied_tariff  = base::mean(Applied_tariff, na.rm = TRUE)
#   ),
#   by = .(year, ImporterISO3, ExporterISO3, hs6_H5)
# ]
# 
# # 6) Save result
# write_csv(dta_yearly, file.path(exp, "dta_CHN_structural_quant_yearly_bis.csv"))
# 
# rm(list=ls())
# 
# 
# dta_yearly <- read_csv(paste0(exp, "dta_CHN_structural_quant_yearly_bis.csv"))
# dta_yearly1<- read_csv(paste0(exp, "dta_CHN_structural_yearly.csv"))
