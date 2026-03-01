

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
setwd("/data/sikeme/TRADE/NTM_trade_war/data")
getwd()

exp <- "/data/sikeme/TRADE/NTM_trade_war/data/created_exposure/NAICS6"

################################################################################
# 1) load data:
################################################################################

# US tot export at HS6 level H4:
US_tot_export <- read_csv("trade/US_tot_export_schott_2012.csv")

# US industry output NAICS 3 digit level
output_NAICS3 <-  read_csv("/data/sikeme/TRADE/NTM_trade_war/data/Census_output/output_NAICS_3.csv")

# crosswalk: industry NAICS to HS product code 
HS_NAICS <- read_csv("/data/sikeme/TRADE/NTM_trade_war/data/crosswalk/clean_HS6_naics3_2012.csv")

# tariff data 


################################################################################
# 2) Merging data 
################################################################################

# a) Merge trade export data 
names(US_tot_export)
names(HS_NAICS)
names(output_NAICS3)
#names(HS_NAICS_export)

# convert product codes in numeric vairable
HS_NAICS$HS6 <- as.numeric(HS_NAICS$HS6)


# Merge HS naics concordance to export data:
Merge_trade <- left_join(US_tot_export, HS_NAICS)
colSums(is.na(Merge_trade))
names(Merge_trade)

# Merge output to export data 
Merge_trade_output <- left_join(Merge_trade, output_NAICS3, by = c("naics3" = "NAICS3_2012", "year" = "year"))

################################################################################

# b) aggregate export at NAICS level of interest

names(Merge_trade_output)
# convert output and export in USD 
# sum export at naics3 digit level
Merge <- Merge_trade_output %>% group_by(year, naics3 ) %>%
  summarise(US_tot_export_USD = sum(export_val_USD, na.rm = TRUE),
    Tot_output_dollars =  first(Tot_output_1000dollars*1000)  )


################################################################################
# 3) create variables of interest
################################################################################


# x_iuu = output sold domestically = tot output - tot export 

Merge <- Merge %>% mutate(
  x_iuu = Tot_output_dollars - US_tot_export_USD ,
  gamma_iuu = x_iuu /Tot_output_dollars)




################################################################################
# 4) Export
################################################################################


write_csv(Merge, paste0(exp, "/gamma_iuu.csv"))






