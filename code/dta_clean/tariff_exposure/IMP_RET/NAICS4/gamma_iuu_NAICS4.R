

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

################################################################################
# 1) load data:


# US tot export at HS6 level H4:
US_tot_export <- read_csv("trade/US_tot_export_schott_2012.csv")
#US_tot_export <- read_csv("trade/US_tot_export_2012.csv")

# US industry output NAICS 3 digit level
output_NAICS4 <-  read_csv("/data/sikeme/TRADE/NTM_trade_war/data/Census_output/output_NAICS_4.csv")

# crosswalk: industry NAICS to HS product code 

HS_NAICS <- read_csv("crosswalk/HS6_NAICS_Diane/NAICS_HS_2012.csv")
HS_NAICS_export <- read_csv("trade/export_NAICS_HS_schott_naics4_2012.csv")

################################################################################

# 2) Merging data 

# Merge trade export data 
# names(US_tot_export)
# class(US_tot_export$HS6)
# US_tot_export$HS6 <- str_pad(as.character(US_tot_export$HS6), width = 6,
#                              side = "left",  pad = "0")
# unique(nchar(US_tot_export$HS6))
# names(HS_NAICS)
# names(output_NAICS4)
# 
# class(HS_NAICS_export$HS6)
# HS_NAICS_export$HS6 <- str_pad(as.character(HS_NAICS_export$HS6), width = 6,
#                                side = "left",  pad = "0")
# unique(nchar(HS_NAICS_export$HS6))



library(dplyr)
# 
# Merge_trade <- left_join(US_tot_export, HS_NAICS, by = c("HS6" = "ProductCode_H4"))
# colSums(is.na(Merge_trade))
Merge_trade <- left_join(US_tot_export, HS_NAICS, by = c("HS6" = "ProductCode_H4"))
colSums(is.na(Merge_trade))

Merge_trade <- left_join(US_tot_export, HS_NAICS_export, by = c("HS6" = "HS6"))
names(Merge_trade)
names(output_NAICS4)

Merge_trade_output <- left_join(Merge_trade, output_NAICS4, by = c("naics4" = "NAICS4_2012", "year" = "year"))



################################################################################

# aggregate export at NAICS level of interest

names(Merge_trade_output)

# Merge <- Merge_trade_output %>% group_by(year, naics4 ) %>%
#   summarise(
#     US_tot_export_1000USD = sum(export_val_USD, na.rm = TRUE),
#     Tot_output_1000dollars =  first(Tot_output_1000dollars)  )


Merge <- Merge_trade_output %>% group_by(year, naics4 ) %>%
  summarise(
    US_tot_export_USD = sum(export_val_USD, na.rm = TRUE),
    Tot_output_dollars =  first(Tot_output_1000dollars*1000)  )

################################################################################

# 3) create variables of interest

# x_iuu = output sold domestically = tot output - tot export 


Merge <- Merge %>% mutate(
  x_iuu = Tot_output_dollars - US_tot_export_USD ,
  share_gamma = x_iuu /Tot_output_dollars)



################################################################################

# 3) create variables of interest

# x_iuu = output sold domestically = tot output - tot export 











