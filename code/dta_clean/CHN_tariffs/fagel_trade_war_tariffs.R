




################################################################################
#                      Fagelbaum tariff data


# Data provide US export data and tariffs on US export (provide retaliatory tariffs)
# what we do: 
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

################################################################################
# directory: 
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git")
################################################################################

# 1) Load data 
library(haven)
US_export <- read_dta("data/tariff_dta/x_flow_hs10_fm_new.dta")

################################################################################


# we can get US export data and associated tariff data faced by US export data 
# we can get Chinese tariffs  from this data 

# checks 
unique(US_export$cty_name)
label(US_export)
# select Chinese export tariffs 
CHN_US_export <- US_export %>% filter(cty_name == "CHINA")

################################################################################

# 2) Select variable of interest: tariffs and export values 

# variables of interest: "Statutory Tariff Rate"/"WTO MFN Tariff Rate" /   "Trade War Export Tariff Increase
summary(CHN_US_export$x_increase)
summary(CHN_US_export$x_stattariff1)
summary(CHN_US_export$x_stattariff2)
summary(CHN_US_export$x_mfn_tariff)

names(CHN_US_export)
CHN_US_export1 <- CHN_US_export %>% select(cty_name,  year,month,mdate,  hs10, hs6, x_val ,x_stattariff1, x_stattariff2, x_mfn_tariff,x_increase ) %>% 
  rename(export_val_mil_USD_HS10 = x_val)

# export  data
write_csv(CHN_US_export1,"data/tariff_dta/CHN_tariff_HS10_Fagel.csv" )


CHN_US_export1  <- read_csv("data/tariff_dta/CHN_tariff_HS10_Fagel.csv" )
################################################################################

# 3) HS Product Aggregation

# tariffs are at HS10 level --> need to get at HS6 level
# could do weighted tariffs or simple average tariffs
# weighted average = trade share whithin HS6 of the HS10 export value time the tariffs 
# simple average = is just an average 

names(CHN_US_export1)
library(dplyr)

# create tot export at HS 6
CHN_US_export2 <- CHN_US_export1 %>%
  group_by(cty_name, year,month,mdate, hs6) %>%
  mutate(tot_export_val_HS6 = sum(export_val_mil_USD_HS10, na.rm = TRUE)) %>%
  ungroup()
# create export shares
CHN_US_export2 <- CHN_US_export2 %>% mutate(
  share_export_HS6 =  export_val_mil_USD_HS10 / tot_export_val_HS6)
summary(CHN_US_export2$share_export_HS6)

# create weighted average and simple average tariff values: at the month-year-hs6 level 
# aggregate at product level
# aggregate at monthly level (drop mdate)
CHN_US_export3 <- CHN_US_export2 %>%
  group_by(cty_name,  year,month,hs6, mdate) %>%
  summarise(
    weighted_x_stattariff1 = if_else(sum(share_export_HS6, na.rm = TRUE) == 0, NA_real_,
      weighted.mean(x_stattariff1, w = share_export_HS6, na.rm = TRUE)    ),
    weighted_x_stattariff2 = if_else(sum(share_export_HS6, na.rm = TRUE) == 0, NA_real_,
      weighted.mean(x_stattariff2, w = share_export_HS6, na.rm = TRUE)    ),
    weighted_x_mfn_tariff = if_else(sum(share_export_HS6, na.rm = TRUE) == 0, NA_real_,
      weighted.mean(x_mfn_tariff, w = share_export_HS6, na.rm = TRUE)  ),
    weighted_x_increase = if_else(sum(share_export_HS6, na.rm = TRUE) == 0, NA_real_,
      weighted.mean(x_increase, w = share_export_HS6, na.rm = TRUE)  ),
    simple_x_stattariff1 = mean(x_stattariff1, na.rm = TRUE),
    simple_x_stattariff2 = mean(x_stattariff2, na.rm = TRUE),
    simple_x_mfn_tariff  = mean(x_mfn_tariff,  na.rm = TRUE),
    simple_x_increase    = mean(x_increase,    na.rm = TRUE),    .groups = "drop"  )  
summary(CHN_US_export3)

CHN_US_export4 <- CHN_US_export3 %>% group_by(cty_name, year, month, hs6) %>%
  summarise(across(  c(weighted_x_stattariff1, weighted_x_stattariff2, weighted_x_mfn_tariff, weighted_x_increase,
        simple_x_stattariff1, simple_x_stattariff2, simple_x_mfn_tariff, simple_x_increase),
      ~ if (all(is.na(.x))) NA_real_ else max(.x, na.rm = TRUE)  ),   .groups = "drop"  )
summary(CHN_US_export4)


# export  data
write_csv(CHN_US_export4,"data/tariff_dta/CHN_tariff_HS6_Fagel.csv" )




