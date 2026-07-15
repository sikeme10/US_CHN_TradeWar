
################################################################################
# 
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
library(readr)
library(dplyr)
library(sf)
library(tigris)
library(ggplot2)
library(viridis)

################################################################################
# LOad data 
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/")

# get 2012 employment to use as weights 

################################################################################
# CZONe level 
################################################################################

library(readxl)
dta <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/QCEW_2012_naics6_CZ.csv")

names(dta)

unique(dta$naics)
length(unique(dta$naics))

# CZ level weights
dta1 <- dta %>% group_by(czone_2012) %>% 
  summarise(emp_2012= sum(emp, na.rm = TRUE))

write_csv(dta1,"/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/QCEW_2012_weights_CZ.csv" )

################################################################################
# county level weights 
################################################################################

################################################################################

library(readxl)
dta <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/QCEW_2012_naics6_county.csv")

names(dta)

unique(dta$naics)
length(unique(dta$naics))

# CZ level weights
dta1 <- dta %>% group_by(fips) %>% 
  summarise(emp_2012= sum(emp, na.rm = TRUE),
            wages_total_2012= sum(wages_total, na.rm = TRUE),)

write_csv(dta1,"/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/QCEW_2012_weights_county.csv" )
