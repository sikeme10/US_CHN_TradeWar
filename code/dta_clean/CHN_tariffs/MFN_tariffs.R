




################################################################################
#                      MFN Chinese tariff cleaning dta


# get WITS TRAINS Chinese tariff data 


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
library(countrycode)
rm(list=ls())

################################################################################

setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git")
################################################################################

library(readr)
dta <- read_csv("data/tariff_dta/CHN_import_tariffs/CHN_WITS_tariff.csv")


################################################################################



# two duty types

unique(dta$DutyType)













