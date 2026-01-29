
################################################################################
#                    Gravity regression analysis: residual approach


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
dta <-  "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/Compare_values/yearly/robust"


################################################################################
# 1) Load data 
################################################################################

# Load FE estimates 
dta <- read_csv(paste0(exp, "/US_ln_NTMs_base_2015.csv"))
unique(dta$ExporterISO3)
colSums(is.na(dta))
unique(dta$year)
names(dta)


SOE <- read_csv("data/SOE_dta/SOE_share_2010.csv")



################################################################################














