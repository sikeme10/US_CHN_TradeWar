


################################################################################
#                     Stochastic frontier regression anlaysis

# get summary stats of coefficients from regression
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

exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/stochastic/"

################################################################################

# Load data 

coef <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/output/stochastic/sfaR_coef_HS2_average_new.csv")
names(coef)

################################################################################

unique(coef$term)

tariffs <- coef %>% filter( term == "log_tariff")

summary(tariffs)

################################################################################
