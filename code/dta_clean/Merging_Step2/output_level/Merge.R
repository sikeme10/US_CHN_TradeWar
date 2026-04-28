
################################################################################
# Code
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

NAICS_industry <- read_csv("data/Census_output/output_level_analysis/NAICS_ouput_industry_maps.csv")

output <- read_csv("data/Census_output/output_level_analysis/NAICS_ouput_industry_maps.csv")

IMP_i <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6/gamma_iuu_naics6.csv")

RET_i <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6/RET_i_naics6_IV.csv")




