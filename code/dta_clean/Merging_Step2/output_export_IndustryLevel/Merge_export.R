
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


rm(list=ls()); gc()
# Set directory
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git/")
getwd()


################################################################################

NAICS_industry <- read_csv("data/Census_output/output_level_analysis/NAICS_ouput_industry_maps.csv")

export <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/trade/exp_ship_ratio_2015_2020.csv")

IMP_i <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6/gamma_iuu_output.csv")

RET_i <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6/RET_i_output_elast_IV.csv")


census_div <- read_csv( "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/crosswalk_CZ_county/census_div_czone_2012.csv")
sector_shares <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/subsector_share_2012.csv")
################################################################################


names(export)
names(IMP_i)
names(RET_i)

IMP_i <- IMP_i %>% select("year", "sector", "industry", "industry_code", IMP_it_2017, subsector)
RET_i <- RET_i %>% select("year", "industry", "industry_code", "RET_i_tariff", "RET_i_NTB", "RET_i_NTB_IV")


export1 <-  left_join(export, IMP_i)
export1 <-  left_join(export1, RET_i)



names(export1)
export1 <- export1 %>%
  group_by(industry) %>%
  fill(sector, subsector, industry_code, .direction = "updown") %>%
  ungroup()

colSums(is.na(export1))
test <- export1 %>% filter(is.na(subsector))


#  drop industries where ALL years are NA for these variables
export1 <- export1 %>%  group_by(industry) %>%
  filter(!all(is.na(IMP_it_2017) & is.na(RET_i_tariff) & is.na(RET_i_NTB) & is.na(RET_i_NTB_IV))) %>%
  ungroup()




# replace NAs with 0 for years before 2018
export2 <- export1 %>%
  mutate(across(c(IMP_it_2017, RET_i_tariff, RET_i_NTB, RET_i_NTB_IV),
                ~ ifelse(year < 2018, 0, .)))

unique(export2$year)

export2 <- export2 %>% filter(year %in% c(2016:2020))

###########################################################################

write_csv(export2, "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/output_reg/merge_export.csv")


# test <- read_csv("TRADE/US_CHN_TradeWar_git/data/output_reg/merge_output.csv")



