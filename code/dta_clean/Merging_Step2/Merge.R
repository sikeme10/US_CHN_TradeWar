
################################################################################
# MFP
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


SUB <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/MFP/SUB.csv")

IMP <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6/IMP_r_CHN_naics6.csv")
names(IMP)

RET <-  read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6/RET_r_CHN_naics6.csv")

EPOP <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/EPOP_2015_2019.csv")
names(EPOP)
################################################################################


unique(SUB$year)
unique(IMP$year)
unique(RET$year)
unique(EPOP$year)

# filter year of interest 
IMP <-IMP %>% filter(year %in% c(2018:2019))
IMP <-IMP %>% select(-IMP_tariff_2015_r) %>% rename(IMP_tariff_r = IMP_tariff_2017_r)

################################################################################
# create an EPOP for 2017, and 2015

EPOP_2017 <- EPOP %>% filter(year == 2017) %>% select(czone_2012,EPOP ) %>%
  rename(EPOP_2017 =EPOP )
EPOP_2015 <- EPOP %>% filter(year == 2015) %>% select(czone_2012,EPOP ) %>%
  rename(EPOP_2015 =EPOP )


EPOP <- left_join(EPOP,EPOP_2017 )
EPOP <- left_join(EPOP,EPOP_2015 )

# create difference in EPOP compared to a baseline  
EPOP1 <- EPOP %>% mutate(
  change_EPOP_2017  = if_else(year > 2017,EPOP -EPOP_2017, NA) ,
  change_EPOP_2015  = if_else(year > 2015,EPOP -EPOP_2015, NA) )

################################################################################
names(SUB)
SUB <- SUB %>% select(czone_2012,year,SUB)

sapply(list(SUB = SUB, IMP = IMP, RET = RET, EPOP = EPOP), 
       function(df) class(df$czone_2012))
sapply(list(SUB = SUB, IMP = IMP, RET = RET, EPOP = EPOP), 
        function(df) class(df$year))



merge <- EPOP1 %>%
  left_join(IMP,  by = c("czone_2012", "year")) %>%
  left_join(RET,  by = c("czone_2012", "year")) %>%
  left_join(SUB, by = c("czone_2012", "year"))
colSums(is.na(merge))

# put 0 instead of NA for some variables 
merge <- merge %>%
  mutate(across(c(IMP_tariff_r, RET_tariff_r, RET_NTB_r, SUB), 
                ~ replace_na(., 0)))





