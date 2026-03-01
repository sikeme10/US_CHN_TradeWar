


################################################################################
                 # Create tau hat (US tariffs on China)

# we obtain US tariffs imposed on CHINA  as well other countries and estimate the tau value 

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
library(labelled)
library(haven)

################################################################################

rm(list=ls())
# Set directory
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git/")
getwd()

exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/tariff_dta"

################################################################################

# load data

US_CHN <- read_csv("data/tariff_dta/teti/US_tariff_on_CHN_yearly.csv")
US_ROW <- read_csv("data/tariff_dta/teti/US_tariff_on_ROW_yearly.csv")

names(US_ROW)
names(US_CHN)

################################################################################
# change some of the variables 
US_CHN <- US_CHN %>% mutate(ExporterISO3 = "CHN") %>% rename(
  US_tariff  = US_tariff_onCHN , US_tariff_2015 =  US_tariff_onCHN_2015 ,
  US_tariff_2017  = US_tariff_onCHN_2017)

US_ROW <- US_ROW %>% mutate(ExporterISO3 = "CHN") %>% rename(
  US_tariff  = US_tariff_onROW , US_tariff_2015 =  US_tariff_onROW_2015 ,
  US_tariff_2017  = US_tariff_onROW_2017)


# merge the data 
merge <- full_join(US_CHN, US_ROW)


################################################################################
# tau create 

names(merge)

# create log of tariff 
merge <- merge %>%
  mutate(across( c(US_tariff, US_tariff_2015, US_tariff_2017), ~ . / 100  ),
         across(c(US_tariff, US_tariff_2015, US_tariff_2017), ~ log(1 + .),   .names = "ln_{.col}")  )

summary(merge)

# create log change of tariff 

merge <- merge %>% mutate(change_log_tariff_2015 = ln_US_tariff - ln_US_tariff_2015,
                          change_log_tariff_2017 = ln_US_tariff - ln_US_tariff_2017)
names(merge)
merge <- merge %>% select(hs6, year, ExporterISO3 , change_log_tariff_2015, change_log_tariff_2017) %>% 
  rename(US_change_log_tariff_2015 = change_log_tariff_2015, US_change_log_tariff_2017 = change_log_tariff_2017)


# export data 
write_csv(merge, "data/created_exposure/tau/import_US_tau_teti.csv")





