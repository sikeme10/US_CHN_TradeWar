

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
library(Hmisc)
library(haven)
library(readxl)
library(readr)


################################################################################

rm(list=ls()); gc()

# Load data 
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git/")
exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/"

dta <- read_excel("data/crosswalk/NAICS_2017_2012/2017_to_2012_NAICS.xlsx")

naics_2012 <- read_csv("data/crosswalk/2012_naics_description.csv")


################################################################################

names(dta)
names(naics_2012)

dta <- dta %>% rename(naics_2017 = `2017 NAICS Code`, naics_2017_description =  `2017 NAICS Title`,
                      naics_2012 = `2012 NAICS Code`, 
                      naics_description = `2012 NAICS Title\r\n(and specific piece of the 2012 industry that is contained in the 2017 industry)`)


# checks

length(unique(dta$naics_2017))
length(unique(dta$naics))
length(unique(naics_2012$naics))


################################################################################

write_csv(dta, "data/crosswalk/NAICS_2017_2012/clean_2017_to_2012_NAICS.xlsx")




