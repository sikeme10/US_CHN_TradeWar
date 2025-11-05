


library(readr)
library(tidyr)
library(dplyr)
library(data.table)
library(stringi)
library(fixest)
library(countrycode)
library(tidyverse)
library(vroom)




rm(list=ls())
# Set directory
setwd("/data/sikeme/TRADE/NTM_trade_war/data")
getwd()

# load data
dta <- read_delim("Census_output/manufacturing_output_NAICS6.dat", delim = "|")

#####################################################################################


# variable of interest is 
names(dta)

# RCPTOT: shipment value, Value of Shipments = total gross output from U.S. factories. reported in $1,000s,
# at NAICS  digit


# rename variable of interest

dta <- dta %>% rename(NAICS6_2012 = NAICS2012, 
                      year= YEAR, 
                      Geo = GEO_TTL,
                      NAICS6_2012_descrpt = NAICS2012_TTL)


dta <- dta %>% select(year ,Geo, NAICS6_2012, NAICS6_2012_descrpt, RCPTOT )

############################################################################
# aggregate at 6 digit level

dta <-dta %>%  mutate(NAICS4_2012 = substr(NAICS6_2012, 1, 4),
                      NAICS3_2012 = substr(NAICS6_2012, 1, 3))
unique(dta$NAICS4_2012)
length(unique(dta$NAICS4_2012))

names(dta)



# aggregation y summing

dta_NAICS4 <- dta  %>% group_by(year, Geo, NAICS4_2012) %>% summarise(
  RCPTOT = sum(RCPTOT, na.rm = TRUE))
dta_NAICS4 <- dta_NAICS4 %>% rename(Tot_output_1000dollars = RCPTOT)%>% mutate(sector = "manufacturing")

dta_NAICS3 <- dta  %>% group_by(year, Geo, NAICS3_2012) %>% summarise(
  RCPTOT = sum(RCPTOT, na.rm = TRUE))
dta_NAICS3 <- dta_NAICS3 %>% rename(Tot_output_1000dollars = RCPTOT) %>% mutate(sector = "manufacturing")

############################################################################

# export 

write_csv(dta_NAICS4 , "/data/sikeme/TRADE/NTM_trade_war/data/Census_output/manufacturing_output_NAICS4.csv")

write_csv(dta_NAICS3 , "/data/sikeme/TRADE/NTM_trade_war/data/Census_output/manufacturing_output_NAICS3.csv")


