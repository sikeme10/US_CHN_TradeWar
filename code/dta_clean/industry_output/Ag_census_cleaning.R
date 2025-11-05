

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





library(readxl)

dta <- read_excel("/data/sikeme/TRADE/NTM_trade_war/data/Census_output/ag_census.xlsx")
names(dta)

dta$NAICS <- as.numeric(gsub(".*\\(([0-9]+)\\).*", "\\1", dta$NAICS_description))


library(stringr)
dta$NAICS <- as.numeric(  str_extract(dta$NAICS_description, "\\d+(?=\\)$)"))
unique(dta$NAICS_description)
dta <- dta %>%  mutate(NAICS_label = if_else(NAICS_description == "Total", "ag_tot", as.character(NAICS)))
unique(dta$NAICS_label)


write_csv(dta, "/data/sikeme/TRADE/NTM_trade_war/data/Census_output/ag_census_clean.csv")

#################################################################################


# use NAICS 4 digiti level 

dta_NAICS4 <- dta %>% filter(nchar(NAICS) == 4)

names(dta_NAICS4)
dta_NAICS4 <- dta_NAICS4 %>% select(NAICS,NAICS_description, Total_in_1000dollars) %>%
  rename(Tot_output_1000dollars = Total_in_1000dollars, NAICS4_2012 = NAICS)%>%
  mutate(  year = 2012, 
           sector = "agriculture")  
write_csv(dta_NAICS4, "/data/sikeme/TRADE/NTM_trade_war/data/Census_output/ag_output_NAICS_4.csv")


dta_NAICS3 <- dta %>% filter(nchar(NAICS) == 3)
dta_NAICS3 <- dta_NAICS3 %>% select(NAICS,NAICS_description, Total_in_1000dollars) %>%
  rename(Tot_output_1000dollars = Total_in_1000dollars,NAICS3_2012 = NAICS) %>%
  mutate(  year = 2012, 
           sector = "agriculture")  


write_csv(dta_NAICS3, "/data/sikeme/TRADE/NTM_trade_war/data/Census_output/ag_output_NAICS_3.csv")
