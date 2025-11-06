


################################################################################
#                      Cepii bilateral gravity charteristics


# get worldbank data for gravity models


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

# download data from:
# http://www.cepii.fr/DATA_DOWNLOAD/gravity/doc/Gravity_documentation.pdf
# s country_id_o when referring to the origin and country_id_d when referring to the destination

################################################################################

# load dta
Gravity <- read_csv("data/gravity/Gravity_V202211.csv")
names(Gravity)

################################################################################
# checks
table(Gravity$year)
table(Gravity$rta_type)

# keep year from 2014
Gravity <- subset(Gravity, year>2014)


################################################################################

# select variable of interest: 

#  rta_coverage: Coverage of the trade agreement. 0 = “no trade agreement”. 1 = “goods
# only”, 2 = “services only”, 3 = “goods and services”, bilateral.

table(Gravity$rta_coverage)
Gravity <- Gravity %>% mutate(rta = ifelse(rta_coverage > 0, 1, 0))
table(Gravity$rta)



# variables of interest are : contig (Dummy equal to 1 if countries are contiguous)
# dist, comlang_off (common language) and "GDPs" and rtas


clean_Gravity <- Gravity[c("year","country_id_o","country_id_d","iso3_o","iso3_d"
                           ,"contig", "dist", "comlang_off", "col_dep_ever", "gdp_d",
                           "gdp_o", "wto_d", "wto_o","eu_o", "eu_d" )]
clean_Gravity <- clean_Gravity %>% rename (Colonial_ties = col_dep_ever)


################################################################################
# select countries of interest:

#the name ends with _o when the information refers to the origin country, and with _d
#when it refers to the destination country
# We want china an all partners 

names(clean_Gravity)
unique(clean_Gravity $iso3_o)
unique(clean_Gravity $country_id_o)

clean_Gravity <- clean_Gravity %>% filter(iso3_d == "CHN")

# rename name of importer and exporter 
clean_Gravity <- clean_Gravity %>% rename(PartnerISO3  = iso3_o , ReporterISO3 = iso3_d )

################################################################################

write_csv(clean_Gravity, "data/gravity/clean_Gravity.csv")













