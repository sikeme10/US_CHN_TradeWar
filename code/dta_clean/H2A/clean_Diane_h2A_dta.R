
################################################################################
# 
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
library(dplyr)

################################################################################
Labor <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/EPOP_county_2015_2019.csv")
# Labor <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/EPOP_county_2015_2019.csv")
length(unique(Labor$fips))
class(Labor$fips)
Labor$fips <- as.numeric(Labor$fips)
All_fips <- unique(Labor$fips)
################################################################################


library(haven)
dta <- read_dta("code/dta_clean/H2A/h2a_annual_2006_2019.dta")
names(dta)
colSums(is.na(dta))
class(dta$fips)

# filter year of interest 
table(dta$year)
dta <- dta %>% filter(year %in% c(2015: 2020))
test <- dta %>% filter(is.na(fips))


# drop fips with NAs
dta <- dta %>% filter(!is.na(fips))
length(unique(dta$fips))
table(dta$year)
class(dta$fips)

################################################################################
# check if all fips are in labor fips# 
inter <- intersect(dta$fips, Labor$fips)
setdiff <- setdiff(dta$fips, Labor$fips)
setdiff

# get rid of absent fips code:
dta <- dta %>% filter(fips %in% inter)

################################################################################

# create a balanced data 
years <- unique(dta$year)

All_fips

library(tidyr)
library(dplyr)

# all_fips should be the unique set of fips codes you want in every year
years <- unique(dta$year)

# create the full fips x year skeleton
balanced <- expand_grid(fips = All_fips, year = years)

# merge back — left_join keeps every fips-year combo, filling NA where dta has no row
dta_balanced <- balanced %>%  left_join(dta, by = c("fips", "year"))



cols <- c("nbr_workers_certified", "nbr_workers_requested")

dta_balanced[cols] <- lapply(dta_balanced[cols], function(x) ifelse(is.na(x), 0, x))
table(dta_balanced$year)


################################################################################
write_csv(dta_balanced, "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/h2a/clean_diane_h2a_dta.csv")

