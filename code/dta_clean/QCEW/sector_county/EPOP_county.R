

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

################################################################################
# Load data 
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/")

library(readxl)
labor <- list.files("QCEW/CZ_emp_2015-2019", pattern = "\\.xlsx$", full.names = TRUE) %>%
  lapply(read_excel) %>%
  bind_rows() 
names(labor)
unique(labor$Year)


Pop <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/Pop/county_population_clean_2015_2020.csv")
names(Pop)

################################################################################



# get all industries 
unique(labor$Industry)
table(labor$Industry, labor$Year)
table(labor$NAICS, labor$Year)
labor <- labor %>% filter(NAICS == 10)
table(labor$Year)
unique(labor$Industry)
names(labor)


# rename some variables
labor <- labor %>% rename(fips = `Area\r\nCode`, year = Year, wage_total = `Annual Total Wages`,
                          estab = `Annual Average Establishment Count`, employment = `Annual Average Employment`)
labor <- labor %>% select(fips, year,Area , Ownership, Own,Industry, wage_total , estab, employment )
unique(labor$year)

table(labor$Ownership,labor$Own)
# labor <- labor %>% filter(Own == 5)
labor <- labor %>% filter(Own == 0)
################################################################################
# aggregate at CZ
################################################################################

class(labor$fips)
unique(nchar(labor$fips))
class(Pop$fips)
unique(nchar(Pop$fips))


length(unique(labor$fips))
length(unique(Pop$fips))

in_common <- intersect(unique(labor$fips), unique(Pop$fips))
in_diff <- setdiff(unique(labor$fips), unique(Pop$fips))


# have to recode some counties to 2012 counties
names(labor)
labor <- labor |>
  mutate(
    fips = case_when(
      fips == "02158" & year >= 2015 ~ "02270",
      fips == "46102" & year >= 2015 ~ "46113",
      TRUE ~ fips    )  )


length(unique(labor$fips))


setdiff(unique(labor$fips), unique(Pop$fips))
length(setdiff(unique(labor$fips), unique(Pop$fips)))

# filter fips  that are in the CZ to county crosswalk
labor1 <- labor %>% filter(fips %in% unique(Pop$fips))
length(unique(labor$fips))
length(unique(labor1$fips))
colSums(is.na(labor1))
table(labor1$year)



# has duplictaes in 2015
# If true duplicates
labor1 <- labor1 %>% distinct()
# Find duplicated fips in 2015
labor1 %>%  filter(year == 2015) %>%  group_by(fips) %>%
  filter(n() > 1) %>%  arrange(fips)

# in 2015, keep Wade-Hampton Census Area in Alask
labor1 <- labor1 %>%
  filter(!(Area %in% c("Wade Hampton Census Area, Alaska", 
                       "Oglala Lakota County, South Dakota") & year == 2015))

# Verify
table(labor1$year)

summary(labor1)
test <- labor1 %>% filter(employment==0)

################################################################################
# merge with pop data 

names(Pop)
unique(Pop$year)
Pop <- Pop %>% filter(year < 2020)
length(unique(Pop$fips))
length(unique(labor1$fips))

class(Pop$fips)
class(labor1$fips)

labor1$year <- as.numeric(labor1$year)
labor3 <- left_join(labor1, Pop)


# create EPOP

summary(labor3)
names(labor3)
labor3 <-labor3 %>% mutate(EPOP = employment / total_pop,
                           EPOP_work = employment / working_age_pop,
                           wage_per_worker = if_else(employment != 0 , wage_total/employment, 0))
summary(labor3)

test <- labor3 %>% filter(EPOP >1)

labor3 <-labor3 %>% mutate(EPOP =if_else(EPOP>1, NA, EPOP))

write_csv(labor3, "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/EPOP_county_2015_2019.csv")





