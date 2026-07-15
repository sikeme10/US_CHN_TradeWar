

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
# LOad data 
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/")

library(readxl)
labor <- list.files("QCEW/CZ_emp_2015-2019", pattern = "\\.xlsx$", full.names = TRUE) %>%
  lapply(read_excel) %>%
  bind_rows() 
names(labor)
unique(labor$Year)


Pop <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/Pop/cz_population_2015_2020.csv")
names(Pop)

cw_cty_czone_2012 <- read_csv("crosswalk_CZ_county/cw_cty_czone_2012.csv")
names(cw_cty_czone_2012)
cw_cty_czone_2012 <- cw_cty_czone_2012 %>% select(cty_fips_2012, czone_2012)
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

class(cw_cty_czone_2012$cty_fips_2012)
unique(nchar(cw_cty_czone_2012$cty_fips_2012))
# make it a character var
cw_cty_czone_2012 <- cw_cty_czone_2012 %>%  mutate(cty_fips_2012 = str_pad(as.character(cty_fips_2012), width = 5, pad = "0"))


length(unique(labor$fips))
length(unique(cw_cty_czone_2012$cty_fips_2012))

# have to recode some counties to 2012 counties 
names(labor)
labor <- labor |>
  mutate(fips = case_when(
    fips == "02158" & year >= 2015 ~ "02270",  # Kusilvak -> Wade Hampton
    fips == "46102" & year >= 2015 ~ "46113",  # Oglala Lakota -> Shannon
    TRUE ~ fips
  ))
length(unique(labor$fips))


setdiff(unique(labor$fips), unique(cw_cty_czone_2012$cty_fips_2012))
length(setdiff(unique(labor$fips), unique(cw_cty_czone_2012$cty_fips_2012)))

# filter fips  that are in the CZ to county crosswalk
labor1 <- labor %>% filter(fips %in% unique(cw_cty_czone_2012$cty_fips_2012))
length(unique(labor$fips))
length(unique(labor1$fips))

# merge the two 
labor2 <- left_join(labor1, cw_cty_czone_2012, by = c("fips" = "cty_fips_2012"))
length(unique(labor2$czone_2012))
names(labor2)
labor2 <- labor2 %>% group_by(year,czone_2012)  %>% 
  summarise(estab = sum(estab, na.rm= TRUE),
            employment = sum(employment, na.rm= TRUE),
            wage_total = sum(wage_total, na.rm= TRUE))
table(labor2$year)
summary(labor2)
test <- labor2 %>% filter(employment==0)

################################################################################
# merge with pop data 


unique(Pop$year)
Pop <- Pop %>% filter(year < 2020)
length(unique(Pop$czone_2012))
length(unique(labor2$czone_2012))


labor2$year <- as.numeric(labor2$year)
labor3 <- left_join(labor2, Pop)


# create EPOP

summary(labor3)
names(labor3)
labor3 <-labor3 %>% mutate(EPOP = employment / total_pop,
                           EPOP_work = employment / working_age_pop,
                           wage_per_worker = if_else(employment != 0 , wage_total/employment, 0))
summary(labor3)

test <- labor3 %>% filter(EPOP >1)

labor3 <-labor3 %>% mutate(EPOP =if_else(EPOP>1, NA, EPOP))

write_csv(labor3, "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/EPOP_2015_2019.csv")





