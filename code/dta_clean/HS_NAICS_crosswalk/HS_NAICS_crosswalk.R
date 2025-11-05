


library(stringr)
library(haven)
library(concordance)
library(readr)
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


# import data 



library(haven)
dta_schott <- read_csv("/data/sikeme/TRADE/NTM_trade_war/data/crosswalk/schott/naics_HS_schott_2012.csv")
dta_D <- read_csv("/data/sikeme/TRADE/NTM_trade_war/data/crosswalk/HS6_NAICS_Diane/NAICS_HS_2012.csv")





######################################################################################
# join both Diane and Schott dta
dta <- full_join(dta_D,dta_schott)



# drop observation where HS6 is NA
dta <- dta %>%   filter(!is.na(HS6))

# Get it at HS6 level;
dta <- dta %>%  select(-HS10)

length(unique(dta$HS6))

# drop duplicates 
dta1 <- distinct(dta)
length(unique(dta1$HS6))


######################################################################################

test <- dta1 %>%  group_by(HS6) %>%    filter(n() > 1) %>%  ungroup()

# get at HS6-naics 6 level

write_csv(dta1, "/data/sikeme/TRADE/NTM_trade_war/data/crosswalk/clean_HS6_naics6_2012.csv")


######################################################################################

# at naics 4  
names(dta1)

# drop naics 6 levels to get distinct codes again 
dta2 <- dta1 %>% select(- naics6_D, -naics )


# drop duplicates 
dta2 <- distinct(dta2)
length(unique(dta1$HS6))

dta3 <- dta2 %>% rename(naics3_S = naics3, naics4_S = naics4) %>% 
  mutate(naics3 = if_else(is.na(naics3_D), naics3_S, naics3_D ),
         naics4 = if_else(is.na(naics4_D), naics4_S, naics4_D ))


write_csv(dta3, "/data/sikeme/TRADE/NTM_trade_war/data/crosswalk/clean_HS6_naics4_2012.csv")


######################################################################################

# at naics 3  
names(dta3)

# drop naics 4 levels to get distinct codes again 
dta4 <- dta3 %>% select(-naics4 , -naics4_D,-naics4_S, -naics3_S )


# drop duplicates 
dta4 <- distinct(dta4)
length(unique(dta4$HS6))
test <- dta4 %>%  group_by(HS6) %>%    filter(n() > 1) %>%  ungroup()


# sometimes one product code allocated to two different NAICS 3 digit code
# can be a problem when calculating the shares where it might not match anymore.... 
dta4_unique <- dta4 %>%  group_by(HS6) %>%
  slice(1) %>%        # keep the first observation per HS6
  ungroup()

write_csv(dta4_unique, "/data/sikeme/TRADE/NTM_trade_war/data/crosswalk/clean_HS6_naics3_2012.csv")








