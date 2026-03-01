
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
library(countrycode)
library(Hmisc)
library(haven)
library(readxl)

################################################################################

# Set directory
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/")
getwd()

exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/"

################################################################################
 
# load data 
# allindustry2012 <- read_dta("TRADE/US_CHN_TradeWar_git/data/QCEW/allindustry/allindustry2012.dta")

# imputed data 
imputed_2012_QCEW_bis <- read_dta("allindustry/qcew2012_imputed6digitnaics.dta")
colSums(is.na(imputed_2012_QCEW_bis))
imputed_2012_QCEW <- read_dta("allindustry/qcew2012_imputednaics6.dta")

# actual QCEW data 
QCEW_2012 <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/QCEW_naics6digit_2012.csv")

################################################################################
# checks 
colSums(is.na(imputed_2012_QCEW))
names(imputed_2012_QCEW)
length(unique(imputed_2012_QCEW$naics))
class(imputed_2012_QCEW$naics)
summary(imputed_2012_QCEW$naics)
length(unique(nchar(imputed_2012_QCEW$naics)))
length(unique(imputed_2012_QCEW$fips))

names(QCEW_2012)
length(unique(QCEW_2012$industry_code))
class(QCEW_2012$industry_code)

length(unique(QCEW_2012$area_fips))

################################################################################
# compare both data 

# check varying industrycodes (missing codes from imputed data )

naics1 <- imputed_2012_QCEW %>% mutate(naics = as.character(naics)) %>% distinct(naics)
naics2 <- QCEW_2012 %>% mutate(industry_code = as.character(industry_code)) %>% distinct(industry_code)

missing_codes <- setdiff(naics2$industry_code, naics1$naics)
missing_with_desc <- QCEW_2012 %>%mutate(industry_code = as.character(industry_code)) %>%
  filter(industry_code %in% missing_codes) %>% select(industry_code, industry_title) %>%  distinct() %>%
  arrange(industry_code)

missing_with_desc


# check varying area_fips (missing codes from imputed data )
fips1 <- imputed_2012_QCEW %>% mutate(fips = str_pad(as.character(fips), width = 5, side = "left", pad = "0")) %>%
  distinct(fips)

fips2 <- QCEW_2012 %>% mutate(area_fips = str_pad(as.character(area_fips), width = 5, side = "left", pad = "0")) %>%
  distinct(area_fips)

missing_fips_with_name <- QCEW_2012 %>% mutate(area_fips = str_pad(as.character(area_fips), 5, "left", "0")) %>%
  anti_join(imputed_2012_QCEW %>%  mutate(fips = str_pad(as.character(fips), 5, "left", "0")) %>%   distinct(fips),
    by = c("area_fips" = "fips")  ) %>%
  select(area_fips, area_title) %>%
  distinct()



