
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
# imputed_2012_QCEW_bis <- read_dta("allindustry/qcew2012_imputed6digitnaics.dta")
# colSums(is.na(imputed_2012_QCEW_bis))
imputed_2012_QCEW <- read_dta("allindustry/qcew2012_imputednaics6.dta")

# actual QCEW data 
QCEW_2012 <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/QCEW_naics6digit_2012.csv")




################################################################################
# checks 
colSums(is.na(imputed_2012_QCEW))
names(imputed_2012_QCEW)

summary(imputed_2012_QCEW$naics)
length(unique(nchar(imputed_2012_QCEW$naics)))
length(unique(imputed_2012_QCEW$fips))

names(QCEW_2012)



names(QCEW_2012)

# select variable of interest 
names(imputed_2012_QCEW)
imputed_2012_QCEW <- imputed_2012_QCEW %>% select(naics, fips, estabs, emp, wages_total)

################################################################################

# NAICS code 
length(unique(imputed_2012_QCEW$naics))
class(imputed_2012_QCEW$naics)

length(unique(QCEW_2012$industry_code))
class(QCEW_2012$industry_code)
# get it harmonized
QCEW_2012 <- QCEW_2012 %>% filter(industry_code %in% unique(imputed_2012_QCEW$naics))

################################################################################

# for area fips code 
length(unique(imputed_2012_QCEW$fips))
length(unique(QCEW_2012$area_fips))
class(QCEW_2012$area_fips)
class(imputed_2012_QCEW$fips)
QCEW_2012$area_fips <- as.numeric(QCEW_2012$area_fips)
QCEW_2012 <- QCEW_2012 %>% filter(!is.na(area_fips))


# Find what's in QCEW_2012 but NOT in imputed_2012_QCEW
missing_from_imputed <- setdiff(unique(QCEW_2012$area_fips), unique(imputed_2012_QCEW$fips))
missing_from_imputed
length(missing_from_imputed)

# Find what's in imputed_2012_QCEW but NOT in QCEW_2012
missing_from_QCEW <- setdiff(unique(imputed_2012_QCEW$fips), unique(QCEW_2012$area_fips))
missing_from_QCEW
length(missing_from_QCEW)

# drop missing 
QCEW_2012 <- QCEW_2012 %>% filter(area_fips %in% unique(imputed_2012_QCEW$fips))


                                  
################################################################################
# compare both data 


merged_QCEW <- left_join(QCEW_2012, imputed_2012_QCEW, by = c("area_fips"     = "fips",
                                "industry_code" = "naics"))

names(merged_QCEW)
table(merged_QCEW$disclosure_code)

# recode undiscclosed busness to N 

# Recode undisclosed businesses to "N"
merged_QCEW <- merged_QCEW %>% mutate(disclosure_code = case_when(
    is.na(disclosure_code) & annual_avg_estabs_count == 0 & annual_avg_emplvl == 0 & total_annual_wages == 0 ~ "N",
    TRUE ~ disclosure_code))



# for merged_QCEW where disclosure_code is na then check if round(annual_avg_estabs_count)  round(estabs)
merged_QCEW <- merged_QCEW %>%
  mutate(estabs_match = ifelse(is.na(disclosure_code), 
                               round(annual_avg_estabs_count) == round(estabs), 
                               NA))
table(merged_QCEW$estabs_match)
test <- merged_QCEW %>% filter(estabs_match == FALSE)


###############################################################################
# At commuting zones 


czone <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/crosswalk_CZ_county/cw_cty_czone_2012.csv")
colSums(is.na(czone))
czone <- czone %>% select(cty_fips_2012, czone_2012)%>% filter(!is.na(czone_2012))

length(unique(czone$cty_fips_2012))
class(czone$cty_fips_2012)

names(imputed_2012_QCEW)
length(unique(imputed_2012_QCEW$fips))
class(imputed_2012_QCEW$fips)




# take only counties 
imputed_2012_QCEW_2 <- imputed_2012_QCEW  %>% filter(fips %in% unique(czone$cty_fips_2012))


# merge with czone data 

imputed_2012_QCEW_2 <- left_join(imputed_2012_QCEW_2,czone , by= c("fips" = "cty_fips_2012") )
colSums(is.na(imputed_2012_QCEW_2))


# aggregate at CZone level 
imputed_2012_QCEW_CZ <- imputed_2012_QCEW_2 %>%  group_by(naics, czone_2012) %>% 
  summarise(estabs = sum(estabs, na.rm = TRUE),
            emp = sum(emp, na.rm = TRUE),
            wages_total = sum(wages_total, na.rm = TRUE))
write_csv(imputed_2012_QCEW_CZ, "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/QCEW_2012_naics6_CZ.csv")

# ###############################################################################
# names(imputed_2012_QCEW_CZ)
# 
# 
# # get total labor L
# tot_labor <- imputed_2012_QCEW_CZ %>% group_by(czone_2012) %>% summarise(
#   estabs = sum(estabs, na.rm = TRUE),
#   emp = sum(emp, na.rm = TRUE),
#   wages_total = sum(wages_total, na.rm = TRUE))
# 
# imputed_2012_QCEW_CZ_2 <- left_join(imputed_2012_QCEW_CZ,tot_labor )
# 
# 
# # get total labor by industry NAICS 3 digit code 
# unique(nchar(imputed_2012_QCEW_CZ_2$naics))
# class(imputed_2012_QCEW_CZ_2$naics)
# imputed_2012_QCEW_CZ_2 <- imputed_2012_QCEW_CZ_2 %>%  mutate(naics3 = as.numeric(substr(as.character(naics), 1, 3)))
# unique(imputed_2012_QCEW_CZ_2$naics3)
# 
# imputed_2012_QCEW_CZ_2 <- imputed_2012_QCEW_CZ_2 %>%
#   mutate( sector = case_when( naics3 %in% 111:115 ~ "Ag", naics3 %in% 311:339 ~ "Manu",
#                               TRUE ~ "NonAg"       )  )
# # get total employment at sector level
# tot_labor_NAICS3 <- imputed_2012_QCEW_CZ_2 %>%
#   group_by(naics, czone_2012) %>%
#   summarise(
#     # Employment
#     empl_Ag        = sum(emp[sector == "Ag"],    na.rm = TRUE),
#     empl_Manu      = sum(emp[sector == "Manu"],  na.rm = TRUE),
#     empl_NonAg     = sum(emp[sector == "NonAg"], na.rm = TRUE),
#     # Establishments
#     estabs_Ag      = sum(estabs[sector == "Ag"],    na.rm = TRUE),
#     estabs_Manu    = sum(estabs[sector == "Manu"],  na.rm = TRUE),
#     estabs_NonAg   = sum(estabs[sector == "NonAg"], na.rm = TRUE),
#     # Wages
#     wages_Ag       = sum(wages_total[sector == "Ag"],    na.rm = TRUE),
#     wages_Manu     = sum(wages_total[sector == "Manu"],  na.rm = TRUE),
#     wages_NonAg    = sum(wages_total[sector == "NonAg"], na.rm = TRUE)  )
# 
# 
# 
# imputed_2012_QCEW_CZ_2 <- left_join(imputed_2012_QCEW_CZ_2,tot_labor_NAICS3 )
# 
# 
# 
# merged_data2 <- merged_data2 %>% mutate(
#   share_labor_ir = annual_avg_emplvl / tot_annual_avg_emplvl)
# summary(merged_data2$share_labor_ir)


