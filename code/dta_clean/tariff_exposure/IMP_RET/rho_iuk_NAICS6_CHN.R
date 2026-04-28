

################################################################################
# we create two variables here: fraction of US industry i sold domestically

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




rm(list=ls())
# Set directory
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git/data")
getwd()

exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure"


################################################################################
# 1) load data:
################################################################################

# US tot import at HS6 level H4:
US_import <- read_csv("trade/US_import_schott_2012.csv")
colSums(is.na(US_import))
test<- US_import %>% filter(is.na(ISO_Code))
unique(test$Country)


# US industry output NAICS 3 digit level
output_NAICS6 <-  read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/Census_output/output_NAICS_6.csv")

# crosswalk: industry NAICS to HS product code 
HS_NAICS <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/crosswalk/clean_HS6_naics6_2012.csv")

# tariff/NTMs
# tariff <- read_csv("data/chen_NTB_tariff/hs2_agriculture_manufacturing_clean.csv")
tariff <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/tau/import_US_tau_teti_CHN.csv")


HS_product <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/HS_codes/HS_2012_2017_merged.csv")

# US tot export 
US_tot_export <- read_csv("trade/US_tot_export_schott_2012.csv")
US_reexport <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/trade/re_export_clean_2012.csv")

################################################################################
# Data cleaning and preparation
################################################################################

names(US_import)
names(HS_NAICS)
names(output_NAICS6)
class(US_import$HS6)
unique(US_import$year)
names(US_tot_export)


# Convert HS6 to character in US_export and HS_NAICS datasets
US_import$HS6 <- as.numeric(US_import$HS6)
HS_NAICS$HS6 <- as.numeric(HS_NAICS$HS6)
tariff$hs6_H4 <- as.numeric(tariff$hs6_H4)
US_reexport$HS6 <- as.numeric(US_reexport$HS6)
HS_product$HS_2012_Product_Code <- as.numeric(HS_product$HS_2012_Product_Code)


# Rename and select columns in US_export data
US_import <- US_import %>%   rename(ExporterISO3  = ISO3_Code, Exporter = Country) %>%
  mutate( ImporterISO3 = "USA") %>%
  select(-cty_code, -ISO_Code)
names(tariff)
tariff <- tariff %>% mutate(ImporterISO3 = "USA")


# Drop year 2012 from US_import and output_NAICS6
US_import <- US_import %>% select(-year) 
output_NAICS6 <- output_NAICS6 %>% select(-year)

# Filter HS2012 product codes
HS_NAICS <- HS_NAICS %>% filter(HS6 %in% HS_product$HS_2012_Product_Code)
tariff <- tariff %>% filter(hs6_H4 %in% HS_product$HS_2012_Product_Code) 
US_import <- US_import %>% filter(HS6 %in% HS_product$HS_2012_Product_Code)
US_tot_export <- US_tot_export %>% filter(HS6 %in% HS_product$HS_2012_Product_Code)
US_reexport <- US_reexport %>% filter(HS6 %in% HS_product$HS_2012_Product_Code)


years <- unique(tariff$year)
years

################################################################################
# Check NAICS codes
################################################################################

# Count unique NAICS codes in output_NAICS6 and HS_NAICS
length(unique(output_NAICS6$NAICS6_2012))
length(unique(HS_NAICS$naics))
class(output_NAICS6$NAICS6_2012)
class(HS_NAICS$naics)

HS_NAICS$naics <- as.numeric(HS_NAICS$naics)

# Find NAICS codes in output_NAICS6 but not in HS_NAICS
missing_in_HSNAICS <- setdiff(output_NAICS6$NAICS6_2012, HS_NAICS$naics)

# Find NAICS codes in HS_NAICS but not in output_NAICS6
missing_in_output <- setdiff(HS_NAICS$naics, output_NAICS6$NAICS6_2012)

# Count missing NAICS codes
length(missing_in_HSNAICS)
length(missing_in_output)



#################################################################################
# Merge data 
#################################################################################


# a) Merge HS naics concordance to export data:
Merge_trade <- left_join(US_import, HS_NAICS)
colSums(is.na(Merge_trade))
names(Merge_trade)
test <- Merge_trade %>% filter(is.na(naics))

# b) Merge with change in tariff data 

# add years before merging to tariffs 
years <- unique(tariff$year)
Merge_trade1 <- bind_rows(lapply(years, function(y) Merge_trade %>% mutate(year = y)))
unique(Merge_trade1$year)
colSums(is.na(Merge_trade1))

class(Merge_trade$HS6)
class(tariff$hs6_H4)
unique(tariff$year)

# merge data
Merge_trade1 <- full_join(Merge_trade1, tariff, by =c("HS6" =  "hs6_H4", "ImporterISO3" = "ImporterISO3",
                                                      "ExporterISO3" = "ExporterISO3" , "year" = "year"))


table(Merge_trade1$year)
colSums(is.na(Merge_trade1))
summary(Merge_trade1$US_change_log_tariff_2017)
test <- Merge_trade1 %>% filter(ExporterISO3 == "CHN")
colSums(is.na(test))
length(unique(Merge_trade1$naics))

# for change in tariffs that are NA put 0
Merge_trade1 <- Merge_trade1 %>%
  mutate(across(c(US_change_log_tariff_2017, US_change_log_tariff_2015,import_val_USD),
                ~ coalesce(., 0)  ))

# c) Merge output to export data 
names(output_NAICS6)
class(Merge_trade1$naics)
class(output_NAICS6$NAICS6_2012)
unique(nchar(output_NAICS6$NAICS6_2012))
unique(nchar(Merge_trade1$naics))
output_NAICS6$NAICS6_2012 <- as.numeric(output_NAICS6$NAICS6_2012)
length(unique(output_NAICS6$NAICS6_2012))
Merge_trade1$naics <- as.numeric(Merge_trade1$naics)
test <- Merge_trade1 %>% filter(is.na(naics))
Merge_trade1 <- Merge_trade1 %>% filter(!is.na(naics))
length(unique(Merge_trade1$naics))

# filter naics that are in output 
Merge_trade1 <- Merge_trade1 %>% filter(naics %in% unique(output_NAICS6$NAICS6_2012))
length(unique(Merge_trade1$naics))
test <- output_NAICS6 %>% filter(!(NAICS6_2012 %in% unique(Merge_trade1$naics) ))



Merge_trade_output <- left_join(Merge_trade1, output_NAICS6, by = c("naics" = "NAICS6_2012"))
colSums(is.na(Merge_trade_output))

################################################################################
# 3. Aggregate at NAICS 6-digit level
################################################################################

names(Merge_trade_output)
colSums(is.na(Merge_trade_output))

# Sum exports and output at NAICS 6-digit level
Merge <- Merge_trade_output %>% 
  group_by(year, naics, ImporterISO3, ExporterISO3, sector, NAICS_description) %>%
  summarise(
    US_import_USD = sum(import_val_USD, na.rm = TRUE),
    US_import_USD_tau_tariff_US_2015 = sum(import_val_USD * US_change_log_tariff_2015, na.rm = TRUE),
    US_import_USD_tau_tariff_US_2017 = sum(import_val_USD * US_change_log_tariff_2017 , na.rm = TRUE),
    Tot_output_USD = first(Tot_output_1000dollars * 1000),
    US_change_log_tariff_2017 = mean(US_change_log_tariff_2017, na.rm = TRUE)  )
colSums(is.na(Merge))



# get US total imports at naics level 
US_tot_imports <-  Merge_trade_output %>% 
  group_by(year, naics, sector, NAICS_description) %>%
  summarise(
    tot_US_import_USD = sum(import_val_USD, na.rm = TRUE))

Merge <- left_join(Merge,US_tot_imports)

# create Rho variable (sum over exporting countries: share export*tau)
names(Merge)
colSums(is.na(Merge))
Merge <- Merge %>% mutate(
  share_US_import = if_else(tot_US_import_USD != 0 , US_import_USD / tot_US_import_USD, 0),
  rho_tau_2015 =  if_else(tot_US_import_USD != 0 ,US_import_USD_tau_tariff_US_2015/ tot_US_import_USD, 0) ,
  rho_tau_2017 = if_else(tot_US_import_USD != 0 ,US_import_USD_tau_tariff_US_2017/ tot_US_import_USD, 0))
summary(Merge)

# Sum over all exporters
Merge1 <- Merge %>% group_by(year, naics, sector, NAICS_description) %>%
  summarise(rho_tau_2015  = sum(rho_tau_2015, na.rm = TRUE),
            rho_tau_2017  = sum(rho_tau_2017, na.rm = TRUE))
summary(Merge1)

###############################################################################
# get US tot export to create gamma
###############################################################################


Merge_trade <- left_join(US_import, HS_NAICS)
colSums(is.na(Merge_trade))
names(Merge_trade)

# get US rexport and tot US export 
US_tot_export <- left_join(US_tot_export, US_reexport)
identical(US_tot_export$export_val_USD, US_tot_export$domestic_export_USD)
# test <- US_tot_export %>%  filter(export_val_USD != tot_export_USD)



names(US_tot_export)
class(US_tot_export$HS6)
# aggregate at naisc level
US_tot_export1 <- left_join(US_tot_export,HS_NAICS)
colSums(is.na(US_tot_export1))
US_tot_export1 <- US_tot_export1 %>% filter(!is.na(naics)) 

# aggregate at naics level
US_tot_export2 <- US_tot_export1 %>% group_by(naics) %>% 
  summarise(tot_export_val_USD = sum(domestic_export_USD, na.rm= TRUE)) # domestic export without reexport



################################################################################
# merge back with US import and US output 
Merge1 <-left_join(Merge1, US_tot_export2)
Merge1 <-left_join(Merge1, output_NAICS6)

# create variables for x_iuu and x_iu
names(Merge1)
Merge1 <- Merge1 %>% mutate(
  x_iuu = if_else(Tot_output_1000dollars*1000 - tot_export_val_USD < 0, 0 ,Tot_output_1000dollars*1000 - tot_export_val_USD), # so that output always greater than exports
  
  gamma_iuu = if_else(Tot_output_1000dollars !=0, x_iuu/ (Tot_output_1000dollars*1000),0))
summary(Merge1)
Merge1 <- Merge1 %>% mutate(gamma_iuu = if_else(gamma_iuu<0, 0, gamma_iuu))

# Create IMP indicator 
Merge1 <- Merge1 %>% mutate(
  IMP_it_2015 = 100*gamma_iuu*rho_tau_2015,
  IMP_it_2017 = 100*gamma_iuu*rho_tau_2017)
summary(Merge1)


################################################################################
# export data 
write_csv(Merge1, paste0(exp,"/NAICS6/gamma_iuu_naics6_CHN.csv"))





