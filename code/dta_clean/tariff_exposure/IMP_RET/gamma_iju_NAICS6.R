

################################################################################

# create variable gamm_iju interacted with tariff here 

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
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git")
getwd()

exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6"


################################################################################
# 1) load data:
################################################################################

# US tot export at HS6 level H4:
US_export <- read_csv("data/trade/US_export_schott_2012.csv")
colSums(is.na(US_export))
test<- US_export %>% filter(is.na(ISO_Code))
unique(test$Country)

# US industry output NAICS 3 digit level
output_NAICS6 <-  read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/Census_output/output_NAICS_6.csv")

# crosswalk: industry NAICS to HS product code 
HS_NAICS <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/crosswalk/clean_HS6_naics6_2012.csv")

# tariff/NTMs
# tariff <- read_csv("data/chen_NTB_tariff/hs2_agriculture_manufacturing_clean.csv")
tariff <- read_csv("data/created_exposure/tau/import_CHN_tau_0.01.csv")


HS_product <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/HS_codes/HS_2012_2017_merged.csv")

################################################################################
# check variable names 

names(tariff)
names(US_export)
names(HS_NAICS)
names(output_NAICS6)
class(US_export$HS6)
unique(US_export$year)

# change names for tariff
names(tariff)
US_export <- US_export %>% rename(ImporterISO3 = ISO3_Code, Importer = Country) %>% mutate(ExporterISO3 = "USA") %>% 
  select(-cty_code, - ISO_Code) 

# select 
HS_NAICS <- HS_NAICS %>% select(-c(naics2_D, naics3_D, naics4_D, naics6_D))


# year variable 
unique(tariff$year)
unique(HS_NAICS$year)
unique(US_export$year)
unique(output_NAICS6$year)
unique(tariff$year)

#drop year 2012
US_export <- US_export %>% select(-year)
output_NAICS6 <- output_NAICS6 %>% select(-year)


#################################################################################
# filter HS2012 product codes 

# Check classes
class(HS_NAICS$HS6)
class(tariff$hs6_H4)
class(US_export$HS6)
class(HS_product$HS_2012_Product_Code)

# check product codes 
length(unique(HS_NAICS$HS6))
length(unique(tariff$hs6_H4))
length(unique(US_export$HS6))

# Convert all to numeric in one step
HS_NAICS$HS6 <- as.numeric(HS_NAICS$HS6)
tariff$hs6_H4 <- as.numeric(tariff$hs6_H4)
US_export$HS6 <- as.numeric(US_export$HS6)
HS_product$HS_2012_Product_Code <- as.numeric(HS_product$HS_2012_Product_Code)

# select only product tht are in HS2012 revision
HS_NAICS  <- HS_NAICS  %>% filter(HS6       %in% HS_product$HS_2012_Product_Code)
tariff    <- tariff    %>% filter(hs6_H4    %in% HS_product$HS_2012_Product_Code)
US_export <- US_export %>% filter(HS6       %in% HS_product$HS_2012_Product_Code)


#############
# filter naics codes 
length(unique(output_NAICS6$NAICS6_2012))
length(unique(HS_NAICS$naics))

# NAICS in output_NAICS6 but NOT in HS_NAICS
missing_in_HSNAICS <- setdiff(output_NAICS6$NAICS6_2012, HS_NAICS$naics)

# NAICS in HS_NAICS but NOT in output_NAICS6
missing_in_output <- setdiff(HS_NAICS$naics, output_NAICS6$NAICS6_2012)

length(missing_in_HSNAICS)
length(missing_in_output)

#################################################################################
# Merge data 
#################################################################################


# a) Merge HS naics concordance to export data:
Merge_trade <- left_join(US_export, HS_NAICS)
colSums(is.na(Merge_trade))
names(Merge_trade)



# b) Merge with change in tariff data 
class(Merge_trade$HS6)
class(tariff$hs6_H4)

Merge_trade1 <- left_join(Merge_trade, tariff, by =c("HS6" =  "hs6_H4", "ImporterISO3" = "ImporterISO3",
                                                     "ExporterISO3" = "ExporterISO3" ))
colSums(is.na(Merge_trade1))
summary(Merge_trade1$diff_ln_AVE_FE_wmean)
summary(Merge_trade1$CHN_diff_log_tariff_2017)
test <- Merge_trade1 %>% filter(ImporterISO3 == "CHN")
colSums(is.na(test))


# for change in tariffs that are NA put 0
Merge_trade1 <- Merge_trade1 %>%
  mutate(across(c(diff_ln_AVE_FE,diff_ln_AVE_FE_bench,diff_ln_AVE_FE_wmean, CHN_diff_log_tariff_2017),
                ~ coalesce(., 0)  ))

# c) Merge output to export data 
names(output_NAICS6)
class(Merge_trade1$naics)
class(output_NAICS6$NAICS6_2012)
unique(nchar(output_NAICS6$NAICS6_2012))
unique(nchar(Merge_trade1$naics))
output_NAICS6$NAICS6_2012 <- as.numeric(output_NAICS6$NAICS6_2012)
Merge_trade1$naics <- as.numeric(Merge_trade1$naics)
test <- Merge_trade1 %>% filter(is.na(naics))
Merge_trade1 <- Merge_trade1 %>% filter(!is.na(naics))
# filter naics that are in output 
Merge_trade1 <- Merge_trade1 %>% filter(naics %in% unique(output_NAICS6$NAICS6_2012))


Merge_trade_output <- left_join(Merge_trade1, output_NAICS6, by = c("naics" = "NAICS6_2012"))
colSums(is.na(Merge_trade_output))


################################################################################
# 2) Aggregate at naics 6 digit level 
################################################################################

names(Merge_trade_output)
colSums(is.na(Merge_trade_output))

# b) aggregate export at NAICS level of interest

names(Merge_trade_output)
# convert output and export in USD 
# sum export at naics digit level
Merge <- Merge_trade_output %>% group_by(naics,ImporterISO3, NAICS_description ) %>%
  summarise(US_export_USD = sum(export_val_USD, na.rm = TRUE),
            US_export_USD_tau_NTB = sum(export_val_USD*diff_ln_AVE_FE_wmean, na.rm = TRUE),
            US_export_USD_tau_tariff_CHN = sum(export_val_USD*CHN_diff_log_tariff_2017, na.rm = TRUE),
            Tot_output_USD =  first(Tot_output_1000dollars*1000),
            diff_ln_AVE_FE_wmean =mean(diff_ln_AVE_FE_wmean, na.rm =TRUE),
            CHN_diff_log_tariff_2017 =mean(CHN_diff_log_tariff_2017, na.rm =TRUE),)
length(unique(Merge$naics))


################################################################################
# 3) Create Shares:
################################################################################


# x_iju = US export to country j
# gamma_iju = share of US industry i output sold to country j =  x_iju /x_iu

Merge <- Merge %>% mutate(
  gamma_iju = if_else( Tot_output_USD != 0 , US_export_USD /Tot_output_USD,0),
  gamma_iju_tau_CHN_tariffs = if_else( Tot_output_USD != 0 , US_export_USD_tau_tariff_CHN /Tot_output_USD,0),
  gamma_iju_tau_NTB =  if_else( Tot_output_USD != 0 , US_export_USD_tau_NTB /Tot_output_USD,0))

summary(Merge)
summary(Merge$gamma_iju)
test <- Merge %>% filter(is.na(gamma_iju))
test <- Merge %>% filter(gamma_iju >1)
test <- Merge %>% filter(Country == "China")

# dro if output smaller than export 
Merge1 <- Merge %>% filter(gamma_iju <1)
summary(Merge1)

################################################################################
# RET_i for China 

# other countries tau might be null (but need to add data)

Merge_CHN <- Merge1 %>% filter(ImporterISO3 == "CHN") %>% 
  mutate(RET_i_tariff_CHN =  100*gamma_iju_tau_CHN_tariffs,
         RET_i_NTB_CHN =  100*gamma_iju_tau_NTB)
summary(Merge_CHN)

test <- Merge_CHN %>% filter(RET_i_NTB_CHN > 200)


################################################################################
# 4) export
################################################################################


write_csv(Merge1, paste0(exp, "/gamma_iju_naics6.csv"))


write_csv(Merge_CHN, paste0(exp, "/RET_i_CHN_naics6.csv"))


