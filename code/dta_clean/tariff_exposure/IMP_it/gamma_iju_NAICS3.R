

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

exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure"


################################################################################
# 1) load data:
################################################################################

# US tot export at HS6 level H4:
US_export <- read_csv("data/trade/US_export_schott_2012.csv")
colSums(is.na(US_export))
test<- US_export %>% filter(is.na(ISO_Code))
unique(test$Country)

# US industry output NAICS 3 digit level
output_NAICS3 <-  read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/Census_output/output_NAICS_3.csv")

# crosswalk: industry NAICS to HS product code 

HS_NAICS <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/crosswalk/clean_HS6_naics3_2012.csv")


tariff <- read_csv("data/chen_NTB_tariff/hs2_agriculture_manufacturing_clean.csv")
summary(tariff)

################################################################################

names(US_export)
names(HS_NAICS)
names(output_NAICS3)
class(US_export$HS6)
unique(US_export$year)

# drop naics codes to use our concordance 
US_export <- US_export %>% select(-naics4, -naics3, -naics6, -sic)

# convert product codes in numeric vairable
HS_NAICS$HS6 <- as.numeric(HS_NAICS$HS6)


# Merge HS naics concordance to export data:
Merge_trade <- left_join(US_export, HS_NAICS)
colSums(is.na(Merge_trade))
names(Merge_trade)
test <- Merge_trade %>% filter(is.na(ISO_Code))
unique(test$Country)
test <- Merge_trade %>% filter(is.na(Country))

# merge with change in tariff data 
class(Merge_trade$HS2)
class(tariff$HS2)
Merge_trade$HS2 <- as.numeric(Merge_trade$HS2)
Merge_trade <- left_join(Merge_trade, tariff)
colSums(is.na(Merge_trade))
summary(Merge_trade$tau_NTB)
summary(Merge_trade$tau_tariff_CHN)
test <- Merge_trade %>% filter(Country == "China")

# Merge output to export data 
Merge_trade_output <- left_join(Merge_trade, output_NAICS3, by = c("naics3" = "NAICS3_2012", "year" = "year"))


################################################################################
# 2) Aggregate at naics 3 digit level 
################################################################################

names(Merge_trade_output)
colSums(is.na(Merge_trade_output))

# b) aggregate export at NAICS level of interest

names(Merge_trade_output)
# convert output and export in USD 
# sum export at naics3 digit level
Merge <- Merge_trade_output %>% group_by(year, naics3,Country,ISO3_Code,NAICS_description ) %>%
  summarise(US_export_USD = sum(export_val_USD, na.rm = TRUE),
            US_export_USD_tau_NTB = sum(export_val_USD*tau_NTB, na.rm = TRUE),
            US_export_USD_tau_tariff_CHN = sum(export_val_USD*tau_tariff_CHN, na.rm = TRUE),
            Tot_output_USD =  first(Tot_output_1000dollars*1000))



################################################################################
# 3) Create Shares:
################################################################################


# x_iju = US export to country j
# gamma_iju = share of US industry i output sold to country j =  x_iju /x_iu

Merge <- Merge %>% mutate(
  gamma_iju = US_export_USD /Tot_output_USD,
  gamma_iju_tau_CHN_tariffs = US_export_USD_tau_tariff_CHN /Tot_output_USD,
  gamma_iju_tau_NTB = US_export_USD_tau_NTB /Tot_output_USD)

summary(Merge$gamma_iju)
test <- Merge %>% filter(is.na(gamma_iju))
test <- Merge %>% filter(Country == "China")

################################################################################
# RET_i for China 

# other countries tau might be null (but need to add data)

Merge_CHN <- Merge %>% filter(Country == "China") %>% 
  mutate(RET_i_tariff_CHN =  100*gamma_iju_tau_CHN_tariffs,
         RET_i_NTB_CHN =  100*gamma_iju_tau_NTB)

################################################################################
# 4) export
################################################################################


write_csv(Merge, paste0(exp, "/gamma_iju.csv"))


write_csv(Merge_CHN, paste0(exp, "/RET_i_Chen_CHN.csv"))


