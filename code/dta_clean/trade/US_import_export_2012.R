




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


library(haven)
# m_flow_hs10_fm_new <- read_dta("tariff_dta/m_flow_hs10_fm_new.dta")
# names(m_flow_hs10_fm_new)

dta <- read_csv("trade/US_trade_2012.csv")
names(dta)
unique(dta$TradeFlowName)

#################################################################################

# for exports 

US_export <- dta %>% filter(TradeFlowName == "Gross Exp.")
length(unique(US_export$PartnerISO3))

US_tot_export <- US_export %>% filter(PartnerISO3 == "WLD") %>% rename( US_tot_export_1000USD = `TradeValue in 1000 USD`) %>%
  select(Nomenclature, ProductCode, Year, US_tot_export_1000USD)
length(unique(US_tot_export$ProductCode))
write_csv(US_tot_export, "trade/US_tot_export_2012.csv")

#################################################################################
# for imports 

US_import <- dta %>% filter(TradeFlowName == "Gross Imp.")
length(unique(US_import$PartnerISO3))


US_tot_import <- US_import %>% filter(PartnerISO3 == "WLD") %>% rename( US_tot_import_1000USD = `TradeValue in 1000 USD`) %>%
  select(Nomenclature, ProductCode, Year, US_tot_import_1000USD)
length(unique(US_tot_import$ProductCode))

write_csv(US_tot_import, "trade/US_tot_import_2012.csv")













