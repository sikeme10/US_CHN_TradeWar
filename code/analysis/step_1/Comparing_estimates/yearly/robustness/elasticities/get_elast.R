



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
library(sfaR)
library(frontier)

################################################################################
# directory: 
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git")
exp <- "data/elast/"

################################################################################

library(readr)
dta <- read_csv("data/elast/Elasticities_Soderbery2018.csv")
names(dta)

trade <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_gravity/dta_CHN_gravity_yearly.csv")
test <- trade %>% filter(ExporterISO3 == "CHN")
################################################################################
# sigma = import demand elasticity estimate
# omega = inverse export supply elasticity estimate

library(dplyr)

dta <- dta %>%
  rename(ImporterISO3 = iiso,
         ExporterISO3 = eiso,
         hs4 = hs4,
         InverseExportSupplyElasticity = omega,
         InverseExportSupplyElasticitySE = omega_se,
         elasticities = sigma,
         ImportDemandElasticitySE = sigma_se  ) %>%
    
  select(ImporterISO3, ExporterISO3, hs4, elasticities)




################################################################################


# Select china as importer
dta <- dta %>% filter(ImporterISO3 == "CHN")


# check exporter
length(unique(trade$ExporterISO3))
length(unique(dta$ExporterISO3))

# Get the unique exporters from trade$ExporterISO3
unique_exporters_trade <- unique(trade$ExporterISO3)

# Get the unique exporters from dta$ExporterISO3
unique_exporters_dta <- unique(dta$ExporterISO3)

# Check if each exporter from trade is present in dta
exporters_in_dta <- unique_exporters_trade %in% unique_exporters_dta

# Get the exporters from trade that are missing in dta
missing_exporters <- unique_exporters_trade[!exporters_in_dta]
missing_exporters



###############################
# Get the missing exporters

# Get unique values of ExporterISO3, hs4, and ImporterISO3
unique_exporters <- unique(missing_exporters)
unique_hs4 <- unique(dta$hs4)
unique_importers <- unique(dta$ImporterISO3)

# Create all possible combinations using expand.grid()
missing_exporters_expanded <- expand.grid(
  ExporterISO3 = unique_exporters,
  hs4 = unique_hs4,
  ImporterISO3 = unique_importers,
  stringsAsFactors = FALSE)

# Calculate the average elasticities at the HS4 level for missing exporters
avg_elasticities <- dta %>%  group_by(hs4) %>%  summarise(elasticities = mean(elasticities, na.rm = TRUE))

merged_data <- left_join(missing_exporters_expanded, avg_elasticities, by = "hs4")


# Merge the average elasticities back into the original dta
dta_merged <- rbind(dta, merged_data)




# Get the unique exporters from trade$ExporterISO3
unique_exporters_trade <- unique(trade$ExporterISO3)
# Get the unique exporters from dta$ExporterISO3
unique_exporters_dta <- unique(dta_merged$ExporterISO3)
# Check if each exporter from trade is present in dta
exporters_in_dta <- unique_exporters_trade %in% unique_exporters_dta
# Get the exporters from trade that are missing in dta
missing_exporters <- unique_exporters_trade[!exporters_in_dta]
missing_exporters


write_csv(dta_merged, "data/elast/clean_Elasticities_Soderbery2018.csv")

