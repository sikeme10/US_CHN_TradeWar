################################################################################
# Setup
################################################################################

# Load required libraries
library(readr)
library(tidyr) 
library(dplyr)
library(data.table)
library(stringi)
library(fixest)
library(countrycode) 
library(tidyverse)
library(vroom)

# Clear environment
rm(list=ls())

# Set working directory
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git")

# Define output directory for exposure data at NAICS6 level
exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6"

################################################################################
# 1. Load data
################################################################################

# Load US total exports at HS6 level 
US_export <- read_csv("data/trade/US_export_schott_2012.csv")

# Load US industry output at NAICS 6-digit level
output_NAICS6 <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/Census_output/output_NAICS_6.csv")

# Load crosswalk between NAICS industries and HS product codes
HS_NAICS <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/crosswalk/clean_HS6_naics6_2012.csv")

# Load tariff data
# tariff <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/tau/import_tau_year_0.05_IV.csv")
tariff <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/tau/import_tau_year_elast_0.05_IV.csv")


# Load HS product codes
HS_product <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/HS_codes/HS_2012_2017_merged.csv")


################################################################################
length(unique(HS_NAICS$HS6))
length(unique(HS_product$HS_2012_Product_Code))
lapply(list(US_export      = US_export, output_NAICS6  = output_NAICS6,
            HS_NAICS       = HS_NAICS,  tariff         = tariff,  HS_product     = HS_product), function(df) colSums(is.na(df)))

################################################################################
# Data cleaning and preparation
################################################################################
# Convert HS6 to character in US_export and HS_NAICS datasets
US_export$HS6 <- as.numeric(US_export$HS6)
HS_NAICS$HS6 <- as.numeric(HS_NAICS$HS6)
tariff$hs6_H4 <- as.numeric(tariff$hs6_H4)
HS_product$HS_2012_Product_Code <- as.numeric(HS_product$HS_2012_Product_Code)



# Rename and select columns in US_export data
US_export <- US_export %>% 
  rename(ImporterISO3 = ISO3_Code, Importer = Country) %>%
  mutate(ExporterISO3 = "USA") %>%
  select(-cty_code, -ISO_Code)


# Drop year 2012 from US_export and output_NAICS6
US_export <- US_export %>% select(-year) 
output_NAICS6 <- output_NAICS6 %>% select(-year)
HS_NAICS <- HS_NAICS %>% select(HS6, naics)

# Filter HS2012 product codes
HS_NAICS <- HS_NAICS %>% filter(HS6 %in% HS_product$HS_2012_Product_Code)
tariff <- tariff %>% filter(hs6_H4 %in% HS_product$HS_2012_Product_Code) 
US_export <- US_export %>% filter(HS6 %in% HS_product$HS_2012_Product_Code)


years <- unique(tariff$year)
years

################################################################################
# Check NAICS codes
################################################################################

class(HS_NAICS$naics)
class(output_NAICS6$NAICS6_2012)

HS_NAICS$naics <- as.numeric(HS_NAICS$naics)

# Count unique NAICS codes in output_NAICS6 and HS_NAICS
length(unique(output_NAICS6$NAICS6_2012))
length(unique(HS_NAICS$naics))

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
Merge_trade <- left_join(US_export, HS_NAICS)
colSums(is.na(Merge_trade))
names(Merge_trade)
length(unique(Merge_trade$naics))


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
summary(Merge_trade1$diff_ln_AVE_FE_wmean)
summary(Merge_trade1$predicted_diff_ln_AVE_FE_wmean)
summary(Merge_trade1$diff_log_tariff_2017)
test <- Merge_trade1 %>% filter(ImporterISO3 == "CHN")
colSums(is.na(test))
length(unique(Merge_trade1$naics))


# for change in tariffs that are NA put 0
Merge_trade1 <- Merge_trade1 %>%
  mutate(across(c(diff_ln_AVE_FE,diff_ln_AVE_FE_bench,diff_ln_AVE_FE_wmean, predicted_diff_ln_AVE_FE_wmean,
                  diff_log_tariff_2017,export_val_USD),
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
length(unique(Merge_trade_output$naics))
colSums(is.na(Merge_trade_output))

################################################################################
# 3. Aggregate at NAICS 6-digit level from HS6
################################################################################

names(Merge_trade_output)
colSums(is.na(Merge_trade_output))
test <- Merge_trade_output %>% filter(ImporterISO3 == "CHN" & hs2 == 2)


# Sum exports and output at NAICS 6-digit level
Merge <- Merge_trade_output %>% 
  group_by(year, naics, ImporterISO3, NAICS_description) %>%
  summarise(
    US_export_USD = sum(export_val_USD, na.rm = TRUE),
    US_export_USD_tau_NTB = sum(export_val_USD * diff_ln_AVE_FE_wmean, na.rm = TRUE),
    US_export_USD_tau_NTB_IV = sum(export_val_USD * predicted_diff_ln_AVE_FE_wmean, na.rm = TRUE),
    US_export_USD_tau_tariff = sum(export_val_USD * diff_log_tariff_2017, na.rm = TRUE),
    Tot_output_USD = first(Tot_output_1000dollars * 1000),
    diff_ln_AVE_FE_wmean = mean(diff_ln_AVE_FE_wmean, na.rm = TRUE),
    predicted_diff_ln_AVE_FE_wmean = mean(predicted_diff_ln_AVE_FE_wmean, na.rm = TRUE),
    diff_log_tariff_2017 = mean(diff_log_tariff_2017, na.rm = TRUE)  )
colSums(is.na(Merge))
summary(Merge)
test2 <- Merge %>% filter(ImporterISO3 == "CHN")

################################################################################
# 4. Create shares
################################################################################


# x_iju = US export to country j
# gamma_iju = share of US industry i output sold to country j =  x_iju /x_iu


# Calculate gamma_iju (share of US industry i output sold to country j)
Merge <- Merge %>% 
  mutate(
    gamma_iju = if_else(Tot_output_USD != 0, US_export_USD / Tot_output_USD, 0),
    gamma_iju_tau_tariffs = if_else(Tot_output_USD != 0, US_export_USD_tau_tariff / Tot_output_USD, 0),
    gamma_iju_tau_NTB = if_else(Tot_output_USD != 0, US_export_USD_tau_NTB / Tot_output_USD, 0),
    gamma_iju_tau_NTB_IV = if_else(Tot_output_USD != 0, US_export_USD_tau_NTB_IV / Tot_output_USD, 0)  )
summary(Merge)


# Drop observations where output is smaller than exports
summary(Merge$gamma_iju)
test <- Merge %>% filter(is.na(gamma_iju))
test <- Merge %>% filter(gamma_iju >1)
Merge1 <- Merge %>% filter(gamma_iju < 1)

# Calculate RET_i for China
Merge_CHN <- Merge1 %>%   filter(ImporterISO3 == "CHN") %>%
  mutate( RET_i_tariff_CHN = 100 * gamma_iju_tau_tariffs,
          RET_i_NTB_CHN = 100 * gamma_iju_tau_NTB,
          RET_i_NTB_CHN_IV = 100 * gamma_iju_tau_NTB_IV,)
summary(Merge_CHN)
test <- Merge_CHN %>% filter(RET_i_NTB_CHN > 100)
test <- Merge_CHN %>% filter(RET_i_NTB_CHN_IV > 100)


# Calculate RET_i for all countries
Merge_All <- Merge1 %>%  group_by(year, naics, NAICS_description) %>%
  summarise(
    RET_i_tariff  = 100 * sum(gamma_iju_tau_tariffs, na.rm = TRUE),
    RET_i_NTB     = 100 * sum(gamma_iju_tau_NTB, na.rm = TRUE),
    RET_i_NTB_IV  = 100 * sum(gamma_iju_tau_NTB_IV, na.rm = TRUE),
    .groups = "drop"  )
summary(Merge_All)
test <- Merge_All %>% filter(RET_i_NTB > 100)
test <- Merge_All %>% filter(RET_i_NTB_IV > 100)

################################################################################
# 5. Export results
################################################################################

write_csv(Merge1, paste0(exp, "/gamma_iju_naics6_elast_IV.csv"))
write_csv(Merge_All, paste0(exp, "/RET_i_naics6_elast_IV.csv"))


