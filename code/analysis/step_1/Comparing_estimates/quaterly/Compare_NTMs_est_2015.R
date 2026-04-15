################################################################################
#                    Gravity regression analysis: residual approach
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
library(Hmisc)
library(haven)
library(sfaR)
library(frontier)

################################################################################
# Directories
################################################################################

setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git")
exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/Compare_values/quaterly/"

# Base year for all pre-period comparisons
base_year <- 2015

################################################################################
# 1) Load data
################################################################################

# Quarterly bilateral trade data (exporter-importer-product-quarter)
trade <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_gravity/dta_CHN_gravity_quaterly.csv")
names(trade)
table(trade$year)
table(trade$ExporterISO3)

# Fixed effects estimates from the PPML gravity regression
fe <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/output/FE/quaterly/gravity_pois_FE.csv")

################################################################################
# 2) Build main dataset from FE estimates
################################################################################

names(fe)
colSums(is.na(fe))

# Start from FE estimates as the base dataset
dta <- fe

# Log-linearize tariffs: log(1 + t/100) so that a 0% tariff gives 0
dta <- dta %>% mutate(log_tariff = log(1 + Applied_tariff / 100))
summary(dta$log_tariff)
summary(dta$Applied_tariff)

# Drop observations where FE is entirely missing (no usable gravity estimate)
# dta <- dta %>% filter(!if_all(c(FE), is.na))

write_csv(dta, paste0(exp, "estimates_reduced_form.csv"))

################################################################################
# 3) Reload and add product classification variables
################################################################################

dta <- read_csv(paste0(exp, "estimates_reduced_form.csv"))
dta <- dta %>% mutate(log_tariff = log(1 + Applied_tariff / 100))

summary(dta$FE)

# Verify HS6 code structure
class(dta$hs6_H5)
unique(nchar(dta$hs6_H5))

# Map HS2 codes to HS sections (21 sections per WCO classification)
# and aggregate sections into broad sectors (Agriculture, Manufacturing, Other)
dta$hs2 <- as.numeric(dta$hs2)

dta <- dta %>% mutate(
  hs_section = case_when(
    hs2 %in% 1:5   ~ 1,   # Live animals & animal products
    hs2 %in% 6:14  ~ 2,   # Vegetable products
    hs2 %in% 15    ~ 3,   # Animal/vegetable fats
    hs2 %in% 16:24 ~ 4,   # Prepared foodstuffs
    hs2 %in% 25:27 ~ 5,   # Mineral products
    hs2 %in% 28:38 ~ 6,   # Chemical products
    hs2 %in% 39:40 ~ 7,   # Plastics & rubber
    hs2 %in% 41:43 ~ 8,   # Raw hides & leather
    hs2 %in% 44:46 ~ 9,   # Wood & wood products
    hs2 %in% 47:49 ~ 10,  # Pulp & paper
    hs2 %in% 50:63 ~ 11,  # Textiles
    hs2 %in% 64:67 ~ 12,  # Footwear & headgear
    hs2 %in% 68:70 ~ 13,  # Stone, glass & ceramic
    hs2 %in% 71    ~ 14,  # Precious metals & stones
    hs2 %in% 72:83 ~ 15,  # Base metals
    hs2 %in% 84:85 ~ 16,  # Machinery & electrical equipment
    hs2 %in% 86:89 ~ 17,  # Transport equipment
    hs2 %in% 90:92 ~ 18,  # Precision instruments
    hs2 %in% 93    ~ 19,  # Arms & ammunition
    hs2 %in% 94:96 ~ 20,  # Miscellaneous manufactures
    hs2 %in% 97    ~ 21   # Works of art
  ),
  sector = case_when(
    hs_section %in% 1:4  ~ "Ag",
    hs_section %in% 5:20 ~ "Manu",
    TRUE                 ~ "Other"
  )
)

table(dta$sector)
table(dta$hs_section)
table(dta$hs2, dta$hs_section)
################################################################################
# 4) Construct trade-share weights for the weighted FE benchmark
#
#    For each (year, hs4) cell, compute total non-US trade. Each exporter's
#    share of that total serves as a weight when computing the weighted mean
#    FE across non-US exporters -- this down-weights small exporters.
################################################################################

# Collapse to yearly exporter-product totals (from quarterly data)
trade <- dta %>%  group_by(year, hs6_H5, hs2, hs4, ImporterISO3, ExporterISO3) %>%
  summarise(Trade_value_USD = sum(Trade_value_USD, na.rm = TRUE), .groups = "drop")

# Total non-US trade at hs4 level, used as denominator for shares
trade_dta_hs4 <- trade %>%  filter(ExporterISO3 != "USA") %>%
  group_by(year, hs4) %>%
  summarise(tot_hs4_trade = sum(Trade_value_USD, na.rm = TRUE), .groups = "drop")

colSums(is.na(trade_dta_hs4))

# Merge totals back and compute each exporter's share within hs4-year cell
# Share is set to 0 when the denominator is 0 to avoid division issues
dta <- left_join(dta, trade_dta_hs4)
dta <- dta %>%
  mutate(share_hs4_trade = if_else(tot_hs4_trade != 0, Trade_value_USD / tot_hs4_trade, 0))

colSums(is.na(dta))
summary(dta$share_hs4_trade)

# Diagnose any remaining NAs in share (should be zero after the if_else above)
test <- dta %>% filter(is.na(share_hs4_trade))

################################################################################
# 5) Compute FE benchmarks across exporters within each product-quarter cell
#
#    FE_bench:  maximum FE across all exporters (frontier / best-practice)
#    FE_wmean:  trade-weighted mean FE among non-US exporters (reference level)
################################################################################

dta <- dta %>%
  group_by(year, quarter, hs6_H5) %>%
  mutate(
    # Best-practice frontier: highest FE observed for this product-quarter
    FE_bench = {
      m <- max(FE, na.rm = TRUE)
      ifelse(is.infinite(m), NA_real_, m)
    },
    # Trade-weighted mean FE excluding the US exporter
    # Captures the average competitive position of non-US exporters
    FE_wmean = {
      m <- weighted.mean(
        FE[ExporterISO3 != "USA"],
        w  = share_hs4_trade[ExporterISO3 != "USA"],
        na.rm = TRUE
      )
      ifelse(is.nan(m), NA_real_, m)
    }
  ) %>%
  ungroup()

summary(dta$FE_bench)
summary(dta$FE_wmean)

test <- dta %>% filter(is.na(FE_wmean))

################################################################################
# 6) Compute base-year averages for FE and tariffs
#
#    For each exporter-product pair, store the base_year mean of each FE
#    measure and log tariff. These serve as the pre-trade-war reference point
#    for all subsequent difference calculations.
################################################################################

dta <- dta %>%  group_by(ExporterISO3, hs6_H5) %>%
  mutate(
    FE_pre          = mean(FE[year == base_year],         na.rm = TRUE),
    FE_pre_bench    = mean(FE_bench[year == base_year],   na.rm = TRUE),
    FE_pre_wmean    = mean(FE_wmean[year == base_year],   na.rm = TRUE),
    log_tariff_pre  = mean(log_tariff[year == base_year], na.rm = TRUE)
  ) %>%
  ungroup()

summary(dta$log_tariff)
test <- dta %>% filter(is.na(log_tariff_pre))

################################################################################
# 7) Add trade elasticities by sector (from the literature)
#
#    Ag:   3.00  (relatively elastic agricultural goods)
#    Manu: 1.97  (Chen et al. estimate for manufacturing)
#    Other: 5.00
################################################################################

dta <- dta %>%
  mutate(elasticities = case_when(
    sector == "Ag"   ~ 3.00,
    sector == "Manu" ~ 1.97,
    sector == "Other"~ 5.00
  ))

dta <- dta %>% arrange(year, quarter, hs6_H5)

################################################################################
# 8) Compute differences in FE relative to base year
#
#    Three variants:
#    (a) Raw FE difference
#    (b) FE relative to the frontier benchmark (removes common trends)
#    (c) FE relative to trade-weighted non-US mean (demeans for global shocks)
#
#    All differences are only defined for years strictly after base_year.
################################################################################

dta <- dta %>%
  mutate(
    # (a) Raw change in exporter FE relative to base year
    diff_FE = if_else(
      year > base_year & !is.na(FE) & !is.na(FE_pre),
      FE - FE_pre,
      NA_real_
    ),
    
    # (b) Change in FE gap relative to frontier, differenced against base year
    #     Positive values: exporter is catching up to the frontier
    diff_FE_bench = if_else(
      year > base_year & !is.na(FE) & !is.na(FE_pre),
      (FE - FE_bench) - (FE_pre - FE_pre_bench),
      NA_real_
    ),
    
    # (c) Change in FE gap relative to non-US weighted mean, differenced against base year
    #     Controls for global product-level demand shocks
    diff_FE_wmean = if_else(
      year > base_year & !is.na(FE) & !is.na(FE_pre),
      (FE - FE_wmean) - (FE_pre - FE_pre_wmean),
      NA_real_
    ),
    
    # Tariff change relative to base year (in log points)
    diff_log_tariff = if_else(
      year > base_year & !is.na(log_tariff) & !is.na(log_tariff_pre),
      log_tariff - log_tariff_pre,
      NA_real_
    )
  )

# Sanity checks
test <- dta %>% filter(year > base_year)
unique(test$year)
colSums(is.na(test))
summary(dta$diff_FE)
summary(dta$diff_FE_bench)
summary(dta$diff_log_tariff)

################################################################################
# 9) Convert FE differences to ad-valorem equivalent (AVE) NTMs
#
#    Formula: ln_AVE = 1/(1 - sigma) * FE
#    where sigma is the trade elasticity. Defined only for the post-trade-war
#    window (base_year+1 through base_year+5).
################################################################################

dta <- dta %>%
  mutate(
    # Level AVE for the full sample
    ln_AVE_FE = (1 / (1 - elasticities)) * FE,
    
    # AVE difference relative to base year (post-period only)
    diff_ln_AVE_FE = if_else(
      year %in% (base_year + 1):2020,
      (1 / (1 - elasticities)) * diff_FE,
      NA
    ),
    
    # AVE difference using frontier benchmark
    diff_ln_AVE_FE_bench = if_else(
      year %in% (base_year + 1):2020,
      (1 / (1 - elasticities)) * diff_FE_bench,
      NA
    ),
    
    # AVE difference using trade-weighted non-US mean
    diff_ln_AVE_FE_wmean = if_else(
      year %in% (base_year + 1):2020,
      (1 / (1 - elasticities)) * diff_FE_wmean,
      NA
    )
  )

summary(dta$ln_AVE_FE)
summary(dta$diff_ln_AVE_FE_bench)
summary(dta$diff_ln_AVE_FE_wmean)
colSums(is.na(dta))

################################################################################
# 10) Export full dataset
################################################################################

write_csv(dta, paste0(exp, "estimates_reduced_form_base_", base_year, "_FE.csv"))
dta <- read_csv(paste0(exp, "estimates_reduced_form_base_", base_year, "_FE.csv"))

################################################################################
# 11) Extract US exporter subset for downstream NTM analysis
################################################################################

US <- dta %>% filter(ExporterISO3 == "USA")

summary(US)
unique(US$hs2)
unique(US$hs_section)

write_csv(US, paste0(exp, "US_ln_NTMs_base_", base_year, "_FE.csv"))
US <- read_csv(paste0(exp, "US_ln_NTMs_base_", base_year, "_FE.csv"))


