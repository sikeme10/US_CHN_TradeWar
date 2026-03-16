


################################################################################
#                      Teti tariff data

 
# The data provide tariff rates imposed and faced by the United States vis-à-vis all
# trading partners at the 6-digit product level. Tariff rates are observed on each date
# when changes occurred between 2018 and 2025.
# Each variable t_YYYYMMDD reports the effective applied tariff rate (in percent) on that specific date.
# All product codes are harmonized to the HS2017 nomenclature.

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
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(concordance)

################################################################################
# directory: 
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git")
################################################################################

# 1) Load data 
dta <- read_csv("data/tariff_dta/teti/GTD-tradeWar_hs6.csv")
names(dta)

MFN <- read_csv("data/tariff_dta/WITS_MFN/ROW_US_MFN_tariff.csv")
names(MFN)
table(MFN$`Tariff Year`)
names(MFN) <- gsub(" ", "_", names(MFN))
unique(MFN$Reporter_Name)
unique(MFN$Partner_Name)
# =============================================================================
# ROW Retaliatory Tariffs on US (excl. China) — from Teti et al.
# =============================================================================
# Input:  Daily HS6-level retaliatory tariff data (`dta`) from 2018 onward
# Output: Yearly HS6 panel of ROW→US tariffs, keeping only products with
#         time-varying tariffs (i.e. actual retaliatory changes)
# =============================================================================

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(readr)

# -----------------------------------------------------------------------------
# 1. Filter to ROW imports from the US (drop USA and CHN as importers)
# -----------------------------------------------------------------------------
ROW_import <- dta %>%  filter(!importer %in% c("USA", "CHN")) %>%
  select(-eu1, -eu2, -v2017)

# -----------------------------------------------------------------------------
# 2. Reshape daily tariff columns (t_YYYY-MM-DD) to long format
# -----------------------------------------------------------------------------

ROW_long <- ROW_import %>%
  select(importer, exporter, hs6, starts_with("t_")) %>%
  pivot_longer(
    cols      = starts_with("t_"),
    names_to  = "var",
    values_to = "tariff",
    values_drop_na = TRUE
  ) %>%
  mutate(date = ymd(str_remove(var, "^t_"))) %>%
  select(importer, exporter, hs6, date, tariff) %>%
  arrange(importer, exporter, hs6, date)

# -----------------------------------------------------------------------------
# 3. Aggregate to monthly level (max tariff within each month)
# -----------------------------------------------------------------------------
# Multiple tariff changes can occur within a month; taking the max captures
# the highest rate that applied during that period.

ROW_monthly <- ROW_long %>%  mutate(year = year(date), month = month(date)) %>%
  group_by(importer, exporter, hs6, year, month) %>%
  summarise(tariff = max(tariff, na.rm = TRUE), .groups = "drop")


# -----------------------------------------------------------------------------
# 4. Aggregate to yearly level (mean of monthly tariffs)
# -----------------------------------------------------------------------------
# Averaging monthly values gives an effective annual tariff rate that
# accounts for mid-year changes.

ROW_yearly <- ROW_monthly %>%  group_by(importer, exporter, hs6, year) %>%
  summarise(tariff = mean(tariff, na.rm = TRUE), .groups = "drop") %>%
  mutate(nomenclature = "HS2017")

# -----------------------------------------------------------------------------
# 6. Keep only products with time-varying tariffs (actual retaliation)
# -----------------------------------------------------------------------------
# Products where the tariff is constant across all years show no retaliatory
# action and are dropped — they add no identifying variation.


# monthly
ROW_retaliation_monthly <- ROW_monthly %>%  group_by(importer, exporter, hs6) %>%
  filter(n_distinct(tariff) > 1) %>%  ungroup()

# Store the set of retaliating importers (used later to filter MFN data)
selected_exporter <- unique(ROW_retaliation_monthly$importer)


ROW_monthly <- ROW_monthly %>%  rename( ImporterISO3 = importer,ExporterISO3 = exporter,hs6_H5 = hs6  ) %>%
  filter(ImporterISO3 %in% selected_exporter)

write_csv(ROW_monthly, "data/tariff_dta/teti/ROW_US_tariff_HS6_Teti_monthly.csv")

ROW_monthly <- read_csv("data/tariff_dta/teti/ROW_US_tariff_HS6_Teti_monthly.csv")

selected_exporter <- unique(ROW_monthly$ImporterISO3)

#### yearly level
ROW_retaliation_yearly <- ROW_yearly %>%  group_by(importer, exporter, hs6) %>%
  filter(n_distinct(tariff) > 1) %>%  ungroup()

# Store the set of retaliating importers (used later to filter MFN data)
selected_exporter <- unique(ROW_retaliation_yearly$importer)

ROW_yearly <- ROW_yearly %>%  rename( ImporterISO3 = importer,ExporterISO3 = exporter,hs6_H5 = hs6  ) %>%
  filter(ImporterISO3 %in% selected_exporter)


# export 
write_csv(ROW_yearly, "data/tariff_dta/teti/ROW_US_tariff_HS6_Teti_yearly.csv")

ROW_yearly <- read_csv("data/tariff_dta/teti/ROW_US_tariff_HS6_Teti_yearly.csv")


# -----------------------------------------------------------------------------
# 7. Rename columns to match MFN panel conventions
# -----------------------------------------------------------------------------



# =============================================================================
# MFN Tariff Data Cleaning Pipeline
# =============================================================================
# Input:  Raw MFN tariff data with partner/reporter country pairs
# Output: Balanced monthly panel of HS6 (H5 concordance) tariff rates
#         for selected importers, with AHS and MFN duty types wide-format
# =============================================================================

# -----------------------------------------------------------------------------
# 1. Filter to US exports for 2017–2020
# -----------------------------------------------------------------------------

MFN <- MFN %>%  filter( Partner_Name == "United States", Tariff_Year %in% 2015:2020)

# -----------------------------------------------------------------------------
# 2. Add ISO3 country codes
# -----------------------------------------------------------------------------

MFN <- MFN %>%
  mutate( ImporterISO3 = countrycode(Reporter_Name, "country.name", "iso3c"),
          ExporterISO3 = countrycode(Partner_Name,  "country.name", "iso3c")  )

# Drop observations where importer code couldn't be matched
MFN <- MFN %>% filter(!is.na(ImporterISO3))

# -----------------------------------------------------------------------------
# 3. Keep only selected importers and relevant duty types
# -----------------------------------------------------------------------------

MFN <- MFN %>%  filter( ImporterISO3 %in% selected_exporter,
                        DutyType %in% c("AHS", "MFN")  )

# -----------------------------------------------------------------------------
# 4. Select and rename variables
# -----------------------------------------------------------------------------

MFN <- MFN %>%
  select(
    Native_Nomen, Product, ExporterISO3, ImporterISO3,
    year = Tariff_Year, DutyType, Weighted_Average, Simple_Average
  )

# -----------------------------------------------------------------------------
# 5. Pivot duty types wide (one row per product-year-country pair)
# -----------------------------------------------------------------------------
# Creates: Weighted_Average_AHS, Weighted_Average_MFN,
#          Simple_Average_AHS,   Simple_Average_MFN

MFN_wide <- MFN %>%
  pivot_wider(
    names_from  = DutyType,
    values_from = c(Weighted_Average, Simple_Average),
    names_sep   = "_"
  )

# -----------------------------------------------------------------------------
# 6. Concord product codes to HS2017 (H5) at 6-digit level
# -----------------------------------------------------------------------------
# H5 products keep their code; H4 products are mapped via concordance table

MFN_wide <- MFN_wide %>%
  mutate(
    hs6_H5 = case_when(
      Native_Nomen == "H5" ~ Product,
      Native_Nomen == "H4" ~ concord_hs(
        Product, origin = "HS4", destination = "HS5", dest.digit = 6, all = FALSE
      ),
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(hs6_H5))

# -----------------------------------------------------------------------------
# 7. Collapse duplicates from imperfect concordance (keep minimum tariff)
# -----------------------------------------------------------------------------

safe_min <- function(x) if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE)

MFN_collapsed <- MFN_wide %>%  group_by(hs6_H5, year, ExporterISO3, ImporterISO3) %>%
  summarise(
    across(
      c(Weighted_Average_AHS, Weighted_Average_MFN,
        Simple_Average_AHS,   Simple_Average_MFN),
      safe_min    ),    .groups = "drop"  )

# -----------------------------------------------------------------------------
# 8. Balance the panel across all product × year × country combinations
# -----------------------------------------------------------------------------

MFN_balanced <- MFN_collapsed %>%
  mutate(year = as.integer(year)) %>%
  distinct(hs6_H5, ExporterISO3, ImporterISO3, year, .keep_all = TRUE) %>%
  complete(
    hs6_H5       = unique(hs6_H5),
    year         = sort(unique(year)),
    ExporterISO3 = unique(ExporterISO3),
    ImporterISO3 = unique(ImporterISO3)  ) %>%
  arrange(hs6_H5, ExporterISO3, ImporterISO3, year)

# Verify balance
stopifnot(
  nrow(MFN_balanced) ==
    n_distinct(MFN_balanced$hs6_H5) *
    n_distinct(MFN_balanced$year) *
    n_distinct(MFN_balanced$ExporterISO3) *
    n_distinct(MFN_balanced$ImporterISO3))

# -----------------------------------------------------------------------------
# 9. Expand to monthly frequency
# -----------------------------------------------------------------------------

MFN_monthly <- MFN_balanced %>%  crossing(month = 1:12) %>%
  arrange(hs6_H5, ExporterISO3, ImporterISO3, year, month)

# -----------------------------------------------------------------------------
# 10. Fill missing tariff values (carry forward, then backward within group)
# -----------------------------------------------------------------------------

MFN_monthly <- MFN_monthly %>%
  arrange(ExporterISO3, hs6_H5, year, month) %>%
  group_by(ExporterISO3, hs6_H5) %>%
  fill(
    Weighted_Average_AHS, Weighted_Average_MFN,
    Simple_Average_AHS,   Simple_Average_MFN,
    .direction = "downup"
  ) %>%
  ungroup()

# Quick check: remaining NAs
cat("Remaining NAs:\n")
print(colSums(is.na(MFN_monthly)))


################################################################################
# Merge back with teti 
names(ROW_monthly)



write_csv(MFN_monthly, "data/tariff_dta/teti/ROW_tariff_on_US_monthly.csv")

################################################################################

# Merge back with Teti

ROW_monthly <- read_csv("data/tariff_dta/teti/ROW_US_tariff_HS6_Teti_monthly.csv")


MFN_monthly$hs6_H5 <- as.numeric(MFN_monthly$hs6_H5)

# merge the data
US_tariff2 <- left_join(MFN_monthly, ROW_monthly)
table(US_tariff2$year)
length(unique(US_tariff2$ImporterISO3))

################################################################################

# fill values 
US_tariff2 <- US_tariff2 %>% arrange(ImporterISO3, ExporterISO3, hs6_H5, year, month) %>%
  group_by(ImporterISO3,ExporterISO3, hs6_H5) %>% 
  fill(tariff, .direction = "down" ) %>%  ungroup()
names(US_tariff2)



US_tariff2 <- US_tariff2 %>% 
  mutate(tariff2 = case_when(is.na(Weighted_Average_AHS) & !is.na(tariff) ~ tariff,
                             !is.na(Weighted_Average_AHS) & is.na(tariff) ~ Weighted_Average_AHS,
                             !is.na(Weighted_Average_AHS) & !is.na(tariff) ~ pmax(Weighted_Average_AHS, tariff),
                             TRUE ~ NA_real_))
colSums(is.na(US_tariff2))



US_tariff2 <- US_tariff2 %>% arrange(ImporterISO3,ExporterISO3, hs6_H5, year, month) %>% 
  group_by(ImporterISO3,ExporterISO3,hs6_H5) %>% 
  fill(tariff2, .direction = "up" ) %>%  ungroup()

US_tariff2 <- US_tariff2 %>% select(ImporterISO3, ExporterISO3, hs6_H5,year,month,tariff2 ) %>% 
  rename(ROW_tariff_onUS = tariff2)



################################################################################

# create baseline variables 
US_baseline <- US_tariff2  %>%  filter(year %in% c(2015, 2017)) %>%
  pivot_wider(names_from = year, values_from = ROW_tariff_onUS, 
              names_prefix = "ROW_tariff_onUS_" )

US_tariff <- US_tariff %>% filter(year %in% c(2018:2020))

US_tariff <- left_join(US_tariff, US_baseline)


test <- US_tariff %>% filter(ExporterISO3 == "CAN")
test <- US_tariff %>% filter(US_tariff_onROW %in% c(10, 25))


write_csv(US_tariff, "data/tariff_dta/teti/US_tariff_on_ROW_monthly.csv")

US_tariff <- read_csv( "data/tariff_dta/teti/US_tariff_on_ROW_monthly.csv")
names(US_tariff)



# at the yearly level 
US_tariff_yearly <- US_tariff %>% group_by(ExporterISO3, hs6,year) %>% 
  summarise(US_tariff_onROW = mean(US_tariff_onROW, na.rm = TRUE),
            US_tariff_onROW_2015 = mean(US_tariff_onROW_2015, na.rm = TRUE),
            US_tariff_onROW_2017 = mean(US_tariff_onROW_2017, na.rm = TRUE))

write_csv(US_tariff_yearly, "data/tariff_dta/teti/US_tariff_on_ROW_yearly.csv")









