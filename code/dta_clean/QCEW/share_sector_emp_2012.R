

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


dta <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/QCEW_2012_naics6_CZ.csv")

class(dta$naics)

# Extract 2-digit NAICS sector
dta <- dta %>%  mutate(naics2 = as.integer(naics %/% 10000))

# Classify into three broad sectors
dta <- dta %>%
  mutate(sector = case_when(
    naics2 == 11               ~ "ag_mining",
    naics2 == 21               ~ "ag_mining",
    naics2 >= 31 & naics2 <= 33 ~ "manufacturing",
    TRUE                        ~ "other"
  ))

# Compute total employment by CZ and sector
sector_emp <- dta %>%  group_by(czone_2012, sector) %>%
  summarise(emp_sector = sum(emp, na.rm = TRUE), .groups = "drop")

# Compute total employment by CZ
total_emp <- dta %>%  group_by(czone_2012) %>%
  summarise(emp_total = sum(emp, na.rm = TRUE), .groups = "drop")

# Compute shares
sector_shares <- sector_emp %>%  left_join(total_emp, by = "czone_2012") %>%
  mutate(share = emp_sector / emp_total) %>%
  select(czone_2012, sector, share) %>%
  pivot_wider(names_from = sector, values_from = share, values_fill = 0) %>%
  rename(share_mfg = manufacturing,
         share_ag_mining = ag_mining,
         share_other = other)

# Quick check: shares should sum to 1
sector_shares %>%  mutate(total = share_mfg + share_ag_mining + share_other) %>%
  summary()

names(sector_shares)
write_csv(sector_shares,"/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/sector_share_2012.csv" )

