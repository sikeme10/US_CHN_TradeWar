

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
HS_NAICS <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/crosswalk/clean_HS6_naics6_2012.csv")

colSums(is.na(HS_NAICS))

################################################################################
#1) By created subsectorw
################################################################################
names(HS_NAICS)
colSums(is.na(HS_NAICS))
HS_NAICS <- HS_NAICS %>% select(naics, subsector, ag_subsector, naics_description)
HS_NAICS <- HS_NAICS %>% distinct() %>% filter(!is.na(naics))

# check if duplicates:
test <- HS_NAICS %>%  filter(duplicated(naics) | duplicated(naics, fromLast = TRUE))
test <- HS_NAICS %>%  group_by(naics) %>%
  summarise(n_subsector = n_distinct(subsector),
            subsectors = paste(unique(subsector), collapse = ", ")  ) %>%
  filter(n_subsector > 1)
# if non ag and crop/livestock put it in crop and livestock
HS_NAICS <- HS_NAICS %>%  group_by(naics) %>%
  mutate(
    subsector = case_when(
      # forestry + crop + nonag → crop (put this first: most specific)
      all(c("forestry", "crop", "nonag") %in% subsector) ~ "crop",
      
      # nonag + livestock → livestock
      any(subsector == "livestock") & any(subsector == "nonag") ~ "livestock",
      
      # nonag + crop → crop
      any(subsector == "crop") & any(subsector == "nonag") ~ "crop",
      
      # nonag + forestry → nonag
      any(subsector == "forestry") & any(subsector == "nonag") ~ "nonag",
      
      # otherwise keep original value
      TRUE ~ subsector    )  ) %>%  ungroup()
test <- HS_NAICS %>%  group_by(naics) %>%
  summarise(n_subsector = n_distinct(subsector),
            subsectors = paste(unique(subsector), collapse = ", ")  ) %>%
  filter(n_subsector > 1)
table(HS_NAICS$subsector)
length(unique(HS_NAICS$naics))
# manually adjust some other 
HS_NAICS <- HS_NAICS %>%
  mutate( subsector = case_when(naics == 311225 ~ "crop", naics == 311119 ~ "livestock",TRUE ~ subsector)  )



sectors <- HS_NAICS %>%  group_by(naics, subsector) %>%
  summarise(ag_subsector = paste(ag_subsector, collapse = ", "), .groups = "drop")
length(unique(sectors$naics))


################################################################################

# Extract 2-digit NAICS sector
names(dta)
sapply(dta,class)
sapply(HS_NAICS,class)

dta1 <- left_join(dta, sectors)
colSums(is.na(dta1))
unique(dta1$subsector)


# if subsector is NA then put non ag 
dta1 <- dta1 %>% mutate(subsector = if_else(is.na(subsector), "other", subsector))
table(dta1$subsector)
 


# Compute total employment by CZ and sector
sector_emp <- dta1 %>%  group_by(czone_2012, subsector) %>%
  summarise(emp_sector = sum(emp, na.rm = TRUE), .groups = "drop")

# Compute total employment by CZ
total_emp <- dta1 %>%  group_by(czone_2012) %>%
  summarise(emp_total = sum(emp, na.rm = TRUE), .groups = "drop")



# Compute shares
sector_shares <- sector_emp %>%  left_join(total_emp, by = "czone_2012") %>%
  mutate(share = emp_sector / emp_total) %>%
  select(czone_2012, subsector, share) %>%
  pivot_wider(names_from = subsector, values_from = share, values_fill = 0) %>%
  rename(share_crop = crop,
         share_livestock = livestock,
         share_forestry = forestry,
         share_nonag = nonag,
         share_other = other)

# Quick check: shares should sum to 1
sector_shares %>%  mutate(total = share_crop + share_livestock + share_forestry +share_nonag+  share_other) %>%
  summary()

names(sector_shares)
write_csv(sector_shares,"/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/subsector_share_2012.csv" )


################################################################################
#2) By created sectors
################################################################################

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

