

################################################################################
# MFP
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
library(readxl)
library(dplyr)
library(purrr)

################################################################################

# load data
dta_dir <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/MFP/fsa_payments"

read_year <- function(year) {
  yr_dir <- file.path(dta_dir, year)
  files <- list.files(yr_dir, pattern = "\\.xlsx$", full.names = TRUE)
  
  cat(sprintf("Reading %d files for %s...\n", length(files), year))
  
  df <- map_dfr(files, function(f) {
    cat(sprintf("  %s\n", basename(f)))
    read_excel(f)
  })
  
  df$year <- as.integer(year)
  cat(sprintf("  Total rows: %s\n\n", format(nrow(df), big.mark = ",")))
  return(df)
}

fsa_2017 <- read_year("2017")
fsa_2018 <- read_year("2018")
fsa_2019 <- read_year("2019")
# Bind all years
fsa_all <- bind_rows(fsa_2017, fsa_2018, fsa_2019)
unique(fsa_all$year)

################################################################################

# get Fips code
fsa_all <- fsa_all %>%
  mutate(state_fips  = sprintf("%02d", `State FSA Code`),
         county_fips = sprintf("%03d", `County FSA Code`),
         fips        = paste0(state_fips, county_fips)  )

################################################################################
fsa_2017 <- fsa_all %>% filter(year == 2017)
unique(fsa_2017$`Accounting Program Description`)

  
# get only MFP program 
names(fsa_all)
unique(fsa_all$`Accounting Program Description`)
fsa_all_mfp <- fsa_all %>%  filter(grepl("MARKET FACILITATION", `Accounting Program Description`))
unique(fsa_all_mfp$`Accounting Program Description`)
unique(fsa_all_mfp$year)


fsa_all_mfp <- fsa_all_mfp %>% rename(MFP_prog = `Accounting Program Description` )

################################################################################
# aggregate at fips level


fsa_all_mfp_prog <- fsa_all_mfp %>% group_by(MFP_prog, year) %>%
  summarise(MFP_USD = sum(`Disbursement Amount`, na.rm = TRUE))
length(unique(fsa_all_mfp_year$fips))


fsa_all_mfp_prog_wide <- fsa_all_mfp %>%
  mutate(MFP_prog = case_when(
    MFP_prog == "MARKET FACILITATION PROG-SPECIALTY CROPS"  ~ "specialty_crops",
    MFP_prog == "MARKET FACILITATION PROGRAM - CROPS"       ~ "crops",
    MFP_prog == "MARKET FACILITATION PROGRAM - DAHG"        ~ "dahg",
    TRUE ~ MFP_prog  # keeps any other values as-is
  )) %>%
  group_by(MFP_prog, year) %>%
  summarise(MFP_USD = sum(`Disbursement Amount`, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from  = MFP_prog,
    values_from = MFP_USD,
    values_fill = 0
  )




write_csv(fsa_all_mfp_prog,"/data/sikeme/TRADE/US_CHN_TradeWar_git/data/MFP/SUB_crop_type_year.csv" )








