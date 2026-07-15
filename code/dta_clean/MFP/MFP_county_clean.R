

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


fsa_2018 <- read_year("2018")
fsa_2019 <- read_year("2019")
# Bind all years
fsa_all <- bind_rows(fsa_2018, fsa_2019)


################################################################################

# get Fips code
fsa_all <- fsa_all %>%
  mutate(state_fips  = sprintf("%02d", `State FSA Code`),
         county_fips = sprintf("%03d", `County FSA Code`),
         fips        = paste0(state_fips, county_fips)  )

################################################################################

# get only MFP program 
names(fsa_all)
unique(fsa_all$`Accounting Program Description`)
fsa_all_mfp <- fsa_all %>%  filter(grepl("MARKET FACILITATION", `Accounting Program Description`))
unique(fsa_all_mfp$`Accounting Program Description`)



################################################################################
# aggregate at fips level

names(fsa_all_mfp)

fsa_all_mfp_year <- fsa_all_mfp %>% group_by(fips,state_fips ,county_fips, year) %>%
  summarise(MFP_USD = sum(`Disbursement Amount`, na.rm = TRUE))
length(unique(fsa_all_mfp_year$fips))
names(fsa_all_mfp_year)

write_csv(fsa_all_mfp_year,"/data/sikeme/TRADE/US_CHN_TradeWar_git/data/MFP/MFP_county_year.csv" )


#################################################################################

MFP_year_county <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/MFP/MFP_county_year.csv")
names(MFP_year_county)
MFP_year_county <- MFP_year_county %>% select(fips,year, MFP_USD )
dta_pop <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/Pop/county_population_clean_2015_2020.csv" )
names(dta_pop)
dta_pop <- dta_pop %>% select(fips,year, total_pop,  working_age_pop)

class(MFP_year_county$fips)
class(dta_pop$fips)

# use data from 2017
dta_pop <- dta_pop %>% filter(year == 2017) %>% select(-year)

#  balance MFP data
# Build skeleton from 2017 countyones only
distinct_county <- dta_pop %>% distinct(fips)

# Get all distinct years from MFP data
distinct_years <- MFP_year_county %>% distinct(year)

# Create balanced panel skeleton: all 2017 czones x all MFP years
panel_skeleton <- distinct_county %>%  cross_join(distinct_years)

# Join MFP onto balanced skeleton, fill missing with 0
MFP_balanced <- panel_skeleton %>%
  left_join(MFP_year_county, by = c("year", "fips")) %>%
  mutate(MFP_USD = replace_na(MFP_USD, 0))
table(MFP_balanced$year)
  
MFP_balanced <- left_join(MFP_balanced ,dta_pop)
names(MFP_balanced)

# create  farm subsidies per working age population
MFP_balanced <- MFP_balanced %>% mutate(SUB = MFP_USD / working_age_pop)
summary(MFP_balanced)


write_csv(MFP_balanced,"/data/sikeme/TRADE/US_CHN_TradeWar_git/data/MFP/SUB_county.csv" )

test21 <-read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/MFP/SUB.csv" )
