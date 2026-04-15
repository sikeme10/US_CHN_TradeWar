

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
################################################################################

czone <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/crosswalk_CZ_county/cw_cty_czone_2012.csv")
names(czone)
length(unique(czone$cty_fips_2012))

class(fsa_all_mfp_year$fips)
class(czone$cty_fips_2012)

fsa_all_mfp_year$fips <- as.numeric(fsa_all_mfp_year$fips)
summary(fsa_all_mfp_year)
summary(czone)
czone <- czone %>% select(cty_fips_2012, czone_2012)

#################################################################################
# merge the two 
fsa_all_mfp_year1 <- left_join(fsa_all_mfp_year, czone, by = c("fips" = "cty_fips_2012"))

# aggregate at CZ and year level
names(fsa_all_mfp_year1)

MFP_year_CZ <- fsa_all_mfp_year1 %>% group_by(year, czone_2012) %>%
  summarise(MFP_USD = sum(MFP_USD, na.rm = TRUE))


write_csv(MFP_year_CZ,"/data/sikeme/TRADE/US_CHN_TradeWar_git/data/MFP/MFP_CZ_year.csv" )

#################################################################################

MFP_year_CZ <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/MFP/MFP_CZ_year.csv")
names(MFP_year_CZ)
dta_pop <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/Pop/cz_population_2015_2020.csv" )
names(dta_pop)


# use data from 2017
dta_pop <- dta_pop %>% filter(year == 2017) %>% select(-year)

#  balance MFP data
# Build skeleton from 2017 czones only
distinct_CZ <- dta_pop %>% distinct(czone_2012)

# Get all distinct years from MFP data
distinct_years <- MFP_year_CZ %>% distinct(year)

# Create balanced panel skeleton: all 2017 czones x all MFP years
panel_skeleton <- distinct_CZ %>%  cross_join(distinct_years)

# Join MFP onto balanced skeleton, fill missing with 0
MFP_balanced <- panel_skeleton %>%
  left_join(MFP_year_CZ, by = c("year", "czone_2012")) %>%
  mutate(MFP_USD = replace_na(MFP_USD, 0))
table(MFP_balanced$year)
  
MFP_balanced <- left_join(MFP_balanced ,dta_pop)
names(MFP_balanced)

# create  farm subsidies per working age population
MFP_balanced <- MFP_balanced %>% mutate(SUB = MFP_USD / working_age_pop)
summary(MFP_balanced)


write_csv(MFP_balanced,"/data/sikeme/TRADE/US_CHN_TradeWar_git/data/MFP/SUB.csv" )

test21 <-read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/MFP/SUB.csv" )
