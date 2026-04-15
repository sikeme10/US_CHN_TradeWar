
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


setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/")


# =============================================================================
# Download and Process SEER County Population Data (2015-2020)
# Produces: total population by county FIPS and year
#
# Source: https://seer.cancer.gov/popdata/download.html
#   File: 1990-2024, Expanded Races by Origin, 20 Age Groups, All States (adjusted)
#   Format: Fixed-width ASCII, 26 bytes per record
#   Dictionary: https://seer.cancer.gov/popdata/popdic.html
#
# Data layout (1-indexed columns):
#   1-4   Year (numeric)
#   5-6   State postal abbreviation (character)
#   7-8   State FIPS code (numeric, zero-padded)
#   9-11  County FIPS code (numeric, zero-padded)
#   14    Race (1=White, 2=Black, 3=AIAN, 4=API)
#   15    Origin (0=Non-Hispanic, 1=Hispanic)
#   16    Sex (1=Male, 2=Female)
#   17-18 Age group
#   19-26 Population (numeric)
# =============================================================================

library(readr)
library(dplyr)

# --- 1. Download the gzipped text file from SEER ---
url <- paste0(
  "https://seer.cancer.gov/popdata/yr1990_2024.20ages/",
  "us.1990_2024.20ages.adjusted.txt.gz"
)
dest <- "Pop/us.1990_2024.20ages.adjusted.txt.gz"

if (!file.exists(dest)) {
  message("Downloading SEER population data (~90 MB)...")
  download.file(url, dest, mode = "wb")
  message("Download complete.")
}

# --- 2. Read the fixed-width file ---
message("Reading and parsing fixed-width file...")

col_positions <- fwf_positions(
  start = c(1,  5,  7,  9, 14, 15, 16, 17, 19),
  end   = c(4,  6,  8, 11, 14, 15, 16, 18, 26),
  col_names = c(
    "year", "state_abbr", "state_fips", "county_fips",
    "race", "origin", "sex", "age", "population"
  )
)

raw <- read_fwf(
  dest,
  col_positions = col_positions,
  col_types = cols(
    year         = col_integer(),
    state_abbr   = col_character(),
    state_fips   = col_character(),
    county_fips  = col_character(),
    race         = col_integer(),
    origin       = col_integer(),
    sex          = col_integer(),
    age          = col_integer(),
    population   = col_integer()
  ),
  progress = TRUE
)

message(sprintf("Read %s total records.", format(nrow(raw), big.mark = ",")))


# --- 3. Filter to 2015-2020, drop Katrina/Rita dummy state (FIPS 99) ---
filtered <- raw %>% filter(year >= 2015, year <= 2020, state_fips != "99")

# --- 4. Aggregate both totals per county-year ---
#   Working age = age groups 04-12 = ages 15-64
#   Age group codes (20-group file):
#     00=<1, 01=1-4, 02=5-9, 03=10-14,
#     04=15-19, 05=20-24, ..., 12=60-64,
#     13=65-69, ..., 19=90+

county_pop <- filtered %>%
  mutate(fips = paste0(state_fips, county_fips)) %>%
  group_by(fips, state_fips, county_fips, state_abbr, year) %>%
  summarise(
    total_pop       = sum(population, na.rm = TRUE),
    working_age_pop = sum(population[age >= 4 & age <= 12], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(fips, year)

message(sprintf(
  "Result: %s rows, %s unique counties, years %d-%d",
  format(nrow(county_pop), big.mark = ","),
  format(n_distinct(county_pop$fips), big.mark = ","),
  min(county_pop$year),
  max(county_pop$year)
))

# --- 5. Write output ---
outfile <- "Pop/county_population_2015_2020.csv"
write_csv(county_pop, outfile)
message(sprintf("Wrote %s", outfile))




# --- 6. Quick sanity check ---

message("\nSanity check: US totals by year")
county_pop %>%
  group_by(year) %>%
  summarise(
    us_total_pop       = sum(total_pop),
    us_working_age_pop = sum(working_age_pop),
    pct_working_age    = round(100 * sum(working_age_pop) / sum(total_pop), 1),
    .groups = "drop"
  ) %>%
  print()

################################################################################
# Aggregate at CZ levels

dta <- read_csv("Pop/county_population_2015_2020.csv")
names(dta)
length(unique(dta$fips))

class(dta$fips)
unique(nchar(dta$fips))
# have to recode some counties to 2012 counties 
names(dta)
dta <- dta |>
  mutate(fips = case_when(
    fips == "02158" & year >= 2015 ~ "02270",  # Kusilvak -> Wade Hampton
    fips == "46102" & year >= 2015 ~ "46113",  # Oglala Lakota -> Shannon
    TRUE ~ fips
  ))
length(unique(dta$fips))


czone <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/crosswalk_CZ_county/cw_cty_czone_2012.csv")
names(czone)
length(unique(czone$cty_fips_2012))
czone <- czone %>% select(cty_fips_2012, czone_2012)


class(czone$cty_fips_2012)

dta$fips <- str_pad(as.character(dta$fips), width = 5, pad = "0")
czone$cty_fips_2012 <- str_pad(as.character(czone$cty_fips_2012), width = 5, pad = "0")

length(unique(dta$fips))
length(unique(czone$cty_fips_2012))

# merge CZ and county level data 

dta <- left_join(dta, czone, by = c("fips" = "cty_fips_2012"))

# merge at CZ - year level

names(dta)
dta_CZ <- dta %>% group_by(year, czone_2012) %>% 
  summarise(working_age_pop = sum(working_age_pop, na.rm =TRUE),
            total_pop = sum(total_pop, na.rm =TRUE))

table(dta_CZ$year)

write_csv(dta_CZ,"Pop/cz_population_2015_2020.csv" )

