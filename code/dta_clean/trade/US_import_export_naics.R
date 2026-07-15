




library(readr)
library(tidyr)
library(dplyr)
library(data.table)
library(stringi)
library(fixest)
library(countrycode)
library(tidyverse)
library(vroom)




rm(list=ls())
# Set directory
setwd("/data/sikeme/TRADE/NTM_trade_war/data")
getwd()


library(haven)
library(dplyr)

# Set your working directory to where the files live
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/trade/schott/annual")

years <- 2015:2020

# --- Import files ---
imp_list <- lapply(years, function(y) {
  file <- paste0("imp_detl_", y, "_12n.dta")
  cat("Reading", file, "\n")
  df <- read_dta(file)
  df$year <- y
  return(df)
})

import <- bind_rows(imp_list)

# --- Export files ---
exp_list <- lapply(years, function(y) {
  file <- paste0("exp_detl_", y, "_12n.dta")
  cat("Reading", file, "\n")
  df <- read_dta(file)
  df$year <- y
  return(df)
})

export <- bind_rows(exp_list)
names(import)
###############################################################################
# --- Load reference data ---
countryCodes <- fread("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/trade/schott/country_codes.csv")
product_concord <- fread("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/trade/schott/hts_concordances_20190712_198906_201901.csv")

# --- Reusable cleaning function for import or export ---
clean_trade_data <- function(dt, value_col, countryCodes) {
  # dt <- import
  setDT(dt)
  setDT(countryCodes)
  
  # Merge country codes
  dt <- merge(dt, countryCodes, by.x = "cty_code", by.y = "Country_Code", all.x = TRUE)
  
  n_before <- uniqueN(dt$cty_code)
  missing_ctys <- unique(dt[is.na(Country), cty_code])
  if (length(missing_ctys) > 0) {
    message("Dropping ", length(missing_ctys), " unmatched country codes: ",
            paste(missing_ctys, collapse = ", "))
  }
  dt <- dt[!is.na(Country)]
  
  # Clean HS10 code once
  dt[, HS10 := str_pad(as.character(commodity), width = 10, side = "left", pad = "0")]
  dt[, HS6  := substr(HS10, 1, 6)]
  
  # Clean NAICS once (kept only if needed downstream; drop if not)
  dt[, naics6 := naics]
  dt[, naics4 := substr(as.character(naics6), 1, 4)]
  dt[, naics3 := substr(as.character(naics6), 1, 3)]
  
  # --- Check for duplicates before aggregating ---
  dup_key <- c("year", "cty_code", "HS10", "naics6")
  n_dups <- sum(duplicated(dt, by = dup_key))
  
  if (n_dups > 0) {
    message(n_dups, " duplicate rows found on key: ", paste(dup_key, collapse = ", "))
    message("These will be collapsed (summed) in the aggregation step below.")
  } else {
    message("No duplicates found on key: ", paste(dup_key, collapse = ", "))
  }
  
  # Single aggregation straight to HS6 level (skips the intermediate HS10/sic/naics pass)
  agg <- dt[, .(value_USD = sum(get(value_col), na.rm = TRUE)),
            by = .(year, cty_code, Country, ISO_Code, ISO3_Code, sic, naics)]
  
  return(agg)
}

# --- Apply to imports ---
US_import1 <- clean_trade_data(import, value_col = "gen_val_yr", countryCodes = countryCodes)
setnames(US_import1, "value_USD", "import_val_USD")
write_csv(US_import1, "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/trade/schott/yearl_US_import_naics.csv")

# --- Apply to exports ---
US_export1 <- clean_trade_data(export, value_col = "all_val_yr", countryCodes = countryCodes)
setnames(US_export1, "value_USD", "export_val_USD")
write_csv(US_export1, "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/trade/schott/yearl_US_export_naics.csv")

names(US_export1)

###############################################################################
# import necessary data
###############################################################################

US_import1 <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/trade/schott/yearl_US_import_naics.csv")
US_export1 <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/trade/schott/yearl_US_export_naics.csv")

# convert industry level
industry_map <- read_csv("data/Census_output/output_level_analysis/NAICS_ouput_industry_maps.csv")
names(industry_map)
output_NAICS6 <-  read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/Census_output/output_NAICS_6.csv")
names(output_NAICS6)
output_NAICS6 <- output_NAICS6 %>% rename(Tot_output_1000dollars_2012 = Tot_output_1000dollars) %>% select(-year)

###############################################################################
# merge industry code with with output data
###############################################################################

library(stringr)

industry_map <- industry_map %>%
  mutate(
    map_naics = str_extract(industry_code, "^[0-9]+") %>% str_remove("0+$")
  )


# map with output first:
setDT(industry_map)
setDT(output_NAICS6)
library(data.table)
setDT(industry_map)
setDT(output_NAICS6)

industry_map[, map_naics := as.character(map_naics)]
output_NAICS6[, NAICS6_2012 := as.character(NAICS6_2012)]

all_prefixes <- unique(industry_map$map_naics)
all_prefixes <- all_prefixes[!is.na(all_prefixes) & all_prefixes != ""]

match_longest_prefix <- function(code, prefixes) {
  if (is.na(code) || code == "") return(NA_character_)
  candidates <- prefixes[startsWith(code, prefixes)]
  if (length(candidates) == 0) return(NA_character_)
  candidates[which.max(nchar(candidates))]
}

output_NAICS6[, matched_map_naics := vapply(
  NAICS6_2012, match_longest_prefix, prefixes = all_prefixes, FUN.VALUE = character(1)
)]

cat("Matched:", sum(!is.na(output_NAICS6$matched_map_naics)), "of", nrow(output_NAICS6), "\n")

# --- Join industry info using the matched prefix ---
output_industry <- merge(
  output_NAICS6,
  unique(industry_map[, .(map_naics, industry, industry_code)], by = "map_naics"),
  by.x = "matched_map_naics", by.y = "map_naics",
  all.x = TRUE
)

# Diagnostics
cat("Total rows:", nrow(output_industry), "\n")
cat("Unmatched rows:", sum(is.na(output_industry$industry)), "\n")

output_industry[is.na(industry), .(NAICS6_2012)] %>% distinct()


write_csv(output_industry,"/data/sikeme/TRADE/US_CHN_TradeWar_git/data/Census_output/output_industry_code.csv" )


# drop if NA for industry code
colSums(is.na(output_industry))

output_industry1 <- output_industry %>% filter(!is.na(industry))



################################################################################
# merge export to industry code
################################################################################

industry_map <- industry_map %>%
  mutate(map_naics = str_extract(industry_code, "^[0-9]+") %>% str_remove("0+$"))

setDT(industry_map)
setDT(US_export1)

industry_map[, map_naics := as.character(map_naics)]
US_export1[, naics := as.character(naics)]


US_export1[, naics_clean := str_extract(naics, "^[0-9]+")]

cat("Rows changed by cleaning:", sum(US_export1$naics != US_export1$naics_clean, na.rm = TRUE), "\n")
cat("Rows with no leading digits at all (fully non-numeric):", sum(is.na(US_export1$naics_clean)), "\n")



all_prefixes <- unique(industry_map$map_naics)
all_prefixes <- all_prefixes[!is.na(all_prefixes) & all_prefixes != ""]

match_longest_prefix <- function(code, prefixes) {
  if (is.na(code) || code == "") return(NA_character_)
  candidates <- prefixes[startsWith(code, prefixes)]
  if (length(candidates) == 0) return(NA_character_)
  candidates[which.max(nchar(candidates))]
}

US_export1[, matched_map_naics := vapply(
  naics_clean, match_longest_prefix, prefixes = all_prefixes, FUN.VALUE = character(1))]

cat("Matched:", sum(!is.na(US_export1$matched_map_naics)), "of", nrow(US_export1), "\n")


US_export1 <- merge(
  US_export1,
  unique(industry_map[, .(map_naics, industry, industry_code)], by = "map_naics"),
  by.x = "matched_map_naics", by.y = "map_naics",
  all.x = TRUE)

cat("Total rows:", nrow(US_export1), "\n")
cat("Unmatched rows:", sum(is.na(US_export1$industry)), "\n")

US_export1 <- US_export1 %>% filter(!is.na(industry))

###############################################################################
# aggregate values at industr level
###############################################################################

length(unique(US_export1$industry))
length(unique(output_industry1$industry))
names(output_industry1)
names(US_export1)

output_industry1 <- output_industry1 %>% select(industry, industry_code, Tot_output_1000dollars)
US_export1 <- US_export1 %>%  select(industry, industry_code,year, Country,ISO3_Code, export_val_USD )


# sum over industry 

output_industry2 <- output_industry1 %>%  group_by(industry, industry_code) %>%
  summarise(Tot_output_USD = sum(Tot_output_1000dollars *1000, na.rm = TRUE))

US_export2 <- US_export1 %>%  group_by(industry, industry_code, year) %>%
  summarise(export_val_USD = sum(export_val_USD, na.rm = TRUE))
table(US_export2$year)

# merge the two data
merged_data <- full_join(output_industry2,US_export2 )
colSums(is.na(merged_data))

# create the share Export-to-shipment ratio

merged_data <- merged_data %>%  mutate(exp_ship_ratio_2012 = if_else(Tot_output_USD != 0, 100 * export_val_USD / Tot_output_USD, 0))
merged_data <- merged_data %>% mutate(exp_ship_ratio_2012 =  if_else(exp_ship_ratio_2012 > 100, NA, exp_ship_ratio_2012))
summary(merged_data)


write_csv(merged_data, "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/trade/exp_ship_ratio_2015_2020.csv")

###################################################################################

merged_data <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/trade/exp_ship_ratio_2015_2020.csv")
names(merged_data)

library(dplyr)

# --- Get the 2017 baseline value for each industry ---
baseline_2017 <- merged_data %>%
  filter(year == 2017) %>%
  select(industry_code, ratio_2017 = exp_ship_ratio_2012)

# --- Merge baseline back onto full data and compute change vs 2017 for every year t ---
merged_data <- merged_data %>%
  left_join(baseline_2017, by = "industry_code") %>%
  mutate(change_exp_ship_ratio = exp_ship_ratio_2012 - ratio_2017)

# --- Specific 2016 vs 2017 change, one row per industry ---
change_2016_2017 <- merged_data %>%
  filter(year %in% c(2016, 2017)) %>%
  select(industry_code, year, exp_ship_ratio_2012) %>%
  tidyr::pivot_wider(names_from = year, values_from = exp_ship_ratio_2012, names_prefix = "ratio_") %>%
  mutate(change_2016_2017 = ratio_2017 - ratio_2016)

merged_data <- merged_data %>%
  left_join(change_2016_2017 %>% select(industry_code, change_2016_2017), by = "industry_code")

write_csv(merged_data, "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/trade/exp_ship_ratio_2015_2020.csv")


