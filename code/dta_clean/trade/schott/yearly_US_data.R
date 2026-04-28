################################################################################
# Gravity regression analysis: residual approach (parameterized)
################################################################################

rm(list=ls());gc()

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
# USER CHOICES (change these only)
################################################################################


library(haven)
library(dplyr)

# Set the directory path
data_dir <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/trade/schott/annual"

# Generate file names for 2000-2020
years <- 2010:2020
file_names <- paste0("exp_detl_", years, "_12n.dta")
file_paths <- file.path(data_dir, file_names)

# Check which files actually exist
existing_paths <- file_paths[file.exists(file_paths)]
missing <- file_names[!file.exists(file_paths)]

if (length(missing) > 0) {
  cat("Missing files:\n")
  cat(paste(missing, collapse = "\n"), "\n")
}

# Read and combine all existing files
combined_data <- bind_rows(
  lapply(existing_paths, function(f) {
    cat("Reading:", basename(f), "\n")
    read_dta(f)
  })
)


################################################################################

names(combined_data)
length(unique(combined_data$cty_code))

################################################################################

# get countrycode: https://www.census.gov/foreign-trade/schedules/c/country.txt

###############################################################
# Add country names and ISO codes to combined_data
################################################################################
library(readr)
library(dplyr)
library(stringr)
library(countrycode)

# ------------------------------------------------------------------------------
# Step 1: Download and parse the Census Schedule C country code file
# ------------------------------------------------------------------------------
# The file is pipe-delimited with header rows and dashes; we need to clean it.
country_url <- "https://www.census.gov/foreign-trade/schedules/c/country.txt"

# Read raw lines (skip headers and decorative rows)
raw_lines <- readLines(country_url)

# Keep only lines that look like data rows (start with a digit-ish code followed by |)
data_lines <- raw_lines[str_detect(raw_lines, "^\\s*\\d{4}\\s*\\|")]

# Parse pipe-delimited content
country_lookup <- read.table(
  text   = data_lines,
  sep    = "|",
  header = FALSE,
  quote  = "",                    # <-- disable quote handling
  strip.white = TRUE,
  stringsAsFactors = FALSE,
  col.names = c("cty_code", "country_name", "iso2"),
  fill = TRUE                     # <-- safety net for any short rows
) %>%
  mutate(
    cty_code = as.numeric(cty_code),
    country_name = str_trim(country_name),
    iso2 = str_trim(iso2)
  )

# Quick sanity check
head(country_lookup)
# cty_code              country_name iso2
# 1     1000  United States of America   US
# 2     1010                 Greenland   GL
# 3     1220                    Canada   CA
# ...

# ------------------------------------------------------------------------------
# Step 2: Add ISO3 codes using the countrycode package
# ------------------------------------------------------------------------------
country_lookup <- country_lookup %>%
  mutate(
    iso3 = countrycode(iso2,
                       origin = "iso2c",
                       destination = "iso3c",
                       warn = FALSE)
  )

# Inspect any unmatched rows (these are usually special territories,
# "rest of world" buckets, or codes Census uses that aren't real countries)
country_lookup %>% filter(is.na(iso3))

# ------------------------------------------------------------------------------
# Step 3: Merge into combined_data
# ------------------------------------------------------------------------------
# Make sure cty_code types match before joining
combined_data <- combined_data %>%  mutate(cty_code = as.numeric(cty_code)) %>%
  left_join(country_lookup, by = "cty_code")

# Check the merge: how many rows failed to match?
sum(is.na(combined_data$country_name))
colSums(is.na(combined_data))
test <- combined_data %>% filter(is.na(country_name))
unique(test$cty_code)

combined_data %>%
  filter(is.na(country_name)) %>%
  distinct(cty_code)

names(combined_data)
# drop country codes without country name
combined_data <- combined_data %>% filter(!is.na(country_name))
colSums(is.na(combined_data))

# for missing isocdes
test <- combined_data %>% filter(is.na(iso2))
unique(test$country_name)

combined_data <- combined_data %>%
  mutate( iso2 = ifelse(country_name == "Namibia", "NA", iso2)  )

# get isocodes 3
combined_data <- combined_data %>%
  mutate(iso3 = countrycode(iso2, origin      = "iso2c", destination = "iso3c", warn        = FALSE),
    # Namibia fix: countrycode will return NA because iso2 = "NA" 
    # looks like a missing value internally
    iso3 = ifelse(country_name == "Namibia", "NAM", iso3)  )
colSums(is.na(combined_data))
test <- combined_data %>% filter(is.na(iso3))
unique(test$country_name)

# Verify
combined_data %>%  distinct(cty_code, country_name, iso2, iso3) %>% 
  filter(is.na(iso3))
combined_data <- combined_data %>%
  mutate(iso3 = case_when(
    country_name == "Congo, Democratic Republic of the Congo (formerly Za" ~ "COD",
    country_name == "Kosovo"                                               ~ "XKX",  
      TRUE ~ iso3 ) ) %>%
  # Drop West Bank and Gaza - not sovereign states, no ISO3
  filter(!country_name %in% c("West Bank administered by Israel",
                              "Gaza Strip administered by Israel"))

# Verify
combined_data %>% filter(is.na(iso3)) %>% distinct(country_name)

#############
# get product codes at HS6
class(combined_data$commodity)
unique(nchar(combined_data$commodity))

# make it as character and add the 0 if necessary
combined_data <- combined_data %>%
  mutate(commodity = str_pad(formatC(as.numeric(commodity), format = "fg", flag = "0"),  # converts sci notation to integer string
      width = 10, pad   = "0",  side  = "left"    ),
      hs6 = substr(commodity, 1, 6),
      hs4 = substr(commodity, 1, 4),
      hs2 = substr(commodity, 1, 2)  )

# Verify
unique(nchar(combined_data$commodity))  # should only be 10
unique(nchar(combined_data$hs6))        # should only be 6
any(grepl("\\.", combined_data$commodity))  # should be FALSE
any(grepl("e", combined_data$commodity))    # should be FALSE
unique(nchar(combined_data$commodity))       # should be 10 only
head(combined_data$commodity)              

# get HS revision;
# See how many unique hs6 codes appear per year
combined_data %>% group_by(year) %>%
  summarise(n_hs6 = n_distinct(hs6)) %>%
  print(n = Inf)

# Check if specific codes appear/disappear across revisions
# HS 2012 made ~200 changes at 6-digit level vs HS 2007
# HS 2017 made ~600 changes vs HS 2012

# Flag which revision applies by year
combined_data <- combined_data %>%
  mutate(hs_revision = case_when( year <= 2011 ~ "HS2007",
                                  year <= 2016 ~ "HS2012",
                                  year >= 2017 ~ "HS2017"  ))
# concordance 
library(concordance)
hs3_codes <- combined_data %>% filter(hs_revision == "HS2007") %>% distinct(hs6)
hs4_codes <- combined_data %>% filter(hs_revision == "HS2012") %>% distinct(hs6)

hs3_to_hs5 <- hs3_codes %>%
  mutate(hs6_hs5 = concord_hs(sourcevar  = hs6, origin     = "HS3",destination = "HS5",
                              dest.digit = 6, all        = FALSE  ) )

hs4_to_hs5 <- hs4_codes %>% mutate(hs6_hs5 = concord_hs(sourcevar = hs6, origin = "HS4",
                                                         destination = "HS5",  dest.digit  = 6,  all = FALSE    )  )

combined_data <- combined_data %>%
  left_join(hs3_to_hs5, by = "hs6", suffix = c("", "_hs3match")) %>%
  left_join(hs4_to_hs5, by = "hs6", suffix = c("", "_hs4match")) %>%
  mutate(hs6_hs5 = case_when(
    hs_revision == "HS2017" ~ hs6,          # already HS5, no conversion needed
    hs_revision == "HS2007" ~ hs6_hs5,      # from hs3_to_hs5 join
    hs_revision == "HS2012" ~ hs6_hs5_hs4match,  )  ) %>%
  select(-hs6_hs5_hs4match)

unique(combined_data$hs6_hs5)
length(unique(combined_data$hs6_hs5))



################################################################################
# select variable of interest:
names(combined_data)
combined_data1 <- combined_data %>%
  select(country_name, iso3,hs6,hs_revision,  hs6_hs5,    year,   export_value_USD = all_val_yr  )


################################################################################
# at HS6 level

combined_data1 <- combined_data1 %>%
  group_by(iso3, country_name, hs6_hs5, year) %>%
  summarise(export_value_USD = sum(export_value_USD, na.rm = TRUE),
            .groups = "drop")
write_csv(combined_data1, "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/trade/schott/annual/US_export_clean.csv")



