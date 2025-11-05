install.packages(c("rvest", "dplyr", "readr"))
library(rvest)
library(dplyr)
library(purrr)
library(readr)
library(countrycode)

#########################################################################
rm(list=ls())
# Set directory
setwd("/data/sikeme/TRADE/NTM_trade_war/data")
getwd()


##################################################################################

# get country codes from wesbsite to join later with trade data from Schott



url <- "https://www.census.gov/foreign-trade/schedules/c/countryname.html"
page <- read_html(url)




tables <- page %>% html_table(fill = TRUE)
length(tables)  # number of tables found

country_codes <- tables %>%
  keep(~ ncol(.x) >= 3) %>%  # ignore empty or malformed tables
  map_dfr(~ {
    # Clean up column names and structure
    df <- .x
    names(df)[1:3] <- c("Country", "Country_Code", "ISO_Code")
    df
  })

colSums(is.na(country_codes))
test <- country_codes %>% filter(is.na(ISO_Code ))
test
# Somehow Namibia is missing, put it back manually 
country_codes <- country_codes %>%
  mutate(ISO_Code = if_else(Country == "Namibia", "NA", as.character(ISO_Code)))


################################################################################

# assuming your merged dataframe is called `country_codes`
# and has a column "ISO_Code" with 2-letter codes (like "US", "FR", "CN")


country_codes <- country_codes %>%
  mutate(
    ISO3_Code = countrycode(ISO_Code, "iso2c", "iso3c"),
    ISO3_Code = case_when(
      ISO_Code == "KV" ~ "XKX",   # Kosovo (temporary code)
      ISO_Code %in% c("GZ", "WE") ~ "PSE",  # Palestinian Territories
      TRUE ~ ISO3_Code
    )
  )
colSums(is.na(country_codes))

write_csv(country_codes, "trade/schott/country_codes.csv")
