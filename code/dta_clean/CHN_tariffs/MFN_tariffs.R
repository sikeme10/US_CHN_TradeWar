




################################################################################
#                      MFN Chinese tariff cleaning dta


# get WITS TRAINS Chinese tariff data 


################################################################################


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


rm(list=ls())

################################################################################

setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git")
################################################################################

# load 
dta <- read_csv("data/tariff_dta/CHN_import_tariffs/CHN_WITS_tariff.csv")
names(dta)

################################################################################



# two duty types

unique(dta$DutyType)

dta <- dta %>%
  select(all_of(c(
    "Selected Nomen","Native Nomen","Reporter","Reporter Name",
    "Product","Product Name","Partner","Partner Name",
    "Tariff Year","Trade Year","Trade Source",
    "DutyType","Simple Average","Weighted Average"
  )))

# pivot wider: create columns per DutyType for both averages
dta_wide <- dta %>%
  pivot_wider(
    names_from  = DutyType,
    values_from = c(`Simple Average`, `Weighted Average`),
    # nice column names like "Simple Average - Ad Valorem"
    names_glue  = "{.value}_{DutyType}"
    # if you have duplicate rows per key+DutyType, add one of:
    # values_fn = list(`Simple Average` = mean, `Weighted Average` = mean, .names = "mean"),
    # or: values_fn = list(`Simple Average` = dplyr::first, `Weighted Average` = dplyr::first)
  )



################################################################################
# country isocodes

# retrieve from website isocdes 
url <- "https://wits.worldbank.org/wits/wits/witshelp/Content/Codes/Country_Codes.htm"

# Read all tables
page <- read_html(url)
tables <- page %>% html_nodes("table")
raw <- tables %>% html_table(fill = TRUE) %>% .[[1]]

# Remove the first two redundant header rows
df <- raw[-c(1,2), ]

# Set proper column names
names(df) <- c("Country", "ISO3", "Code")

names(df)
names(dta)

# rename so that get ISOcodes for Partner 
df <- df %>% rename(ExporterISO3 = ISO3)

dta <- left_join(dta, df, by = c("Partner" = "Code"))
colSums(is.na(dta))
test <- dta %>% filter(is.na(ExporterISO3))
unique(test$`Partner Name`)
dta <- dta %>%  mutate(    ExporterISO3 = if_else(`Partner Name` == "Sudan", "SDN", ExporterISO3)  )
dta <- dta %>%  filter(!is.na(ExporterISO3))

################################################################################


write_csv(dta_wide, "data/tariff_dta/CHN_import_tariffs/CHN_WITS_tariff_clean.csv")











