




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
library(read_html)
library(html_nodes)
library(html_table)
library(rvest)
library(xml2)
library(magrittr)
library(tidyverse)

rm(list=ls())

################################################################################

setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git")

################################################################################
# 1) load 
################################################################################

dta <- read_csv("data/tariff_dta/CHN_import_tariffs/CHN_WITS_tariff.csv")
names(dta)

################################################################################
# 2) Arrange by Duty types 
################################################################################

# two duty types

unique(dta$DutyType)

dta <- dta %>%
  select(all_of(c(
    "Selected Nomen","Native Nomen","Reporter","Reporter Name",
    "Product","Product Name","Partner","Partner Name",
    "Tariff Year","Trade Year","Trade Source",
    "DutyType","Simple Average","Weighted Average"  )))

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
# 3) country isocodes
################################################################################


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
names(dta_wide)

# rename so that get ISOcodes for Partner 
df <- df %>% rename(ExporterISO3 = ISO3)

dta_wide <- left_join(dta_wide, df, by = c("Partner" = "Code"))
colSums(is.na(dta_wide))
test <- dta_wide %>% filter(is.na(ExporterISO3))
unique(test$`Partner Name`)
dta_wide <- dta_wide %>%  mutate(    ExporterISO3 = if_else(`Partner Name` == "Sudan", "SDN", ExporterISO3)  )
dta_wide <- dta_wide %>%  filter(!is.na(ExporterISO3))

################################################################################
# get Product codes at HS4 and HS5 revisions
################################################################################

names(dta_wide)
length(unique(dta_wide$ExporterISO3))

dta_wide <- dta_wide %>% rename( year = `Trade Year`, hs6 = Product, NomenCode = `Native Nomen`)

# harmonize product code 

table(dta_wide$NomenCode,dta_wide$year)

# get data Hs6 for  revision and HS 5 revision
class(dta_wide$hs6)
unique(nchar(dta_wide$hs6))


dta_wide <- dta_wide %>%
  mutate(
    # ensure 6-digit character HS codes
    hs6 = str_pad(as.character(hs6), 6, pad = "0"),
    # target HS4 view (6-digit codes concorded to HS4 basis)
    hs6_H4 = case_when(   NomenCode == "H4" ~ hs6,
                          NomenCode == "H5" ~ concord_hs(hs6, origin = "HS5", destination = "HS4",
                                                         dest.digit = 6, all = FALSE),  TRUE ~ NA_character_ ),
    hs6_H5 = case_when(   NomenCode == "H5" ~ hs6,
                          NomenCode == "H4" ~ concord_hs(hs6,  origin = "HS4", destination = "HS5",
                                                         dest.digit = 6, all = FALSE),    TRUE ~ NA_character_    )  )

colSums(is.na(dta_wide))
length(unique(dta_wide$hs6_H4))
length(unique(dta_wide$hs6_H5))

################################################################################
# export data 

write_csv(dta_wide, "data/tariff_dta/CHN_import_tariffs/CHN_WITS_tariff_clean.csv")











