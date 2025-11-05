
################################################################################
# we Chen et al NTB and tariff changes values at HS2 level

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


################################################################################

# load data
library(readxl)
dta <- read_excel("chen_NTB_tariff/hs2_agriculture_manufacturing.xlsx")

################################################################################
names(dta)
dta <- dta %>% rename(US_import_share = `U.S. Share of Imports`,
                      tau_NTB = `Δ Non-Tariff Barriers`,
                      tau_tariff_CHN = `Δ Tariff`)
names(dta)

# Put Partner and Isocdes for CHina

dta <- dta %>% mutate(Country = "China",
                      ISO3_Code = "CHN")





write_csv(dta, "chen_NTB_tariff/hs2_agriculture_manufacturing_clean.csv")

