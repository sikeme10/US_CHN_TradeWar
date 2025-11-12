





################################################################################
#                      Merging data for gravity model


# merge trade dta with tariffs and gravity model 
# at HS6 product level 
# chinese imports from exporting source countries 
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
library(countrycode)
library(Hmisc)
library(haven)
library(concordance)

################################################################################
# directory: 
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git")
################################################################################



# Load data 
fajgelbaum <- read_csv("data/tariff_dta/CHN_tariff_HS6_Fagel.csv")
teti <- read_csv("data/tariff_dta/CHN_tariff_HS6_Teti.csv")
MFN_WITS <- read_csv( "data/tariff_dta/CHN_import_tariffs/CHN_WITS_tariff_clean.csv")

names(fajgelbaum)
names(teti)
table(teti$nomenclature) 
names(MFN_WITS)
table(MFN_WITS$`Native Nomen`)

################################################################################
# Harmonize Variables across data 
################################################################################


# 1) rename some variables to get it harmonized 

MFN_WITS <- MFN_WITS %>% rename( year = `Trade Year`, hs6 = Product, NomenCode = `Native Nomen`)
teti <- teti %>% rename(ImporterISO3 = importer , ExporterISO3 = exporter ) %>% 
  mutate(NomenCode = "H5")

# make sure put US and CHina tariffs 
fajgelbaum <- fajgelbaum %>% mutate(ImporterISO3 = "CHN",  ExporterISO3 = "USA", NomenCode = "H5")


  
# data at different product concordance "HS4" (2012), and "HS5" (2017).
  
################################################################################

# 2) select year 

years <- c(2015:2020)

MFN_WITS <- MFN_WITS %>% filter(year %in% years)
teti <- teti %>% filter(year %in% years)
fajgelbaum <- fajgelbaum %>% filter(year %in% years)


################################################################################

# Compare teti and fajgelbaum 

names(teti)
names(fajgelbaum)

# check duplicates by variables;
any(duplicated(teti[c("hs6", "year", "month")]))
# have to average or take the max or the tariff rate 
library(dplyr)

teti <- teti %>%  group_by(    ImporterISO3,    ExporterISO3,    hs6,    year,
    month,    nomenclature,    NomenCode  ) %>%
  summarise(    tariff = max(tariff, na.rm = TRUE),    .groups = "drop"  )
unique(teti$year)


# check duplicates by variables;
any(duplicated(fajgelbaum[c("hs6", "year", "month")]))
unique(fajgelbaum$year)

# fajgelbaum in tariff percentage
fajgelbaum <- fajgelbaum %>%  select(-cty_name, -simple_x_stattariff1, -simple_x_stattariff2,
         -simple_x_mfn_tariff, -simple_x_increase) %>%
    # create new (scaled) variables and compute tariff_rate
  mutate(
    weighted_x_stattariff1 = weighted_x_stattariff1 * 100,
    weighted_x_stattariff2 = weighted_x_stattariff2 * 100,
    weighted_x_mfn_tariff  = weighted_x_mfn_tariff  * 100,
    weighted_x_increase    = weighted_x_increase    * 100,
    fajgel_tariff = pmin(weighted_x_stattariff1, weighted_x_mfn_tariff, na.rm = TRUE) +
      weighted_x_increase  )

teti <- teti %>% rename(teti_tariff= tariff)

trade_war_tariffs <- full_join(teti, fajgelbaum)
# check duplicates 
any(duplicated(trade_war_tariffs[c("hs6", "year", "month")]))


#  have to re balance the data so that unique observation for hs6", "year", "month"
length(unique(trade_war_tariffs$hs6))
unique(trade_war_tariffs$year)
unique(trade_war_tariffs$month)
panel_balanced <- trade_war_tariffs %>%
  expand(
    hs6   = unique(hs6),
    year  = sort(unique(year)),
    month = sort(unique(month))  )
length(unique(trade_war_tariffs$hs6))
table(panel_balanced$year)
table(panel_balanced$month)

# can merge it back with tariff data
trade_war_tariffs1 <- left_join(panel_balanced,trade_war_tariffs )


# checks 
names(trade_war_tariffs1)
unique(trade_war_tariffs1$nomenclature)
unique(trade_war_tariffs1$NomenCode)
# fill up values that are NAs;
trade_war_tariffs1 <- trade_war_tariffs1 %>%
  mutate(across(c(ImporterISO3, ExporterISO3, nomenclature, NomenCode), ~ {
    fill_value <- unique(na.omit(.x))
    if (length(fill_value) == 1) replace(.x, is.na(.x), fill_value) else .x
  }))
colSums(is.na(trade_war_tariffs1))
# for tariff data: fill it so that takes the value of previous time when the value 
trade_war_tariffs2 <- trade_war_tariffs1 %>% select(-mdate) %>%
  group_by(ImporterISO3, ExporterISO3, hs6, nomenclature, NomenCode,year) %>%  # include ALL key vars
  arrange(month, .by_group = TRUE) %>%
  fill(teti_tariff, fajgel_tariff, .direction = "down") %>%
  ungroup()
colSums(is.na(trade_war_tariffs2))


write_csv(trade_war_tariffs2, "data/tariff_dta/trade_war_tariffs.csv")

################################################################################

names(MFN_WITS)
length(unique(MFN_WITS$ExporterISO3))

# harmonize product code 

table(MFN_WITS$NomenCode,MFN_WITS$year)

# get data Hs6 for  revision and HS 5 revision
class(MFN_WITS$hs6)
unique(nchar(MFN_WITS$hs6))


MFN_WITS <- MFN_WITS %>%
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
# at yealy levle: MFN_WITS
# tariff trade war is at monthly level.. 


################################################################################
names(trade_war_tariffs2)
names(MFN_WITS)

trade_war_tariffs3 <- full_join(MFN_WITS, trade_war_tariffs2, by =  c(hs6_H5 = ))












