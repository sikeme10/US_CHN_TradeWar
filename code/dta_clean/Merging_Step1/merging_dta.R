
################################################################################
#                      Merging data for gravity model


# merge trade dta with tariffs and gravity model 
# at HS6 product level 
# Chinese imports from exporting source countries 
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

################################################################################
# directory: 
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git")

exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_gravity/"

################################################################################
# 1) Load data 
################################################################################

# trade data 
trade <- read_csv("data/trade/GTA_CHN_import/CHN_import_2015_2020.csv")
names(trade)

# gravity data
gravity <- read_csv("data/gravity/clean_Gravity.csv")
names(gravity)

# worldbank data
worldbank <- read_csv("data/gravity/Worldbank_dta.csv")
names(worldbank)

# tariff data 
tariff <- read_csv("data/tariff_dta/trade_war_tariffs.csv")
names(tariff)
MFN <- read_csv("data/tariff_dta/CHN_import_tariffs/CHN_WITS_tariff_clean.csv")
names(MFN)

################################################################################
# 2) Check each data to merge them: get combination of product and partner to balance data
################################################################################

# a) check product codes 

length(unique(trade$`HS6 Code`))
length(unique(trade$hs6_H4))
length(unique(trade$hs6_H5))
length(unique(tariff$hs6_H4))
length(unique(tariff$hs6))
length(unique(MFN$hs6_H5))
length(unique(MFN$hs6_H4))

# get unique variation in productCode using HS 2017 revision: We are stcking with those productCode 
ProductCode_H5 <- unique(c(trade$hs6_H5))
ProductCode_H5 <- ProductCode_H5[!is.na(ProductCode_H5)]
any(is.na(ProductCode_H5))

# b) trade partners 
length(unique(trade$ExporterISO3))
unique(trade$`Trade Partner`)
length(unique(MFN$`Partner Name`))
unique(MFN$`Partner Name`)

# order partner based on traded volume 
partner_order <- trade %>%  group_by(ExporterISO3) %>%
  summarise(total_usd = sum(Trade_value_USD, na.rm = TRUE)) %>%
  arrange(desc(total_usd)) %>%
  pull(ExporterISO3)
partner_order

Partners <- partner_order[1:100]
# Check if present in MFN data 
in_mfn <- Partners %in% MFN$ExporterISO3
# Missing partners
missing <- Partners[!in_mfn]
missing
# get name of those countries 
test <- trade %>% filter(ExporterISO3 %in% missing)
unique(test$`Trade Partner`)
# Remove missing partners from Partners vector
Partners <- Partners[Partners %in% MFN$ExporterISO3]


# c) time 
table(trade$year)
table(trade$month)
trade$month <- as.numeric(trade$month)

################################################################################
# 3) Get Balanced data 
################################################################################

# change some of the variables names to harmonize
# get data of unique combination of Product-code, Partners, Year and month 
#  have to re balance the data so that unique observation for hs6", "year", "month", and exporter 

panel_balanced <- expand_grid(
  hs6_H5       = ProductCode_H5,
  year         = 2015:2020,
  month        = 1:12,
  ExporterISO3 = Partners,
  ImporterISO3 = "CHN")
table(panel_balanced$year)
table(panel_balanced$month)


# merge everything:
dups <- trade %>%  group_by(hs6_H5, year, month, ExporterISO3, ImporterISO3) %>%
  filter(n() > 1)
unique(dups$`Trade Partner`)
# drop duplicates for spain 
trade <- trade %>%  filter(!`Trade Partner` %in% c(  "Melilla",  "Canary Islands",
    "French Polynesia","Society Islands", "Tuamotu Islands","Gambier Islands",
    "Marquesas Islands", "Tubuai Islands","Ceuta","NL Antilles (Bonaire)",  "NL Antilles (Saba)"  ))
# drop NA as product code 
trade <- trade %>%filter(!is.na(hs6_H5))
colSums(is.na(trade))
dta <- left_join(panel_balanced, trade)

names(dta)
head(dta)
colSums(is.na(dta))

# refill trade values based on NAs 
dta <- dta %>% mutate(
  Trade_value_USD = if_else(is.na(Trade_value_USD), 0, Trade_value_USD))

dta <- dta %>%  group_by(hs6_H5, year, month, ExporterISO3) %>% 
  fill(ImporterISO3,    `HS2 Code`,  `HS4 Code`,`HS6 Code`, `HS6 Description`,
    hs6_H4,    .direction = "updown"  )


dta <- dta %>% group_by(hs6_H5) %>%   
  fill(ImporterISO3,  `HS2 Code`,  `HS4 Code`, `HS6 Code`,     `HS6 Description`,
           hs6_H4,   .direction = "updown"  )
table(dta$year)
table(dta$month)
colSums(is.na(dta))

# select variable of interest 
dta <- dta %>%  select(hs6_H5,year, month,ExporterISO3,  ImporterISO3,
  `HS6 Description`,  hs6_H4,  Trade_value_USD, `Unit Price`)


################################################################################
# 4) merge with MFN tariff data 
################################################################################

names(MFN)
colSums(is.na(MFN))

# select MFN data of interest
MFN <- MFN %>% select (year,ExporterISO3, hs6_H5, `Weighted Average_MFN`,
                           `Weighted Average_AHS`, `Weighted Average_PRF`) %>% 
  rename(Weighted_MFN = `Weighted Average_MFN`,  Weighted_AHS = `Weighted Average_AHS`)
MFN <- MFN %>% filter(!is.na(hs6_H5))


# checks
dups2 <- MFN %>%  group_by(hs6_H5, year, ExporterISO3)%>%  filter(n() > 1)
colSums(is.na(MFN))
colSums(is.na(dta))
unique(MFN$year)

# Join MFN tariff data and trade data 
dta1 <- left_join(dta, MFN)

#checks
# dups <- dta1 %>%  group_by(hs6_H5, year, month, ExporterISO3, ImporterISO3) %>%  filter(n() > 1)
colSums(is.na(dta1))


# fill up missing values 
# MFN tariffs are homogeneous across partners 
dta1 <- dta1 %>%  group_by(hs6_H5, year) %>%  fill( Weighted_MFN, .direction = "updown"  )
dta1 <- dta1 %>%  group_by(hs6_H5) %>% arrange(year) %>%  fill( Weighted_MFN, .direction = "updown"  )
dta1 <- dta1 %>%  group_by(hs6_H5,ImporterISO3) %>% arrange(year) %>%  fill( Weighted_AHS, .direction = "down"  )


# save data
write_csv(dta1, paste0(exp, "dta_CHN_gravity1.csv"))

################################################################################
# 5) Merge with gravity characteristics
################################################################################

# import back data 
dta1 <- read_csv(paste0(exp, "dta_CHN_gravity1.csv"))

names(dta1)
colSums(is.na(dta1))
names(gravity)
colSums(is.na(gravity))

# rename gravity data 
gravity <- gravity %>% rename(ExporterISO3 = PartnerISO3,  ImporterISO3 = ReporterISO3, 
                              Importer_GDP = gdp_d, Exporter_GDP = gdp_o, Exporter_wto = wto_o,
                              Exporter_eu = eu_o) %>% select(-country_id_o, -country_id_d, -eu_d, -wto_d)

dups2 <- gravity %>%  group_by(year, ExporterISO3)%>%  filter(n() > 1)
# remove duplicates by keeping first observation (non NA)
library(dplyr)

gravity <- gravity %>%  group_by(year, ExporterISO3, ImporterISO3) %>%
  summarise(across( c(contig, dist, comlang_off, Colonial_ties, Importer_GDP, Exporter_GDP, Exporter_wto, Exporter_eu),
      ~ { vals <- .x[!is.na(.x)]      # drop NAs
        if (length(vals) == 0) NA   # all NA → keep NA
        else vals[1]               # otherwise take first non-NA
      }    ),    .groups = "drop"  )


dta2 <- left_join(dta1, gravity)

names(dta2)
colSums(is.na(dta2))

################################################################################
# 5) Merge with worldbank characteristics
################################################################################

names(dta2)
colSums(is.na(dta2))
names(worldbank)
colSums(is.na(worldbank))


# rename gravity data 
worldbank <- worldbank %>% rename(ExporterISO3 = iso3c,  Exporter_GDP_current_USD = GDP_current_USD, 
                              Exporter_GDPperCap_current_USD = GDP_per_capita_current_USD ,
                              Exporter_Gross_Cap_formation_current_USD = Gross_Cap_formation_current_USD,
                              Exporter_Ag_land_K2 = Ag_land_K2, Exporter_Exchange_rate_LCU_per_USD = Exchange_rate_LCU_per_USD) %>% 
  select(-country,-iso2c)

# check duplicates
dups2 <- worldbank %>%  group_by(year, ExporterISO3)%>%  filter(n() > 1)
# remove duplicates by keeping non NA
worldbank <- worldbank %>% filter(!is.na(ExporterISO3))

# merge with trade data
dta2 <- left_join(dta2, worldbank)
names(dta2)
colSums(is.na(dta2))

# fill in some missing variables
dta2 <- dta2 %>%  group_by(ExporterISO3) %>% arrange(year) %>%  
  fill( Colonial_ties, Exporter_GDP_current_USD, Exporter_GDPperCap_current_USD,
        Exporter_Gross_Cap_formation_current_USD, Exporter_Ag_land_K2,
        Exporter_Exchange_rate_LCU_per_USD, .direction = "updown"  )


write_csv(dta2, paste0(exp, "dta_CHN_gravity2.csv"))

################################################################################
# 5) Merge with US tariffs
################################################################################

dta2 <- read_csv(paste0(exp, "dta_CHN_gravity2.csv"))


names(dta2)
colSums(is.na(dta2))
names(tariff)
colSums(is.na(tariff))


tariff <- tariff  %>% rename(hs6_H5 = hs6) %>% 
  select(hs6_H5, year, month, ExporterISO3,  ImporterISO3,
         teti_tariff_2,  fajgel_tariff_2)

# check duplicates
dups2 <- tariff %>%  group_by(hs6_H5, year,month, ExporterISO3)%>%  filter(n() > 1)
# remove duplicates by taking average tariff rates 
tariff <- tariff %>%  group_by(hs6_H5, year, month, ExporterISO3, ImporterISO3) %>%
  summarise(teti_tariff_2   = mean(teti_tariff_2, na.rm = TRUE),
    fajgel_tariff_2 = mean(fajgel_tariff_2, na.rm = TRUE),    .groups = "drop"  )
colSums(is.na(tariff))
# convert back product code in character
tariff <- tariff %>%
  mutate( hs6_H5 = as.character(hs6_H5),
          hs6_H5 = str_pad(as.character(hs6_H5), width = 6, side = "left", pad = "0") )
class(tariff$hs6_H5)  
unique(nchar(tariff$hs6_H5))

# filter NAs  
tariff <- tariff %>% filter(!is.na(hs6_H5))
unique(tariff$ImporterISO3)
tariff$ImporterISO3 <- "CHN"
tariff <- tariff %>% filter(!is.na( month))
colSums(is.na(tariff))


# merge with trade data
dta2 <- left_join(dta2, tariff)
names(dta2)
colSums(is.na(dta2))

################################################################################
# Tariffs
################################################################################

# create a tariff variable that takes the value of trade war tariffs for US china and MFN?AHS else

# for retaliatory tariff data: fill it so that takes the value of previous time when the value 
dta2 <- dta2 %>% 
  group_by(ImporterISO3, ExporterISO3, hs6_H5) %>%  # include ALL key vars
  arrange(year, month, .by_group = TRUE) %>%
  fill(teti_tariff_2, fajgel_tariff_2, .direction = "down") %>%  ungroup()
colSums(is.na(dta2))

# create final tariff variable
dta2 <- dta2 %>% 
  mutate( Applied_tariff = case_when( ExporterISO3 == "USA" ~ if_else(!is.na(teti_tariff_2),
                teti_tariff_2, min(Weighted_AHS, Weighted_MFN, na.rm = TRUE)),
                TRUE ~ min(Weighted_MFN, Weighted_AHS, na.rm = TRUE))) %>% ungroup()
summary(dta2$Applied_tariff)
colSums(is.na(dta2))


#fill tariff variable if different from USA 
# dta2 <- dta2 %>%
#   group_by(ExporterISO3, hs6_H5) %>%
#   arrange(year, month, .by_group = TRUE) %>%
#   mutate(
#     Applied_tariff = if (unique(ExporterISO3) != "USA") {
#       tidyr::fill(cur_data(), Applied_tariff, .direction = "updown")$Applied_tariff
#     } else { Applied_tariff}  ) %>%
#   ungroup()

  
test <- dta2 %>% filter(is.na(Applied_tariff))
unique(test$ExporterISO3)
table(test$year, test$ExporterISO3)

################################################################################
# checks
table(dta2$year)
table(dta2$month)
length(unique(dta2$hs6_H5))
length(unique(dta2$ExporterISO3))

################################################################################

# add product HS2 and HS4 
names(dta2)

################################################################################

# save final data
write_csv(dta2, paste0(exp, "dta_CHN_gravity_3.csv"))

################################################################################
dta2 <- read_csv(paste0(exp, "dta_CHN_gravity_3.csv"))








