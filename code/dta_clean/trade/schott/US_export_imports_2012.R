













###############################################################################

# Get trade data at yearly level for 2012 to estimate RET and IMP (calculate shares)
# code requires COuntryCode

###############################################################################

library(readr)
library(tidyr)
library(dplyr)
library(data.table)
library(stringi)
library(fixest)
library(countrycode)
library(tidyverse)
library(vroom)
library(stringr)
library(labelled)

rm(list=ls())
# Set directory
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git/data")
getwd()


###############################################################################

# Load data

# imports:
library(haven)
US_import <- read_dta("trade/schott/imp_detl_2012_12n.dta")
US_export <- read_dta("trade/schott/exp_detl_2012_12n.dta")

countryCodes <- read_csv("trade/schott/country_codes.csv")
product_concord <- read_table("trade/schott/hts_concordances_20190712_198906_201901.csv")


####################################################################################



# Merge country codes 
names(countryCodes)
names(US_import)

US_import <- left_join(US_import, countryCodes, by = c("cty_code" = "Country_Code") )
length(unique(US_import$cty_code))
unique(US_import$Country)
test <-US_import %>% filter(is.na(Country))
unique(test$cty_code)
US_import <- US_import %>% filter(!is.na(Country))

colSums(is.na(US_import))
US_export <- left_join(US_export, countryCodes, by = c("cty_code" = "Country_Code") )
length(unique(US_export$cty_code))
colSums(is.na(US_export))
test <-US_export %>% filter(is.na(Country))
unique(test$cty_code)
test <-US_export %>% filter(is.na( ISO_Code))
US_export <- US_export %>% filter(!is.na(Country))

###############################################################################
# FOR imports
###############################################################################

library(labelled)
var_label(US_import)

unique(US_import$month)
summary(US_import$gen_val_yr)

# imports value given by : gen_val_yr

US_import1 <- US_import %>%
  select(commodity, cty_code, year, gen_val_yr, sic, naics, Country, ISO_Code, ISO3_Code)

US_import1 <-US_import1 %>% group_by(commodity, year, cty_code, sic, naics, Country, ISO_Code, ISO3_Code) %>%
  summarise(import_val_USD = sum(gen_val_yr, na.rm = TRUE))
  

# change HS product code in character
unique(nchar(US_import1$commodity))
US_import1$commodity <- format(US_import1$commodity, scientific = FALSE)
US_import1$commodity <- str_pad(US_import1$commodity, width = 10, side = "left", pad = "0")
US_import1$commodity <- str_pad(as.character(US_import1$commodity), width = 10,
                                side = "left",    pad = "0")
US_import1 <- US_import1 %>% rename(HS10 = commodity) %>% 
  mutate(HS6 = substr(as.character(HS10), 1, 6))


# change NAICS code in character
unique(nchar(US_import1$naics))

US_import1 <- US_import1 %>% rename(naics6 = naics) %>% 
  mutate(naics4 = substr(as.character(naics6), 1, 4),
         naics3 = substr(as.character(naics6), 1, 3))
names(US_import1)

# aggregate at HS6 level:
# check dupliactaes
any(duplicated(US_import1[, c("year", "cty_code", "Country", "ISO_Code", "ISO3_Code", "HS10")]))

US_import1 <- US_import1 %>% group_by(year, cty_code,Country,ISO_Code,ISO3_Code,HS6 ) %>% 
  summarise(import_val_USD = sum(import_val_USD, na.rm = TRUE))


write_csv(US_import1, "trade/US_import_schott_2012.csv")
################################################################################
# get total import value 

US_tot_import <- US_import1 %>% group_by(year,HS6 ) %>% 
  summarise(
    import_val_USD = sum(import_val_USD, na.rm= TRUE)  )
length(unique(US_tot_import$HS6))


write_csv(US_tot_import, "trade/US_tot_import_schott_2012.csv")


################################################################################



###############################################################################
# FOR exports
###############################################################################


var_label(US_export)

unique(US_export$month)
summary(US_export$gen_val_yr)

# exports value given by : all_val_yr ("15-Digit Year-to-Date Total Value")

US_export1 <- US_export %>%
  select(commodity, cty_code, year, all_val_yr, sic, naics, Country, ISO_Code, ISO3_Code)

US_export1 <-US_export1 %>% group_by(commodity, year, cty_code, sic, naics, Country, ISO_Code, ISO3_Code) %>%
  summarise(export_val_USD = sum(all_val_yr, na.rm = TRUE))


# change HS product code in character
unique(nchar(US_export1$commodity))
US_export1$commodity <- format(US_export1$commodity, scientific = FALSE)
US_export1$commodity <- str_pad(US_export1$commodity, width = 10, side = "left", pad = "0")
US_export1$commodity <- str_pad(as.character(US_export1$commodity), width = 10,
                                side = "left",    pad = "0")
US_export1 <- US_export1 %>% rename(HS10 = commodity) %>% 
  mutate(HS6 = substr(as.character(HS10), 1, 6))
unique(nchar(US_export1$HS6))
class(US_export1$HS6)



# change NAICS code in character
unique(nchar(US_export1$naics))

US_export1 <- US_export1 %>% rename(naics6 = naics) %>% 
  mutate(naics4 = substr(as.character(naics6), 1, 4),
         naics3 = substr(as.character(naics6), 1, 3))

# aggregate at HS6 level:
# check dupliactaes
any(duplicated(US_export1[, c("year", "cty_code", "Country", "ISO_Code", "ISO3_Code", "HS10")]))

US_export1 <- US_export1 %>% group_by(year, cty_code,Country,ISO_Code,ISO3_Code,HS6 ) %>% 
  summarise(export_val_USD = sum(export_val_USD, na.rm = TRUE))

write_csv(US_export1, "trade/US_export_schott_2012.csv")
################################################################################
# get total export value 

US_tot_export <- US_export1 %>% group_by(year,HS6 ) %>% 
  summarise(
    export_val_USD = sum(export_val_USD, na.rm= TRUE)  )
length(unique(US_tot_export$HS6))
unique(nchar(US_tot_export$HS6))
class(US_tot_export$HS6)
write_csv(US_tot_export, "trade/US_tot_export_schott_2012.csv")


################################################################################
# get export and import concordance between product and NAICS 
################################################################################

export_NAICS_HS <- unique(US_export1[, c("naics3", "HS6")])
# Check which has multiple naics for an HS code
HS6_multi_naics <- export_NAICS_HS %>%  group_by(HS6) %>%  summarise(n_naics3 = n_distinct(naics3)) %>%  filter(n_naics3 > 1)
class(HS6_multi_naics$HS6)

write_csv(export_NAICS_HS, "trade/export_NAICS_HS_schott_naics3_2012.csv")

# for imports
import_NAICS_HS <- unique(US_import1[, c("naics3", "HS6")])
# Check which has multiple naics for an HS code
HS6_multi_naics <- import_NAICS_HS %>%  group_by(HS6) %>%  summarise(n_naics3 = n_distinct(naics3)) %>%  filter(n_naics3 > 1)

write_csv(import_NAICS_HS, "trade/import_NAICS_HS_schott_naics3_2012.csv")



export_NAICS_HS <- unique(US_export1[, c("naics4", "HS6")])
# Check which has multiple naics for an HS code
HS6_multi_naics <- export_NAICS_HS %>%  group_by(HS6) %>%  summarise(n_naics4 = n_distinct(naics4)) %>%  filter(naics4 > 1)
class(HS6_multi_naics$HS6)

write_csv(export_NAICS_HS, "trade/export_NAICS_HS_schott_naics4_2012.csv")

# for imports
import_NAICS_HS <- unique(US_import1[, c("naics4", "HS6")])
# Check which has multiple naics for an HS code
HS6_multi_naics <- import_NAICS_HS %>%  group_by(HS6) %>%  summarise(n_naics4 = n_distinct(naics4)) %>%  filter(naics4 > 1)

write_csv(import_NAICS_HS, "trade/import_NAICS_HS_schott_naics4_2012.csv")


################################################################################
