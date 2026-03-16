

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

rm(list=ls())


# Set directory
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git/data")
getwd()


################################################################################


library(readxl)

dta <- read_excel("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/Census_output/ag_census.xlsx")
names(dta)

dta$NAICS <- as.numeric(gsub(".*\\(([0-9]+)\\).*", "\\1", dta$NAICS_description))


library(stringr)
dta$NAICS <- as.numeric(  str_extract(dta$NAICS_description, "\\d+(?=\\)$)"))
unique(dta$NAICS_description)
dta <- dta %>%  mutate(NAICS_label = if_else(NAICS_description == "Total", "ag_tot", as.character(NAICS)))
unique(dta$NAICS_label)


write_csv(dta, "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/Census_output/ag_census_clean.csv")

#################################################################################

unique(nchar(dta$NAICS))
unique(dta$NAICS_description)
names(dta)
unique(dta$NAICS)
# need to rematch some of the numebr 
# Create a mapping of NAICS codes


# 111110 - Soybean Farming → 11111
# 111120 - Oilseed (except Soybean) Farming → 11112
# 111130 - Dry Pea and Bean Farming → 11113
# 111140 - Wheat Farming → 11114
# 111150 - Corn Farming → 11115
# 111160 - Rice Farming → 11116
# 111199 - All Other Grain Farming → 11119
# 111411 - Mushroom Production → 111211
# 111910 - Tobacco Farming → 11191
# 111920 - Cotton Farming → 11192
# 111930 - Sugarcane Farming → 11193
# 111940 - Hay Farming → 11194
# 111991 - Sugar Beet Farming → 11199
# 111992 - Peanut Farming → 11199
# 111998 - All Other Miscellaneous Crop Farming → 11199
# 112210 - Hog and Pig Farming → 1122
# 112410 - Sheep Farming → 11241
# 112420 - Goat Farming → 11242
# 112511 - Finfish Farming and Fish Hatcheries → 1125
# 112512 - Shellfish Farming → 1125
# 112910 - Apiculture → 11291
# 112930 - Fur-Bearing Animal and Rabbit Production → 11293
# 112990 - All Other Animal Production → 11299


# Create a mapping of NAICS codes
naics_mapping <- c(
  "11111" = "111110",
  "11112" = "111120",
  "11113" = "111130",
  "11114" = "111140",
  "11115" = "111150",
  "11116" = "111160",
  "11119" = "111199",
  "11191" = "111910",
  "11192" = "111920",
  "11193" = "111930",
  "11194" = "111940",
  "1122" = "112210",
  "11241" = "112410",
  "11242" = "112420",
  "11291" = "112910",
  "11293" = "112930",
  "11299" = "112990"
)

# Convert dta$NAICS to character vector
dta$NAICS <- as.character(dta$NAICS)

# Replace the NAICS codes based on the mapping, leaving unmapped codes unchanged
dta$NAICS <- recode(dta$NAICS, !!!naics_mapping, .default = dta$NAICS)

#################################################################################

# Use NAICS 6 digit level
dta_NAICS6 <- dta %>% filter(nchar(NAICS) == 6)

names(dta_NAICS6)
dta_NAICS6 <- dta_NAICS6 %>% select(NAICS,NAICS_description, Total_in_1000dollars) %>%
  rename(Tot_output_1000dollars = Total_in_1000dollars, NAICS6_2012 = NAICS)%>%
  mutate(  year = 2012, 
           sector = "agriculture")  
write_csv(dta_NAICS6, "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/Census_output/ag_output_NAICS_6.csv")


# use NAICS 4 digit level 

dta_NAICS4 <- dta %>% filter(nchar(NAICS) == 4)

names(dta_NAICS4)
dta_NAICS4 <- dta_NAICS4 %>% select(NAICS,NAICS_description, Total_in_1000dollars) %>%
  rename(Tot_output_1000dollars = Total_in_1000dollars, NAICS4_2012 = NAICS)%>%
  mutate(  year = 2012, 
           sector = "agriculture")  
write_csv(dta_NAICS4, "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/Census_output/ag_output_NAICS_4.csv")


# use NAICS 3 digit level 

dta_NAICS3 <- dta %>% filter(nchar(NAICS) == 3)
dta_NAICS3 <- dta_NAICS3 %>% select(NAICS,NAICS_description, Total_in_1000dollars) %>%
  rename(Tot_output_1000dollars = Total_in_1000dollars,NAICS3_2012 = NAICS) %>%
  mutate(  year = 2012, 
           sector = "agriculture")  


write_csv(dta_NAICS3, "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/Census_output/ag_output_NAICS_3.csv")
