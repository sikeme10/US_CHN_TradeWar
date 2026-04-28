


################################################################################
# we create RET at County level

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
library(readr)
library(dplyr)
library(sf)
library(tigris)
library(ggplot2)
library(viridis)



rm(list=ls()); gc()
# Set directory
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git/")
getwd()



################################################################################

# U.Gross Output by Industry - Detail Level
# in [Millions of dollars]



NAICS <- read_csv( "data/crosswalk/2012_naics_description.csv")

################################################################################



library(readxl)
output_2016 <- read_excel("data/Census_output/BEA/output_1997-2024.xlsx", sheet = "Sheet1")
output_2024 <- read_excel("data/Census_output/BEA/output_1997-2024.xlsx", sheet = "Sheet2")

description <- read_excel("data/Census_output/BEA/output_1997-2024.xlsx", sheet = "IO_NAICS")

HS_NAICS <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/crosswalk/clean_HS6_naics6_2012.csv")
################################################################################


description <- description %>% select(Description_sector , Description_subsector,
                                      Detail,`Related 2017 NAICS Codes`, Description_NAICS) %>%
  rename(industry = Description_subsector)
names(description)


names(HS_NAICS)


HS_NAICS <- unique(HS_NAICS[, c("naics", "naics_description")])
length(unique(HS_NAICS$naics))



################################################################################
names(output_2016)



output_2016 <- output_2016 %>%   pivot_longer(cols = `1997`:`2016`,
               names_to = "year",
               values_to = "gross_output_millions_USD") %>% rename(industry = Description)
colSums(is.na(output_2016))
colSums(is.na(description))
output_2016 <- output_2016 %>% rename(industry_code  =`IO Code`)

# for post 2016
names(output_2024)

output_2024 <- output_2024 %>% select(-`Line (in billions)`)

output_2024 <- output_2024 %>%   pivot_longer(cols = `2017`:`2024`,
                                              names_to = "year",
                                              values_to = "gross_output_billions_USD")

################################################################################
names(HS_NAICS)
names(description)
sapply(HS_NAICS, class)
unique(nchar(HS_NAICS$naics))

HS_NAICS <- HS_NAICS %>% filter(!is.na(naics))
setDT(HS_NAICS)
HS_NAICS[, naics := as.character(naics)]
HS_NAICS[, `:=`(
  naics_3 = substr(naics, 1, 3),
  naics_4 = substr(naics, 1, 4),
  naics_5 = substr(naics, 1, 5)
)]



description <- description %>%
  rename(sector      = Description_sector,
         industry    = industry,           # no change needed
         industry_code      = Detail,
         naics_2017 = `Related 2017 NAICS Codes`,
         naics_descr  = Description_NAICS)
    
length(unique(description$naics_descr))
length(unique(HS_NAICS$naics_description))

############### match HS_naics to output level data naics 
setDT(description)

# Ensure character
description[, naics_2017 := as.character(naics_2017)]
HS_NAICS[, naics := as.character(naics)]

# Try each level, coarsening until a match is found
HS_NAICS[, naics_match := NA_character_]
HS_NAICS[, naics_match_level := NA_integer_]

desc_keys <- unique(description$naics_2017)

HS_NAICS[, naics_match_level := fcase(
  naics   %in% desc_keys, 6L,
  naics_5 %in% desc_keys, 5L,
  naics_4 %in% desc_keys, 4L,
  naics_3 %in% desc_keys, 3L,
  default = NA_integer_
)]

HS_NAICS[, naics_match := fcase(
  naics_match_level == 6L, naics,
  naics_match_level == 5L, naics_5,
  naics_match_level == 4L, naics_4,
  naics_match_level == 3L, naics_3,
  default = NA_character_
)]

# Now merge
result <- merge(HS_NAICS, description,
                by.x = "naics_match",
                by.y = "naics_2017",
                all.x = TRUE)

# Check what level most matches landed on
table(result$naics_match_level)

# unselect NAs:
unique(result$industry_code)
result1 <- result %>% filter(!is.na(industry_code))

include_industry <- unique(result1$industry)
include_industry_code <- unique(result1$industry_code)

result1 <- result1 %>% select(industry,industry_code, naics, naics_description )

write_csv(result1, "data/Census_output/output_level_analysis/NAICS_ouput_industry_maps.csv")

################################################################################
# check how match to output data 


################################################################################
unique(description$industry)
unique(output_2016$industry)
unique(output_2024$industry)

length(unique(description$industry))
length(unique(output_2016$industry))
length(unique(output_2024$industry))

# for 2016
intersect(unique(output_2016$industry), unique(description$industry))
length(intersect(unique(output_2016$industry), unique(description$industry)))


intersect(unique(output_2016$industry),include_industry)
length(intersect(unique(output_2016$industry),include_industry))


# for 2024
intersect(unique(output_2024$industry),include_industry)
length(intersect(unique(output_2024$industry),include_industry))



# keep only output that we have:
length(unique(output_2024$industry))
output_2024 <- output_2024 %>% filter(industry %in% include_industry)

length(unique(output_2016$industry))
length(unique(output_2016$industry_code))
output_2016 <- output_2016 %>% filter(industry %in% include_industry)



# harmoanize across the two data 

output_2016 <- output_2016 %>% select(industry, year,gross_output_millions_USD) %>%
  mutate(gross_output_USD = gross_output_millions_USD * 1e6) %>% select(-gross_output_millions_USD)

output_2024 <- output_2024 %>%
  mutate(gross_output_USD = gross_output_billions_USD * 1e9)%>% select(-gross_output_billions_USD)



output <- rbind(output_2016, output_2024)
write_csv(output, "data/Census_output/output_level_analysis/output_industry_1997_2024.csv")

##################################################################################







