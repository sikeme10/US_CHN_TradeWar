
################################################################################

# Get China imports dta from other countries to construct gravity dataframe


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

################################################################################


rm(list=ls())
# Set directory
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git")
getwd()


# 1) Load data

# multiple CSV data files and want to load each file, 
# then automatically rename each dataframe based on the Year column it contains

# a. List all your data files
files <- c( "data/trade/GTA_CHN_import/DataExtract.csv",
  "data/trade/GTA_CHN_import/DataExtract-2.csv",
  "data/trade/GTA_CHN_import/DataExtract-3.csv",
  "data/trade/GTA_CHN_import/DataExtract-4.csv",
  "data/trade/GTA_CHN_import/DataExtract-5.csv")

# 2. Create an empty list to store each dataset
data_list <- list()

# 3. Loop through each file
for (f in files) {
  # Read the file
  dta <- read_csv(f)
  
  # Extract the unique years
  years <- sort(unique(dta$Year))
  
  # Collapse them into one string
  year_str <- paste(years, collapse = "_")
  
  # Create a new variable name based on the years
  new_name <- paste0("dta_", year_str)
  
  # Assign the dataframe to that name (optional, for reference)
  assign(new_name, dta)
  
  # Add the dataframe to the list
  data_list[[new_name]] <- dta
  
  # Print confirmation
  message("Loaded dataframe: ", new_name)
}

# 4. Merge (bind) all dataframes in the list
dta <- bind_rows(data_list)


################################################################################

# 2) Checks: 

names(dta)
table(dta$Year)
table(dta$Month)
table(dta$Reporter)
unique(dta$`Trade Partner`)
table(dta$Year, dta$Month)
colSums(is.na(dta))

################################################################################

# get ISOCode 3 for partners 
unique(dta$`Trade Partner`)
unique(dta$`Trade Partner ISO Code`)

dta$PartnerISO3 <- countrycode(dta$`Trade Partner ISO Code`, 
                        origin = "iso2c", 
                        destination = "iso3c")
test <- dta %>% filter(is.na(PartnerISO3))
unique(test$`Trade Partner`)
unique(test$`Trade Partner`)

# For Namibia, have to give them manually:
dta <- dta %>% mutate(
  PartnerISO3 = if_else(`Trade Partner` == "Namibia", "NAM", PartnerISO3))

# we can drop the rest of the countries 
dta <- dta %>% filter(!is.na(PartnerISO3))
colSums(is.na(dta))

################################################################################
# select year of interest:
years <- c(2015:2020)
dta <- dta %>% filter(Year %in% years)
unique(dta$Year)

################################################################################
# get HS revision in line 
names(dta)
class(dta$`HS6 Code`)
unique(nchar(dta$`HS6 Code`))

dta <- dta %>%  mutate( 
  hs6_H4 = case_when(Year < 2017 ~ `HS6 Code`,
                     Year >= 2017 ~ concord_hs(`HS6 Code`, origin = "HS5", destination = "HS4",dest.digit  = 6, all= FALSE),  TRUE ~ NA_character_),
  hs6_H5 = case_when(Year >= 2017 ~ `HS6 Code`,
                     Year < 2017 ~ concord_hs(`HS6 Code`,origin = "HS4",destination = "HS5",dest.digit  = 6,all = FALSE),TRUE ~ NA_character_   )  )
  
colSums(is.na(dta))
length(unique(dta$`HS6 Code`))
length(unique(dta$hs6_H5))
length(unique(dta$hs6_H4))

test <- dta %>% filter(Year >=2017)
length(unique(test$`HS6 Code`))


HS4 <- data("hs5_hs4")


################################################################################
# rename some of the variables 

names(dta)
dta <- dta %>% rename(year = Year, month = Month)



################################################################################
# select Partner:
unique(dta$`Trade Partner`)


# order partner based on traded volume 
partner_order <- dta %>%  group_by(`Trade Partner`) %>%
  summarise(total_usd = sum(USD, na.rm = TRUE)) %>%
  arrange(desc(total_usd)) %>%
  pull(`Trade Partner`)

  
################################################################################
# export data 

write_csv(dta,  "data/trade/GTA_CHN_import/CHN_import_2015_2023.csv")



