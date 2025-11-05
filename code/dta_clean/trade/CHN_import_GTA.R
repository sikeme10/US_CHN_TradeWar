
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

# export data 

write_csv(dta,  "data/trade/GTA_CHN_import/CHN_import_2015_2023.csv")



