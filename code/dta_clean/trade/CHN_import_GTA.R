
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
library(concordance)

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
                     Year >= 2017 ~ concord(`HS6 Code`, origin = "HS5", destination = "HS4",dest.digit  = 6, all= FALSE),  TRUE ~ NA_character_),
  # hs6_H5 = case_when(Year >= 2017 ~ `HS6 Code`, Year < 2017 ~ concord(`HS6 Code`,origin = "HS4",destination = "HS5",dest.digit  = 6,all = FALSE),TRUE ~ NA_character_   )
  hs6_H5 =`HS6 Code` )

# dta <- dta %>%  mutate(hs6_H5_combined = concord( sourcevar   = `HS6 Code`,origin = "HS5",  destination = "HS5",   dest.digit  = 6, all= FALSE))

  
colSums(is.na(dta))
length(unique(dta$`HS6 Code`))
length(unique(dta$hs6_H5))
length(unique(dta$hs6_H5))
length(unique(dta$hs6_H4))

test <- dta %>% filter(Year >=2017)
length(unique(test$`HS6 Code`))
HS_to_H5 <- read_csv("data/crosswalk/HS_concordance/Concordance_HS_to_H5.CSV")
H5_to_H4 <- read_csv("data/crosswalk/HS_concordance/Concordance_H5_to_H4.CSV")
length(unique(H5_to_H4$`HS 2017 Product Code`))
HS2017_list <- unique(H5_to_H4$`HS 2017 Product Code`)
HS_list <- unique(HS_to_H5$`HS - Combined  Product Code`)

same_codes <- intersect(unique(dta$`HS6 Code`), HS_list)
length(same_codes)
diff_codes <- setdiff(unique(dta$`HS6 Code`), HS_list)
diff_codes <- setdiff(unique(dta$`HS6 Code`), HS2017_list)
length(diff_codes)

################################################################################
# rename some of the variables 

names(dta)
dta <- dta %>% rename(year = Year, month = Month, ExporterISO3 = PartnerISO3, Trade_value_USD = USD) %>% 
  mutate(ImporterISO3 = "CHN")
  



################################################################################
# select Partner:
unique(dta$`Trade Partner`)


# order partner based on traded volume 
partner_order <- dta %>%  group_by(`Trade Partner`) %>%
  summarise(total_usd = sum(Trade_value_USD, na.rm = TRUE)) %>%
  arrange(desc(total_usd)) %>%
  pull(`Trade Partner`)

################################################################################
# duplicates present 
# sometimes if unit is different provide different observations for  each type of unit 
names(dta)
unique(dta$`Trade Direction`)
# check duplicates 
dups <- dta %>%  group_by(year, month, Reporter,`Trade Direction`, `Trade Partner`,`Trade Partner ISO Code`,
                          `HS2 Code`,`HS4 Code`,`HS6 Code`,`HS6 Description`, ExporterISO3, hs6_H4, hs6_H5,
                          ImporterISO3) %>%  filter(n() > 1)

summary(dta$`Unit Price`)

dta1 <- dta %>% group_by(year, month, Reporter,`Trade Direction`, `Trade Partner`,`Trade Partner ISO Code`,
                        `HS2 Code`,`HS4 Code`,`HS6 Code`,`HS6 Description`, ExporterISO3, hs6_H4, hs6_H5,
                        ImporterISO3) %>% 
  summarise(
    Trade_value_USD = sum(Trade_value_USD, na.rm = TRUE),
    Unit_Price = mean(`Unit Price`, na.rm = TRUE),
        # Combine character values of Primary Units into one string separated by "/"
    `Primary Units` = paste(unique(`Primary Units`), collapse = "/"),
    .groups = "drop")
summary(dta1$Unit_Price)
test <- dta1 %>% filter(Unit_Price>1000)
dups <- dta1 %>%  group_by(year, month, Reporter,`Trade Direction`, `Trade Partner`,`Trade Partner ISO Code`,
                          `HS2 Code`,`HS4 Code`,`HS6 Code`,`HS6 Description`, ExporterISO3, hs6_H4, hs6_H5,
                          ImporterISO3) %>%  filter(n() > 1)

################################################################################
# if we want qunatity as a variable 

# if instead keep only kg 
# keep all data but remove the non-KG rows only when duplicates exist
dta2 <- dta %>%
  group_by(
    year, month, Reporter, `Trade Direction`, `Trade Partner`,
    `Trade Partner ISO Code`, `HS2 Code`, `HS4 Code`, `HS6 Code`,
    `HS6 Description`, ExporterISO3, hs6_H4, hs6_H5, ImporterISO3) %>%
  mutate(dup = n() > 1) %>%
  filter(!dup | (`Primary Units` == "KG")) %>%  
  select(-dup) %>% ungroup()

dups <- dta2 %>%  group_by(year, month, Reporter,`Trade Direction`, `Trade Partner`,`Trade Partner ISO Code`,
                          `HS2 Code`,`HS4 Code`,`HS6 Code`,`HS6 Description`, ExporterISO3, hs6_H4, hs6_H5,
                          ImporterISO3) %>%  filter(n() > 1)
dta2 <- dta2 %>% rename(Quantity = `Primary Quantity`, Unit_Price = `Primary Units`)



################################################################################
# export data 

write_csv(dta1,  "data/trade/GTA_CHN_import/CHN_import_2015_2020.csv")


write_csv(dta2,  "data/trade/GTA_CHN_import/CHN_import_quant_2015_2020.csv")

