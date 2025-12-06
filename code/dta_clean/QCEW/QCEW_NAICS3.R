


################################################################################
# we create two variables here: fraction of US industry i sold domestically

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




rm(list=ls())
# Set directory
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/NAICS_3")
getwd()

exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/NAICS_3"

################################################################################
# read all files 

files <- list.files(pattern = "*.csv")

# Extract numeric prefix (first three digits)
file_nums <- as.numeric(sub("^([0-9]{3}).*$", "\\1", files))

# Filter for files between 111 and 423
filtered_files <- files[file_nums >= 111 & file_nums <= 423]

all_data <- lapply(filtered_files, read.csv)
merged_data <- bind_rows(all_data)

################################################################################

unique(merged_data$industry_code)
# filter and get codes from 111 to 423

# merged_data <- merged_data %>% filter(industry_code > 102 & industry_code < 423)
unique(merged_data$industry_code)


################################################################################

# get employment data

names(merged_data)

summary(merged_data$annual_avg_emplvl)

# select data of interest:
merged_data1 <- merged_data %>% select(area_fips, own_code, industry_code, year, area_title, annual_avg_emplvl)

###############################################################################

# get total labor L
tot_labor <- merged_data1 %>% group_by(area_fips, year, area_title) %>% summarise(
  tot_annual_avg_emplvl = sum(annual_avg_emplvl, na.rm = TRUE))

merged_data2 <- left_join(merged_data1,tot_labor )


# get total labor by industry NAICS 3 digit code 
merged_data_sector <- merged_data2 %>%
  mutate( sector = case_when( industry_code %in% 111:115 ~ "Ag",
      industry_code %in% 311:339 ~ "Manu",
      TRUE ~ "NonAg"       )  )


tot_labor_NAICS3 <- merged_data_sector %>%
  group_by(area_fips, area_title, year) %>%
  summarise(empl_Ag     = sum(annual_avg_emplvl[sector == "Ag"], na.rm = TRUE),
    empl_Manu   = sum(annual_avg_emplvl[sector == "Manu"], na.rm = TRUE),
    empl_NonAg  = sum(annual_avg_emplvl[sector == "NonAg"], na.rm = TRUE)  )

merged_data2 <- left_join(merged_data2,tot_labor_NAICS3 )



merged_data2 <- merged_data2 %>% mutate(
  share_labor_ir = annual_avg_emplvl / tot_annual_avg_emplvl)
summary(merged_data2$share_labor_ir)

write_csv(merged_data2,  "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/clean_labor_share_2012.csv")











