


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
setwd("/data/sikeme/TRADE/NTM_trade_war/data/QCEW/NAICS_3")
getwd()

exp <- "/data/sikeme/TRADE/NTM_trade_war/data/QCEW/NAICS_3"

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

tot_labor <- merged_data1 %>% group_by(area_fips, year, area_title) %>% summarise(
  tot_annual_avg_emplvl = sum(annual_avg_emplvl, na.rm = TRUE))



merged_data2 <- left_join(merged_data1,tot_labor )

merged_data2 <- merged_data2 %>% mutate(
  share_labor_ir = annual_avg_emplvl / tot_annual_avg_emplvl)


write_csv(merged_data2,  "/data/sikeme/TRADE/NTM_trade_war/data/QCEW/clean_labor_share_2012.csv")











