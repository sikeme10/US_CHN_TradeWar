


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
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/NAICS_6")
getwd()

exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/NAICS_6"

################################################################################
# read all files 

files <- list.files(pattern = "*.csv")

# Extract numeric prefix (first three digits)
file_nums <- as.numeric(sub("^([0-9]{3}).*$", "\\1", files))

# Filter for files between 111 and 423
# filtered_files <- files[file_nums >= 111 & file_nums <= 423]

all_data <- lapply(files, read.csv)
merged_data <- bind_rows(all_data)

################################################################################

unique(merged_data$industry_code)
length(unique(merged_data$industry_code))
unique(merged_data$area_fips)
length(unique(merged_data$area_fips))

################################################################################

# get employment data

names(merged_data)

summary(merged_data$annual_avg_emplvl)

# select data of interest:
merged_data1 <- merged_data %>% select(area_fips, own_code, industry_code, industry_title, year, area_title, disclosure_code,
                                       annual_avg_estabs_count, annual_avg_emplvl, total_annual_wages)
colSums(is.na(merged_data1))

###############################################################################

write_csv(merged_data1,  "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/QCEW_naics6digit_2012.csv")





