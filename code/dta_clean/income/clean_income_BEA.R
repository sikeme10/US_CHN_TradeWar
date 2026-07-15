
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
library(sfaR)
library(frontier)
library(ggplot2)


library(readr)
dta <- read_csv("BEA_personal_income/CAINC1__ALL_AREAS_1969_2024.csv")



names(dta)
length(unique(dta$GeoFIPS))
unique(dta$Description)


personal_income <- dta %>% filter(Description == "Personal income (thousands of dollars)") %>% 
  select(- c("Region", "TableName" , "LineCode",  "IndustryClassification") )
pop <- dta %>% filter(Description == "Population (persons) 1/") %>% 
  select(- c("Region", "TableName" , "LineCode",  "IndustryClassification") )
income_per_cap <- dta %>% filter(Description == "Per capita personal income (dollars) 2/") %>% 
  select(- c("Region", "TableName" , "LineCode",  "IndustryClassification") )



# pivot longer
personal_income_long <- personal_income %>%
  pivot_longer( cols = matches("^\\d{4}$"),  # selects columns that are 4-digit years
                names_to = "year",
                values_to = "personal_income_in1000USD"  ) %>%   
  mutate(year = as.integer(year))  %>% select(- c("Description", "Unit") )

pop_long <- pop %>%
  pivot_longer( cols = matches("^\\d{4}$"),  # selects columns that are 4-digit years
                names_to = "year",
                values_to = "pop_count"  ) %>%   
  mutate(year = as.integer(year))  %>% select(- c("Description", "Unit") )

income_per_cap_long <- income_per_cap %>%
  pivot_longer( cols = matches("^\\d{4}$"),  # selects columns that are 4-digit years
                names_to = "year",
                values_to = "income_per_cap_inUSD"  ) %>%   
  mutate(year = as.integer(year))  %>% select(- c("Description", "Unit") )s


dta_merge <- merge(personal_income_long, pop_long)
dta_merge <- merge(dta_merge, income_per_cap_long)


colSums(is.na(dta_merge))


write_csv(dta_merge, "BEA_personal_income/income_county.csv")


