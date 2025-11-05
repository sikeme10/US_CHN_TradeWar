




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
setwd("/data/sikeme/TRADE/NTM_trade_war/data")
getwd()

################################################################################

# for NAICS 3: 

# Load data
Ag_3 <- read_csv("Census_output/ag_output_NAICS_3.csv")
Min_3<- read_csv("Census_output/mining_output_NAICS3.csv")
Man_3 <- read_csv("Census_output/manufacturing_output_NAICS3.csv")



library(dplyr)

output_NAICS3 <- bind_rows(Ag_3, Min_3, Man_3)
output_NAICS3 <- output_NAICS3 %>% mutate(Geo = "US")

write_csv(output_NAICS3, "/data/sikeme/TRADE/NTM_trade_war/data/Census_output/output_NAICS_3.csv")

################################################################################

# for NAICS 4: 

# Load data
Ag_4 <- read_csv("Census_output/ag_output_NAICS_4.csv")
Min_4<- read_csv("Census_output/mining_output_NAICS4.csv")
Man_4 <- read_csv("Census_output/manufacturing_output_NAICS4.csv")



library(dplyr)

output_NAICS4 <- bind_rows(Ag_4, Min_4, Man_4)
output_NAICS4 <- output_NAICS4 %>% mutate(Geo = "US")

write_csv(output_NAICS4, "/data/sikeme/TRADE/NTM_trade_war/data/Census_output/output_NAICS_4.csv")











