




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
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git/data")
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

write_csv(output_NAICS3, "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/Census_output/output_NAICS_3.csv")

################################################################################

# for NAICS 4: 

# Load data
Ag_4 <- read_csv("Census_output/ag_output_NAICS_4.csv")
Min_4<- read_csv("Census_output/mining_output_NAICS4.csv")
Man_4 <- read_csv("Census_output/manufacturing_output_NAICS4.csv")



library(dplyr)

output_NAICS4 <- bind_rows(Ag_4, Min_4, Man_4)
output_NAICS4 <- output_NAICS4 %>% mutate(Geo = "US")

write_csv(output_NAICS4, "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/Census_output/output_NAICS_4.csv")



################################################################################

# for NAICS 4: 

# Load data
Ag_6 <- read_csv("Census_output/ag_output_NAICS_6.csv")
Min_6<- read_csv("Census_output/mining_output_NAICS6.csv")
Man_6 <- read_csv("Census_output/manufacturing_output_NAICS6.csv")
Cons_6 <- read_csv("Census_output/construction_output_NAICS6.csv")
output_6 <- read_csv("Census_output/census_output_NAICS6.csv")

library(dplyr)

output_NAICS6 <- bind_rows(Ag_6, output_6)
# output_NAICS6 <- bind_rows(Ag_6, Min_6, Man_6,Cons_6)

names(output_NAICS6)
output_NAICS6 <- output_NAICS6 %>% select(-us)

# names(output_NAICS6)
# output_NAICS6 <- output_NAICS6 %>% 
#   mutate(NAICS_description =  if_else(is.na(NAICS_description),NAICS6_2012_descrpt , NAICS_description)) %>% 
#   select( -NAICS6_2012_descrpt) %>% relocate(year, Geo, sector, NAICS3_2012, NAICS4_2012, NAICS6_2012, NAICS_description)

output_NAICS6 <- output_NAICS6 %>%  mutate(NAICS4_2012 = substr(NAICS6_2012, 1, 4),
                      NAICS3_2012 = substr(NAICS6_2012, 1, 3))
length(unique(output_NAICS6$NAICS6_2012))

write_csv(output_NAICS6, "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/Census_output/output_NAICS_6.csv")




# test <- read_csv( "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/Census_output/output_NAICS_6.csv")



