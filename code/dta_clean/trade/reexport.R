
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
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git/data")
getwd()


################################################################################
library(readr)
dta <- read_csv("trade/re_export_2012.csv", skip = 3)
names(dta)


HS_product <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/HS_codes/HS_2012_2017_merged.csv")
names(HS_product)
length(unique(HS_product$HS_2012_Product_Code))
sapply(HS_product, class)


################################################################################



# create HS6 code 
dta$hs6 <- substr(dta$Commodity, 1, 6)
unique(dta$hs6)
length(unique(dta$hs6))
table(dta$`Domestic/Foreign`)

# get total export and rexport 
dta_wide <- dta %>%
  pivot_wider(names_from = `Domestic/Foreign`,
              values_from = `Value ($US)`,
              values_fill = 0  )
# put 0 if NA 



names(dta_wide)


dta_wide <- dta_wide %>% select(hs6, 'Domestic Exports', 'Foreign Exports') %>% 
  rename(domestic_export_USD = 'Domestic Exports', reexport_USD = 'Foreign Exports', HS6 = "hs6")

sum(dta_wide$HS6 %in% HS_product$HS_2012_Product_Code)





write_csv(dta_wide, "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/trade/re_export_clean_2012.csv")







