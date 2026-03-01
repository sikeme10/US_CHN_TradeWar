




################################################################################
#                      Fajgelbaum tariff data

# https://www.aeaweb.org/articles?id=10.1257/aeri.20230094
# https://www.openicpsr.org/openicpsr/project/194689/version/V1/view?path=/openicpsr/194689/fcr:versions/V1/GTW_replication/data/processed/z_chus_w.dta&type=file

################################################################################

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
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)


################################################################################
# directory: 
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git")
################################################################################

# 1) Load data 
library(readxl)
dta_CHN <- read_excel("data/tariff_dta/Fajgelbaum/retaliatory_tariffs.xlsx", sheet = "china")
dta_CHN2 <- read_excel("data/tariff_dta/Fajgelbaum/retaliatory_tariffs.xlsx", sheet = "china_may2019")
dta_CHN3<- read_excel("data/tariff_dta/Fajgelbaum/retaliatory_tariffs_2019.xlsx", sheet = "china")

dta_CAN <- read_excel("data/tariff_dta/Fajgelbaum/retaliatory_tariffs.xlsx", sheet = "canada")
dta_MEX <- read_excel("data/tariff_dta/Fajgelbaum/retaliatory_tariffs.xlsx", sheet = "mexico")
dta_EU  <- read_excel("data/tariff_dta/Fajgelbaum/retaliatory_tariffs.xlsx", sheet = "eu")
dta_TUR <- read_excel("data/tariff_dta/Fajgelbaum/retaliatory_tariffs.xlsx", sheet = "turkey")
dta_IND <- read_excel("data/tariff_dta/Fajgelbaum/retaliatory_tariffs.xlsx", sheet = "india")
dta_RUS <- read_excel("data/tariff_dta/Fajgelbaum/retaliatory_tariffs.xlsx", sheet = "russia")


# for China get weights 

################################################################################
# Harmonize data first 


names(dta_CHN)
names(dta_CHN2)
names(dta_CAN)
names(dta_MEX)
names(dta_EU)
names(dta_TUR)
names(dta_IND)
names(dta_RUS)


# drop within those data if variable called retal_group

clean_trade_df <- function(df, iso_code) {
  
  # 1) Drop unwanted columns by pattern
  df <- df[, !grepl('^Source|^\\.\\.\\.|DOUBLE CHECK THREAT DATE',
                    names(df)), 
           drop = FALSE]
  
  # 2) Drop retal_group if present
  if ("retal_group" %in% names(df)) {
    df <- df[, names(df) != "retal_group", drop = FALSE]
  }
  
  # 3) Add ImporterISO3
  df$ImporterISO3 <- iso_code
  
  return(df)
}

dta_CHN <- clean_trade_df(dta_CHN, "CHN")
dta_CHN2 <- clean_trade_df(dta_CHN2, "CHN")
dta_CHN3 <- clean_trade_df(dta_CHN3, "CHN")
dta_CAN <- clean_trade_df(dta_CAN, "CAN")
dta_MEX <- clean_trade_df(dta_MEX, "MEX")
dta_EU  <- clean_trade_df(dta_EU,  "EU")
dta_TUR <- clean_trade_df(dta_TUR, "TUR")
dta_IND <- clean_trade_df(dta_IND, "IND")
dta_RUS <- clean_trade_df(dta_RUS, "RUS")

###############################################################################
# Chinese import tariffs on US 
###############################################################################
  
# first get Chinese retaliatory tariff data 
class(dta_CHN$hs8)
class(dta_CHN2$hs8)
dta_CHN2$hs8 <- as.numeric(dta_CHN2$hs8)

class(dta_CHN3$hs8)
dta_CHN3 <- dta_CHN3 %>% mutate(hs8 = str_remove_all(hs8, "\\."))
dta_CHN3$hs8 <- as.numeric(dta_CHN3$hs8)


 # join the two data 
CHN <- full_join(dta_CHN, dta_CHN2)
CHN <- full_join(CHN, dta_CHN3)

names(CHN)
CHN <- CHN %>% select(hs8, effective_date, ImporterISO3, tariff )

# get HS8 and create HS6 variables
class(CHN$hs8)
CHN$hs8 <- as.character(CHN$hs8)
unique(nchar(CHN$hs8))
CHN <- CHN %>%  mutate(hs8 = str_pad(hs8, width = 8, side = "left", pad = "0"))

# create hs6 level variable
CHN <- CHN %>%  mutate(hs6 = substr(hs8, 1, 6))

  
# for time create year and month variable 
library(lubridate)
class(CHN$effective_date)
CHN <- CHN %>% mutate(year  = year(effective_date),  month = month(effective_date))

names(CHN)
# need to aggregate at the month level: by taking the max of the value 
CHN <- CHN %>% group_by(ImporterISO3, hs6, hs8, year, month) %>%
  summarise(tariff = max(tariff, na.rm = TRUE), .groups = "drop"  )


# hs6 simple average tariff
CHN1 <- CHN %>% group_by(ImporterISO3, hs6, year, month) %>%
  summarise(tariff = mean(tariff, na.rm = TRUE), .groups = "drop"  )

write_csv(CHN1,"data/tariff_dta/Fajgelbaum/clean_CHN_tariff_hs6.csv")


#################################################################################
