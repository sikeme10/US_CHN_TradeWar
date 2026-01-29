

################################################################################
#                    Gravity regression analysis: residual approach


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
library(sfaR)
library(frontier)
library(forcats)
library(polyglotr)
library(stringr)
library(labelled)
library(janitor)
library(readxl)
################################################################################


# directory: 
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git")
exp <- "data/SOE_dta/2010"


# 
# ################################################################################
# 
# # For each Data
# 
# ################################################################################
# ################################################################################
# #  1) Load data 
# ################################################################################
# 
# files_2010 <- list.files( "data/SOE_dta/2010", pattern = "\\.csv$", full.names = TRUE)
# 
# 
# 
# dta <- read_csv("data/SOE_dta/2010/201001_clean.csv")
# names(dta)
# 
# 
# ################################################################################
# #  2) Clean data 
# ################################################################################
# 
# #  create an SOE variable
# table(dta$EntNatNm)
# unique(dta$EntNatNm)
# dta <- dta %>% mutate(
#     SOE = case_when(EntNatNm %in% c("state-owned enterprises", "collective enterprise") ~ 1,
#                     is.na(EntNatNm)  ~ NA_real_,
#                     TRUE  ~ 0    )  )
# table(dta$SOE, useNA = "ifany")
# 
# 
# ################################################################################
# # select variable of interest 
# 
# 
# # filter import from China 
# unique(dta$ImpExpTypeNm)
# 
# dta_import <- dta %>% filter(ImpExpTypeNm == "import")
# 
# # filter US exports 
# dta_import <- dta_import %>% filter(ISO3 == "USA")
# 
# ################################################################################
# 
# 
# # look at imports per firms (SOE and Non-SOE)
# 
# 
# # checks 
# sum <- dta_import %>% group_by(SOE) %>% summarise(trade_value = sum(USD, na.rm = TRUE)) %>% 
#   mutate(total_trade_value = sum(trade_value, na.rm = TRUE), 
#          share = trade_value/total_trade_value)
# sum
# 
# # get import by product and 
# names(dta_import)
# 
# dta_import <- dta_import %>% group_by(Year,EndDt,HSCd,HSNm , SOE  ) %>% 
#   summarise(Trade_value_USD = sum(USD, na.rm = TRUE),
#             Quantity = sum(Quantity, na.rm =TRUE ))
# 

################################################################################

# In a loop

################################################################################


library(readr)
library(dplyr)
library(purrr)
library(stringr)

files_2010 <- list.files("data/SOE_dta/2010", pattern = "\\.csv$", full.names = TRUE)

# Function to process one file
process_one <- function(path) {
  message("Processing: ", basename(path))
  
  dta <- read_csv(path, show_col_types = FALSE)
  
  dta %>%
    mutate(
      SOE = case_when(
        EntNatNm %in% c("state-owned enterprises", "collective enterprise") ~ 1,
        is.na(EntNatNm) ~ NA_real_,
        TRUE ~ 0      )    ) %>%
    filter(ImpExpTypeNm == "import", ISO3 == "USA") %>%
    group_by(EndDt, HSCd, HSNm, SOE) %>%
    summarise(
      Trade_value_USD = sum(USD, na.rm = TRUE),
      Quantity        = sum(Quantity, na.rm = TRUE),
      .groups = "drop"    ) %>%
    mutate(source_file = basename(path))
}

# Loop over all files + bind
dta_import_all <- map_dfr(files_2010, process_one)

dta_import_all

################################################################################
# aggregate a importer and year level
################################################################################


names(dta_import_all)


# at year level
unique(dta_import_all$EndDt)
dta_import_all  <- dta_import_all %>% mutate(Year = str_sub(EndDt, 1,4))
unique(dta_import_all$Year)

# aggregate it:
dta_import_all  <- dta_import_all %>% group_by(Year, HSCd, HSNm,SOE) %>%
  summarise(Trade_value_USD = sum(Trade_value_USD, na.rm = TRUE),
            Quantity = sum(Quantity, na.rm =TRUE )  )
  


# at product level
class(dta_import_all$HSCd)
dta_import_all <- dta_import_all %>% mutate(hs6 =  str_sub(HSCd, 1,6))

# aggregate at HS 6 level:
names(dta_import_all)
dta_import <- dta_import_all %>% group_by(Year, hs6 , SOE  ) %>% 
  summarise(Trade_value_USD = sum(Trade_value_USD, na.rm = TRUE),
            Quantity = sum(Quantity, na.rm =TRUE ))
length(unique(dta_import$hs6))

# concord to HS2017
library(concordance)
library(concordance)

dta_import$hs6_H5 <- concord_hs(sourcevar   = dta_import$hs6,
                                origin = "HS3",   # HS3 = 2007
                                destination = "HS5",   # HS5 = 2017
                                dest.digit  = 6,
                                all         = FALSE)

colSums(is.na(dta_import))
length(unique(dta_import$hs6_H5))

# drop NAs
test <- dta_import %>% filter(is.na(hs6_H5))
dta_import <- dta_import %>% filter(!is.na(hs6_H5))

# aggregate again at HS2017 level:

dta_import <- dta_import %>% group_by(Year, hs6_H5, SOE  ) %>% 
  summarise(Trade_value_USD = sum(Trade_value_USD, na.rm = TRUE),
            Quantity = sum(Quantity, na.rm =TRUE )  )

################################################################################
# Pivot wider by SOE- non SOE and other
################################################################################
names(dta_import)

dta_import2 <- dta_import %>%
  mutate(SOE = case_when( SOE == 1        ~ "SOE",  SOE == 0        ~ "non_SOE",
                          is.na(SOE)      ~ "Unknown"    )  )

# 2) Pivot longer (edit the cols you want to stack)
dta_import2 <- dta_import2 %>%
  pivot_wider( names_from  = SOE,
    values_from = c(Trade_value_USD, Quantity),
    names_glue  = "{.value}_{SOE}",
    values_fill = 0  )

################################################################################
# get total quantity 

dta_import2 <- dta_import2 %>% mutate(
  tot_Trade_value_USD = Trade_value_USD_SOE + Trade_value_USD_non_SOE + Trade_value_USD_Unknown,
  tot_Quantity = Quantity_SOE + Quantity_non_SOE + Quantity_Unknown,
  share_value_SOE = Trade_value_USD_SOE / tot_Trade_value_USD)

################################################################################

write_csv(dta_import2, "data/SOE_dta/SOE_share_2010.csv")



################################################################################





