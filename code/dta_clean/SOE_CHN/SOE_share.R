

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
         # is.na(EntNatNm) ~ NA_real_,
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
unique(dta_import_all$SOE)

colSums(is.na(dta_import_all))



# 
# test <- dta_import_all %>% filter(EntCd =="4111960144")
# length(unique(dta_import_all$EntNm))
# 
# # fill up for missing SOE or not 
# dta_import_all1 <- dta_import_all %>% group_by(EntCd) %>%
#   fill(all_of(c("EntNm", "EntNatCd", "EntNatNm")), .direction = "downup") %>%
#   ungroup()
# 
# colSums(is.na(dta_import_all))
# colSums(is.na(dta_import_all1))





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
# pick country of interest 
################################################################################




library(readr)
library(dplyr)
library(purrr)
library(stringr)
library(tidyr)
library(concordance)

build_trade_panel <- function(iso3_code,
                              trade_direction = "import",
                              data_dir        = "data/SOE_dta/2010") {
  
  files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)
  
  process_one <- function(path) {
    message("Processing: ", basename(path))
    
    read_csv(path, show_col_types = FALSE) %>%
      mutate(
        SOE = case_when(
          EntNatNm %in% c("state-owned enterprises", "collective enterprise") ~ 1,
          # is.na(EntNatNm) ~ NA_real_,
          TRUE ~ 0
        )
      ) %>%
      filter(ImpExpTypeNm == trade_direction, ISO3 == iso3_code) %>%
      group_by(EndDt, HSCd, HSNm, SOE) %>%
      summarise(
        Trade_value_USD = sum(USD, na.rm = TRUE),
        Quantity        = sum(Quantity, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(source_file = basename(path))
  }
  
  dta_all <- map_dfr(files, process_one)
  
  if (nrow(dta_all) == 0) {
    warning("No rows found for ISO3 == '", iso3_code,
            "' with direction '", trade_direction, "'.")
    return(dta_all)
  }
  
  # Aggregate to year level
  dta_all <- dta_all %>%
    mutate(Year = str_sub(EndDt, 1, 4)) %>%
    group_by(Year, HSCd, HSNm, SOE) %>%
    summarise(
      Trade_value_USD = sum(Trade_value_USD, na.rm = TRUE),
      Quantity        = sum(Quantity, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Aggregate to HS6
  dta_all <- dta_all %>%
    mutate(hs6 = str_sub(HSCd, 1, 6)) %>%
    group_by(Year, hs6, SOE) %>%
    summarise(
      Trade_value_USD = sum(Trade_value_USD, na.rm = TRUE),
      Quantity        = sum(Quantity, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Concord HS2007 (HS3) to HS2017 (HS5)
  dta_all$hs6_H5 <- concord_hs(
    sourcevar   = dta_all$hs6,
    origin      = "HS3",
    destination = "HS5",
    dest.digit  = 6,
    all         = FALSE
  )
  
  n_dropped <- sum(is.na(dta_all$hs6_H5))
  if (n_dropped > 0) {
    message("Dropping ", n_dropped, " rows with no HS2017 concordance.")
  }
  
  dta_all <- dta_all %>%
    filter(!is.na(hs6_H5)) %>%
    group_by(Year, hs6_H5, SOE) %>%
    summarise(
      Trade_value_USD = sum(Trade_value_USD, na.rm = TRUE),
      Quantity        = sum(Quantity, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Pivot wider by SOE status
  dta_wide <- dta_all %>%
    mutate(
      SOE = case_when(
        SOE == 1   ~ "SOE",
        SOE == 0   ~ "non_SOE"
      )
    ) %>%
    pivot_wider(
      names_from  = SOE,
      values_from = c(Trade_value_USD, Quantity),
      names_glue  = "{.value}_{SOE}",
      values_fill = 0
    )
  
  # Make sure all expected columns exist even if a category is absent
  expected_cols <- c(
    "Trade_value_USD_SOE", "Trade_value_USD_non_SOE", 
    "Quantity_SOE",        "Quantity_non_SOE"  )
  for (col in expected_cols) {
    if (!col %in% names(dta_wide)) dta_wide[[col]] <- 0
  }
  
  # Totals and shares
  dta_wide <- dta_wide %>%
    mutate(
      tot_Trade_value_USD = Trade_value_USD_SOE + Trade_value_USD_non_SOE ,
      tot_Quantity        = Quantity_SOE + Quantity_non_SOE ,
      share_value_SOE     = ifelse(tot_Trade_value_USD > 0,
                                   Trade_value_USD_SOE / tot_Trade_value_USD,
                                   NA_real_),
      ISO3            = iso3_code,
      trade_direction = trade_direction
    )
  
  dta_wide
}

USA_imports <- build_trade_panel("USA", trade_direction = "import")
CAN_imports <- build_trade_panel("CAN", trade_direction = "import")
BRA_imports <- build_trade_panel("BRA", trade_direction = "import")
AUS_imports <- build_trade_panel("AUS", trade_direction = "import")
JPN_imports <- build_trade_panel("JPN", trade_direction = "import")


write_csv(USA_imports, "data/SOE_dta/SOE_share_USA_2010.csv")
write_csv(CAN_imports, "data/SOE_dta/SOE_share_CAN_2010.csv")
write_csv(BRA_imports, "data/SOE_dta/SOE_share_BRA_2010.csv")
write_csv(AUS_imports, "data/SOE_dta/SOE_share_AUS_2010.csv")
write_csv(JPN_imports, "data/SOE_dta/SOE_share_JPN_2010.csv")


###############################################################################
# get total US import trade shares in 2010
###############################################################################
data_dir        = "data/SOE_dta/2010"
trade_direction = "import"



  
  files <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)
  
  process_one <- function(path) {
    message("Processing: ", basename(path))
    
    read_csv(path, show_col_types = FALSE) %>%
      mutate(
        SOE = case_when(
          EntNatNm %in% c("state-owned enterprises", "collective enterprise") ~ 1,
          # is.na(EntNatNm) ~ NA_real_,
          TRUE ~ 0
        )
      ) %>%
      filter(ImpExpTypeNm == trade_direction) %>%
      group_by(EndDt, HSCd, HSNm, ISO3) %>%
      summarise(
        Trade_value_USD = sum(USD, na.rm = TRUE),
        Quantity        = sum(Quantity, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(source_file = basename(path))
  }
  
  dta_all <- map_dfr(files, process_one)
  
# Aggregate to year level
  dta_all <- dta_all %>%    mutate(Year = str_sub(EndDt, 1, 4)) %>%
    group_by(Year, HSCd, HSNm, ISO3) %>%
    summarise(Trade_value_USD = sum(Trade_value_USD, na.rm = TRUE),
              Quantity        = sum(Quantity, na.rm = TRUE),
              .groups = "drop"    )
  
  # Aggregate to HS6
  dta_all_hs4 <- dta_all %>%
    mutate(hs6 = str_sub(HSCd, 1, 6),
           hs4 = str_sub(HSCd, 1, 4),) %>%
    group_by(Year, hs4, ISO3) %>%
    summarise(
      Trade_value_USD = sum(Trade_value_USD, na.rm = TRUE),
      Quantity        = sum(Quantity, na.rm = TRUE),
      .groups = "drop"    )
  colSums(is.na(dta_all_hs4))
 
  
  tot_trade <- dta_all_hs4 %>% group_by(Year, hs4)%>%
    summarise(
      tot_Trade_value_USD = sum(Trade_value_USD, na.rm = TRUE),
      tot_Quantity        = sum(Quantity, na.rm = TRUE))
    
    
  dta_all_hs4 <- left_join(dta_all_hs4, tot_trade)

  
 
  # Totals and US import shares 
  US_dta_hs4 <- dta_all_hs4 %>% filter(ISO3 == "USA") %>% 
    mutate(share_import_value = ifelse(tot_Trade_value_USD > 0, Trade_value_USD / tot_Trade_value_USD,
                                   NA_real_)   )
  summary(US_dta_hs4)
  


write_csv(US_dta_hs4, "data/SOE_dta/US_import_share_2010.csv")

