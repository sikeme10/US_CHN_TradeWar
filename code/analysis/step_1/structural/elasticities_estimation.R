
################################################################################
#                     Structural model analysis 

# estimation of the elasticities 

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

################################################################################
# directory: 
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git")

exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_structural/"


################################################################################
# 1) Load data 
################################################################################
# dta <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_structural/dta_CHN_structural_quant_yearly.csv")


dta <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_structural/dta_CHN_structural_quant_yearly_drop0.csv")
names(dta)
colSums(is.na(dta))

################################################################################

# Put NA for 0 quantities ?
summary(dta$d_log_Trade_value_USD)
dta1 <- dta %>%
  mutate( d_log_Trade_value_USD = ifelse( d_log_Trade_value_USD == 0 , NA, d_log_Trade_value_USD ) )


# add HS section for each HS6 product code 
class(dta$hs6_H5)
dta <- dta %>% mutate( HS2 =  as.numeric(substr( as.character(hs6_H5) ,1,2) ))
unique(dta$hs2)


dta <- dta %>% mutate(
  HS_section = case_when(
    HS2 %in% 1:5 ~ 1,
    HS2 %in% 6:14 ~ 2,
    HS2 %in% 15 ~ 3,
    HS2 %in% 16:24 ~ 4,
    HS2 %in% 25:27 ~ 5,
    HS2 %in% 28:38 ~ 6,
    HS2 %in% 39:40 ~ 7,
    HS2 %in% 41:43 ~ 8,
    HS2 %in% 44:46 ~ 9,
    HS2 %in% 47:49 ~ 10,
    HS2 %in% 50:63 ~ 11,
    HS2 %in% 64:67 ~ 12,
    HS2 %in% 68:70 ~ 13,
    HS2 %in% 71 ~ 14,
    HS2 %in% 72:83 ~ 15,
    HS2 %in% 84:85 ~ 16,
    HS2 %in% 86:89 ~ 17,
    HS2 %in% 90:92 ~ 18,
    HS2 %in% 93 ~ 19,
    HS2 %in% 94:96 ~ 20,
    HS2 %in% 97 ~ 21  ),
  sector = case_when(
    HS_section %in% 1:4   ~ "Ag",
    HS_section %in% 5:20  ~ "Manu",
    TRUE                  ~ "Other"))
table(dta$sector)


####################################################################################################
# 2) regression 
################################################################################


sector_value = "Ag"
sub_dta <- dta %>% filter( sector == sector_value )

# a) For trade values 

# FE :  product-country fixed effect

reg_V <- feols( d_log_Trade_value_USD ~ d_log_Applied_tariff  | ExporterISO3^hs6_H5 , data = sub_dta )
reg_V
reg_V <- feols( d_log_Trade_value_USD ~ d_log_Applied_tariff  | hs6_H5 , data = sub_dta )
reg_V

b_V <- coef(reg_V)["d_log_Applied_tariff"]



# b) For Price 

reg_Q <- feols( d_log_Quantity ~ d_log_Applied_tariff  | ExporterISO3^hs6_H5 , data = sub_dta )
reg_Q
reg_Q <- feols( d_log_Quantity ~ d_log_Applied_tariff  | hs6_H5 , data = sub_dta )
reg_Q

b_Q <- coef(reg_Q)["d_log_Applied_tariff"]


# b) For Price 
reg_P <- feols( d_log_Unit_Price ~ d_log_Applied_tariff  | year + ExporterISO3^hs6_H5 , data = sub_dta )
reg_P
reg_P <- feols( d_log_Unit_Price ~ d_log_Applied_tariff  | hs6_H5 , data = sub_dta )
reg_P
b_P <- coef(reg_P)["d_log_Applied_tariff"]



################################################################################
# 2) estimate elasticities coefficients 
################################################################################

# supply elasticities
gamma <- b_Q / b_P
gamma

epsilon <- -b_Q / (1+b_P)
epsilon



################################################################################
# 2) in a loop for each sector 
################################################################################



library(dplyr)
library(fixest)

# Get all sectors
all_sectors <- sort(unique(dta$sector))

# Helper to safely extract the coefficient (returns NA if regression fails)
get_beta <- function(reg_obj, name = "d_log_Applied_tariff") {
  out <- tryCatch(coef(reg_obj)[name], error = function(e) NA_real_)
  return(out)}

# Empty results data.frame
results_sector <- data.frame(
  sector = character(),
  b_V    = numeric(),
  b_Q    = numeric(),
  b_P    = numeric(),
  gamma  = numeric(),
  epsilon = numeric(),
  stringsAsFactors = FALSE)

for (s in all_sectors) {
  
  cat("Running sector:", s, "\n")
  
  sub_dta <- dta %>% filter(sector == s)
  if (nrow(sub_dta) == 0) next
  
  ## 1) Trade values
  # (you can switch FE spec here if you prefer ExporterISO3^hs6_H5)
  reg_V <- feols(
    d_log_Trade_value_USD ~ d_log_Applied_tariff | hs6_H5,
    data = sub_dta )
  b_V <- get_beta(reg_V)
  
  ## 2) Quantity
  reg_Q <- feols(
    d_log_Quantity ~ d_log_Applied_tariff | hs6_H5,
    data = sub_dta )
  b_Q <- get_beta(reg_Q)
  
  ## 3) Price
  # again, change FE part if you want year + ExporterISO3^hs6_H5
  reg_P <- feols(
    d_log_Unit_Price ~ d_log_Applied_tariff | hs6_H5,
    data = sub_dta )
  b_P <- get_beta(reg_P)
  
  ## 4) Elasticities
  # protect against division by zero / NA
  if (is.na(b_Q) || is.na(b_P) || abs(b_P) < 1e-10) {
    gamma   <- NA_real_
    epsilon <- NA_real_
  } else {
    gamma   <- b_Q / b_P
    epsilon <- -b_Q / (1 + b_P)
  }
  
  ## 5) Store results
  results_sector <- rbind(
    results_sector,
    data.frame(
      sector  = s,
      b_V     = b_V,
      b_Q     = b_Q,
      b_P     = b_P,
      gamma   = gamma,
      epsilon = epsilon,
      stringsAsFactors = FALSE
    ) )}

results_sector



################################################################################
# 2) in a loop for each sector 
################################################################################


library(dplyr)
library(fixest)

# Get all HS sections
all_sections <- sort(unique(dta$HS_section))

# Helper: safely extract the coefficient (returns NA if regression fails)
get_beta <- function(reg_obj, name = "d_log_Applied_tariff") {
  out <- tryCatch(coef(reg_obj)[name], error = function(e) NA_real_)
  return(out)
}

# Empty results data.frame
results_HS_section <- data.frame(
  HS_section = character(),
  b_V        = numeric(),
  b_Q        = numeric(),
  b_P        = numeric(),
  gamma      = numeric(),
  epsilon    = numeric(),
  stringsAsFactors = FALSE
)

for (sec in all_sections) {
  
  cat("Running HS_section:", sec, "\n")
  
  sub_dta <- dta %>% filter(HS_section == sec)
  if (nrow(sub_dta) == 0) next
  
  ## 1) Trade values
  # (use your preferred FE; here: hs6_H5 FE as in your last code)
  reg_V <- feols(
    d_log_Trade_value_USD ~ d_log_Applied_tariff | hs6_H5,
    data = sub_dta
  )
  b_V <- get_beta(reg_V)
  
  ## 2) Quantity
  reg_Q <- feols(
    d_log_Quantity ~ d_log_Applied_tariff | hs6_H5,
    data = sub_dta
  )
  b_Q <- get_beta(reg_Q)
  
  ## 3) Price
  reg_P <- feols(
    d_log_Unit_Price ~ d_log_Applied_tariff | hs6_H5,
    data = sub_dta
  )
  b_P <- get_beta(reg_P)
  
  ## 4) Elasticities
  if (is.na(b_Q) || is.na(b_P) || abs(b_P) < 1e-10) {
    gamma   <- NA_real_
    epsilon <- NA_real_
  } else {
    gamma   <- b_Q / b_P
    epsilon <- -b_Q / (1 + b_P)
  }
  
  ## 5) Store results
  results_HS_section <- rbind(
    results_HS_section,
    data.frame(
      HS_section = sec,
      b_V        = b_V,
      b_Q        = b_Q,
      b_P        = b_P,
      gamma      = gamma,
      epsilon    = epsilon,
      stringsAsFactors = FALSE
    )
  )
}

results_HS_section


