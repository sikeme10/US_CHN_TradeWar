
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

