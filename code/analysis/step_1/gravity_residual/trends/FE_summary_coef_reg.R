
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

################################################################################
# directory: 
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git")
exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/summary/prelim_reg/"

################################################################################

coefs <- read_csv( "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/summary/prelim_reg/gravity_pois_FE_coeff.csv")

################################################################################

names(coefs)


################################################################################

# get average of coefs estimated
coef_significant <- coefs %>% filter(p_value < 0.05) 




FE <- coef_significant %>% group_by(term) %>% 
  summarise( mean_estimates = mean(estimate, na.rm = TRUE),
             median_estimates = median(estimate, na.rm = TRUE),
             mean_se= mean(se, na.rm = TRUE),
             median_se = median(estimate, na.rm = TRUE),
             mean_pseudo_R2 = mean(pr2, na.rm = TRUE),
             median_pseudo_R2 = median(pr2, na.rm = TRUE))

write_csv(FE, paste0(exp, "summary_stat_coef_FE.csv"))




