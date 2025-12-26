


################################################################################
#                     Stochastic frontier regression anlaysis

# get summary stats of coefficients from regression
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

exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/stochastic/"

################################################################################

# Load data 

coef <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/output/stochastic/sfaR_coef_HS2_average_new.csv")
names(coef)

################################################################################

# get average of coefs estimated
coef_significant <- coef %>% filter(pvalue < 0.05) 


unique(coef$model)

TN <- coef_significant %>% filter(model == "TN") %>% group_by(term) %>% 
  summarise( mean_estimates = mean(estimate, na.rm = TRUE),
             median_estimates = median(estimate, na.rm = TRUE),
             mean_se= mean(se, na.rm = TRUE),
             median_se = median(estimate, na.rm = TRUE),
             mean_gamma = mean(gamma, na.rm = TRUE),
             share_significant_LR_test = sum(p_ineff < 0.05, na.rm = TRUE) / n(),
             .groups = "drop")

TN_tariff <- coef_significant %>% filter(model == "TN_muTariff") %>% group_by(term) %>% 
  summarise( mean_estimates = mean(estimate, na.rm = TRUE),
             median_estimates = median(estimate, na.rm = TRUE),
             mean_se= mean(se, na.rm = TRUE),
             median_se = median(estimate, na.rm = TRUE),
             mean_gamma = mean(gamma, na.rm = TRUE),
             share_significant_LR_test = sum(p_ineff < 0.05, na.rm = TRUE) / n(),
             .groups = "drop")


# tariffs <- coef %>% filter( term == "log_tariff")

write_csv(TN_tariff, paste0(exp, "summary_stat_coef_TN_tariff.csv"))
write_csv(TN, paste0(exp, "summary_stat_coef_TN.csv"))

################################################################################
