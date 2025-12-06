



################################################################################
#                     Stochastic frontier regression anlaysis


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

exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/Compare_values/"

################################################################################
# 1) Load data 
################################################################################


dta <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/output/stochastic/sfaR_efficiency_average.csv")
dta_mu <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/output/stochastic/sfaR_efficiency_average_mu.csv")
names(dta)
colSums(is.na(dta))

################################################################################
# select US 

US <- dta %>% filter(ExporterISO3 == "USA")
US_mu <- dta_mu %>% filter(ExporterISO3 == "USA")

names(US)
names(US_mu)

US <- US %>% select(hs6_H5, year, month , log_tariff , u,teJLMS)

US_mu <- US_mu %>% select(hs6_H5, year, month , log_tariff , u,teJLMS) %>% rename(
  u_tariff = u, teJLMS_tariff = teJLMS)

US <- full_join(US, US_mu)
colSums(is.na(US))
################################################################################

# corrplot 

ggplot(US, aes(x = u, y = u_tariff)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme_minimal()

ggplot(US, aes(x = teJLMS, y = teJLMS_tariff)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme_minimal()

################################################################################

