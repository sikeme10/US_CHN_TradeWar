
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

exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_gravity/"

################################################################################
# 1) Load data 
################################################################################


dta <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_gravity/dta_CHN_gravity_3.csv")
names(dta)
colSums(is.na(dta))

################################################################################

# get HS2 and HS 4 level 
class(dta$hs6_H5)
unique(nchar(dta$hs6_H5))
dta <- dta %>%
  mutate(hs2 = substr( hs6_H5 ,1,2),
         hs4 = substr( hs6_H5 ,1,4))
length(unique((dta$hs2)))
length(unique((dta$hs4)))


names(dta)
dta <- dta %>%
  group_by(month, ExporterISO3, hs4) %>%
  mutate(
    log_Trade_value_USD = log(Trade_value_USD+1),
    log_Trade_value_USD_mean_15_17 = mean(log_Trade_value_USD[year %in% 2015:2017], na.rm = TRUE),
    log_Trade_value_USD_demeaned = log_Trade_value_USD - log_Trade_value_USD_mean_15_17  ) %>%
  ungroup()
summary(dta$log_Trade_value_USD_demeaned)

# create a time varibale 

################################################################################
HS2 <- unique(dta$hs2)[1]
dta2 <- dta %>% filter(hs2 == HS2)
colSums(is.na(dta2))

# try to do a log linearized specifcation with SFA 
y <- " log_Trade_value_USD_demeaned "
x <- c("contig","dist","comlang_off","Colonial_ties","Importer_GDP","Exporter_wto",
  "Exporter_eu","Exporter_GDP_current_USD",
  "Exporter_Gross_Cap_formation_current_USD", "Exporter_Ag_land_K2",
  "Exporter_Exchange_rate_LCU_per_USD","Applied_tariff")
FE <- c("factor(month)",  "factor(year)")


form_str <- paste0( "y ~ ", paste(c(x), collapse = " + "))

frontier_formula <- as.formula(form_str)
frontier_formula

mod_sfaR <- sfacross(
  formula   = log_Trade_value_USD_demeaned ~ contig + dist + comlang_off +Colonial_ties+
    Importer_GDP + Exporter_wto + Exporter_GDP_current_USD + Exporter_Ag_land_K2 +
    Applied_tariff ,
  data      = dta2,
  udist     = "hnormal",  # half-normal inefficiency
  S         = 1L,         # 1 for production-type frontier (more y = better)
  logDepVar = TRUE,       # dependent variable is in log (here log(y+1))
  method    = "bfgs")

eff_sfa <- efficiencies(mod_sfaR)
eff_sfa$u 
res_sfa <- residuals(mod_sfaR)


mod_sfaR <- sfacross(
  formula   = log_Trade_value_USD_demeaned ~ contig + dist + comlang_off +Colonial_ties+
    Importer_GDP + Exporter_wto + Exporter_GDP_current_USD + Exporter_Ag_land_K2 +
    Applied_tariff,
  data        = dta2,
  udist       = "hnormal",
  S           = 1L,
  logDepVar   = TRUE,
  method      = "nlminb",
  hessianType = 2L,       # <- key change
  printInfo   = TRUE
)

# residuals (v - u)
res_sfa <- residuals(mod_sfaR)
# (in-)efficiency measures
eff_sfa <- efficiencies(mod_sfaR)
dta2$u_ineff      <- eff_sfa$u





################################################################################

# Install from GitHub
install.packages("remotes")t
remotes::install_github("clemenshaerder/fepsfrontieR")
library(fepsfrontieR)

data("css_data")

# Estimate frontier
fm <- fepsfrontier(dta ~ x1 + x2, data = dta2, id = "i", time = "t")

# Show results
summary(fm)

# Efficiency
eff <- efficiency(fm)
plot(eff)




