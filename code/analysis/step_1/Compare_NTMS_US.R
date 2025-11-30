

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
exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/Compare_values/"

################################################################################
# 1) Load data 
################################################################################

trade <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/output/summary/prelim_reg/gravity_pois_FE.csv")
names(trade)
US_NTMs <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/output/Compare_values/US_ln_NTMs.csv")
names(US_NTMs)

################################################################################
# get share of export from US

trade <- trade %>% select(year,month,hs2, hs4, hs6_H5, ExporterISO3, ImporterISO3,Applied_tariff,Trade_value_USD)

total_exp <- trade %>% group_by(year,month,hs2, hs4, hs6_H5, ImporterISO3,) %>% 
  summarise( tot_Trade_value_USD = sum(Trade_value_USD, na.rm = TRUE))


US_trade <- trade %>% filter(ExporterISO3 == "USA") 
US_trade <- left_join(US_trade, total_exp)
names(US_trade)
colSums(is.na(US_trade))

# share of export <- 
US_trade <- US_trade %>% mutate(share_US_export = Trade_value_USD *100 / tot_Trade_value_USD )

# clean ntm data
US_NTMs <- US_NTMs %>% select(year,month,hs2, hs4, hs6_H5, ExporterISO3, ImporterISO3,
                              u, teJLMS , FE_benchmark_exporter , u_benchmark_exporter,
                              elastcities , ln_AVE_FE , ln_AVE_u)

# join NTm and trade data 
US <- left_join(US_trade, US_NTMs)
colSums(is.na(US))


# create a log of tariff variable 
summary(US$Applied_tariff)
US <- US %>% mutate(ln_tariff  = log(1+ Applied_tariff/100))
summary(US$ln_tariff)


################################################################################
# aggregate at the yearly level 

# average of tariffs and NTMs AVE 

US_yearly <- US %>% group_by(year,hs2, hs4, hs6_H5, ExporterISO3, ImporterISO3) %>% 
  summarise(Trade_value_USD =  sum(Trade_value_USD, na.rm = TRUE),
            tot_Trade_value_USD =  sum(tot_Trade_value_USD, na.rm = TRUE),
            ln_tariff =  mean(ln_tariff, na.rm = TRUE),
            ln_AVE_FE =  mean(ln_AVE_FE, na.rm = TRUE),
            ln_AVE_u =  mean(ln_AVE_u, na.rm = TRUE)    ) %>%
  mutate(across(everything(), ~ ifelse(is.nan(.), NA, .)))

colSums(is.na(US_yearly))

US_yearly_hs2 <- US_yearly %>% group_by(year,hs2, ExporterISO3, ImporterISO3) %>% 
  summarise(tot_Trade_value_USD_HS2 =  sum(tot_Trade_value_USD, na.rm = TRUE))
  
US_yearly <- left_join(US_yearly, US_yearly_hs2)
names(US_yearly)

US_yearly <- US_yearly %>% mutate(
  weights_HS2_CHN_export = tot_Trade_value_USD / tot_Trade_value_USD_HS2)
names(US_yearly)

# Aggregate at the HS2 level;
US_yearly_HS2 <- US_yearly %>% group_by(year,hs2, ExporterISO3, ImporterISO3) %>% 
  summarise(Trade_value_USD = sum(Trade_value_USD, na.rm = TRUE),
            tot_Trade_value_USD = sum(tot_Trade_value_USD, na.rm = TRUE),
            ln_tariff = mean(weights_HS2_CHN_export*ln_tariff, na.rm = TRUE),
            ln_AVE_FE = mean(weights_HS2_CHN_export*ln_AVE_FE, na.rm = TRUE),
            ln_AVE_u = mean(weights_HS2_CHN_export*ln_AVE_u, na.rm = TRUE)  )%>%
  mutate(across(everything(), ~ ifelse(is.nan(.), NA, .)))

US_yearly_HS2 <- US_yearly_HS2 %>% mutate(
  share_US_export = if_else(!is.na(tot_Trade_value_USD), Trade_value_USD/tot_Trade_value_USD, 0) )
names(US_yearly_HS2)    
table(US_yearly_HS2$year)


################################################################################
# Create lag variable to get change year by year 

# get lag variable  and duoble lag variables 

US_yearly_HS2 <- US_yearly_HS2 %>% 
  arrange(ExporterISO3, ImporterISO3, hs2, year) %>%   # make sure it's ordered
  group_by(ExporterISO3, ImporterISO3, hs2) %>% 
  mutate(
    # 1-year lags
    ln_tariff_lag        = lag(ln_tariff, 1),
    ln_AVE_FE_lag        = lag(ln_AVE_FE, 1),
    ln_AVE_u_lag         = lag(ln_AVE_u, 1),
    share_US_export_lag  = lag(share_US_export, 1),
    year_lag             = lag(year, 1),
    
    # 2-year lags (lag of lag)
    ln_tariff_lag2       = lag(ln_tariff, 2),
    ln_AVE_FE_lag2       = lag(ln_AVE_FE, 2),
    ln_AVE_u_lag2        = lag(ln_AVE_u, 2),
    share_US_export_lag2 = lag(share_US_export, 2),
    year_lag2            = lag(year, 2),
    
    # 1-year changes (only for consecutive years)
    d_ln_tariff = if_else(year - year_lag == 1, ln_tariff - ln_tariff_lag, NA_real_),
    d_ln_AVE_FE = if_else(year - year_lag == 1, ln_AVE_FE - ln_AVE_FE_lag, NA_real_),
    d_ln_AVE_u  = if_else(year - year_lag == 1, ln_AVE_u  - ln_AVE_u_lag, NA_real_),
    d_share_US_export = if_else(
      year - year_lag == 1,
      share_US_export - share_US_export_lag,
      NA_real_
    ),
    
    # 2-year changes (optional; delete if you don't need them)
    d2_ln_tariff = if_else(year - year_lag2 == 2, ln_tariff - ln_tariff_lag2, NA_real_),
    d2_ln_AVE_FE = if_else(year - year_lag2 == 2, ln_AVE_FE - ln_AVE_FE_lag2, NA_real_),
    d2_ln_AVE_u  = if_else(year - year_lag2 == 2, ln_AVE_u  - ln_AVE_u_lag2, NA_real_),
    d2_share_US_export = if_else(
      year - year_lag2 == 2,
      share_US_export - share_US_export_lag2,
      NA_real_
    )
  ) %>% 
  ungroup()

names(US_yearly_HS2)

# get import share for 2017 US export tp China
Import_share_2017 <- US_yearly_HS2 %>% select(hs2, ExporterISO3, ImporterISO3, year, share_US_export) %>%
  filter(year == 2017) %>% rename(share_US_export_2017 = share_US_export) %>% select(-year)

Change_tariffs_2018 <- US_yearly_HS2 %>% 
  select(    hs2,    ExporterISO3,    ImporterISO3,    year,
    d_ln_tariff,    d_ln_AVE_FE,    d_ln_AVE_u  ) %>%   filter(year == 2018) %>% 
  rename(  d_ln_tariff_2018_2017   = d_ln_tariff,  d_ln_AVE_FE_2018_2017   = d_ln_AVE_FE,
           d_ln_AVE_u_2018_2017    = d_ln_AVE_u) %>% select(-year)

Change_tariffs_2019 <- US_yearly_HS2 %>% 
  select(    hs2,    ExporterISO3,    ImporterISO3,    year,
             d2_ln_tariff,    d2_ln_AVE_FE,    d2_ln_AVE_u  ) %>%   filter(year == 2019) %>% 
  rename(d_ln_tariff_2019_2017   = d2_ln_tariff, d_ln_AVE_FE_2019_2017   = d2_ln_AVE_FE,
         d_ln_AVE_u_2019_2017    = d2_ln_AVE_u ) %>% select(-year)

Final_HS2 <- Import_share_2017 %>% 
  left_join(Change_tariffs_2018, by = c("hs2", "ExporterISO3", "ImporterISO3")) %>% 
  left_join(Change_tariffs_2019, by = c("hs2", "ExporterISO3", "ImporterISO3"))
names(Final_HS2)
Final_HS2$hs2 <- as.numeric(Final_HS2$hs2)


############### 
# merge with Chen data 
Chen <- read_csv("data/chen_NTB_tariff/hs2_agriculture_manufacturing_clean.csv")
names(Chen)
Chen <- Chen %>% select(-Country, - ISO3_Code) %>% rename(hs2 = HS2 , Chen_US_import_share =US_import_share,
                                                          Chen_tau_tariff_CHN = tau_tariff_CHN, 
                                                          Chen_tau_NTB = tau_NTB)
Final_HS3 <- left_join(Final_HS2,Chen)
  
write_csv(Final_HS3, paste0(exp, "Compare_NTM_chen.csv"))




