



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

# Load FE estimates 
fe <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/output/summary/prelim_reg/gravity_pois_FE.csv")
res <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/output/summary/prelim_reg/gravity_pois_residual.csv")
sf <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/output/stochastic/sfaR_efficiency_average_merged.csv")

################################################################################

# merge the data 


# select data we want 
names(fe)
names(res)
names(sf)


fe <- fe %>% select(year,month,hs2, hs4, hs6_H5, ExporterISO3, ImporterISO3,Trade_value_USD, Applied_tariff, fe_id, FE)
res <- res %>% select(year,month,hs2, hs4, hs6_H5, ExporterISO3, ImporterISO3,Applied_tariff, residual)
sf <- sf %>% select(year,month,hs2, hs4, hs6_H5, ExporterISO3, ImporterISO3,Applied_tariff, u,teJLMS,
                    u_tariff , teJLMS_tariff)

# merge all data 
library(dplyr)

# Full join all three datasets
dta <- fe %>%  full_join(res, by = c("year", "month", "hs2", "hs4", "hs6_H5",
                                     "ExporterISO3", "ImporterISO3", "Trade_value_USD","Applied_tariff")) %>%
  full_join(sf,  by = c("year", "month", "hs2", "hs4", "hs6_H5",
                        "ExporterISO3", "ImporterISO3", "Applied_tariff"))
colSums(is.na(dta))

# drop Nas
dta <- dta %>%  filter(!if_all(c(FE,  teJLMS), is.na))
write_csv(dta , paste0(exp, "estimates_reduced_form.csv"))


dta <- read_csv( paste0(exp, "estimates_reduced_form.csv"))


################################################################################
# # add some product level variables 
################################################################################

# check values:
summary(dta$residual)
summary(dta$FE)
summary(dta$u)

# add HS section for each HS6 product code 
class(dta$hs6_H5)
unique(nchar(dta$hs6_H5))
unique(dta$hs2)


dta <- dta %>% mutate(
  hs_section = case_when(
    hs2 %in% 1:5 ~ 1,
    hs2 %in% 6:14 ~ 2,
    hs2 %in% 15 ~ 3,
    hs2 %in% 16:24 ~ 4,
    hs2 %in% 25:27 ~ 5,
    hs2 %in% 28:38 ~ 6,
    hs2 %in% 39:40 ~ 7,
    hs2 %in% 41:43 ~ 8,
    hs2 %in% 44:46 ~ 9,
    hs2 %in% 47:49 ~ 10,
    hs2 %in% 50:63 ~ 11,
    hs2 %in% 64:67 ~ 12,
    hs2 %in% 68:70 ~ 13,
    hs2 %in% 71 ~ 14,
    hs2 %in% 72:83 ~ 15,
    hs2 %in% 84:85 ~ 16,
    hs2 %in% 86:89 ~ 17,
    hs2 %in% 90:92 ~ 18,
    hs2 %in% 93 ~ 19,
    hs2 %in% 94:96 ~ 20,
    hs2 %in% 97 ~ 21  ),
  sector = case_when(
    hs_section %in% 1:4   ~ "Ag",
    hs_section %in% 5:20  ~ "Manu",
    TRUE                  ~ "Other"))
table(dta$sector)
table(dta$hs_section)
names(dta)


################################################################################
# BENCHMARK VALUES : 2017 
################################################################################
# If we want to create a benchmark for each values 
# we take the max value of the FE and u among all exporter for a specific product, month, year
# add FE country bechmarks:

dta <- dta %>% group_by(year, month, hs6_H5) %>%
  mutate( FE_bench_exp = {
    m <- max(FE, na.rm = TRUE)
    ifelse(is.infinite(m), NA_real_, m)
  },
  u_bench_exp = {
    m <- min(u, na.rm = TRUE)
    ifelse(is.infinite(m), NA_real_, m)
  } ,
  u_tariff_bench_exp = {
    m <- min(u_tariff, na.rm = TRUE)
    ifelse(is.infinite(m), NA_real_, m)
  } ) %>%  ungroup()
summary(dta$FE_bench_exp)
summary(dta$u_bench_exp)
summary(dta$u_tariff_bench_exp)

# if we want to use pre 2017 as bechnmark 
dta <- dta %>%
  group_by(month, ExporterISO3, hs6_H5) %>%
  mutate( FE_pre_2017 = mean(FE[year %in% c(2015)], na.rm = TRUE),
          FE_pre_2017_bench = mean(FE_bench_exp[year %in% c(2015)], na.rm = TRUE),
          u_pre_2017 = mean(u[year %in% c(2015)], na.rm = TRUE),
          u_pre_2017_bench = mean(u_bench_exp[year %in% c(2015)], na.rm = TRUE),
          u_tariff_pre_2017 = mean(u_tariff[year %in% c(2015)], na.rm = TRUE),
          u_tariff_pre_2017_bench = mean(u_tariff_bench_exp[year %in% c(2015)], na.rm = TRUE),
          log_tariff_pre_2017 = mean(log_tariff[year %in% c(2015)], na.rm = TRUE)) %>%  ungroup()

dta <- dta %>%
  group_by(ExporterISO3, hs6_H5) %>%
  mutate( FE_pre_2017 = mean(FE[year %in% c(2015)], na.rm = TRUE),
          FE_pre_2017_bench = mean(FE_bench_exp[year %in% c(2015)], na.rm = TRUE),
          u_pre_2017 = mean(u[year %in% c(2015)], na.rm = TRUE),
          u_pre_2017_bench = mean(u_bench_exp[year %in% c(2015)], na.rm = TRUE),
          u_tariff_pre_2017 = mean(u_tariff[year %in% c(2015)], na.rm = TRUE),
          u_tariff_pre_2017_bench = mean(u_tariff_bench_exp[year %in% c(2015)], na.rm = TRUE),
          log_tariff_pre_2017 = mean(log_tariff[year %in% c(2015)], na.rm = TRUE)) %>%  ungroup()


################################################################################

# add elasticities from chen et al.

dta <- dta %>% mutate(elasticities = case_when(sector == "Ag" ~  3 ,
                                              sector == "Manu" ~ 1.97,
                                              sector == "Other" ~ 5 ))

dta <- dta %>% arrange(year, month, hs6_H5)

################################################################################

# create adjusted values of efficencies and FE estimates

base_year <- 2015

dta <- dta %>%
  mutate(
    # Difference in FE relative to 2017 baseline
    diff_FE_2017 = if_else( year > base_year & !is.na(FE) & !is.na(FE_pre_2017),
                            FE - FE_pre_2017,      NA_real_    ),
    
    # Difference in FE with benchmarks relative to 2017 baseline
    diff_FE_2017_bench = if_else( year > base_year & !is.na(FE) & !is.na(FE_pre_2017),
                                  (FE - FE_bench_exp) - (FE_pre_2017 - FE_pre_2017_bench),      NA_real_    ),
    
    # Difference in u (inefficiency)  relative to 2017 baseline
    diff_u_2017 = if_else(year > base_year & !is.na(u) & !is.na(u_pre_2017),
                          - u + u_pre_2017,      NA_real_    ),
    
    # Difference in u (inefficiency) with benchmarks relative to 2017 baseline
    diff_u_2017_bench = if_else(year > base_year & !is.na(u) & !is.na(u_pre_2017),
                                (- u + u_bench_exp) + (u_pre_2017 -  u_pre_2017_bench),      NA_real_    ),
    
    # Difference in tariff-adjusted u relative to 2017 baseline
    diff_u_tariff_2017 = if_else( year > base_year & !is.na(u_tariff) & !is.na(u_tariff_pre_2017),
                                  - u_tariff +  u_tariff_pre_2017 ,     NA_real_    ),
    
    # Difference in tariff-adjusted u with benchmarks relative to 2017 baseline
    diff_u_tariff_2017_bench = if_else( year > base_year & !is.na(u_tariff) & !is.na(u_tariff_pre_2017),
                                        (- u_tariff + u_tariff_bench_exp) +  (u_tariff_pre_2017 - u_tariff_pre_2017_bench ) ,     NA_real_    ),
    
    # Difference in log tariffs relative to 2017 baseline
    diff_log_tariff_2017 = if_else(  year > base_year & !is.na(log_tariff) & !is.na(log_tariff_pre_2017),
                                     log_tariff - log_tariff_pre_2017,      NA_real_    )  )
summary(dta$diff_FE_2017)
summary(dta$diff_u_2017)
summary(dta$diff_u_tariff_2017)



# estimate ln(1+T) and changes in ln(1+T)

dta <- dta %>%
  mutate(
    ln_AVE_FE = (1/(1-elasticities))*FE,
    ln_AVE_u = (1/(1-elasticities))*-u,
    ln_AVE_u_tariff = (1/(1-elasticities))*-u_tariff,
    diff_ln_AVE_FE =if_else(year %in% c(2016:2020), (1/(1-elasticities))*diff_FE_2017, NA) ,
    diff_ln_AVE_u =if_else(year %in% c(2016:2020), (1/(1-elasticities))*diff_u_2017, NA) ,
    diff_ln_AVE_u_tariff =if_else(year %in% c(2016:2020), (1/(1-elasticities))*diff_u_tariff_2017, NA),
    # with benchmarks:
    diff_ln_AVE_FE_bench =if_else(year %in% c(2016:2020), (1/(1-elasticities))*diff_FE_2017_bench, NA) ,
    diff_ln_AVE_u_bench =if_else(year %in% c(2016:2020), (1/(1-elasticities))*diff_u_2017_bench, NA) ,
    diff_ln_AVE_u_tariff_bench =if_else(year %in% c(2016:2020), (1/(1-elasticities))*diff_u_tariff_2017_bench, NA),
  )
summary(dta$ln_AVE_FE)
summary(dta$ln_AVE_u)
summary(dta$ln_AVE_u_tariff)
summary(dta$diff_ln_AVE_FE)
summary(dta$diff_ln_AVE_u)
summary(dta$diff_ln_AVE_u_tariff)

################################################################################

# # estimating AVEs:
# names(dta)
# dta <- dta %>%
#   mutate( AVE_FE = exp((1/(1-elastcities))*FE)-1,
#           AVE_sf = exp((1/(1-elastcities))*-u)-1,
#           AVE_sf_tariff = exp((1/(1-elastcities))*-u_tariff)-1)
# 
# # estimate AVEs in 2017
# dta <- dta %>%
#   group_by(month, ExporterISO3, hs6_H5) %>%
#   mutate(
#     AVE_FE_pre_2017_m = mean(AVE_FE[year %in% c(2016:2017)], na.rm = TRUE),
#     AVE_sf_pre_2017_m = mean(AVE_sf[year %in% c(2016:2017)], na.rm = TRUE),
#     AVE_sf_tariff_pre_2017_m = mean(AVE_sf_tariff[year %in% c(2016:2017)], na.rm = TRUE),
#     Applied_tariff_pre_2017_m = mean(Applied_tariff[year %in% c(2016:2017)], na.rm = TRUE)) %>%  ungroup()
# summary(dta)
# 
# # estimating change in AVEs and change in tariffs 
# dta <- dta %>% filter(year %in% c(2017,2020)) %>%
#   mutate( diff_AVE_FE = AVE_FE - AVE_FE_pre_2017_m ,
#           diff_AVE_sf = AVE_sf - AVE_sf_pre_2017_m ,
#           diff_AVE_sf_tariff =AVE_sf_tariff - AVE_sf_tariff_pre_2017_m,
#           diff_tariff = (Applied_tariff - Applied_tariff_pre_2017_m)/100)
# summary(dta)


write_csv(dta , paste0(exp, "estimates_reduced_form_robust.csv"))


################################################################################

# Plotting ln(1+AVE)

################################################################################
US <- dta  %>% filter(ExporterISO3 == "USA")
US <- US %>%  mutate(date = as.Date(paste(year, month, "01", sep = "-"))  ) 
summary(US)
colSums(is.na(US))

write_csv(US, paste0(exp, "US_AVE_NTMs_robust_time.csv"))
US <- read_csv(paste0(exp, "US_AVE_NTMs_robust_time.csv"))
