

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
exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/Compare_values/yearly/robust/"

################################################################################


################################################################################
# 1) Load data 
################################################################################

# Load FE estimates 
fe <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/output/FE/yearly/gravity_pois_FE.csv")
fe_boot <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/output/FE/yearly/boot/gravity_pois_FE_boot_fixef")


################################################################################
# 1) merge the data from different estimation methods
################################################################################



# select data we want 
names(fe)
names(fe_boot)
colSums(is.na(fe))
nrow(fe_boot)/(nrow(fe)-1439475) 



fe <- fe %>% select(-FE, -residual)

# merge the two 
dta <- right_join(fe, fe_boot)

dta <- dta %>% select(year,hs2, hs4, hs6_H5, ExporterISO3, ImporterISO3,Trade_value_USD, Applied_tariff, draw, fe_id, FE)


# merge all data 


# create a log of tariff 
dta <- dta %>% mutate(log_tariff = log(1+Applied_tariff/100) )
summary(dta$log_tariff)
summary(dta$Applied_tariff)

# drop Nas
dta <- dta %>%  filter(!if_all(c(FE), is.na))



################################################################################
# add product level variables 
################################################################################

# check values:
summary(dta$FE)

# add HS section for each HS6 product code 
class(dta$hs6_H5)
unique(nchar(dta$hs6_H5))
unique(dta$hs2)
table(dta$hs2)

dta$hs2 <- as.numeric(dta$hs2)
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
table(dta$hs2)
table(dta$hs2, dta$hs_section)
names(dta)


################################################################################
# Changes in AVEs
################################################################################
# add FE country bechmarks:

# If we want to create a benchmark for each values 
# we take the max value of the FE and u among all exporter for a specific product, month, year
dta <- dta %>% group_by(year, draw, hs6_H5) %>%
  mutate( FE_bench = {
    m <- max(FE, na.rm = TRUE)
    ifelse(is.infinite(m), NA_real_, m)
  }
  ) %>%  ungroup()
summary(dta$FE_bench)

# we want to look at changes in FE and u relative to 2015 levels


# save values of FE in pre trade war period, but also for the benchmark part 
dta <- dta %>%  group_by(ExporterISO3, draw, hs6_H5) %>%
  mutate( FE_pre_2015 = mean(FE[year %in% c(2015)], na.rm = TRUE),
          FE_pre_2015_bench = mean(FE_bench[year %in% c(2015)], na.rm = TRUE),
          
          log_tariff_pre_2015 = mean(log_tariff[year %in% c(2015)], na.rm = TRUE)) %>%  ungroup()

summary(dta$log_tariff)

################################################################################
# Add elasticities
################################################################################

# add elasticities from chen et al.

dta <- dta %>% mutate(elasticities = case_when(sector == "Ag" ~  3 ,
                                               sector == "Manu" ~ 1.97,
                                               sector == "Other" ~ 5 ))

dta <- dta %>% arrange(year, hs6_H5)

################################################################################
# create differences in AVEs
################################################################################


# create adjusted values of efficiency and FE estimates
# do the difference with 2015 baseline 
dta <- dta %>%
  mutate(
    # Difference in FE relative to 2015 baseline
    diff_FE_2015 = if_else( year > 2015 & !is.na(FE) & !is.na(FE_pre_2015),
                            FE - FE_pre_2015,      NA_real_    ),
    
    # Difference in FE with benchmarks relative to 2015 baseline
    diff_FE_2015_bench = if_else( year > 2015 & !is.na(FE) & !is.na(FE_pre_2015),
                                  (FE - FE_bench) - (FE_pre_2015 - FE_pre_2015_bench),      NA_real_    ),
    
    # Difference in log tariffs relative to 2015 baseline
    diff_log_tariff_2015 = if_else(  year > 2015 & !is.na(log_tariff) & !is.na(log_tariff_pre_2015),
                                     log_tariff - log_tariff_pre_2015,      NA_real_    )  )

test  <- dta %>% filter(year >2015)
colSums(is.na(test))
summary(dta$diff_FE_2015)
summary(dta$diff_FE_2015_bench)

summary(dta$diff_log_tariff_2015)
################################################################################

dta <- dta %>%
  mutate(
    ln_AVE_FE = (1/(1-elasticities))*FE,
    diff_ln_AVE_FE =if_else(year %in% c(2016:2020), (1/(1-elasticities))*diff_FE_2015, NA) ,
    # with benchmarks:
    diff_ln_AVE_FE_bench =if_else(year %in% c(2016:2020), (1/(1-elasticities))*diff_FE_2015_bench, NA) 
  )
summary(dta$ln_AVE_FE)
names(dta)
colSums(is.na(dta))

################################################################################
# export results

write_csv(dta , paste0(exp, "estimates_reduced_form_base_2015_FE_boot.csv"))

dta <- read_csv( paste0(exp, "estimates_reduced_form_base_2015_FE_boot.csv"))
names(dta)

################################################################################
# get US data 
################################################################################


US <- dta  %>% filter(ExporterISO3 == "USA")

summary(US)
names(US)
unique(US$hs2)
unique(US$hs_section)
names(US)
# US1 <- US %>% filter(year %in% c(2018,2019))
write_csv(US, paste0(exp, "US_ln_NTMs_base_2015_FE_boot.csv"))


US <- read_csv(paste0(exp, "US_ln_NTMs_base_2015_FE_boot.csv"))









