

################################################################################
#                              Create tau hat (CHINA tariffs on US)


# we obtain Chinese tariffs imposed on US  as well other countries and estimate the tau value 
# We create a log change in tariff vairable relative to 2017

################################################################################


library(readr)
library(tidyr)
library(dplyr)
library(data.table)
library(stringi)
library(fixest)
library(countrycode)
library(tidyverse)
library(vroom)
library(labelled)
library(haven)

################################################################################

rm(list=ls())
# Set directory
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git/")
getwd()

exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/tariff_dta"

################################################################################

# load data

dir_dta <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/Compare_values/yearly/robust/"
estimated_tau <- read_csv(paste0(dir_dta, "US_ln_NTMs_base_2017_FE_boot_hs4.csv"))
tariffs <- read_csv(paste0(dir_dta, "estimates_log_tariff_FE_boot.csv"))
# 
# teti <- read_csv("data/tariff_dta/teti/ROW_US_tariff_HS6_Teti.csv")
# ROW_US_MFN_tariff <- read_csv("data/tariff_dta/teti/US_tariff_on_ROW_yearly.csv")



################################################################################
# 1) for estimated chinese tariff rate 
################################################################################

quant <- 0.05

FE_q   <- quantile(estimated_tau$diff_ln_AVE_FE,        quant,      na.rm = TRUE)
FE_qH  <- quantile(estimated_tau$diff_ln_AVE_FE,        1 - quant,  na.rm = TRUE)
FE_b_q <- quantile(estimated_tau$diff_ln_AVE_FE_bench,  quant,      na.rm = TRUE)
FE_b_qH<- quantile(estimated_tau$diff_ln_AVE_FE_bench,  1 - quant,  na.rm = TRUE)
FE_w_q <- quantile(estimated_tau$diff_ln_AVE_FE_wmean,  quant,      na.rm = TRUE)
FE_w_qH<- quantile(estimated_tau$diff_ln_AVE_FE_wmean,  1 - quant,  na.rm = TRUE)

cat("FE [1%,99%]:", FE_q, FE_qH,"| Bench [1%,99%]:", FE_b_q, FE_b_qH, "| Wmean [1%,99%]:", FE_w_q, FE_w_qH, "\n")

estimated_tau1 <- estimated_tau %>%
  mutate(diff_ln_AVE_FE        = ifelse(diff_ln_AVE_FE        < FE_q   | diff_ln_AVE_FE        > FE_qH,  NA, diff_ln_AVE_FE),
         diff_ln_AVE_FE_bench  = ifelse(diff_ln_AVE_FE_bench  < FE_b_q | diff_ln_AVE_FE_bench  > FE_b_qH,NA, diff_ln_AVE_FE_bench),
         diff_ln_AVE_FE_wmean  = ifelse(diff_ln_AVE_FE_wmean  < FE_w_q | diff_ln_AVE_FE_wmean  > FE_w_qH,NA, diff_ln_AVE_FE_wmean)  )

names(estimated_tau1)
estimated_tau1 <- estimated_tau1 %>% group_by(year, hs2,hs4, ExporterISO3, ImporterISO3) %>%
  summarise(diff_ln_AVE_FE = mean(diff_ln_AVE_FE, na.rm = TRUE),
            diff_ln_AVE_FE_bench = mean(diff_ln_AVE_FE_bench, na.rm = TRUE),
            diff_ln_AVE_FE_wmean = mean(diff_ln_AVE_FE_wmean, na.rm = TRUE),
            diff_log_tariff_2017 = mean(diff_log_tariff_2017, na.rm = TRUE) )


estimated_tau1 <- estimated_tau1 %>%  filter(year %in% c(2018:2019))

colSums(is.na(estimated_tau1))


################################################################################
# Filter tariffs for USA exports (2018-2019)
################################################################################

unique(tariffs$ExporterISO3)

tariffs <- tariffs %>%
  filter(ExporterISO3 == "USA",
         year %in% 2018:2019) %>%
  rename(CHN_diff_log_tariff_2017 = diff_log_tariff_2017) %>%
  select(year, hs2, hs4, hs6_H5,
         ExporterISO3, ImporterISO3,
         hs_section, sector,
         diff_log_tariff_2015, CHN_diff_log_tariff_2017  )
colSums(is.na(tariffs))

################################################################################

# mereg NTMs and tariffs 
names(tariffs)
names(estimated_tau1)

estimated_tau2 <- full_join(estimated_tau1,tariffs )


estimated_tau3 <- estimated_tau2  %>% group_by(hs2,hs4,hs6_H5, ExporterISO3, ImporterISO3) %>%
  summarise(diff_ln_AVE_FE = mean(diff_ln_AVE_FE, na.rm = TRUE),
            diff_ln_AVE_FE_bench = mean(diff_ln_AVE_FE_bench, na.rm = TRUE),
            diff_ln_AVE_FE_wmean = mean(diff_ln_AVE_FE_wmean, na.rm = TRUE),
            CHN_diff_log_tariff_2017 = mean(CHN_diff_log_tariff_2017, na.rm = TRUE) )
colSums(is.na(estimated_tau2))

estimated_tau3_year <- estimated_tau2 %>% group_by(year, hs2,hs4,hs6_H5, ExporterISO3, ImporterISO3) %>%
  summarise(diff_ln_AVE_FE = mean(diff_ln_AVE_FE, na.rm = TRUE),
            diff_ln_AVE_FE_bench = mean(diff_ln_AVE_FE_bench, na.rm = TRUE),
            diff_ln_AVE_FE_wmean = mean(diff_ln_AVE_FE_wmean, na.rm = TRUE),
            CHN_diff_log_tariff_2017 = mean(CHN_diff_log_tariff_2017, na.rm = TRUE) )
colSums(is.na(estimated_tau2))
class(estimated_tau2$hs6_H5)



# concord for product code 
library(concordance)
estimated_tau3 <- estimated_tau3 %>%  mutate(hs6_H4 = concord(hs6_H5, origin = "HS5", destination = "HS4", dest.digit = 6, all = FALSE))
colSums(is.na(estimated_tau3))

any(duplicated(estimated_tau3$hs6_H4))

estimated_tau2_h4 <- estimated_tau3 %>%
  group_by(hs2, hs4, hs6_H4, ExporterISO3, ImporterISO3) %>%
  summarise(
    diff_ln_AVE_FE = mean(diff_ln_AVE_FE, na.rm = TRUE),
    diff_ln_AVE_FE_bench = mean(diff_ln_AVE_FE_bench, na.rm = TRUE),
    diff_ln_AVE_FE_wmean = mean(diff_ln_AVE_FE_wmean, na.rm = TRUE),
    CHN_diff_log_tariff_2017 = mean(CHN_diff_log_tariff_2017, na.rm = TRUE),
    .groups = "drop"  )


# concord for product code 
library(concordance)
estimated_tau3_year <- estimated_tau3_year %>%  mutate(hs6_H4 = concord(hs6_H5, origin = "HS5", destination = "HS4", dest.digit = 6, all = FALSE))
colSums(is.na(estimated_tau3_year))

any(duplicated(estimated_tau3_year$hs6_H4))

estimated_tau2_h4_year <- estimated_tau3_year %>%
  group_by(year, hs2, hs4, hs6_H4, ExporterISO3, ImporterISO3) %>%
  summarise(
    diff_ln_AVE_FE = mean(diff_ln_AVE_FE, na.rm = TRUE),
    diff_ln_AVE_FE_bench = mean(diff_ln_AVE_FE_bench, na.rm = TRUE),
    diff_ln_AVE_FE_wmean = mean(diff_ln_AVE_FE_wmean, na.rm = TRUE),
    CHN_diff_log_tariff_2017 = mean(CHN_diff_log_tariff_2017, na.rm = TRUE),
    .groups = "drop"  )


# get tariffs applied to U.S.
write_csv(estimated_tau2_h4,  paste0("data/created_exposure/tau/import_CHN_tau_", quant, ".csv"))
write_csv(estimated_tau2_h4_year,  paste0("data/created_exposure/tau/import_CHN_tau_year_", quant, ".csv"))
# 
# 
# ################################################################################
# # 2) for ROW on US tariff rate
# ################################################################################
# 
# names(teti)
# unique(teti$exporter)
# unique(teti$importer)
# 
# 
# # aggregate at the yearly level
# teti <- teti %>% group_by(importer, exporter, hs6, year, nomenclature) %>%
#   summarise(tariff = mean(tariff, na.rm =  TRUE ))
# # keep only exporter where tariff changes
# teti <- teti %>%
#   group_by(importer, exporter, hs6) %>%
#   filter(n_distinct(tariff) > 1) %>%    # keep only groups where tariff varies over time
#   ungroup()
# unique(teti$importer)
# 
# 
# 
# 
# 
# # create log of tariff
# merge <- merge %>%
#   mutate(across( c(US_tariff, US_tariff_2015, US_tariff_2017), ~ . / 100  ),
#          across(c(US_tariff, US_tariff_2015, US_tariff_2017), ~ log(1 + .),   .names = "ln_{.col}")  )
# 
# summary(merge)
# 
# # create log change of tariff
# 
# merge <- merge %>% mutate(change_log_tariff_2015 = ln_US_tariff - ln_US_tariff_2015,
#                           change_log_tariff_2017 = ln_US_tariff - ln_US_tariff_2017)
# names(merge)
# merge <- merge %>% select(hs6, year, ExporterISO3 , change_log_tariff_2015, change_log_tariff_2017) %>%
#   rename(US_change_log_tariff_2015 = change_log_tariff_2015, US_change_log_tariff_2017 = change_log_tariff_2017)
# 



