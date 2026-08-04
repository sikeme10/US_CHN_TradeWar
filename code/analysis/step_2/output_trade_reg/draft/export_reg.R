

################################################################################
# Code
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
library(readr)
library(dplyr)
library(sf)
library(tigris)
library(ggplot2)
library(viridis)


rm(list=ls()); gc()
# Set directory
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git/")
getwd()


################################################################################


dta <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/output_reg/merge_export.csv")

names(dta)
################################################################################


sub_dta <- dta %>% filter(year %in% c(2018:2019))
# without mining 
sub_dta$is_mining <- as.integer(grepl("mining", sub_dta$industry, ignore.case = TRUE))
test <- sub_dta %>% filter(is_mining == TRUE)
sub_dta <- sub_dta %>% filter(is_mining == FALSE)

reg1 <- feols(change_exp_ship_ratio ~   IMP_it_2017 + RET_i_tariff + RET_i_NTB_IV,
              data = sub_dta)
summary(reg1)
tbl <- etable(reg1)
tbl

reg1 <- feols(change_exp_ship_ratio ~ 0+ IMP_it_2017 + RET_i_tariff ,
              data = sub_dta,  cluster = ~industry)

reg2 <- feols(change_exp_ship_ratio ~ 0+ IMP_it_2017 + RET_i_tariff +RET_i_NTB_IV ,
              data = sub_dta, cluster = ~industry)


reg3 <- feols(change_exp_ship_ratio ~ 0+ (year):change_2016_2017 + as.factor(year):subsector + 
                IMP_it_2017 + RET_i_tariff ,
              data = sub_dta, cluster = ~industry)
reg4 <- feols(change_exp_ship_ratio ~ 0+ (year):change_2016_2017 + as.factor(year):subsector + 
                IMP_it_2017 + RET_i_tariff + RET_i_NTB,
              data = sub_dta, cluster = ~industry)

reg5 <- feols(change_exp_ship_ratio ~ 0+ (year):change_2016_2017 + as.factor(year):subsector + 
                IMP_it_2017 + RET_i_tariff + RET_i_NTB_IV,
              data = sub_dta, cluster = ~industry)


tbl <- etable(reg1, reg2,reg3, reg4,reg5)
tbl

################################################################################




# ---- Models without controls ----
m1 <- feols(change_exp_ship_ratio ~ 0 + IMP_it_2017 + RET_i_tariff,
            data = sub_dta)

m2 <- feols(change_exp_ship_ratio ~ 0 + IMP_it_2017 + RET_i_tariff + RET_i_NTB,
            data = sub_dta)

m3 <- feols(change_exp_ship_ratio ~ 0 + IMP_it_2017 + RET_i_tariff + RET_i_NTB_IV,
            data = sub_dta)

tbl_no_controls <- etable(
  m1, m2, m3,
  title = "Without controls",
  dict = c(IMP_it_2017 = "IMP",
           RET_i_tariff = "RET tariff",
           RET_i_NTB = "RET NTM",
           RET_i_NTB_IV = "RET NTM(IV)"),
  fitstat = ~ n + r2,
  digits = 3,
  digits.stats = 2,
  tex = FALSE
)

tbl_no_controls

# ---- Outcome trends only ----
m1 <- feols(change_exp_ship_ratio ~ 0 + IMP_it_2017 + RET_i_tariff +
              I(year - 2017):change_2016_2017,
            data = sub_dta)
m2 <- feols(change_exp_ship_ratio ~ 0 + IMP_it_2017 + RET_i_tariff + RET_i_NTB +
              I(year - 2017):change_2016_2017,
            data = sub_dta)
m3 <- feols(change_exp_ship_ratio ~ 0 + IMP_it_2017 + RET_i_tariff + RET_i_NTB_IV +
              I(year - 2017):change_2016_2017,
            data = sub_dta)

tbl_trends <- etable(
  m1, m2, m3,
  title = "Outcome trends only",
  dict = c(IMP_it_2017 = "IMP", RET_i_tariff = "RET tariff",
           RET_i_NTB = "RET NTM", RET_i_NTB_IV = "RET NTM(IV)"),
  fitstat = ~ n + r2, digits = 3, digits.stats = 2, tex = FALSE)
tbl_trends

# ---- Sector x year FE only ----
m4 <- feols(change_exp_ship_ratio ~ 0 + IMP_it_2017 + RET_i_tariff |
              year^subsector,
            data = sub_dta, cluster = ~industry)
m5 <- feols(change_exp_ship_ratio ~ 0 + IMP_it_2017 + RET_i_tariff + RET_i_NTB |
              year^subsector,
            data = sub_dta, cluster = ~industry)
m6 <- feols(change_exp_ship_ratio ~ 0 + IMP_it_2017 + RET_i_tariff + RET_i_NTB_IV |
              year^subsector,
            data = sub_dta, cluster = ~industry)

tbl_sectorFE <- etable(
  m4, m5, m6,
  title = "Sector x year FE only",
  dict = c(IMP_it_2017 = "IMP", RET_i_tariff = "RET tariff",
           RET_i_NTB = "RET NTM", RET_i_NTB_IV = "RET NTM(IV)"),
  fitstat = ~ n + r2, digits = 3, digits.stats = 2, tex = FALSE
)
tbl_sectorFE




# ---- Both controls ----
m7 <- feols(change_exp_ship_ratio ~ 0 + IMP_it_2017 + RET_i_tariff +
              I(year - 2017):change_2016_2017 |
              year^subsector+ year,
            data = sub_dta)
m8 <- feols(change_exp_ship_ratio ~ 0 + IMP_it_2017 + RET_i_tariff + RET_i_NTB +
              I(year - 2017):change_2016_2017 |
              year^subsector + year,
            data = sub_dta)
m9 <- feols(change_exp_ship_ratio ~ 0 + IMP_it_2017 + RET_i_tariff + RET_i_NTB_IV +
              I(year - 2017):change_2016_2017 |
              year^subsector + year,
            data = sub_dta)

tbl_both <- etable(
  m7, m8, m9,
  title = "Both controls",
  dict = c(IMP_it_2017 = "IMP", RET_i_tariff = "RET tariff",
           RET_i_NTB = "RET NTM", RET_i_NTB_IV = "RET NTM(IV)"),
  fitstat = ~ n + r2, digits = 3, digits.stats = 2,tex = FALSE
)
tbl_both



# ---- Both controls without year FE----
m7 <- feols(change_exp_ship_ratio ~ 0 + IMP_it_2017 + RET_i_tariff +
              I(year - 2017):change_2016_2017 |
              year^subsector,
            data = sub_dta)
m8 <- feols(change_exp_ship_ratio ~ 0 + IMP_it_2017 + RET_i_tariff + RET_i_NTB +
              I(year - 2017):change_2016_2017 |
              year^subsector ,
            data = sub_dta)
m9 <- feols(change_exp_ship_ratio ~ 0 + IMP_it_2017 + RET_i_tariff + RET_i_NTB_IV +
              I(year - 2017):change_2016_2017 |
              year^subsector ,
            data = sub_dta)

tbl_both <- etable(
  m7, m8, m9,
  title = "Both controls",
  dict = c(IMP_it_2017 = "IMP", RET_i_tariff = "RET tariff",
           RET_i_NTB = "RET NTM", RET_i_NTB_IV = "RET NTM(IV)"),
  fitstat = ~ n + r2, digits = 3, digits.stats = 2,tex = FALSE
)
tbl_both



# ---- Both controls ----
m6 <- feols(change_exp_ship_ratio ~ 0 + IMP_it_2017 + RET_i_tariff + RET_i_NTB,
            data = sub_dta)
m7 <- feols(change_exp_ship_ratio ~ 0 + IMP_it_2017 + RET_i_tariff + RET_i_NTB
               |year,
            data = sub_dta)
m8 <- feols(change_exp_ship_ratio ~ 0 + IMP_it_2017 + RET_i_tariff + RET_i_NTB +
              I(year - 2017):change_2016_2017 |year,
            data = sub_dta)
m9 <- feols(change_exp_ship_ratio ~ 0 + IMP_it_2017 + RET_i_tariff + RET_i_NTB +
              I(year - 2017):change_2016_2017 |
              year^subsector + year,
            data = sub_dta)

tbl_both <- etable(
  m6, m7, m8, m9,
  title = "Both controls",
  dict = c(IMP_it_2017 = "IMP", RET_i_tariff = "RET tariff",
           RET_i_NTB = "RET NTM", RET_i_NTB_IV = "RET NTM(IV)"),
  fitstat = ~ n + r2, digits = 3, digits.stats = 2,tex = FALSE
)
tbl_both


#################################################################################
# Ag: crop and livestock
#################################################################################
# check only ag output?
names(sub_dta)
unique(sub_dta$subsector)
ag_dta <- sub_dta %>% filter(subsector %in% c("crop", "livestock"))



# ---- Models without controls ----
m1 <- feols(change_exp_ship_ratio ~ 0 + IMP_it_2017 + RET_i_tariff,
            data = ag_dta)

m2 <- feols(change_exp_ship_ratio ~ 0 + IMP_it_2017 + RET_i_tariff + RET_i_NTB,
            data = ag_dta)

m3 <- feols(change_exp_ship_ratio ~ 0 + IMP_it_2017 + RET_i_tariff + RET_i_NTB_IV,
            data = ag_dta)

tbl_no_controls <- etable(
  m1, m2, m3,  title = "Without controls",
  dict = c(IMP_it_2017 = "IMP", RET_i_tariff = "RET tariff",
           RET_i_NTB = "RET NTM",  RET_i_NTB_IV = "RET NTM(IV)"),
  fitstat = ~ n + r2,digits = 3,
  digits.stats = 2,  tex = FALSE)

tbl_no_controls

# ---- Outcome trends only ----
m1 <- feols(change_exp_ship_ratio ~ 0 + IMP_it_2017 + RET_i_tariff +
              I(year - 2017):change_2016_2017,
            data = ag_dta)
m2 <- feols(change_exp_ship_ratio ~ 0 + IMP_it_2017 + RET_i_tariff + RET_i_NTB +
              I(year - 2017):change_2016_2017,
            data = ag_dta)
m3 <- feols(change_exp_ship_ratio ~ 0 + IMP_it_2017 + RET_i_tariff + RET_i_NTB_IV +
              I(year - 2017):change_2016_2017,
            data = ag_dta)

tbl_trends <- etable(
  m1, m2, m3,  title = "Outcome trends only",
  dict = c(IMP_it_2017 = "IMP", RET_i_tariff = "RET tariff",
           RET_i_NTB = "RET NTM", RET_i_NTB_IV = "RET NTM(IV)"),
  fitstat = ~ n + r2, digits = 3, digits.stats = 2, tex = FALSE)
tbl_trends

# ---- Sector x year FE only ----
m4 <- feols(change_exp_ship_ratio ~ 0 + IMP_it_2017 + RET_i_tariff |
              year^subsector,
            data = ag_dta, cluster = ~industry)
m5 <- feols(change_exp_ship_ratio ~ 0 + IMP_it_2017 + RET_i_tariff + RET_i_NTB |
              year^subsector,
            data = ag_dta, cluster = ~industry)
m6 <- feols(change_exp_ship_ratio ~ 0 + IMP_it_2017 + RET_i_tariff + RET_i_NTB_IV |
              year^subsector,
            data = ag_dta, cluster = ~industry)

tbl_sectorFE <- etable(  m4, m5, m6,
  title = "Sector x year FE only",
  dict = c(IMP_it_2017 = "IMP", RET_i_tariff = "RET tariff",
           RET_i_NTB = "RET NTM", RET_i_NTB_IV = "RET NTM(IV)"),
  fitstat = ~ n + r2, digits = 3, digits.stats = 2, tex = FALSE)
tbl_sectorFE

# ---- Both controls ----
m7 <- feols(change_exp_ship_ratio ~ 0 + IMP_it_2017 + RET_i_tariff +
              I(year - 2017):change_2016_2017 |
              year^subsector,
            data = ag_dta)
m8 <- feols(change_exp_ship_ratio ~ 0 + IMP_it_2017 + RET_i_tariff + RET_i_NTB +
              I(year - 2017):change_2016_2017 |
              year^subsector,
            data = ag_dta)
m9 <- feols(change_exp_ship_ratio ~ 0 + IMP_it_2017 + RET_i_tariff + RET_i_NTB_IV +
              I(year - 2017):change_2016_2017 |
              year^subsector,
            data = ag_dta)

tbl_both <- etable(
  m7, m8, m9,  title = "Both controls",
  dict = c(IMP_it_2017 = "IMP", RET_i_tariff = "RET tariff",
           RET_i_NTB = "RET NTM", RET_i_NTB_IV = "RET NTM(IV)"),
  fitstat = ~ n + r2, digits = 3, digits.stats = 2,tex = FALSE)
tbl_both

#################################################################################
# Crop
#################################################################################
# check only ag output?
names(sub_dta)
unique(sub_dta$subsector)
ag_dta <- sub_dta %>% filter(subsector %in% c("crop"))

# ---- Both controls ----
m7 <- feols(change_exp_ship_ratio ~ 0 + IMP_it_2017 + RET_i_tariff +
              I(year - 2017):change_2016_2017 |
              year^subsector,
            data = ag_dta)
m8 <- feols(change_exp_ship_ratio ~ 0 + IMP_it_2017 + RET_i_tariff + RET_i_NTB +
              I(year - 2017):change_2016_2017 |
              year^subsector,
            data = ag_dta)
m9 <- feols(change_exp_ship_ratio ~ 0 + IMP_it_2017 + RET_i_tariff + RET_i_NTB_IV +
              I(year - 2017):change_2016_2017 |
              year^subsector,
            data = ag_dta)

tbl_both <- etable(
  m7, m8, m9,  title = "Both controls",
  dict = c(IMP_it_2017 = "IMP", RET_i_tariff = "RET tariff",
           RET_i_NTB = "RET NTM", RET_i_NTB_IV = "RET NTM(IV)"),
  fitstat = ~ n + r2, digits = 3, digits.stats = 2,tex = FALSE)
tbl_both


#################################################################################
# livestock
#################################################################################
# check only ag output?
names(sub_dta)
unique(sub_dta$subsector)
ag_dta <- sub_dta %>% filter(subsector %in% c("livestock"))

# ---- Both controls ----
m7 <- feols(change_exp_ship_ratio ~ 0 + IMP_it_2017 + RET_i_tariff +
              I(year - 2017):change_2016_2017 |
              year^subsector,
            data = ag_dta)
m8 <- feols(change_exp_ship_ratio ~ 0 + IMP_it_2017 + RET_i_tariff + RET_i_NTB +
              I(year - 2017):change_2016_2017 |
              year^subsector,
            data = ag_dta)
m9 <- feols(change_exp_ship_ratio ~ 0 + IMP_it_2017 + RET_i_tariff + RET_i_NTB_IV +
              I(year - 2017):change_2016_2017 |
              year^subsector,
            data = ag_dta)

tbl_both <- etable(
  m7, m8, m9,  title = "Both controls",
  dict = c(IMP_it_2017 = "IMP", RET_i_tariff = "RET tariff",
           RET_i_NTB = "RET NTM", RET_i_NTB_IV = "RET NTM(IV)"),
  fitstat = ~ n + r2, digits = 3, digits.stats = 2,tex = FALSE)
tbl_both

#################################################################################
# nonag
#################################################################################
names(sub_dta)
unique(sub_dta$subsector)
ag_dta <- sub_dta %>% filter(subsector %in% c("nonag"))

# ---- Both controls ----
m7 <- feols(change_exp_ship_ratio ~ 0 + IMP_it_2017 + RET_i_tariff +
              I(year - 2017):change_2016_2017 |
              year^subsector,
            data = ag_dta)
m8 <- feols(change_exp_ship_ratio ~ 0 + IMP_it_2017 + RET_i_tariff + RET_i_NTB +
              I(year - 2017):change_2016_2017 |
              year^subsector,
            data = ag_dta)
m9 <- feols(change_exp_ship_ratio ~ 0 + IMP_it_2017 + RET_i_tariff + RET_i_NTB_IV +
              I(year - 2017):change_2016_2017 |
              year^subsector,
            data = ag_dta)

tbl_both <- etable(
  m7, m8, m9,  title = "Both controls",
  dict = c(IMP_it_2017 = "IMP", RET_i_tariff = "RET tariff",
           RET_i_NTB = "RET NTM", RET_i_NTB_IV = "RET NTM(IV)"),
  fitstat = ~ n + r2, digits = 3, digits.stats = 2,tex = FALSE)
tbl_both





