
################################################################################
# 
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

################################################################################
# LOad data 
dta <- read_csv( "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/industry_reg/merge_employment.csv")
names(dta)


exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/step_2/regression/elast/industry/"
if (!dir.exists(exp)) dir.create(exp, recursive = TRUE)


################################################################################

dta <- dta %>% filter(year %in% c(2015:2019))
sub_dta <- dta %>% filter(year %in% c(2018:2019))
names(sub_dta)
################################################################################
# look at total wage
################################################################################


#with NTMs IV
# ---- regressions ----
reg1 <- feols(ln_change_wage_total ~ IMP_it_2017 + RET_i_tariff + MFP_USD,
              data = sub_dta)

reg2 <- feols(ln_change_wage_total ~ IMP_it_2017 + RET_i_tariff + MFP_USD |
                year,
              data = sub_dta)

reg3 <- feols(ln_change_wage_total ~ IMP_it_2017 + RET_i_tariff  + MFP_USD|
                year^subsector,
              data = sub_dta)

reg4 <- feols(ln_change_wage_total ~ IMP_it_2017 + RET_i_tariff + MFP_USD +
                year:ln_wage_total_2017 |
                year^subsector,
              data = sub_dta)

# ---- table ----
tbl <- etable(reg1, reg2, reg3, reg4,
              headers = c("(1)", "(2)", "(3)", "(4)"),
              drop    = c("ln_wage_total_2017"),   # drops the year:pretrend interaction terms
              digits  = 4,
              fitstat = ~ n + r2 + ar2 + f + f.p + rmse,
              extralines = list(
                "-^Year FE"                  = list("No",  "Yes", "Yes", "Yes"),
                "-^Year x subsector FE"      = list("No",  "No",  "Yes", "Yes"),
                "-^Pre-trend wage 2017"      = list("No",  "No",  "No",  "Yes")
              ))
tbl
write_csv(as.data.frame(tbl), paste0(exp, "tot_wage.csv"))


#with NTMs 
# ---- regressions ----
reg1 <- feols(ln_change_wage_total ~ IMP_it_2017 + RET_i_tariff + RET_i_NTB,
              data = sub_dta, cluster = ~naics)
reg2 <- feols(ln_change_wage_total ~ IMP_it_2017 + RET_i_tariff + RET_i_NTB |
                year,    data = sub_dta, cluster = ~naics)
reg3 <- feols(ln_change_wage_total ~ IMP_it_2017 + RET_i_tariff + RET_i_NTB |
                year^subsector +  year ,
              data = sub_dta, cluster = ~naics)
reg4 <- feols(ln_change_wage_total ~ IMP_it_2017 + RET_i_tariff + RET_i_NTB +
                year:ln_wage_total_2017 |
                year^subsector + year,
              data = sub_dta, cluster = ~naics)


# ---- table ----
tbl <- etable(reg1, reg2, reg3, reg4,
              headers = c("(1)", "(2)", "(3)", "(4)"),
              drop    = c("ln_wage_total_2017"),   # drops the year:pretrend interaction terms
              digits  = 4,
              fitstat = ~ n + r2 + ar2 + f + f.p + rmse,
              extralines = list(
               #  "-^Year FE"                  = list("No",  "Yes", "Yes", "Yes"),
                # "-^Year x subsector FE"      = list("No",  "No",  "Yes", "Yes"),
                "-^Pre-trend wage 2017"      = list("No",  "No",  "No",  "Yes")
              ))
tbl
write_csv(as.data.frame(tbl), paste0(exp, "tot_wage_NTM.csv"))



#with NTMs IV
# ---- regressions ----
reg1 <- feols(ln_change_wage_total ~ IMP_it_2017 + RET_i_tariff + RET_i_NTB_IV + MFP_USD,
              data = sub_dta, cluster = ~naics)


reg2 <- feols(ln_change_wage_total ~ IMP_it_2017 + RET_i_tariff + RET_i_NTB_IV + MFP_USD|
                year,
              data = sub_dta, cluster = ~naics)


reg3 <- feols(ln_change_wage_total ~ IMP_it_2017 + RET_i_tariff + RET_i_NTB_IV + MFP_USD |
                year^subsector,
              data = sub_dta)

reg4 <- feols(ln_change_wage_total ~ IMP_it_2017 + RET_i_tariff + RET_i_NTB_IV + MFP_USD +
                year:ln_wage_total_2017 |
                year^subsector,
              data = sub_dta, cluster = ~naics)


# ---- table ----
tbl <- etable(reg1, reg2, reg3, reg4,
              headers = c("(1)", "(2)", "(3)", "(4)"),
              drop    = c("ln_wage_total_2017"),   # drops the year:pretrend interaction terms
              digits  = 4,
              fitstat = ~ n + r2 + ar2 + f + f.p + rmse,
              extralines = list(
                "-^Year FE"                  = list("No",  "Yes", "Yes", "Yes"),
                "-^Year x subsector FE"      = list("No",  "No",  "Yes", "Yes"),
                "-^Pre-trend wage 2017"      = list("No",  "No",  "No",  "Yes")
              ))
tbl
write_csv(as.data.frame(tbl), paste0(exp, "tot_wage_NTM_IV.csv"))

################################################################################
# Crop
#################################################################################
# check only ag output?
names(sub_dta)
unique(sub_dta$subsector)
ag_dta <- sub_dta %>% filter(subsector %in% c("crop"))

# ---- Both controls ----

m7 <- feols(ln_change_wage_total ~ 0 + IMP_it_2017 + RET_i_tariff + MFP_USD +
              I(year - 2017):ln_wage_total_2017 |
              year^subsector,
            data = ag_dta)
m8 <- feols(ln_change_wage_total ~ 0 + IMP_it_2017 + RET_i_tariff + RET_i_NTB + MFP_USD +
              I(year - 2017):ln_wage_total_2017 |
              year^subsector,
            data = ag_dta)
m9 <- feols(ln_change_wage_total ~ 0 + IMP_it_2017 + RET_i_tariff + RET_i_NTB_IV + MFP_USD +
              I(year - 2017):ln_wage_total_2017 |
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
names(sub_dta)
unique(sub_dta$subsector)
ag_dta <- sub_dta %>% filter(subsector %in% c("livestock"))

# ---- Both controls ----

m7 <- feols(ln_change_wage_total ~ 0 + IMP_it_2017 + RET_i_tariff + MFP_USD +
              I(year - 2017):ln_wage_total_2017 |
              year^subsector,
            data = ag_dta)
m8 <- feols(ln_change_wage_total ~ 0 + IMP_it_2017 + RET_i_tariff + RET_i_NTB + MFP_USD +
              I(year - 2017):ln_wage_total_2017 |
              year^subsector,
            data = ag_dta)
m9 <- feols(ln_change_wage_total ~ 0 + IMP_it_2017 + RET_i_tariff + RET_i_NTB_IV + MFP_USD +
              I(year - 2017):ln_wage_total_2017 |
              year^subsector,
            data = ag_dta)

tbl_both <- etable(
  m7, m8, m9,  title = "Both controls",
  dict = c(IMP_it_2017 = "IMP", RET_i_tariff = "RET tariff",
           RET_i_NTB = "RET NTM", RET_i_NTB_IV = "RET NTM(IV)"),
  fitstat = ~ n + r2, digits = 3, digits.stats = 2,tex = FALSE)
tbl_both

#################################################################################
# non ag


# check only ag output?
names(sub_dta)
unique(sub_dta$subsector)
ag_dta <- sub_dta %>% filter(subsector %in% c("nonag"))

# ---- Both controls ----

m7 <- feols(ln_change_wage_total ~ 0 + IMP_it_2017 + RET_i_tariff + MFP_USD +
              I(year - 2017):ln_wage_total_2017 |
              year^subsector,
            data = ag_dta)
m8 <- feols(ln_change_wage_total ~ 0 + IMP_it_2017 + RET_i_tariff + RET_i_NTB + MFP_USD +
              I(year - 2017):ln_wage_total_2017 |
              year^subsector,
            data = ag_dta)
m9 <- feols(ln_change_wage_total ~ 0 + IMP_it_2017 + RET_i_tariff + RET_i_NTB_IV + MFP_USD +
              I(year - 2017):ln_wage_total_2017 |
              year^subsector,
            data = ag_dta)

tbl_both <- etable(
  m7, m8, m9,  title = "Both controls",
  dict = c(IMP_it_2017 = "IMP", RET_i_tariff = "RET tariff",
           RET_i_NTB = "RET NTM", RET_i_NTB_IV = "RET NTM(IV)"),
  fitstat = ~ n + r2, digits = 3, digits.stats = 2,tex = FALSE)
tbl_both




################################################################################
# Totl employment
################################################################################



#with NTMs IV
# ---- regressions ----
reg1 <- feols(ln_change_employment ~ IMP_it_2017 + RET_i_tariff + MFP_USD ,
              data = sub_dta)

reg2 <- feols(ln_change_employment ~ IMP_it_2017 + RET_i_tariff + MFP_USD |
                year,
              data = sub_dta)

reg3 <- feols(ln_change_employment ~ IMP_it_2017 + RET_i_tariff  + MFP_USD |
                year^subsector,
              data = sub_dta)

reg4 <- feols(ln_change_employment ~ IMP_it_2017 + RET_i_tariff + MFP_USD +
                year:ln_employment_2017 |
                year^subsector,
              data = sub_dta)

# ---- table ----
tbl <- etable(reg1, reg2, reg3, reg4,
              headers = c("(1)", "(2)", "(3)", "(4)"),
              drop    = c("ln_employment_2017"),   # drops the year:pretrend interaction terms
              digits  = 4,
              fitstat = ~ n + r2 + ar2 + f + f.p + rmse,
              extralines = list(
                # "-^Year FE"                  = list("No",  "Yes", "Yes", "Yes"),
                # "-^Year x subsector FE"      = list("No",  "No",  "Yes", "Yes"),
                "-^Pre-trend wage 2017"      = list("No",  "No",  "No",  "Yes")
              ))
tbl
write_csv(as.data.frame(tbl), paste0(exp, "tot_emp.csv"))


#with NTMs 
# ---- regressions ----
reg1 <- feols(ln_change_employment ~ IMP_it_2017 + RET_i_tariff + RET_i_NTB + MFP_USD,
              data = sub_dta)

reg2 <- feols(ln_change_employment ~ IMP_it_2017 + RET_i_tariff + RET_i_NTB + MFP_USD|
                year,
              data = sub_dta)

reg3 <- feols(ln_change_employment ~ IMP_it_2017 + RET_i_tariff + RET_i_NTB+ MFP_USD |
                year^subsector,
              data = sub_dta)

reg4 <- feols(ln_change_employment ~ IMP_it_2017 + RET_i_tariff + RET_i_NTB + MFP_USD +
                year:ln_employment_2017 |
                year^subsector,  data = sub_dta)

# ---- table ----
tbl <- etable(reg1, reg2, reg3, reg4,
              headers = c("(1)", "(2)", "(3)", "(4)"),
              drop    = c("ln_employment_2017"),   # drops the year:pretrend interaction terms
              digits  = 4,
              fitstat = ~ n + r2 + ar2 + f + f.p + rmse,
              extralines = list(
                # "-^Year FE"                  = list("No",  "Yes", "Yes", "Yes"),
                # "-^Year x subsector FE"      = list("No",  "No",  "Yes", "Yes"),
                "-^Pre-trend wage 2017"      = list("No",  "No",  "No",  "Yes")
              ))
tbl

reg1 <- feols(ln_change_employment ~ IMP_it_2017 + RET_i_tariff + RET_i_NTB,
              data = sub_dta, cluster = ~naics)

reg2 <- feols(ln_change_employment ~ IMP_it_2017 + RET_i_tariff + RET_i_NTB |
                year,
              data = sub_dta, cluster = ~naics)

reg3 <- feols(ln_change_employment ~ IMP_it_2017 + RET_i_tariff + RET_i_NTB |
                year^subsector,
              data = sub_dta, cluster = ~naics)

reg4 <- feols(ln_change_employment ~ IMP_it_2017 + RET_i_tariff + RET_i_NTB +
                year:ln_employment_2017 |
                year^subsector,
              data = sub_dta, cluster = ~naics)

# ---- table ----
tbl <- etable(reg1, reg2, reg3, reg4,
              headers = c("(1)", "(2)", "(3)", "(4)"),
              drop    = c("ln_employment_2017"),   # drops the year:pretrend interaction terms
              digits  = 4,
              fitstat = ~ n + r2 + ar2 + f + f.p + rmse,
              extralines = list(
                # "-^Year FE"                  = list("No",  "Yes", "Yes", "Yes"),
                # "-^Year x subsector FE"      = list("No",  "No",  "Yes", "Yes"),
                "-^Pre-trend wage 2017"      = list("No",  "No",  "No",  "Yes")
              ))
tbl
write_csv(as.data.frame(tbl), paste0(exp, "tot_emp_NTM.csv"))



#with NTMs IV
# ---- regressions ----
reg1 <- feols(ln_change_employment ~ IMP_it_2017 + RET_i_tariff + RET_i_NTB_IV,
              data = sub_dta)

reg2 <- feols(ln_change_employment ~ IMP_it_2017 + RET_i_tariff + RET_i_NTB_IV |
                year,
              data = sub_dta)

reg3 <- feols(ln_change_employment ~ IMP_it_2017 + RET_i_tariff + RET_i_NTB_IV |
                year^subsector,
              data = sub_dta)

reg4 <- feols(ln_change_employment ~ IMP_it_2017 + RET_i_tariff + RET_i_NTB_IV +
                year:ln_employment_2017 |
                year^subsector,
              data = sub_dta)

# ---- table ----
tbl <- etable(reg1, reg2, reg3, reg4,
              headers = c("(1)", "(2)", "(3)", "(4)"),
              drop    = c("ln_employment_2017"),   # drops the year:pretrend interaction terms
              digits  = 4,
              fitstat = ~ n + r2 + ar2 + f + f.p + rmse,
              extralines = list(
                "-^Year FE"                  = list("No",  "Yes", "Yes", "Yes"),
                "-^Year x subsector FE"      = list("No",  "No",  "Yes", "Yes"),
                "-^Pre-trend wage 2017"      = list("No",  "No",  "No",  "Yes")
              ))
tbl
write_csv(as.data.frame(tbl), paste0(exp, "tot_emp_NTM_IV.csv"))



################################################################################
# Crop
#################################################################################
# check only ag output?
names(sub_dta)
unique(sub_dta$subsector)
ag_dta <- sub_dta %>% filter(subsector %in% c("crop"))

# ---- Both controls ----

m7 <- feols(ln_change_employment ~ 0 + IMP_it_2017 + RET_i_tariff + MFP_USD +
              I(year - 2017):ln_employment_2017 |
              year^subsector,
            data = ag_dta)
m8 <- feols(ln_change_employment ~ 0 + IMP_it_2017 + RET_i_tariff + RET_i_NTB + MFP_USD +
              I(year - 2017):ln_employment_2017 |
              year^subsector,
            data = ag_dta)
m9 <- feols(ln_change_employment ~ 0 + IMP_it_2017 + RET_i_tariff + RET_i_NTB_IV + MFP_USD +
              I(year - 2017):ln_employment_2017 |
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
names(sub_dta)
unique(sub_dta$subsector)
ag_dta <- sub_dta %>% filter(subsector %in% c("livestock"))

# ---- Both controls ----

m7 <- feols(ln_change_employment ~ 0 + IMP_it_2017 + RET_i_tariff + MFP_USD +
              I(year - 2017):ln_employment_2017 |
              year^subsector,
            data = ag_dta)
m8 <- feols(ln_change_employment ~ 0 + IMP_it_2017 + RET_i_tariff + RET_i_NTB + MFP_USD +
              I(year - 2017):ln_employment_2017 |
              year^subsector,
            data = ag_dta)
m9 <- feols(ln_change_employment ~ 0 + IMP_it_2017 + RET_i_tariff + RET_i_NTB_IV + MFP_USD +
              I(year - 2017):ln_employment_2017 |
              year^subsector,
            data = ag_dta)

tbl_both <- etable(
  m7, m8, m9,  title = "Both controls",
  dict = c(IMP_it_2017 = "IMP", RET_i_tariff = "RET tariff",
           RET_i_NTB = "RET NTM", RET_i_NTB_IV = "RET NTM(IV)"),
  fitstat = ~ n + r2, digits = 3, digits.stats = 2,tex = FALSE)
tbl_both

#################################################################################
# non ag


# check only ag output?
names(sub_dta)
unique(sub_dta$subsector)
ag_dta <- sub_dta %>% filter(subsector %in% c("nonag"))

# ---- Both controls ----

m7 <- feols(ln_change_employment ~ 0 + IMP_it_2017 + RET_i_tariff + MFP_USD +
              I(year - 2017):ln_employment_2017 |
              year^subsector,
            data = ag_dta)
m8 <- feols(ln_change_employment ~ 0 + IMP_it_2017 + RET_i_tariff + RET_i_NTB + MFP_USD +
              I(year - 2017):ln_employment_2017 |
              year^subsector,
            data = ag_dta)
m9 <- feols(ln_change_employment ~ 0 + IMP_it_2017 + RET_i_tariff + RET_i_NTB_IV + MFP_USD +
              I(year - 2017):ln_employment_2017 |
              year^subsector,
            data = ag_dta)

tbl_both <- etable(
  m7, m8, m9,  title = "Both controls",
  dict = c(IMP_it_2017 = "IMP", RET_i_tariff = "RET tariff",
           RET_i_NTB = "RET NTM", RET_i_NTB_IV = "RET NTM(IV)"),
  fitstat = ~ n + r2, digits = 3, digits.stats = 2,tex = FALSE)
tbl_both


################################################################################
# looking at subsector level
################################################################################

# for ag:
unique(dta$subsector)
sub_ag <- sub_dta %>% filter(subsector %in% c("crop", "livestock"))



#with NTMs 
# ---- regressions ----
reg1 <- feols(ln_change_wage_total ~ IMP_it_2017 + RET_i_tariff + RET_i_NTB + MFP_USD,
              data = sub_ag)

reg2 <- feols(ln_change_wage_total ~ IMP_it_2017 + RET_i_tariff + RET_i_NTB + MFP_USD |
                year,
              data = sub_ag)

reg3 <- feols(ln_change_wage_total ~ IMP_it_2017 + RET_i_tariff + RET_i_NTB + MFP_USD |
                year^subsector,
              data = sub_ag)

reg4 <- feols(ln_change_wage_total ~ IMP_it_2017 + RET_i_tariff + RET_i_NTB + MFP_USD +
                year:ln_wage_total_2017 |
                year^subsector,
              data = sub_ag)

# ---- table ----
tbl <- etable(reg1, reg2, reg3, reg4,
              headers = c("(1)", "(2)", "(3)", "(4)"),
              drop    = c("ln_wage_total_2017"),   # drops the year:pretrend interaction terms
              digits  = 4,
              fitstat = ~ n + r2 + ar2 + f + f.p + rmse,
              extralines = list(
                "-^Year FE"                  = list("No",  "Yes", "Yes", "Yes"),
                "-^Year x subsector FE"      = list("No",  "No",  "Yes", "Yes"),
                "-^Pre-trend wage 2017"      = list("No",  "No",  "No",  "Yes")
              ))
tbl
write_csv(as.data.frame(tbl), paste0(exp, "ag_wage_NTM.csv"))



#with NTMs IV
# ---- regressions ----
reg1 <- feols(ln_change_wage_total ~ IMP_it_2017 + RET_i_tariff + RET_i_NTB_IV + MFP_USD,
              data = sub_ag)

reg2 <- feols(ln_change_wage_total ~ IMP_it_2017 + RET_i_tariff + RET_i_NTB_IV + MFP_USD |
                year,
              data = sub_ag)

reg3 <- feols(ln_change_wage_total ~ IMP_it_2017 + RET_i_tariff + RET_i_NTB_IV + MFP_USD |
                year^subsector,
              data = sub_ag)

reg4 <- feols(ln_change_wage_total ~ IMP_it_2017 + RET_i_tariff + RET_i_NTB_IV + MFP_USD +
                year:ln_wage_total_2017 |
                year^subsector,
              data = sub_ag)

# ---- table ----
tbl <- etable(reg1, reg2, reg3, reg4,
              headers = c("(1)", "(2)", "(3)", "(4)"),
              drop    = c("ln_wage_total_2017"),   # drops the year:pretrend interaction terms
              digits  = 4,
              fitstat = ~ n + r2 + ar2 + f + f.p + rmse,
              extralines = list(
                "-^Year FE"                  = list("No",  "Yes", "Yes", "Yes"),
                "-^Year x subsector FE"      = list("No",  "No",  "Yes", "Yes"),
                "-^Pre-trend wage 2017"      = list("No",  "No",  "No",  "Yes")
              ))
tbl
write_csv(as.data.frame(tbl), paste0(exp, "ag_wage_NTM_IV.csv"))





#with NTMs 
# ---- regressions ----
reg1 <- feols(ln_change_employment ~ IMP_it_2017 + RET_i_tariff + RET_i_NTB + MFP_USD ,
              data = sub_ag)

reg2 <- feols(ln_change_employment ~ IMP_it_2017 + RET_i_tariff + RET_i_NTB + MFP_USD |
                year,
              data = sub_ag)

reg3 <- feols(ln_change_employment ~ IMP_it_2017 + RET_i_tariff + RET_i_NTB + MFP_USD |
                year^subsector,
              data = sub_ag)

reg4 <- feols(ln_change_employment ~ IMP_it_2017 + RET_i_tariff + RET_i_NTB + MFP_USD +
                year:ln_employment_2017 |
                year^subsector,
              data = sub_ag)

# ---- table ----
tbl <- etable(reg1, reg2, reg3, reg4,
              headers = c("(1)", "(2)", "(3)", "(4)"),
              drop    = c("ln_employment_2017"),   # drops the year:pretrend interaction terms
              digits  = 4,
              fitstat = ~ n + r2 + ar2 + f + f.p + rmse,
              extralines = list(
                "-^Year FE"                  = list("No",  "Yes", "Yes", "Yes"),
                "-^Year x subsector FE"      = list("No",  "No",  "Yes", "Yes"),
                "-^Pre-trend emp 2017"      = list("No",  "No",  "No",  "Yes")
              ))
tbl
write_csv(as.data.frame(tbl), paste0(exp, "ag_emp_NTM.csv"))

