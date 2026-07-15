
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
dta <- read_csv( "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/Pop_reg_dta/merge_elast_subsector_county_sector_specific.csv")

names(dta)

exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/step_2/regression/"


################################################################################

################################################################################
library(etable)
library(fixest)

sub_dta <- dta %>% filter(year %in% c(2018:2019))
names(sub_dta)

################################################################################
# for Crop 
################################################################################
# get corrplot

ag_dta <- sub_dta %>% filter(subsector == c("crop")) %>% 
  select(year, fips,division,delta_EPOP,  IMP_tariff_tot_ir_crop, RET_tariff_tot_r_crop, RET_NTB_tot_r_crop,
         RET_NTB_tot_IV_r_crop,  SUB , share_crop, division , delta_EPOP_pretrend)
summary(ag_dta)
library(corrplot)
cor_matrix <- cor(ag_dta, use = "complete.obs")
corrplot(cor_matrix, method = "circle", addCoef.col = "black", number.cex = 0.6, tl.cex = 0.7)

# when adding controls:
reg1 <- feols(delta_EPOP ~ IMP_tariff_tot_ir_crop + RET_tariff_tot_r_crop + RET_NTB_tot_r_crop + SUB |
                year, data = ag_dta, cluster = ~fips)
reg2 <- feols(delta_EPOP ~ IMP_tariff_tot_ir_crop + RET_tariff_tot_r_crop + RET_NTB_tot_r_crop + SUB +
                SUB + as.factor(year):share_crop  |
                year, data = ag_dta, cluster = ~fips)
reg3 <- feols(delta_EPOP ~ IMP_tariff_tot_ir_crop + RET_tariff_tot_r_crop + RET_NTB_tot_r_crop+ SUB  +
                SUB + as.factor(year):share_crop +
                as.factor(division):as.factor(year) |
                year, data = ag_dta, cluster = ~fips)
reg4 <- feols(delta_EPOP ~ IMP_tariff_tot_ir_crop + RET_tariff_tot_r_crop + RET_NTB_tot_r_crop + SUB +
                SUB + as.factor(year):share_crop +
                as.factor(division):as.factor(year) + year:delta_EPOP_pretrend |
                year, data = ag_dta, cluster = ~fips)

tbl <- etable(reg1, reg2, reg3, reg4,
              headers = c("(1)", "(2)", "(3)", "(4)"),
              drop    = c("share_ag_mining", "share_mfg", "share_other",
                          "division", "change_EPOP_2017_2016"),
              digits  = 4,
              fitstat = ~ n + r2 + ar2 + f + f.p + rmse,
              extralines = list(
                "-^Year FE"                        = list("Yes", "Yes", "Yes", "Yes"),
                "-^Sector x year-month FE"               = list("No",  "Yes", "Yes", "Yes"),
                "-^Census division x year-month FE"      = list("No",  "No",  "Yes", "Yes"),
                "-^Pre-trend CZ outcome 2017"            = list("No",  "No",  "No",  "Yes")
              ))

tbl

# with IV 
reg1 <- feols(delta_EPOP ~ IMP_tariff_tot_ir_crop + RET_tariff_tot_r_crop + RET_NTB_tot_IV_r_crop + SUB |
                year, data = ag_dta)
reg2 <- feols(delta_EPOP ~ IMP_tariff_tot_ir_crop + RET_tariff_tot_r_crop + RET_NTB_tot_IV_r_crop + SUB +
                SUB + as.factor(year):share_crop  |
                year, data = ag_dta)
reg3 <- feols(delta_EPOP ~ IMP_tariff_tot_ir_crop + RET_tariff_tot_r_crop + RET_NTB_tot_IV_r_crop+ SUB  +
                SUB + as.factor(year):share_crop +
                as.factor(division):as.factor(year) |
                year, data = ag_dta)
reg4 <- feols(delta_EPOP ~ IMP_tariff_tot_ir_crop + RET_tariff_tot_r_crop + RET_NTB_tot_IV_r_crop + SUB +
                SUB + as.factor(year):share_crop +
                as.factor(division):as.factor(year) + year:delta_EPOP_pretrend |
                year, data = ag_dta)

tbl <- etable(reg1, reg2, reg3, reg4,
              headers = c("(1)", "(2)", "(3)", "(4)"),
              drop    = c("share_ag_mining", "share_mfg", "share_other",
                          "division", "change_EPOP_2017_2016"),
              digits  = 4,
              fitstat = ~ n + r2 + ar2 + f + f.p + rmse,
              extralines = list(
                "-^Year FE"                        = list("Yes", "Yes", "Yes", "Yes"),
                "-^Sector x year-month FE"               = list("No",  "Yes", "Yes", "Yes"),
                "-^Census division x year-month FE"      = list("No",  "No",  "Yes", "Yes"),
                "-^Pre-trend CZ outcome 2017"            = list("No",  "No",  "No",  "Yes")
              ))

tbl



################################################################################
# for Livestock 
################################################################################

unique(sub_dta$subsector)
ag_dta <- sub_dta %>% filter(subsector == c("livestock")) %>% 
  select(year, fips,division,delta_EPOP,  IMP_tariff_tot_ir_livestock, RET_tariff_tot_r_livestock, RET_NTB_tot_r_livestock,
         RET_NTB_tot_IV_r_livestock,  SUB , share_livestock, division , delta_EPOP_pretrend)
summary(ag_dta)
library(corrplot)
cor_matrix <- cor(ag_dta, use = "complete.obs")
corrplot(cor_matrix, method = "circle", addCoef.col = "black", number.cex = 0.9, tl.cex = 0.9)



# when adding controls:
reg1 <- feols(delta_EPOP ~ IMP_tariff_tot_ir_livestock + RET_tariff_tot_r_livestock + RET_NTB_tot_r_livestock + SUB |
                year, data = ag_dta, cluster = ~fips)
reg2 <- feols(delta_EPOP ~ IMP_tariff_tot_ir_livestock + RET_tariff_tot_r_livestock + RET_NTB_tot_r_livestock + SUB +
                SUB + as.factor(year):share_livestock  |
                year, data = ag_dta, cluster = ~fips)
reg3 <- feols(delta_EPOP ~ IMP_tariff_tot_ir_livestock + RET_tariff_tot_r_livestock + RET_NTB_tot_r_livestock+ SUB  +
                SUB + as.factor(year):share_livestock +
                as.factor(division):as.factor(year) |
                year, data = ag_dta, cluster = ~fips)
reg4 <- feols(delta_EPOP ~ IMP_tariff_tot_ir_livestock + RET_tariff_tot_r_livestock + RET_NTB_tot_r_livestock + SUB +
                SUB + as.factor(year):share_livestock +
                as.factor(division):as.factor(year) + year:delta_EPOP_pretrend |
                year, data = ag_dta, cluster = ~fips)

tbl <- etable(reg1, reg2, reg3, reg4,
              headers = c("(1)", "(2)", "(3)", "(4)"),
              drop    = c("share_ag_mining", "share_mfg", "share_other",
                          "division", "change_EPOP_2017_2016"),
              digits  = 4,
              fitstat = ~ n + r2 + ar2 + f + f.p + rmse,
              extralines = list(
                "-^Year FE"                        = list("Yes", "Yes", "Yes", "Yes"),
                "-^Sector x year-month FE"               = list("No",  "Yes", "Yes", "Yes"),
                "-^Census division x year-month FE"      = list("No",  "No",  "Yes", "Yes"),
                "-^Pre-trend CZ outcome 2017"            = list("No",  "No",  "No",  "Yes")
              ))

tbl

# with IV 
reg1 <- feols(delta_EPOP ~ IMP_tariff_tot_ir_livestock + RET_tariff_tot_r_livestock + RET_NTB_tot_IV_r_livestock + SUB |
                year, data = ag_dta)
reg2 <- feols(delta_EPOP ~ IMP_tariff_tot_ir_livestock + RET_tariff_tot_r_livestock + RET_NTB_tot_IV_r_livestock + SUB +
                SUB + as.factor(year):share_livestock  |
                year, data = ag_dta)
reg3 <- feols(delta_EPOP ~ IMP_tariff_tot_ir_livestock + RET_tariff_tot_r_livestock + RET_NTB_tot_IV_r_livestock+ SUB  +
                SUB + as.factor(year):share_livestock +
                as.factor(division):as.factor(year) |
                year, data = ag_dta)
reg4 <- feols(delta_EPOP ~ IMP_tariff_tot_ir_livestock + RET_tariff_tot_r_livestock + RET_NTB_tot_IV_r_livestock + SUB +
                SUB + as.factor(year):share_livestock +
                as.factor(division):as.factor(year) + year:delta_EPOP_pretrend |
                year, data = ag_dta)

tbl <- etable(reg1, reg2, reg3, reg4,
              headers = c("(1)", "(2)", "(3)", "(4)"),
              drop    = c("share_ag_mining", "share_mfg", "share_other",
                          "division", "change_EPOP_2017_2016"),
              digits  = 4,
              fitstat = ~ n + r2 + ar2 + f + f.p + rmse,
              extralines = list(
                "-^Year FE"                        = list("Yes", "Yes", "Yes", "Yes"),
                "-^Sector x year-month FE"               = list("No",  "Yes", "Yes", "Yes"),
                "-^Census division x year-month FE"      = list("No",  "No",  "Yes", "Yes"),
                "-^Pre-trend CZ outcome 2017"            = list("No",  "No",  "No",  "Yes")
              ))
tbl





################################################################################
# for non ag 
################################################################################

unique(sub_dta$subsector)
ag_dta <- sub_dta %>% filter(subsector == c("nonag")) %>% 
  select(year, fips,division,delta_EPOP,  IMP_tariff_tot_ir_nonag, RET_tariff_tot_r_nonag, RET_NTB_tot_r_nonag,
         RET_NTB_tot_IV_r_nonag,  SUB , share_nonag, division , delta_EPOP_pretrend)
summary(ag_dta)

library(corrplot)
cor_matrix <- cor(ag_dta, use = "complete.obs")
corrplot(cor_matrix, method = "circle", addCoef.col = "black", number.cex = 0.9, tl.cex = 0.9)


# when adding controls:
reg1 <- feols(delta_EPOP ~ IMP_tariff_tot_ir_nonag + RET_tariff_tot_r_nonag + RET_NTB_tot_r_nonag + SUB |
                year, data = ag_dta, cluster = ~fips)
reg2 <- feols(delta_EPOP ~ IMP_tariff_tot_ir_nonag + RET_tariff_tot_r_nonag + RET_NTB_tot_r_nonag + SUB +
                SUB + as.factor(year):share_nonag  |
                year, data = ag_dta, cluster = ~fips)
reg3 <- feols(delta_EPOP ~ IMP_tariff_tot_ir_nonag + RET_tariff_tot_r_nonag + RET_NTB_tot_r_nonag+ SUB  +
                SUB + as.factor(year):share_nonag +
                as.factor(division):as.factor(year) |
                year, data = ag_dta, cluster = ~fips)
reg4 <- feols(delta_EPOP ~ IMP_tariff_tot_ir_nonag + RET_tariff_tot_r_nonag + RET_NTB_tot_r_nonag + SUB +
                SUB + as.factor(year):share_nonag +
                as.factor(division):as.factor(year) + year:delta_EPOP_pretrend |
                year, data = ag_dta, cluster = ~fips)

tbl <- etable(reg1, reg2, reg3, reg4,
              headers = c("(1)", "(2)", "(3)", "(4)"),
              drop    = c("share_ag_mining", "share_mfg", "share_other",
                          "division", "change_EPOP_2017_2016"),
              digits  = 4,
              fitstat = ~ n + r2 + ar2 + f + f.p + rmse,
              extralines = list(
                "-^Year FE"                        = list("Yes", "Yes", "Yes", "Yes"),
                "-^Sector x year-month FE"               = list("No",  "Yes", "Yes", "Yes"),
                "-^Census division x year-month FE"      = list("No",  "No",  "Yes", "Yes"),
                "-^Pre-trend CZ outcome 2017"            = list("No",  "No",  "No",  "Yes")
              ))

tbl

# with IV 
reg1 <- feols(delta_EPOP ~ IMP_tariff_tot_ir_nonag + RET_tariff_tot_r_nonag + RET_NTB_tot_IV_r_nonag + SUB |
                year, data = ag_dta)
reg2 <- feols(delta_EPOP ~ IMP_tariff_tot_ir_nonag + RET_tariff_tot_r_nonag + RET_NTB_tot_IV_r_nonag + SUB +
                SUB + as.factor(year):share_nonag  |
                year, data = ag_dta)
reg3 <- feols(delta_EPOP ~ IMP_tariff_tot_ir_nonag + RET_tariff_tot_r_nonag + RET_NTB_tot_IV_r_nonag+ SUB  +
                SUB + as.factor(year):share_nonag +
                as.factor(division):as.factor(year) |
                year, data = ag_dta)
reg4 <- feols(delta_EPOP ~ IMP_tariff_tot_ir_nonag + RET_tariff_tot_r_nonag + RET_NTB_tot_IV_r_nonag + SUB +
                SUB + as.factor(year):share_nonag +
                as.factor(division):as.factor(year) + year:delta_EPOP_pretrend |
                year, data = ag_dta, cluster = ~fips)

tbl <- etable(reg1, reg2, reg3, reg4,
              headers = c("(1)", "(2)", "(3)", "(4)"),
              drop    = c("share_ag_mining", "share_mfg", "share_other",
                          "division", "change_EPOP_2017_2016"),
              digits  = 4,
              fitstat = ~ n + r2 + ar2 + f + f.p + rmse,
              extralines = list(
                "-^Year FE"                        = list("Yes", "Yes", "Yes", "Yes"),
                "-^Sector x year-month FE"               = list("No",  "Yes", "Yes", "Yes"),
                "-^Census division x year-month FE"      = list("No",  "No",  "Yes", "Yes"),
                "-^Pre-trend CZ outcome 2017"            = list("No",  "No",  "No",  "Yes")
              ))

tbl


################################################################################



