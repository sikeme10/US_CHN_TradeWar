


################################################################################
# we create RET at County level

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




rm(list=ls())
# Set directory
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git/data")
getwd()

exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/summary/exposure_maps"


################################################################################


# load data:
gamma <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6/gamma_iuu_naics6.csv")
# gamma <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/RET_i_Chen_CHN.csv")

labor<- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/QCEW_2012_naics6_CZ.csv")

################################################################################

################################################################################

# check and get NAICs to use
names(gamma)
names(labor)
unique(gamma$year)
# can choose to have
length(unique(gamma$naics))
length(unique(labor$naics))


naics_to_use <- unique(gamma$naics)



################################################################################
# Step 1: compute total employment at CZ level from labor ONLY (no year dimension)
names(labor)
# labor <-labor%>% filter(naics %in% naics_to_use)

tot_labor <- labor %>%   group_by(czone_2012) %>% 
  summarise(
    tot_estabs_CZ     = sum(estabs,       na.rm = TRUE),
    tot_emp_CZ        = sum(emp,          na.rm = TRUE),
    tot_wages_total_CZ = sum(wages_total, na.rm = TRUE)  )

# Step 2: compute labor shares at naics-CZ level from labor ONLY
labor <- labor %>%
  left_join(tot_labor, by = "czone_2012") %>%
  mutate(share_labor_ir = if_else(tot_emp_CZ > 0, emp / tot_emp_CZ, 0))
summary(labor)

labor <-labor%>% filter(naics %in% naics_to_use)

################################################################################

# Step 3: now merge with gamma (which has year variation)
merge_data <- left_join(gamma, labor, join_by(naics == naics))
length(unique(merge_data$naics))
names(merge_data)

# Step 4: create exposure — share is now year-invariant, gamma varies by year
merge_data <- merge_data %>% mutate(
  IMP_tariff_2015_ir = IMP_it_2015 * share_labor_ir,
  IMP_tariff_2017_ir = IMP_it_2017 * share_labor_ir)
summary(merge_data)
length(unique(merge_data$naics))



###############################################################################

# a) aggregate at CZ level: get REP_r
names(merge_data)
IMP_r <- merge_data %>% group_by(year, czone_2012) %>% 
  summarise(IMP_tariff_2015_r = sum(IMP_tariff_2015_ir, na.rm = TRUE),
            IMP_tariff_2017_r = sum(IMP_tariff_2017_ir, na.rm = TRUE))
summary(IMP_r)  
table(IMP_r$year)

write_csv(IMP_r, paste0("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6/IMP_r_naics6.csv") )





