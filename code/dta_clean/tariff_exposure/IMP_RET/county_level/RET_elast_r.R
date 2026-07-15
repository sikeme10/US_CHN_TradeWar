


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


# load data:
gamma <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6/RET_i_naics6_elast_IV.csv")
# gamma <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6/RET_i_CHN_naics6.csv")
# gamma <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/RET_i_Chen_CHN.csv")

labor<- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/QCEW_2012_naics6_county.csv")

# get subsector classification from Diane
sectors <- read_csv("crosswalk/HS6_NAICS_Diane/NAICS_industry_2012.csv")
HS_NAICS <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/crosswalk/clean_HS6_naics6_2012.csv")


################################################################################





################################################################################
# Step 1: compute total employment at county level from labor ONLY (no year dimension)
names(labor)
# labor <-labor%>% filter(naics %in% naics_to_use)

tot_labor <- labor %>%   group_by(fips) %>% 
  summarise(
    tot_estabs_county     = sum(estabs,       na.rm = TRUE),
    tot_emp_county        = sum(emp,          na.rm = TRUE),
    tot_wages_total_county = sum(wages_total, na.rm = TRUE)  )

# Step 2: compute labor shares at naics-county level from labor ONLY
labor <- labor %>%
  left_join(tot_labor, by = "fips") %>%
  mutate(share_labor_ir = if_else(tot_emp_county > 0, emp / tot_emp_county, 0))
summary(labor)

################################################################################

# Step 3: now merge with gamma (which has year variation)
merge_data <- left_join(gamma, labor, join_by(naics == naics))

# Step 4: create exposure — share is now year-invariant, gamma varies by year
merge_data <- merge_data %>% mutate(
  RET_tariff_ir = RET_i_tariff * share_labor_ir,
  RET_NTB_ir    = RET_i_NTB    * share_labor_ir,
  RET_NTB_ir_IV    = RET_i_NTB_IV    * share_labor_ir,)

length(unique(merge_data$naics))
summary(merge_data)

################################################################################
# get RET_ir at naics and county level
################################################################################



# get total labor by industry NAICS 3 digit code
unique(nchar(merge_data$naics))
class(merge_data$naics)
merge_data <- merge_data %>%  mutate(naics3 = as.numeric(substr(as.character(naics), 1, 3)))
unique(merge_data$naics3)

merge_data <- merge_data %>% mutate( sector = case_when( naics3 %in% 111:115 ~ "Ag", naics3 %in% 311:339 ~ "Manu",
                              TRUE ~ "NonAg"       )  )
table(merge_data$sector, merge_data$naics3)



###############################################################################

# a) aggregate at county level: get REP_r
names(merge_data)
RET_r <- merge_data %>% group_by(year, fips) %>% 
  summarise(RET_tariff_r = sum(RET_tariff_ir, na.rm = TRUE),
            RET_NTB_r = sum(RET_NTB_ir, na.rm = TRUE),
            RET_NTB_IV_r = sum(RET_NTB_ir_IV, na.rm = TRUE))
summary(RET_r)  
unique(RET_r$year)
test <- RET_r %>% filter(RET_NTB_r > 1.5)  

write_csv(RET_r, paste0("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6/county/RET_elast_r_naics6_IV.csv") )



# 
# # b) aggregate at county level and sector : get REP_sector
# names(merge_data)
# RET_sect_r <- merge_data %>% group_by(year, county, sector) %>% 
#   summarise(RET_tariff_sect_r = sum(RET_tariff_ir, na.rm = TRUE),
#             RET_NTB_sect_r = sum(RET_NTB_ir, na.rm = TRUE))
# summary(RET_sect_r)  
# test <- RET_sect_r %>% filter(RET_NTB_sect_r > 1.5)  
# 
# write_csv(RET_sect_r, paste0("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6/RET_r_sector_CHN_naics6.csv") )


###############################################################################

