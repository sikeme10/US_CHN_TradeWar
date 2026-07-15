
################################################################################

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


SUB <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/MFP/SUB_county.csv")

IMP <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6/county/IMP_r_naics6.csv")
IMP_tot_industry <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6/county/IMP_r_subsectors_IV.csv")

names(IMP)


RET <-  read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6/county/RET_elast_r_naics6_IV.csv")
RET_tot_industry <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6/county/RET_elast_r_subsectors_IV.csv")
# RET_share_industry <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6/RET_r_industry_labor_IV.csv")

EPOP <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/EPOP_emp_county_subsector_2015_2019.csv")
names(EPOP)

Labor <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/QCEW_2012_weights_county.csv")

census_div <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/crosswalk_CZ_county/census_county_division_2012.csv")
sector_shares <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/subsector_share_county_2012.csv")
names(sector_shares)
names(census_div)
################################################################################


unique(SUB$year)
unique(IMP$year)
unique(RET$year)
unique(EPOP$year)
unique(Labor$year)


# filter year of interest 
IMP <-IMP %>% filter(year %in% c(2018:2019))
IMP <-IMP %>% select(-IMP_tariff_2015_r) %>% rename(IMP_tariff_r = IMP_tariff_2017_r)

summary(EPOP)


################################################################################
# create an EPOP for 2017, and 2015

################################################################################
names(SUB)
SUB <- SUB %>% select(fips,year,SUB)

sapply(list(SUB = SUB, IMP = IMP, RET = RET, EPOP = EPOP), 
       function(df) class(df$fips))
sapply(list(SUB = SUB, IMP = IMP, RET = RET, EPOP = EPOP), 
        function(df) class(df$year))

sapply(list(SUB = SUB, IMP = IMP, RET = RET, EPOP = EPOP), 
       function(df) length(unique(df$fips)))

SUB$fips <- as.numeric(SUB$fips)
EPOP$fips <- as.numeric(EPOP$fips)


merge <- EPOP %>%
  left_join(IMP,  by = c("fips", "year")) %>%
  left_join(RET,  by = c("fips", "year")) %>%
  left_join(SUB, by = c("fips", "year"))
colSums(is.na(merge))

# put 0 instead of NA for some variables 
merge <- merge %>%
  mutate(across(c(IMP_tariff_r, RET_tariff_r, RET_NTB_r,RET_NTB_IV_r, SUB), 
                ~ replace_na(., 0)))

Exposure_2019 <- merge %>% filter(year == 2019) %>% 
  select(fips, IMP_tariff_r, RET_tariff_r, RET_NTB_r, RET_NTB_IV_r, SUB) %>%
  rename( IMP_tariff_r_2019  = IMP_tariff_r,
          RET_tariff_r_2019  = RET_tariff_r,
          RET_NTB_r_2019     = RET_NTB_r,
          RET_NTB_IV_r_2019  = RET_NTB_IV_r,
          SUB_2019           = SUB)
Exposure_2019 <- Exposure_2019 %>% distinct()

merge <- left_join(merge, Exposure_2019)


# get 2012 Labor  at county 
names(Labor)

merge <- left_join(merge, Labor)
colSums(is.na(merge))


# merge with census division and sectoral employment
merge <- left_join(merge, census_div)
merge <- left_join(merge, sector_shares)


################################################################################
# export total

write_csv(merge, "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/Pop_reg_dta/merge_elast_county_subsector.csv")


################################################################################
# subsector level
################################################################################
names(merge)
names(RET_tot_industry)
names(IMP_tot_industry)
unique(merge$year)


RET_tot_industry_wide <- RET_tot_industry %>%
  pivot_wider(
    names_from  = subsector,
    values_from = c(RET_tariff_tot_r, RET_NTB_tot_r, RET_NTB_tot_IV_r),
    names_glue  = "{.value}_{subsector}"  ) %>%  filter(year %in% c(2018:2019))

IMP_tot_industry_wide <- IMP_tot_industry %>%
  select(-IMP_tariff_sect_ir) %>%
  pivot_wider(
    names_from  = subsector,
    values_from = IMP_tariff_tot_ir,
    names_glue  = "{.value}_{subsector}"  ) %>%  filter(year %in% c(2018:2019))




merge2 <- left_join(merge,RET_tot_industry_wide )
merge2 <- left_join(merge2,IMP_tot_industry_wide )

vars_to_zero <- c(
  "RET_tariff_tot_r_crop", "RET_tariff_tot_r_forestry", "RET_tariff_tot_r_livestock", "RET_tariff_tot_r_nonag", "RET_tariff_tot_r_mining",
  "RET_NTB_tot_r_crop", "RET_NTB_tot_r_forestry", "RET_NTB_tot_r_livestock", "RET_NTB_tot_r_nonag", "RET_NTB_tot_r_mining",
  "RET_NTB_tot_IV_r_crop", "RET_NTB_tot_IV_r_forestry", "RET_NTB_tot_IV_r_livestock", "RET_NTB_tot_IV_r_nonag", "RET_NTB_tot_IV_r_mining",
  "IMP_tariff_tot_ir_crop", "IMP_tariff_tot_ir_forestry", "IMP_tariff_tot_ir_livestock", "IMP_tariff_tot_ir_nonag", "IMP_tariff_tot_ir_mining")

# put 0 for years pre trade war
merge2 <- merge2 %>%   mutate(across(all_of(vars_to_zero), ~if_else(year < 2018, 0, .)))
colSums(is.na(merge2))
# put 0s if always NA r 0s for a specific fips code
merge3 <- merge2 %>%    group_by(fips) %>% mutate(across(all_of(vars_to_zero),
    ~ if_else(year < 2018 | all(is.na(.) | . == 0),   0,  .    )  )) %>%
  ungroup()
colSums(is.na(merge3))

write_csv(merge2, "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/Pop_reg_dta/merge_elast_subsector_county_sector_specific.csv")




