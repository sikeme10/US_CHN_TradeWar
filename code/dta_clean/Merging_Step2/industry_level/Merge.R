
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

NAICS_industry <- read_csv("data/Census_output/output_level_analysis/NAICS_ouput_industry_maps.csv")

employment <- read_csv( "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/QCEW_industry_clean_2015_2019.csv")

IMP_i <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6/gamma_iuu_naics6.csv")

RET_i <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6/RET_i_naics6_elast_IV.csv")


# get subsector classification from Diane
HS_NAICS <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/crosswalk/clean_HS6_naics6_2012.csv")

census_div <- read_csv( "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/crosswalk_CZ_county/census_div_czone_2012.csv")
sector_shares <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/subsector_share_2012.csv")


SUB <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/MFP/SUB_crop_type_year.csv" )
################################################################################

names(HS_NAICS)
colSums(is.na(HS_NAICS))
HS_NAICS <- HS_NAICS %>% select(naics, subsector, ag_subsector, naics_description)
HS_NAICS <- HS_NAICS %>% distinct() %>% filter(!is.na(naics))

# check if duplicates:
test <- HS_NAICS %>%  filter(duplicated(naics) | duplicated(naics, fromLast = TRUE))
test <- HS_NAICS %>%  group_by(naics) %>%
  summarise(n_subsector = n_distinct(subsector),
            subsectors = paste(unique(subsector), collapse = ", ")  ) %>%
  filter(n_subsector > 1)
# if non ag and crop/livestock put it in crop and livestock
HS_NAICS <- HS_NAICS %>%  group_by(naics) %>%
  mutate(
    subsector = case_when(
      # forestry + crop + nonag → crop (put this first: most specific)
      all(c("forestry", "crop", "nonag") %in% subsector) ~ "crop",
      
      # nonag + livestock → livestock
      any(subsector == "livestock") & any(subsector == "nonag") ~ "livestock",
      
      # nonag + crop → crop
      any(subsector == "crop") & any(subsector == "nonag") ~ "crop",
      
      # nonag + forestry → nonag
      any(subsector == "forestry") & any(subsector == "nonag") ~ "nonag",
      
      # otherwise keep original value
      TRUE ~ subsector    )  ) %>%  ungroup()
test <- HS_NAICS %>%  group_by(naics) %>%
  summarise(n_subsector = n_distinct(subsector),
            subsectors = paste(unique(subsector), collapse = ", ")  ) %>%
  filter(n_subsector > 1)
table(HS_NAICS$subsector)
length(unique(HS_NAICS$naics))
# manually adjust some other 
HS_NAICS <- HS_NAICS %>%
  mutate( subsector = case_when(naics == 311225 ~ "crop", naics == 311119 ~ "livestock",TRUE ~ subsector)  )



sectors <- HS_NAICS %>%  group_by(naics, subsector) %>%
  summarise(ag_subsector = paste(ag_subsector, collapse = ", "), .groups = "drop")
length(unique(sectors$naics))


################################################################################

class(employment$naics)
class(IMP_i$naics)
class(sectors$naics)


names(employment)
names(IMP_i)
names(RET_i)
names(sectors)

IMP_i <- IMP_i %>% select("year", "naics", "sector", IMP_it_2017)
RET_i <- RET_i %>% select("year", "naics", "RET_i_tariff", "RET_i_NTB", "RET_i_NTB_IV")
# sectors <- sectors %>% select(naics, subsector, ag_subsector)


# select  NAICS codes of interest 
length(unique(IMP_i$naics))
length(unique(RET_i$naics))
length(unique(sectors$naics))
length((unique(employment$naics)))
common <- intersect(unique(IMP_i$naics), unique(RET_i$naics))
common <- intersect(unique(IMP_i$naics), unique(sectors$naics))
common <- intersect(unique(RET_i$naics), unique(sectors$naics))
common <- intersect(unique(sectors$naics), unique(employment$naics))

employment1 <- employment %>% filter(naics %in% unique(IMP_i$naics))
table(employment1$year)


# merge all data 
employment1 <-  left_join(employment, IMP_i)
employment1 <-  left_join(employment1, RET_i)



employment1 <- left_join(employment1, sectors)


colSums(is.na(employment1))
names(employment1)
employment1 <- employment1 %>%
  group_by(naics) %>%
  fill(sector, subsector, ag_subsector, .direction = "updown") %>%
  ungroup()

colSums(is.na(employment1))


# filter naics code of interest 
length(unique(IMP_i$naics))
employment2 <- employment1 %>% filter(naics %in% unique(RET_i$naics))
table(employment2$year)
colSums(is.na(employment2))

#################################################################################
# count how many distinct years each naics code appears in
naics_year_coverage <- employment2 %>%  group_by(naics) %>%
  summarise(n_years = n_distinct(year), .groups = "drop")

n_total_years <- n_distinct(employment2$year)  # should be 6, for 2015-2020

# keep only naics codes present in every single year
naics_always_present <- naics_year_coverage %>%
  filter(n_years == n_total_years) %>%  pull(naics)

employment2 <- employment2 %>%  filter(naics %in% naics_always_present)

# confirm it worked
employment2 %>% group_by(year) %>% summarise(n_codes = n_distinct(naics))
colSums(is.na(employment2))
table(employment2$year)
#################################################################################


# replace NAs with 0 for years before 2018
employment2 <- employment2 %>%
  mutate(across(c(IMP_it_2017, RET_i_tariff, RET_i_NTB, RET_i_NTB_IV),
                ~ ifelse(year < 2018, 0, .)))

names(employment2)

employment2 <- employment2 |>
  group_by(naics) |>   # adjust to group_by(naics, fips, own_code) if the check above flags duplicates
  mutate(
    wage_total_2017       = wage_total[year == 2017],
    employment_2017       = employment[year == 2017],
    wage_per_worker_2017  = wage_per_worker[year == 2017],
    
    ln_wage_total          = log(wage_total),
    ln_wage_total_2017     = log(wage_total_2017),
    change_wage_total      = wage_total - wage_total_2017,
    ln_change_wage_total   = ln_wage_total - ln_wage_total_2017,
    
    ln_employment          = log(employment),
    ln_employment_2017     = log(employment_2017),
    change_employment      = employment - employment_2017,
    ln_change_employment   = ln_employment - ln_employment_2017,
    
    ln_wage_per_worker         = log(wage_per_worker),
    ln_wage_per_worker_2017    = log(wage_per_worker_2017),
    change_wage_per_worker     = wage_per_worker - wage_per_worker_2017,
    ln_change_wage_per_worker  = ln_wage_per_worker - ln_wage_per_worker_2017
  ) |>
  ungroup()



###########################################################################

library(dplyr)
library(stringr)

# Objects assumed:
#   mfp          -> image 1 (MFP_prog, year, MFP_USD)
#   employment2  -> image 2 panel (must contain `naics` AND `year`)

# ---- 1. Normalize MFP labels; keep LONG (do not pivot) ----
mfp_long <- SUB %>%
  mutate(
    mfp_category = case_when(
      str_detect(MFP_prog, "SPECIALTY") ~ "SPECIALTY CROPS",
      str_detect(MFP_prog, "DAHG")      ~ "DAHG",
      str_detect(MFP_prog, "CROPS")     ~ "CROPS",
      TRUE                              ~ NA_character_
    )
  ) %>%
  select(mfp_category, year, MFP_USD)

# ---- 2. Map each NAICS code to an MFP category ----
naics_to_mfp <- function(naics) {
  n  <- as.character(naics)
  p4 <- substr(n, 1, 4)
  case_when(
    n %in% c("112120", "112210")      ~ "DAHG",
    p4 == "1111"                      ~ "CROPS",
    p4 %in% c("1112", "1113", "1114") ~ "SPECIALTY CROPS",
    p4 == "1119"                      ~ "CROPS",
    TRUE                              ~ NA_character_
  )
}

employment2 <- employment2 %>%  mutate(mfp_category = naics_to_mfp(naics))

# ---- 3. Join on BOTH category and year ----
# Any year not in {2018, 2019} -> MFP_USD is NA, as it should be.
employment2 <- employment2 %>%  left_join(mfp_long, by = c("mfp_category", "year"))

employment2 <- employment2 %>%  mutate(MFP_USD = coalesce(MFP_USD, 0))

###########################################################################

write_csv(employment2, "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/industry_reg/merge_employment.csv")






