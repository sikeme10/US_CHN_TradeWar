
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
library(readxl)


################################################################################

rm(list=ls())



# Load data 
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/")
exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/"

labor <-  read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/QCEW_industry_2015_2019.csv")
names(labor)

################################################################################

# rename some variables
labor <- labor %>% rename(fips = area_fips, wage_total = total_annual_wages, naics =  industry_code,
                          estab = annual_avg_estabs , employment = annual_avg_emplvl)
labor <- labor %>% select(fips, year, naics, own_code,disclosure_code, wage_total , estab, employment )
table(labor$year)

# get only private industries (0 is for all)
# labor <- labor %>% filter(Own == 5)
labor <- labor %>% filter(own_code == 5)

################################################################################
# check if duplicates fips by naics 

# we want to aggregate at industr level to 
length(unique(labor$naics))
length(unique(labor$fips))
table(labor$year)

test <- labor %>% group_by(naics, year) %>%
  summarise(fips_total = n(),
            fips_distinct = n_distinct(fips),
            fips_duplicates = n() - n_distinct(fips)
            )
summary(test)
dupes <- labor %>%
  group_by(naics, fips, year) %>%
  filter(n() > 1) %>%
  ungroup()


# put NA for undisclosed busisness
table(labor$disclosure_code)
# or to see the actual codes
labor %>%  filter(disclosure_code == "N") %>%
  distinct(naics)

labor <- labor %>%
  mutate(
    employment = as.numeric(employment),
    wage_total = as.numeric(wage_total)
  ) %>%
  mutate(
    employment = ifelse(is.na(disclosure_code),  employment, NA),
    wage_total = ifelse(is.na(disclosure_code),  wage_total, NA)  ) %>%
  mutate(
    wage_per_worker = if_else(!is.na(employment) & employment != 0,
                              wage_total / employment, NA_real_)  )

summary(labor)

################################################################################
# concord NAICS code to 2012 NAICS codes
################################################################################
#we want to convert all NIACS codes to 2012 
# it gets complicated specially for the concrdance that merge from 2012 to 2017

labor <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/QCEW_industry_clean_2015_2019.csv")
table(labor$year)
concord <- read_csv("data/crosswalk/NAICS_2017_2012/clean_2017_to_2012_NAICS.xlsx")
class(concord$naics_2012)
class(concord$naics_2017)
table(labor$year)   # confirm which years are actually present before filtering

# ---- 2. Reconcile code types (character, zero-padded to 6 digits) ----
labor <- labor %>%
  mutate(naics = sprintf("%06d", as.integer(naics)))

concord <- concord %>%
  mutate(naics_2017 = sprintf("%06d", as.integer(naics_2017)),
         naics_2012 = sprintf("%06d", as.integer(naics_2012)))

# ---- 3. Split panel by NAICS vintage ----
# 2015-2016 already coded on NAICS 2012; 2017+ coded on NAICS 2017
labor_2012 <- labor %>% filter(year < 2017)
labor_2017 <- labor %>% filter(year >= 2017)

# ---- 4. Attach crosswalk to the 2017-basis data ----
concord <- concord %>% filter(naics_2017 %in% unique(labor_2017$naics))

labor_2017bis <- labor_2017 %>%
  left_join(concord, by = c("naics" = "naics_2017"))

# diagnostic: which 2017 codes map to multiple 2012 codes (the only ones needing weights)
labor_2017bis %>%  filter(!is.na(naics_2012)) %>%
  distinct(naics, naics_2012) %>%  count(naics) %>%  filter(n > 1)

# ---- 5. Build empirical weights from 2015-2016 (2012-basis) employment ----
# pooled nationally; suppressed/NA employment treated as 0 so it can't poison group sums
emp_1516 <- labor_2012 %>%
  mutate(employment = ifelse(is.na(employment), 0, employment)) %>%
  group_by(naics) %>%                       # in labor_2012, `naics` IS naics_2012
  summarise(emp_1516 = sum(employment), .groups = "drop") %>%
  rename(naics_2012 = naics)

crosswalk <- labor_2017bis %>%  filter(!is.na(naics_2012)) %>%  distinct(naics, naics_2012)

naics_weights <- crosswalk %>%  left_join(emp_1516, by = "naics_2012") %>%
  mutate(emp_1516 = ifelse(is.na(emp_1516), 0, emp_1516)) %>%
  group_by(naics) %>%
  mutate(  grp_total = sum(emp_1516),
           weight = if_else(grp_total > 0, emp_1516 / grp_total, 1 / n())  ) %>%
  ungroup() %>%
  select(naics, naics_2012, weight)

# verify every group's weights sum to 1 (returns 0 rows if all good)
naics_weights %>%  group_by(naics) %>%
  summarise(wsum = sum(weight), .groups = "drop") %>%
  filter(abs(wsum - 1) > 1e-6)

# ---- 6. Allocate 2017+ values to 2012 basis and aggregate ----
labor_2017_on_2012 <- labor_2017bis %>%  filter(!is.na(naics_2012)) %>%
  left_join(naics_weights, by = c("naics", "naics_2012")) %>%
  mutate(
    weight = ifelse(is.na(weight), 1, weight),       # unaffected 1-to-1 codes
    employment_alloc = employment * weight,
    wage_total_alloc = wage_total * weight
  ) %>%
  group_by(fips, year, naics_2012, own_code) %>%
  summarise(
    employment = sum(employment_alloc, na.rm = TRUE),
    wage_total = sum(wage_total_alloc, na.rm = TRUE),
    estab      = sum(estab, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(wage_per_worker = if_else(employment != 0, wage_total / employment, NA_real_))

# ---- 7. Harmonise 2015-2016 to the same schema and stack into one panel ----
labor_2012_clean <- labor_2012 %>%  rename(naics_2012 = naics) %>%
  group_by(fips, year, naics_2012, own_code) %>%
  summarise(
    employment = sum(employment, na.rm = TRUE),
    wage_total = sum(wage_total, na.rm = TRUE),
    estab      = sum(estab, na.rm = TRUE),
    .groups = "drop"  ) %>%
  mutate(wage_per_worker = if_else(employment != 0, wage_total / employment, NA_real_))



labor_final <- bind_rows(labor_2012_clean, labor_2017_on_2012) %>%
  arrange(fips, naics_2012, own_code, year)

table(labor_final$year)





################################################################################

table(labor$disclosure_code)
write_csv(labor, "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/QCEW_industry_clean_2015_2019.csv")

