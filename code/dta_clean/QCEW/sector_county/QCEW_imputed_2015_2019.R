

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
library(haven)
library(sfaR)
library(frontier)
library(ggplot2)

################################################################################
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git/")
library(haven)
dta_2015 <- read_dta("data/QCEW/dtaFiles/qcew2015_imputednaics6.dta")
dta_2016 <- read_dta("data/QCEW/dtaFiles/qcew2016_imputednaics6.dta")
dta_2017 <- read_dta("data/QCEW/dtaFiles/qcew2017_imputednaics6.dta")
dta_2018 <- read_dta("data/QCEW/dtaFiles/qcew2018_imputednaics6.dta")
dta_2019 <- read_dta("data/QCEW/dtaFiles/qcew2019_imputednaics6.dta")

Pop <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/Pop/county_population_clean_2015_2020.csv")
names(Pop)


HS_NAICS <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/crosswalk/clean_HS6_naics6_2012.csv")


################################################################################


names(HS_NAICS)
colSums(is.na(HS_NAICS))
HS_NAICS <- HS_NAICS %>% select(naics, subsector, ag_subsector, naics_description)
HS_NAICS <- HS_NAICS %>% distinct() %>% filter(!is.na(naics))

# # check if duplicates:
# test <- HS_NAICS %>%  filter(duplicated(naics) | duplicated(naics, fromLast = TRUE))
# test <- HS_NAICS %>%  group_by(naics) %>%
#   summarise(n_subsector = n_distinct(subsector),
#             subsectors = paste(unique(subsector), collapse = ", ")  ) %>%
#   filter(n_subsector > 1)
# # if non ag and crop/livestock put it in crop and livestock
# HS_NAICS <- HS_NAICS %>%  group_by(naics) %>%
#   mutate(
#     subsector = case_when(
#       # forestry + crop + nonag → crop (put this first: most specific)
#       all(c("forestry", "crop", "nonag") %in% subsector) ~ "crop",
#       
#       # nonag + livestock → livestock
#       any(subsector == "livestock") & any(subsector == "nonag") ~ "livestock",
#       
#       # nonag + crop → crop
#       any(subsector == "crop") & any(subsector == "nonag") ~ "crop",
#       
#       # nonag + forestry → nonag
#       any(subsector == "forestry") & any(subsector == "nonag") ~ "nonag",
#       
#       # otherwise keep original value
#       TRUE ~ subsector    )  ) %>%  ungroup()
# test <- HS_NAICS %>%  group_by(naics) %>%
#   summarise(n_subsector = n_distinct(subsector),
#             subsectors = paste(unique(subsector), collapse = ", ")  ) %>%
#   filter(n_subsector > 1)
# table(HS_NAICS$subsector)
# length(unique(HS_NAICS$naics))
# # manually adjust some other 
# HS_NAICS <- HS_NAICS %>%
#   mutate( subsector = case_when(naics == 311225 ~ "crop", naics == 311119 ~ "livestock",TRUE ~ subsector)  )
# 


sectors <- HS_NAICS %>%  group_by(naics, subsector) %>%
  summarise(ag_subsector = paste(ag_subsector, collapse = ", "), .groups = "drop")
length(unique(sectors$naics))

# separate mining in subscetor from nonag 
names(sectors)
unique(sectors$ag_subsector)
table(sectors$subsector)
colSums(is.na(sectors))
# sectors$subsector[grepl("mining", sectors$ag_subsector)] <- "mining"


###############################################################################


colSums(is.na(dta_2015))
length(unique(dta_2015$fips))
length(unique(dta_2015$naics))
length(unique(dta_2016$fips))
length(unique(dta_2016$naics))



dta_2015 <- left_join(dta_2015, sectors )
dta_2016 <- left_join(dta_2016, sectors )
dta_2017 <- left_join(dta_2017, sectors )
dta_2018 <- left_join(dta_2018, sectors )
dta_2019 <- left_join(dta_2019, sectors )

colSums(is.na(dta_2015 ))
test <- dta_2015 %>% filter(is.na(subsector))
length(unique(dta_2015$naics))


dta_2015_sum <- dta_2015 %>% group_by(fips, year, subsector) %>% 
  summarise(estabs = sum( estabs, na.rm = TRUE),
            emp = sum(emp, na.rm = TRUE),
            wages_total = sum(wages_total, na.rm = TRUE),
            emp_nonsuppressed  = sum(emp_nonsuppressed , na.rm = TRUE),
            wages_nonsuppressed = sum( wages_nonsuppressed, na.rm = TRUE)  )



summarise_year <- function(dta) {
  dta %>% filter(!is.na(subsector)) %>% # drop all sector not included in our analysis
    group_by(fips, year, subsector) %>%
    summarise(
      estabs              = sum(estabs,              na.rm = TRUE),
      emp                 = sum(emp,                 na.rm = TRUE),
      wages_total         = sum(wages_total,         na.rm = TRUE),
      emp_nonsuppressed   = sum(emp_nonsuppressed,   na.rm = TRUE),
      wages_nonsuppressed = sum(wages_nonsuppressed, na.rm = TRUE),
      .groups = "drop"
    )
}


dta_2015_sum <- summarise_year(dta_2015)
dta_2016_sum <- summarise_year(dta_2016)
dta_2017_sum <- summarise_year(dta_2017)
dta_2018_sum <- summarise_year(dta_2018)
dta_2019_sum <- summarise_year(dta_2019)

######################################################################################
# look into fips code 
class(Pop$fips)
class(dta_2015$fips)


Pop$fips <- as.numeric(Pop$fips)

length(unique(Pop$fips))
length(unique(dta_2015_sum$fips))
length(unique(dta_2016_sum$fips))
length(unique(dta_2017_sum$fips))
length(unique(dta_2018_sum$fips))
length(unique(dta_2019_sum$fips))



test <-  setdiff(unique(Pop$fips), unique(dta_2016_sum$fips))


# have to recode some counties to 2012 counties 
for (yr in 2015:2019) {
  df_name <- paste0("dta_", yr, "_sum")
  df <- get(df_name) |>
    mutate(fips = as.character(fips),
           fips = str_pad(fips, width = 5, pad = "0"),
           fips = case_when(
             fips == "02158" ~ "02270",
             fips == "46102" ~ "46113",
             TRUE ~ fips
           ),
           fips = as.numeric(fips))
  assign(df_name, df)
}


years <- list(dta_2015_sum, dta_2016_sum, dta_2017_sum, dta_2018_sum, dta_2019_sum)
names(years) <- 2015:2019

sapply(years, function(df) {
  missing <- sum(!unique(Pop$fips) %in% unique(df$fips))
  c(in_both = sum(unique(Pop$fips) %in% unique(df$fips)),
    missing_from_year = missing)
})

# two fips code missing 

dta <- bind_rows(dta_2015_sum, dta_2016_sum, dta_2017_sum, dta_2018_sum, dta_2019_sum)

table(dta$year)
colSums(is.na(dta))
test <- dta %>% filter(is.na(subsector))


# select only fips that are in Pop 
dta1 <-  dta %>% filter(fips %in% unique(Pop$fips))
table(dta1$year)
tapply(dta1$fips, dta1$year, function(x) length(unique(x)))
table(dta1$subsector, dta1$year)

dta1 <- dta1 |>  mutate(subsector = ifelse(is.na(subsector), "other", subsector))
dta1 <- dta1 |>
  group_by(fips, year, subsector) |>
  summarise(across(c(estabs, emp, wages_total, emp_nonsuppressed, wages_nonsuppressed), mean),
            .groups = "drop")


#create a balnced data
dta_balanced <- dta1 |>
  complete(fips = unique(dta1$fips),
           year = unique(dta1$year),
           subsector = unique(dta1$subsector),
           fill = list(estabs = 0,
                       emp = 0,
                       wages_total = 0,
                       emp_nonsuppressed = 0,
                       wages_nonsuppressed = 0))
table(dta_balanced$year)
table(dta_balanced$subsector)


# join with Pop data 
table(Pop$year)
dta_balanced1 <- left_join(dta_balanced, Pop)

# create emp_to_pop ratio
names(dta_balanced1)
colSums(is.na(dta_balanced1))
table(dta_balanced1$year)

dta_balanced1 <- dta_balanced1 %>% mutate(EPOP = if_else(total_pop != 0 , emp/total_pop, 0))
summary(dta_balanced1)

test <-  dta_balanced1 %>% filter (EPOP >1)
dta_balanced1 <- dta_balanced1 %>% mutate(EPOP =if_else(EPOP >1, is.na(EPOP), EPOP))


# create log changes:
dta_balanced2 <- dta_balanced1 |>
  group_by(fips, subsector) |>
  arrange(year) |>
  mutate(
    EPOP_2017        = EPOP[year == 2017],
    emp_2017         = emp[year == 2017],
    wages_2017       = wages_total[year == 2017],
    
    # changes relative to 2017
    delta_EPOP       = EPOP - EPOP_2017,
    
    # logs (NA if 0)
    log_emp          = ifelse(emp > 0, log(emp), NA),
    log_wages        = ifelse(wages_total > 0, log(wages_total), NA),
    log_emp_2017     = ifelse(emp_2017 > 0, log(emp_2017), NA),
    log_wages_2017   = ifelse(wages_2017 > 0, log(wages_2017), NA),
    
    # log changes relative to 2017
    delta_log_emp    = log_emp - log_emp_2017,
    delta_log_wages  = log_wages - log_wages_2017,
    
    # pre-trend: 2016 vs 2017
    delta_EPOP_pretrend      = EPOP[year == 2016] - EPOP_2017,
    delta_log_emp_pretrend   = ifelse(emp[year == 2016] > 0, log(emp[year == 2016]), NA) - log_emp_2017,
    delta_log_wages_pretrend = ifelse(wages_total[year == 2016] > 0, log(wages_total[year == 2016]), NA) - log_wages_2017
  ) |>
  select(-EPOP_2017, -emp_2017, -wages_2017, -log_emp_2017, -log_wages_2017) |>
  ungroup()


################################################################################

write_csv(dta_balanced2 , "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/EPOP_emp_county_subsector_2015_2019.csv")








