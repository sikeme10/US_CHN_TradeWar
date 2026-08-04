


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

labor<- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/QCEW_2012_naics6_county.csv")

# get subsector classification from Diane
sectors <- read_csv("crosswalk/HS6_NAICS_Diane/NAICS_industry_2012.csv")
HS_NAICS <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/crosswalk/clean_HS6_naics6_2012.csv")


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


################################################################################
# Step 1: compute total employment at county level from labor ONLY (no year dimension)
names(labor)
unique(labor$naics)
class(labor$naics)
unique(nchar(labor$naics))

labor$sector <- substr(as.character(labor$naics), 1, 2)
unique(labor$sector)

labor$sector_label <- dplyr::case_when(
  labor$sector == "11"                        ~ "Agriculture",
  labor$sector == "21"                        ~ "Mining",
  labor$sector == "23"                        ~ "Construction",
  labor$sector %in% c("31", "32", "33")       ~ "Manufacturing",
  labor$sector == "42"                        ~ "Wholesale Trade",
  labor$sector %in% c("44", "45")             ~ "Retail Trade",
  labor$sector %in% c("48", "49")             ~ "Transportation and Warehousing",
  labor$sector == "51"                        ~ "Information",
  labor$sector == "52"                        ~ "Finance and Insurance",
  labor$sector == "53"                        ~ "Real Estate and Rental and Leasing",
  labor$sector == "54"                        ~ "Professional, Scientific, and Technical Services",
  labor$sector == "55"                        ~ "Management of Companies and Enterprises",
  labor$sector == "56"                        ~ "Administrative and Support and Waste Management",
  labor$sector == "61"                        ~ "Educational Services",
  labor$sector == "62"                        ~ "Health Care and Social Assistance",
  labor$sector == "71"                        ~ "Arts, Entertainment, and Recreation",
  labor$sector == "72"                        ~ "Accommodation and Food Services",
  labor$sector == "81"                        ~ "Other Services",
  labor$sector == "92"                        ~ "Public Administration",
  TRUE                                        ~ "Unknown")

# labor <-labor%>% filter(naics %in% naics_to_use)

# get sector level labor to create sector level labor shares at county
sector_labor <- labor %>%   group_by(fips, sector_label) %>% 
  summarise(
    sector_estabs_county     = sum(estabs,       na.rm = TRUE),
    sector_emp_county        = sum(emp,          na.rm = TRUE),
    sector_wages_total_county = sum(wages_total, na.rm = TRUE)  )

# get county level labor to create total labor shares at county
tot_labor <- labor %>%   group_by(fips) %>% 
  summarise(
    tot_estabs_county     = sum(estabs,       na.rm = TRUE),
    tot_emp_county        = sum(emp,          na.rm = TRUE),
    tot_wages_total_county = sum(wages_total, na.rm = TRUE)  )

# Step 2: compute labor shares at naics-county level from labor ONLY
labor <- labor %>%
  left_join(tot_labor, by = "fips") %>%
  left_join(sector_labor, by = c("fips", "sector_label")) %>%
  mutate(share_tot_labor_ir = if_else(tot_emp_county > 0, emp / tot_emp_county, 0),
         share_sector_labor_ir = if_else(tot_emp_county > 0, emp / tot_emp_county, 0),)
summary(labor)


labor <-labor%>% filter(naics %in% naics_to_use)

################################################################################

# Step 3: now merge with gamma (which has year variation)
merge_data <- left_join(gamma, labor, join_by(naics == naics))
length(unique(merge_data$naics))
names(merge_data)

# Step 4: create exposure — share is now year-invariant, gamma varies by year
merge_data <- merge_data %>% mutate(
  IMP_tariff_tot_ir = IMP_it_2017 * share_tot_labor_ir,
  IMP_tariff_sect_ir = IMP_it_2017 * share_tot_labor_ir)



# get total labor by industry NAICS 3 digit code
unique(nchar(merge_data$naics))
class(merge_data$naics)
unique(merge_data$sector_label)

merge_data <- merge_data %>% 
  mutate(sector = case_when( sector_label =="Agriculture"  ~ "Ag", sector_label == "Manufacturing" ~ "Manu",
                             TRUE ~ "Other"       )  )
table(merge_data$sector)


###############################################################################
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

sectors <- HS_NAICS %>%  group_by(naics, subsector) %>%
  summarise(ag_subsector = paste(ag_subsector, collapse = ", "), .groups = "drop")
length(unique(sectors$naics))

merge_data1 <- left_join(merge_data, sectors)
colSums(is.na(merge_data1))
unique(merge_data1$subsector)
test <- merge_data1 %>%  filter(is.na(subsector))
unique(test$NAICS_description)

# quick fix on potato
merge_data1 <- merge_data1 %>% mutate(subsector = ifelse(naics == "111219", "crop", subsector),
                                      ag_subsector = ifelse(naics == "111219", "Vegetable Farming", ag_subsector))



test <- merge_data1 %>% filter(subsector == "crop" & IMP_tariff_tot_ir != 0)
unique(test$NAICS_description)




###############################################################################
# total by industries 
###############################################################################
# a) aggregate at county level: get REP_r
names(merge_data1)
IMP_tot_r <- merge_data1 %>% group_by(year, fips, subsector) %>% 
  summarise(IMP_tariff_tot_ir = sum(IMP_tariff_tot_ir, na.rm = TRUE),
            IMP_tariff_sect_ir = sum(IMP_tariff_sect_ir, na.rm = TRUE))
summary(IMP_tot_r)  
unique(IMP_tot_r$year)
test <- IMP_tot_r %>% filter(IMP_tariff_tot_ir > 1.5)  

write_csv(IMP_tot_r, paste0("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6/county/IMP_r_subsectors_IV.csv") )



