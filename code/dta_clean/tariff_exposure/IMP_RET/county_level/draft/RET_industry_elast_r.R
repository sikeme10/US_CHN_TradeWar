


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
gamma <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6/RET_i_naics6_elast_IV.csv")
# gamma <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6/RET_i_CHN_naics6.csv")
# gamma <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/RET_i_Chen_CHN.csv")

labor<- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/QCEW_2012_naics6_county.csv")

# get subsector classification from Diane
sectors <- read_csv("crosswalk/HS6_NAICS_Diane/NAICS_industry_2012.csv")
HS_NAICS <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/crosswalk/clean_HS6_naics6_2012.csv")


################################################################################


names(gamma)
names(labor)
unique(gamma$year)
# can choose to have
gamma1 <- gamma %>% select(year, naics, NAICS_description,
                           RET_i_tariff , RET_i_NTB,RET_i_NTB_IV)
length(unique(gamma1$naics))
length(unique(labor$naics))


naics_to_use <- unique(gamma1$naics)

# checks class
sapply(gamma1, class)
sapply(sectors, class)




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

# # get sector level labor to create sector level labor shares at county
# sector_labor <- labor %>%   group_by(fips, sector_label) %>% 
#   summarise(
#     sector_estabs_county     = sum(estabs,       na.rm = TRUE),
#     sector_emp_county        = sum(emp,          na.rm = TRUE),
#     sector_wages_total_county = sum(wages_total, na.rm = TRUE)  )
# 
# # get county level labor to create total labor shares at county
# tot_labor <- labor %>%   group_by(fips) %>% 
#   summarise(
#     tot_estabs_county     = sum(estabs,       na.rm = TRUE),
#     tot_emp_county        = sum(emp,          na.rm = TRUE),
#     tot_wages_total_county = sum(wages_total, na.rm = TRUE)  )
# 
# # Step 2: compute labor shares at naics-county level from labor ONLY
# labor <- labor %>%
#   left_join(tot_labor, by = "fips") %>%
#   left_join(sector_labor, by = c("fips", "sector_label")) %>%
#   mutate(share_tot_labor_ir = if_else(tot_emp_county > 0, emp / tot_emp_county, 0),
#          share_sector_labor_ir = if_else(tot_emp_county > 0, emp / tot_emp_county, 0),)
# summary(labor)


# get sector level labor to create sector level labor shares at county

# get county level labor to create total labor shares at county
tot_labor <- labor %>%   group_by(fips) %>% 
  summarise(
    tot_estabs_county     = sum(estabs,       na.rm = TRUE),
    tot_emp_county        = sum(emp,          na.rm = TRUE),
    tot_wages_total_county = sum(wages_total, na.rm = TRUE)  )

# Step 2: compute labor shares at naics-county level from labor ONLY
labor <- labor %>%
  left_join(tot_labor, by = "fips") %>%
  mutate(share_tot_labor_ir = if_else(tot_emp_county > 0, emp / tot_emp_county, 0))


################################################################################

# Step 3: now merge with gamma (which has year variation)
merge_data <- left_join(gamma1, labor, join_by(naics == naics))
colSums(is.na(merge_data))


################################################################################
# get RET_ir at naics and county level
################################################################################

names(merge_data)

# Step 4: create exposure — share is now year-invariant, gamma varies by year
merge_data <- merge_data %>% mutate(
  RET_tariff_tot_ir = RET_i_tariff * share_tot_labor_ir,
  RET_NTB_tot_ir    = RET_i_NTB    * share_tot_labor_ir,
  RET_NTB_tot_ir_IV    = RET_i_NTB_IV    * share_tot_labor_ir)

length(unique(merge_data$naics))
summary(merge_data)



# get total labor by industry NAICS 3 digit code
unique(nchar(merge_data$naics))
class(merge_data$naics)
unique(merge_data$sector_label)

merge_data <- merge_data %>% mutate( sector = case_when( sector_label =="Agriculture"  ~ "Ag", sector_label == "Manufacturing" ~ "Manu",
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

###############################################################################

merge_data1 <- left_join(merge_data, sectors)
colSums(is.na(merge_data1))
unique(merge_data1$subsector)
test <- merge_data1 %>%  filter(is.na(subsector))
unique(test$NAICS_description)


###############################################################################
# drop mining?
###############################################################################

# separate mining in subscetor from nonag 
names(merge_data1)
unique(merge_data1$ag_subsector)
table(merge_data1$subsector)
merge_data1$subsector[grepl("mining", merge_data1$ag_subsector)] <- "mining"

###############################################################################
# total by industries 
###############################################################################

# a) aggregate at county level: get REP_r
names(merge_data)
RET_tot_r <- merge_data %>% group_by(year, fips, sector) %>% 
  summarise(RET_tariff_tot_r = sum(RET_tariff_tot_ir, na.rm = TRUE),
            RET_NTB_tot_r = sum(RET_NTB_tot_ir, na.rm = TRUE),
            RET_NTB_tot_IV_r = sum(RET_NTB_tot_ir_IV, na.rm = TRUE))
summary(RET_tot_r)  
unique(RET_tot_r$year)
test <- RET_tot_r %>% filter(RET_NTB_tot_IV_r > 1.5)  

write_csv(RET_tot_r, paste0("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6/county/RET_elast_r_industry_totlabor_IV.csv") )


# a) aggregate at county level: get REP_r
names(merge_data)
RET_sect_r <- merge_data %>% group_by(year, fips, sector) %>% 
  summarise(RET_tariff_sect_r = sum(RET_tariff_sect_ir, na.rm = TRUE),
            RET_NTB_sect_r = sum(RET_NTB_sect_ir, na.rm = TRUE),
            RET_NTB_sect_IV_r = sum(RET_NTB_sect_ir_IV, na.rm = TRUE))
summary(RET_sect_r)  
unique(RET_sect_r$year)
# test <- RET_sect_r %>% filter(RET_NTB_r > 1.5)  

write_csv(RET_sect_r, paste0("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6/county/RET_elast_r_industry_labor_IV.csv") )



###############################################################################
# total by industries 
###############################################################################
names(merge_data1)
test <- merge_data1 %>% filter(subsector == "crop" & RET_NTB_tot_ir != 0)


# a) aggregate at county level: get REP_r
names(merge_data1)
RET_sub_tot_r <- merge_data1 %>% group_by(year, fips, subsector) %>% 
  summarise(RET_tariff_tot_r = sum(RET_tariff_tot_ir, na.rm = TRUE),
            RET_NTB_tot_r = sum(RET_NTB_tot_ir, na.rm = TRUE),
            RET_NTB_tot_IV_r = sum(RET_NTB_tot_ir_IV, na.rm = TRUE))
summary(RET_sub_tot_r)  
unique(RET_sub_tot_r$year)
test <- RET_sub_tot_r %>% filter(RET_NTB_tot_IV_r > 1.5)  

write_csv(RET_sub_tot_r, paste0("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6/county/RET_elast_r_subsectors_IV.csv") )



# check which undsitres has high RET 

high <- merge_data1 %>%
  distinct(naics, NAICS_description,ag_subsector, sector, RET_NTB_tot_ir) %>%
  arrange(desc(RET_NTB_tot_ir))



