


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
gamma <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6/RET_i_naics6_IV.csv")
# gamma <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6/RET_i_CHN_naics6.csv")
# gamma <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/RET_i_Chen_CHN.csv")

labor<- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/QCEW_2012_naics6_CZ.csv")

# get subsector classification from Diane
sectors <- read_csv("crosswalk/HS6_NAICS_Diane/NAICS_industry_2012.csv")


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
# Step 1: compute total employment at CZ level from labor ONLY (no year dimension)
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

# get sector level labor to create sector level labor shares at CZ
sector_labor <- labor %>%   group_by(czone_2012, sector_label) %>% 
  summarise(
    sector_estabs_CZ     = sum(estabs,       na.rm = TRUE),
    sector_emp_CZ        = sum(emp,          na.rm = TRUE),
    sector_wages_total_CZ = sum(wages_total, na.rm = TRUE)  )

# get CZ level labor to create total labor shares at CZ
tot_labor <- labor %>%   group_by(czone_2012) %>% 
  summarise(
    tot_estabs_CZ     = sum(estabs,       na.rm = TRUE),
    tot_emp_CZ        = sum(emp,          na.rm = TRUE),
    tot_wages_total_CZ = sum(wages_total, na.rm = TRUE)  )

# Step 2: compute labor shares at naics-CZ level from labor ONLY
labor <- labor %>%
  left_join(tot_labor, by = "czone_2012") %>%
  left_join(sector_labor, by = c("czone_2012", "sector_label")) %>%
  mutate(share_tot_labor_ir = if_else(tot_emp_CZ > 0, emp / tot_emp_CZ, 0),
         share_sector_labor_ir = if_else(tot_emp_CZ > 0, emp / tot_emp_CZ, 0),)
summary(labor)

################################################################################

# Step 3: now merge with gamma (which has year variation)
merge_data <- left_join(gamma1, labor, join_by(naics == naics))
colSums(is.na(merge_data))


################################################################################
# get RET_ir at naics and CZ level
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

# merge with sector definition from Diane
names(sectors)
sectors <- sectors %>% select(naics, subsector, ag_subsector)
sectors <- sectors %>% distinct() %>% filter(!is.na(naics))
sectors <- sectors %>%  group_by(naics, subsector) %>%
  summarise(ag_subsector = paste(ag_subsector, collapse = ", "), .groups = "drop")
length(unique(sectors$naics))
test <- sectors %>% filter(duplicated(naics) | duplicated(naics, fromLast = TRUE))
sectors <- sectors %>%  group_by(naics) %>%
  filter(if (n_distinct(subsector) > 1) subsector == "crop" else TRUE) %>%  ungroup()

merge_data1 <- left_join(merge_data, sectors)
colSums(is.na(merge_data1))
unique(merge_data1$subsector)
test <- merge_data1 %>%  filter(is.na(subsector))
unique(test$NAICS_description)



merge_data1 <- merge_data1 %>%
  mutate(subsector = case_when(
    !is.na(subsector)                                                        ~ subsector,
    sector == "Manu"                                                         ~ "nonag",
    NAICS_description == "Software publishers"                               ~ "nonag",
    NAICS_description %in% c(   "Sugarcane farming (11193)",    "Nursery and tree production (111421)",
      "Floriculture production (111422)"    )                                                                        ~ "crop",
    NAICS_description %in% c( "Beef cattle ranching and farming (112111)",
      "Cattle feedlots (112112)", "Chicken egg production (11231)",
      "Broilers and other meat-type chicken production (11232)",
      "Turkey production (11233)",  "Poultry hatcheries (11234)",
      "Other poultry production (11239)"    )    ~ "livestock",    
    TRUE  ~ subsector  ))

# verify
colSums(is.na(merge_data1))
unique(merge_data1$subsector)
unique(merge_data1$ag_subsector )


colSums(is.na(merge_data1))

names(merge_data1)

crop <- merge_data1 %>% filter(subsector== "crop")
unique(crop$NAICS_description)

livestock  <- merge_data1 %>% filter(subsector== "livestock")
unique(livestock$NAICS_description)


###############################################################################
# total by industries 
###############################################################################

# a) aggregate at CZ level: get REP_r at crop level
names(crop)
RET_crop_r <- crop %>% group_by(year, czone_2012, NAICS_description) %>% 
  summarise(RET_i_tariff = sum(RET_i_tariff, na.rm = TRUE),
            RET_i_NTB = sum(RET_i_NTB, na.rm = TRUE),
            RET_i_NTB_IV = sum(RET_i_NTB_IV, na.rm = TRUE),
            
            RET_tariff_tot_r = sum(RET_tariff_tot_ir, na.rm = TRUE),
            RET_NTB_tot_r = sum(RET_NTB_tot_ir, na.rm = TRUE),
            RET_NTB_tot_IV_r = sum(RET_NTB_tot_ir_IV, na.rm = TRUE))
summary(RET_crop_r)  

write_csv(crop, paste0("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6/RET_r_crop_IV.csv") )



# b) aggregate at CZ level: get REP_r at livestock level
names(livestock)
RET_livestock_r <- livestock %>% group_by(year, czone_2012, NAICS_description) %>% 
  summarise(RET_i_tariff = sum(RET_i_tariff, na.rm = TRUE),
            RET_i_NTB = sum(RET_i_NTB, na.rm = TRUE),
            RET_i_NTB_IV = sum(RET_i_NTB_IV, na.rm = TRUE),
            
            RET_tariff_tot_r = sum(RET_tariff_tot_ir, na.rm = TRUE),
            RET_NTB_tot_r = sum(RET_NTB_tot_ir, na.rm = TRUE),
            RET_NTB_tot_IV_r = sum(RET_NTB_tot_ir_IV, na.rm = TRUE))
summary(RET_livestock_r)  

write_csv(RET_livestock_r, paste0("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6/RET_r_livestock_IV.csv") )




