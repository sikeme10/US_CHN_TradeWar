


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

labor<- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/QCEW_2012_naics6_CZ.csv")

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

# crop <- merge_data1 %>% filter(subsector== "crop")
# unique(crop$NAICS_description)
# crop <- crop %>%
#   mutate(sector_group = case_when(
#     # Manufacturing — must come BEFORE farming categories
#     grepl("milling|mfg|manufacturing|canning|processing|roasting",  NAICS_description, ignore.case = TRUE) ~ "Food Manufacturing",
#     # Grain & Oilseed farming
#     grepl("Soybean|Oilseed|Dry pea|Wheat|Corn farming|Rice farming|Other grain",    NAICS_description, ignore.case = TRUE) ~ "Grain & Oilseed Farming",
#     # Fruit & Nut
#     grepl("grove|orchard|vineyard|Berry|strawberry|Tree nut|noncitrus fruit",   NAICS_description, ignore.case = TRUE) ~ "Fruit & Nut Farming",
#     # Vegetable
#     grepl("Potato|vegetable|melon", NAICS_description, ignore.case = TRUE) ~ "Vegetable Farming",
#     
#     # Cotton
#     grepl("Cotton", NAICS_description, ignore.case = TRUE) ~ "Cotton Farming",
#     grepl("textile", NAICS_description, ignore.case = TRUE) ~ "Textile Manufacturing",
#     
#     # Tobacco
#     grepl("Tobacco", NAICS_description, ignore.case = TRUE) ~ "Tobacco Farming",
#     
#     # Nursery
#     grepl("Nursery|Floriculture", NAICS_description, ignore.case = TRUE) ~ "Nursery & Floriculture",
#     
#     # Other crops
#     grepl("Sugarcane|Hay|Sugar beet",   NAICS_description, ignore.case = TRUE) ~ "Other Crops",
#     
#     TRUE ~ "Other"
#   ))
# table(crop$sector_group)
# print(crop %>%   distinct(sector_group, NAICS_description) %>%   arrange(sector_group), n = Inf)
# 
# 
# 
# livestock  <- merge_data1 %>% filter(subsector== "livestock")
# unique(livestock$NAICS_description)
# 
# livestock <- livestock %>%
#   mutate(sector_group = case_when(
#     # Cattle
#     grepl("Beef cattle|Cattle feedlots", NAICS_description) ~ "Cattle Farming",
#     
#     # Hogs
#     grepl("Hog and pig", NAICS_description) ~ "Hog & Pig Farming",
#     
#     # Meat processing — must come BEFORE Poultry Farming
#     grepl("slaughtering|Rendering|byproduct|Poultry processing", NAICS_description, ignore.case = TRUE) ~ "Meat Processing",
#     
#     # Poultry farming
#     grepl("Chicken|Turkey|Poultry|Broiler", NAICS_description, ignore.case = TRUE) ~ "Poultry Farming",
#     
#     # Small livestock
#     grepl("Sheep|Goat", NAICS_description) ~ "Sheep & Goat Farming",
#     
#     # Other animal production
#     grepl("Apiculture|Horse|Fur-bearing|All other animal", NAICS_description) ~ "Other Animal Production",
#     
#     # Dairy manufacturing
#     grepl("milk|butter|Cheese|dairy|Ice cream", NAICS_description, ignore.case = TRUE) ~ "Dairy Manufacturing",
#     
#     # Leather
#     grepl("Leather|hide", NAICS_description, ignore.case = TRUE) ~ "Leather & Hides",
#     
#     TRUE ~ "Other"
#   ))
# print(livestock %>%   distinct(sector_group, NAICS_description) %>%   arrange(sector_group), n = Inf)



crop <- merge_data1 %>% filter(subsector== "crop")
unique(crop$NAICS_description)
crop <- crop %>%
  mutate(sector_group = case_when(
    # Non-food manufacturing carve-outs FIRST
    grepl("miscellaneous chemical|miscellaneous manufacturing|textile|Pulp mills", NAICS_description, ignore.case = TRUE) ~ "Other Manufacturing",
    
    # Food & beverage manufacturing (all lumped together)
    grepl("milling|processing|manufacturing|canning|refining|blending|confectionery|cereal|breakfast|snack|coffee|tea|syrup|spice|sauce|mayonnaise|dressing|nut.*butter|roasted|Breweries|Wineries|Distilleries",
          NAICS_description, ignore.case = TRUE) ~ "Food Manufacturing",
    
    # Farming categories
    grepl("Soybean|Oilseed|Dry pea|Wheat|Corn farming|Rice farming|Other grain",
          NAICS_description, ignore.case = TRUE) ~ "Grain & Oilseed Farming",
    
    
    grepl("orchard|vineyard|Berry|strawberry|Tree nut|noncitrus fruit|Orange groves|Citrus",
          NAICS_description, ignore.case = TRUE) ~ "Fruit & Nut Farming",
    
    grepl("Potato|vegetable|melon", NAICS_description, ignore.case = TRUE) ~ "Vegetable Farming",
    
    grepl("Cotton", NAICS_description, ignore.case = TRUE) ~ "Cotton Farming",
    grepl("Tobacco", NAICS_description, ignore.case = TRUE) ~ "Tobacco Farming",
    grepl("Nursery|Floriculture", NAICS_description, ignore.case = TRUE) ~ "Nursery & Floriculture",
    grepl("Sugarcane|Hay|Sugar beet", NAICS_description, ignore.case = TRUE) ~ "Other Crops",
    
    TRUE ~ "Other"
  ))
table(crop$sector_group)
print(crop %>%   distinct(sector_group, NAICS_description) %>%   arrange(sector_group), n = Inf)



livestock  <- merge_data1 %>% filter(subsector== "livestock")
unique(livestock$NAICS_description)

livestock <- livestock %>%
  mutate(sector_group = case_when(
    # Cattle (kept in case of future data)
    grepl("Beef cattle|Cattle feedlots|Dairy cattle", NAICS_description, ignore.case = TRUE) ~ "Cattle Farming",
    
    # Hogs
    grepl("Hog and pig", NAICS_description, ignore.case = TRUE) ~ "Hog & Pig Farming",
    
    # Meat processing — BEFORE poultry farming
    grepl("slaughtering|Rendering|byproduct|Poultry processing", NAICS_description, ignore.case = TRUE) ~ "Meat Processing",
    
    # Poultry farming (kept in case of future data)
    grepl("Chicken|Turkey|Broiler|Poultry(?! processing)", NAICS_description, ignore.case = TRUE, perl = TRUE) ~ "Poultry Farming",
    
    # Small livestock
    grepl("Sheep|Goat", NAICS_description, ignore.case = TRUE) ~ "Sheep & Goat Farming",
    
    # Other animal production
    grepl("Apiculture|Horse|Fur-bearing|All other animal production", NAICS_description, ignore.case = TRUE) ~ "Other Animal Production",
    
    # Dairy manufacturing
    grepl("milk|butter|Cheese|dairy|Ice cream", NAICS_description, ignore.case = TRUE) ~ "Dairy Manufacturing",
    
    # Leather
    grepl("Leather|hide", NAICS_description, ignore.case = TRUE) ~ "Leather & Hides",
    
    # Catch-all food & chemical mfg linked to livestock chain
    grepl("miscellaneous food manufacturing|organic chemical manufacturing|Dog and cat food|animal food manufacturing", NAICS_description, ignore.case = TRUE) ~ "Other Manufacturing",
    
    TRUE ~ "Other"  ))
print(livestock %>%   distinct(sector_group, NAICS_description) %>%   arrange(sector_group), n = Inf)



###############################################################################
# total by industries 
###############################################################################

# a) aggregate at CZ level: get REP_r at crop level
names(crop)
RET_crop_r <- crop %>% group_by(year, czone_2012, sector_group) %>% 
  summarise(RET_i_tariff = sum(RET_i_tariff, na.rm = TRUE),
            RET_i_NTB = sum(RET_i_NTB, na.rm = TRUE),
            RET_i_NTB_IV = sum(RET_i_NTB_IV, na.rm = TRUE),
            
            RET_tariff_tot_r = sum(RET_tariff_tot_ir, na.rm = TRUE),
            RET_NTB_tot_r = sum(RET_NTB_tot_ir, na.rm = TRUE),
            RET_NTB_tot_IV_r = sum(RET_NTB_tot_ir_IV, na.rm = TRUE))
summary(RET_crop_r)  

write_csv(RET_crop_r, paste0("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6/RET_elast_r_crop_IV.csv") )



# b) aggregate at CZ level: get REP_r at livestock level
names(livestock)
RET_livestock_r <- livestock %>% group_by(year, czone_2012, sector_group) %>% 
  summarise(RET_i_tariff = sum(RET_i_tariff, na.rm = TRUE),
            RET_i_NTB = sum(RET_i_NTB, na.rm = TRUE),
            RET_i_NTB_IV = sum(RET_i_NTB_IV, na.rm = TRUE),
            
            RET_tariff_tot_r = sum(RET_tariff_tot_ir, na.rm = TRUE),
            RET_NTB_tot_r = sum(RET_NTB_tot_ir, na.rm = TRUE),
            RET_NTB_tot_IV_r = sum(RET_NTB_tot_ir_IV, na.rm = TRUE))
summary(RET_livestock_r)  

write_csv(RET_livestock_r, paste0("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6/RET_elast_r_livestock_IV.csv") )




