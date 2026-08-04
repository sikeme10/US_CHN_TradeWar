


rm(list=ls())
# Set directory
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git/data")
getwd()

library(stringr)
library(haven)
library(concordance)
dta_origin <- read_dta("crosswalk/HS6_NAICS_Diane/mappinghsnaics6.dta")
names(dta_origin)

#################################################################################

# Product level codes are at HS0 revisions 

dta <- dta_origin %>% rename(  ProductCode_H0 = hs,  naics6 = naics)
class(dta$ProductCode_H0)

# product code as character:

dta$ProductCode_H0 <- str_pad(as.character(dta$ProductCode_H0), width = 6, side = "left", pad = "0")
length(unique(dta$ProductCode_H0))
unique(dta$ProductCode_H0)
unique(nchar(dta$ProductCode_H0))


# concordance:
dta$ProductCode_H4 <-  concord_hs(sourcevar = dta$ProductCode_H0,  origin = "HS0",  destination = "HS4", dest.digit  = 6 )
dta$ProductCode_H5 <-  concord_hs(sourcevar = dta$ProductCode_H0,  origin = "HS0",  destination = "HS5", dest.digit  = 6 )
# dta$ProductCode_H4 <-  concord_hs(sourcevar = dta$ProductCode_H0,  origin = "HS4",  destination = "HS0", dest.digit  = 6 )
dta %>%  filter(ProductCode_H0 != ProductCode_H4) %>% summarise(n_distinct_observations = n())
length(unique(dta$ProductCode_H4))
length(unique(dta$ProductCode_H5))
length(unique(dta$ProductCode_H0))
colSums(is.na(dta))


class(dta$ProductCode_H4)

dta1 <-dta %>% rename(HS6 = ProductCode_H4)
dta1$HS2 <- as.character(substr(dta1$HS6, 1, 2))


################################################################################
# NAICS from 2002 to NAICS 2012
################################################################################
# current naics codes are from 2002, so we change it to NAICS 2012 (output revision NAICS)
remotes::install_github("insongkim/concordance")
library(concordance)

library(concordance)
library(dplyr)
library(tidyr)
class(dta1$naics6)
unique((nchar(dta1$naics6)))
length(unique(dta1$naics6))


dta1 <- dta1 %>% mutate(naics6 = as.character(naics6))

codes <- dta1 %>%
  filter(!is.na(naics6), !naics6 %in% c("910000", "920000", "990000")) %>%
  distinct(naics6) %>%
  pull(naics6)

res <- concord_naics(sourcevar   = codes,
                     origin      = "NAICS2002",
                     destination = "NAICS2012",
                     dest.digit  = 6,
                     all         = FALSE)

xwalk <- tibble(naics6 = codes, naics6_2012 = as.character(res))

# write_csv(xwalk, "crosswalk/HS6_NAICS_Diane/concordance_naics_2012.csv")


dta1 <- dta1 %>% left_join(xwalk, by = "naics6")
# check the ones that are NA

NAs <- dta1 %>% filter(is.na(naics6_2012))
unique(NAs$naics6)
# rename naics6 so that it is correctly identified in future analysis
dta1 <-  dta1 %>% select(-naics6) %>% rename(naics6 = naics6_2012)

################################################################################
names(dta)

dta1 <- dta1 %>% select(HS2 , HS6 , naics2,naics3,naics4, naics6, j, crop) %>% rename(
  naics6_D = naics6 , naics2_D = naics2, naics3_D = naics3 , naics4_D = naics4)



## 
# get 2012 NAICS code 

write_csv(dta1, "crosswalk/HS6_NAICS_Diane/NAICS_HS_2012.csv")





################################################################################
# just get different NAICS codes and their crop
################################################################################

dta <- read_csv( "crosswalk/HS6_NAICS_Diane/NAICS_HS_2012.csv")


names(dta)
dta <- dta %>%  select(naics2, naics3, naics4, naics6, j, crop) %>%
  rename( naics = naics6, subsector    = j,    ag_subsector = crop  )
unique(dta$subsector)


dta <- dta %>% distinct()


names(dta)
dta <- dta %>% select(naics, subsector, ag_subsector)
dta <- dta %>% distinct() %>% filter(!is.na(naics))
dta <- dta %>%  group_by(naics, subsector) %>%
  summarise(ag_subsector = paste(ag_subsector, collapse = ", "), .groups = "drop")
length(unique(dta$naics))


test <- dta %>% filter(duplicated(naics) | duplicated(naics, fromLast = TRUE))
dta <- dta %>%  group_by(naics) %>%
  filter(if (n_distinct(subsector) > 1) subsector == "crop" else TRUE) %>%  ungroup()




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



write_csv(dta,"crosswalk/HS6_NAICS_Diane/NAICS_industry_2012.csv" )




################################################################################
# at HS5 revision level
################################################################################


class(dta$ProductCode_H5)

dta2 <-dta %>% rename(HS6 = ProductCode_H5)
names(dta2)
length(unique(dta2$HS6))

dta2 <- dta2 %>% select(HS6 , j, crop)

dta2 <- dta2 %>% filter(!is.na(HS6))

write_csv(dta2,"crosswalk/HS6_NAICS_Diane/HS6_HS5_revision_industry_2012.csv" )
colSums(is.na(dta2))
