


library(stringr)
library(haven)
library(concordance)
library(readr)
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


# import data 



library(haven)
dta_schott <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/crosswalk/schott/naics_HS_schott_2012.csv")
output_NAICS6 <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/Census_output/output_NAICS_6.csv")
HS_product <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/HS_codes/HS_2012_2017_merged.csv")



names(dta_schott)
names(HS_product)

######################################################################################
# drop naics where have "X"
dta_schott$naics <- as.numeric(dta_schott$naics)
dta_schott <- dta_schott %>% filter(!is.na(naics))

######################################################################################
# checks 
length(unique(dta_schott$naics))
length(unique(dta_schott$HS6))


length(unique(output_NAICS6$NAICS6_2012))
length(unique(HS_product$HS_2012_Product_Code))



# 1) check product codes first:

sum(unique(dta_schott$HS6) %in% unique(HS_product$HS_2012_Product_Code))

# filter product codes that are in HS_product$HS_2012_Product_Code
dta_schott <- dta_schott %>% filter(HS6 %in% unique(HS_product$HS_2012_Product_Code))
length(unique(dta_schott$HS6))
colSums(is.na(dta_schott))
dta_schott


# 2) check NAICS codes:

sum(unique(dta_schott$naics) %in% unique(output_NAICS6$NAICS6_2012))

# filter product codes that are in output_NAICS6$NAICS6_2012
dta_schott <- dta_schott %>% filter(naics %in% unique(output_NAICS6$NAICS6_2012))
length(unique(dta_schott$naics))
length(unique(dta_schott$HS6))
colSums(is.na(dta_schott))




################################################################################
# multiple products codes map to multiple NAICS code and vice versa


# How many unique NAICS each HS6 maps to
HS6_to_NAICS <- dta_schott %>%  group_by(HS6) %>%
  summarise(n_naics = n_distinct(naics)) %>%  arrange(desc(n_naics))
HS6_to_NAICS

# How many unique HS6 each NAICS maps to
NAICS_to_HS6 <- dta_schott %>%  group_by(naics) %>%
  summarise(n_hs6 = n_distinct(HS6)) %>%  arrange(desc(n_hs6))
NAICS_to_HS6



######################################################################################

# check product codes that are in Charlton and not in Schott and vice versa
names(dta_schott)

dta_schott <-dta_schott %>% select(HS6 , naics)

# drop duplicates
sum(duplicated(dta_schott[, c("HS6", "naics")]))
dta_schott <- dta_schott %>% distinct()



######################################################################################
# whatever product was not matched we can use package to get back 
merge <- dta_schott

length(unique(merge$HS6))
class(merge$HS6)
length(unique(merge$naics))
length(unique(HS_product$HS_2012_Product_Code))
colSums(is.na(merge))


Product_codes1 <- HS_product %>% select(HS_2012_Product_Code, HS_2012_Product_Description) %>%
  rename(HS6 = HS_2012_Product_Code)
length(unique(Product_codes1$HS6))


merge1 <- full_join(merge, Product_codes1, by = "HS6")
length(unique(merge1$HS6))
colSums(is.na(merge1))


colSums(is.na(merge1))
merge1 <- merge1 %>% filter(!is.na(HS6))

# make naics code in numeric
length(unique(merge1$naics))
unique(nchar(merge1$naics))


library(concordance)
merge1 <- merge1 %>%
  mutate(naics1 = ifelse(is.na(naics),
                         sapply(HS6, function(x) {
                           result <- concord_hs_naics(x, origin = "HS4", destination = "NAICS", dest.digit = 6, all = FALSE)
                           result[[1]]
                         }),
                         naics))
colSums(is.na(merge1))
length(unique(merge1$naics1))

merge2 <- merge1 %>% select(HS6, naics1) %>% rename(naics = naics1)
length(unique(merge1$naics1))
length(unique(merge1$HS6))


merge3 <- distinct(merge2)
length(unique(merge3$naics))
length(unique(merge3$HS6))
colSums(is.na(merge3))



# get at HS6-naics 6 level

write_csv(merge3, "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/crosswalk/clean_HS6_naics6_schott_2012.csv")

# 
# ######################################################################################
# 
# # at naics 4  
# names(dta1)
# 
# # drop naics 6 levels to get distinct codes again 
# dta2 <- dta1 %>% select(- naics6_D, -naics )
# 
# 
# # drop duplicates 
# dta2 <- distinct(dta2)
# length(unique(dta1$HS6))
# 
# dta3 <- dta2 %>% rename(naics3_S = naics3, naics4_S = naics4) %>% 
#   mutate(naics3 = if_else(is.na(naics3_D), naics3_S, naics3_D ),
#          naics4 = if_else(is.na(naics4_D), naics4_S, naics4_D ))
# 
# 
# write_csv(dta3, "/data/sikeme/TRADE/NTM_trade_war/data/crosswalk/clean_HS6_naics4_2012.csv")
# 
# 
# ######################################################################################
# 
# # at naics 3  
# names(dta3)
# 
# # drop naics 4 levels to get distinct codes again 
# dta4 <- dta3 %>% select(-naics4 , -naics4_D,-naics4_S, -naics3_S )
# 
# 
# # drop duplicates 
# dta4 <- distinct(dta4)
# length(unique(dta4$HS6))
# test <- dta4 %>%  group_by(HS6) %>%    filter(n() > 1) %>%  ungroup()
# 
# 
# # sometimes one product code allocated to two different NAICS 3 digit code
# # can be a problem when calculating the shares where it might not match anymore.... 
# dta4_unique <- dta4 %>%  group_by(HS6) %>%
#   slice(1) %>%        # keep the first observation per HS6
#   ungroup()
# 
# write_csv(dta4_unique, "/data/sikeme/TRADE/NTM_trade_war/data/crosswalk/clean_HS6_naics3_2012.csv")
# 
# 
# 





