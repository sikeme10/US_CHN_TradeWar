

################################################################################

rm(list=ls())


library(tidyr)
library(dplyr)
library(readr)
library(concordance)
library(readr)
library(readxl )



################################################################################

# trade data
US_export <- read_csv( "data/trade/schott/monthly_export/US_cleaned_export.csv")
head(US_export)

# tariff data
tariff <- read_csv("data/tariff_dta/CHN_tariff_HS6_Fagel.csv")
tariff <- read_csv("data/tariff_dta/trade_war_tariffs.csv")


################################################################################
# 1) Clean trade and tariff data 
################################################################################

# a) Trade data
names(US_export)

# get total export 
US_export_tot <- US_export %>%  group_by(HS10, year,month, sic, naics6, HS6, naics4, naics3) %>%
  summarise(tot_export_val_USD = sum(export_val_USD, na.rm = TRUE))
unique(US_export$ISO3_Code)

# Chinese export 
US_export_CHN <- US_export %>%  filter(ISO3_Code == "CHN") %>%
  rename(CHN_export_val_USD = export_val_USD)

# merge Chinese and total export 
merge_US_export <- full_join(US_export_CHN, US_export_tot)
unique(merge_US_export$year)
class(merge_US_export$HS6)


# b) Tariff data

names(tariff)
class(tariff$hs6)

# select year of interest 
tariff <- tariff %>% select(year, month, hs6, weighted_x_stattariff1, weighted_x_increase) %>%
  mutate(tariff_rate = weighted_x_stattariff1 + weighted_x_increase)
summary(tariff$tariff_rate)


# c) Merge tariff and trade data 

dta <- left_join( merge_US_export,tariff, by = c("HS6"  = "hs6", "year" = "year", "month" = "month" ))
names(dta)


dta <- dta %>%  mutate( CHN_export_val_USD = if_else(is.na(CHN_export_val_USD), 0,CHN_export_val_USD  )  )


# for tariff data: fill it so that takes the value of previous time when the value 
dta <- dta %>%  group_by(HS10, year, HS6) %>%  # include ALL key vars
  arrange(month, .by_group = TRUE) %>%  fill(tariff_rate, .direction = "down") %>%  ungroup()
colSums(is.na(dta))









