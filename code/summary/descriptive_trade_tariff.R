

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
tariff <- tariff %>% select(year, month, hs6, Weighted_AHS , teti_tariff_2, fajgel_tariff_2)
summary(tariff$teti_tariff_2)
summary(tariff$fajgel_tariff_2)

# c) Merge tariff and trade data 

dta <- left_join( merge_US_export,tariff, by = c("HS6"  = "hs6", "year" = "year", "month" = "month" ))
names(dta)
colSums(is.na(dta))

# replace chines export share by NA
dta <- dta %>%  mutate( CHN_export_val_USD = if_else(is.na(CHN_export_val_USD), 0,CHN_export_val_USD  )  )


# for tariff data: fill it so that takes the value of previous time when the value 
dta <- dta %>%  group_by(HS10, year, HS6) %>%  # include ALL key vars
  arrange(month, .by_group = TRUE) %>%  fill(teti_tariff_2 ,fajgel_tariff_2 , .direction = "down") %>%  ungroup()
colSums(is.na(dta))

################################################################################
# 2) HS Product Aggregation
################################################################################

# tariffs are at HS10 level --> need to get at HS6 level
# could do weighted tariffs or simple average tariffs
# weighted average = trade share whithin HS6 of the HS10 export value time the tariffs 
# simple average = is just an average 

names(dta)
# create tot export at naics 3 
tot_dta_naics3 <- dta %>%
  group_by(year,month, naics3) %>%
  mutate(tot_export_CHN_naics3 = sum(CHN_export_val_USD, na.rm = TRUE)) %>%
  ungroup()


# create tot export at naics 4 
tot_dta_naics4 <- dta %>%
  group_by(year,month, naics4) %>%
  mutate(tot_export_CHN_naics4 = sum(CHN_export_val_USD, na.rm = TRUE)) %>%
  ungroup()



################################################################################
# aggregate at naics 3 digit level 
################################################################################
names(tot_dta_naics3)



dta_naics3 <- tot_dta_naics3 %>% group_by(year, month, naics3) %>% 
  summarise(CHN_export_val_USD = sum(CHN_export_val_USD, na.rm = TRUE),
            tot_export_val_USD = sum(tot_export_val_USD, na.rm = TRUE),
            share_CHN_export = CHN_export_val_USD / tot_export_val_USD*100,
            teti_simple_average = mean(teti_tariff_2, na.rm= TRUE),
            fajgel_simple_average = mean(fajgel_tariff_2, na.rm= TRUE),
            teti_weighted_average = mean((CHN_export_val_USD/tot_export_CHN_naics3)*teti_tariff_2, na.rm= TRUE),
            fajgel_weighted_average = mean((CHN_export_val_USD/tot_export_CHN_naics3)*fajgel_tariff_2, na.rm= TRUE)            )
unique(merge_US_export_naics3$naics3)  
names(merge_US_export_naics3)



#details on naics code 

# Define lookup table for 3-digit Agriculture NAICS codes
naics3_lookup <- c(
  "111" = "Crop Production",
  "112" = "Animal Production and Aquaculture",
  "113" = "Forestry and Logging",
  "114" = "Fishing, Hunting and Trapping",
  "115" = "Support Activities for Agriculture and Forestry")

# Add sector classification to your merged dataset
dta_naics3 <- dta_naics3 %>%
  mutate(
    # ensure naics3 is treated as numeric or character safely
    naics3_chr = substr(gsub("\\D", "", as.character(naics3)), 1, 3),
    sector = if_else(naics3_chr %in% names(naics3_lookup), "Ag", "NonAg"),
    sector_desc = naics3_lookup[naics3_chr]  )



# Combine Year and Month into a proper Date variable (1st of each month)
dta_naics3 <- dta_naics3 %>%
  mutate( year = as.integer(year),    month = as.integer(month),
          date = make_date(year = year, month = month, day = 1)  )
names(dta_naics3)


# Plot: only Ag sector
ggplot(
  subset(dta_naics3, sector == "Ag"),
  aes(x = date, y = teti_weighted_average, color = as.factor(sector_desc))) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    x = "Date",
    y = "weighted tariff rate",
    color = "Product Code",
    title = "Share of U.S. Agricultural Exports to China over Time by Product Code"  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5)  )


ggplot(
  subset(dta_naics3, sector == "Ag"),
  aes(x = date, y = share_CHN_export, color = as.factor(sector_desc))) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    x = "Date",
    y = "weighted tariff rate",
    color = "Product Code",
    title = "Share of U.S. Agricultural Exports to China over Time by Product Code"  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5)  )





