

################################################################################

rm(list=ls())


library(tidyr)
library(dplyr)
library(readr)
library(concordance)
library(readr)
library(readxl )

exp <- c("/data/sikeme/TRADE/US_CHN_TradeWar_git/output/summary/")


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


# Plot: only tariff for ag 
ggplot(  subset(dta_naics3, naics3 %in% c(111, 112))) +
  geom_line(aes(x = date, y = teti_weighted_average, color = as.factor(sector_desc)), size = 1) +
  geom_line(aes(x = date, y = fajgel_weighted_average, color = as.factor(sector_desc)),size = 1, linetype = "dotted"  ) +
  geom_point(aes(x = date, y = teti_weighted_average, color = as.factor(sector_desc))) +
  geom_point(aes(x = date, y = fajgel_weighted_average, color = as.factor(sector_desc))) +
  labs( x = "Date",  y = "Weighted Tariff Rate",  color = "Product Code",  title = "Share of U.S. Agricultural Exports to China over Time by Product Code"  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom", legend.title = element_text(face = "bold"),  plot.title = element_text(face = "bold", hjust = 0.5)  )


ggplot( subset(dta_naics3, sector == "Ag"),
  aes(x = date, y = teti_weighted_average, color = as.factor(sector_desc))) +
  geom_line(size = 1) +
  geom_point() +
  labs(  x = "Date",y = "weighted tariff rate",  color = "Product Code",
    title = "Share of U.S. Agricultural Exports to China over Time by Product Code"  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom",  legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5)  )



names(dta_naics3)
# Reshape to long format
d_long <- dta_naics3 %>%
  filter(naics3 %in% c(111, 112)) %>%
  pivot_longer(  cols = c(share_CHN_export, teti_weighted_average),  names_to = "variable",   values_to = "value"  )
# with normal geom
ggplot(  d_long,  aes(x = date, y = value, color = sector_desc,  linetype = variable)) +
  geom_line(size = 1) +
  facet_wrap(~ sector_desc, scales = "free_y") +
  labs( x = "Date",  y = "Value",   color = "Sector",  linetype = "Series",
    title = "Exports to China and Tariffs Over Time by Sector"  ) +
  scale_linetype_manual(values = c(  "share_CHN_export" = "solid","teti_weighted_average" = "dotted"  )) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5)  )
# with geom_smooth
p <- ggplot(d_long, aes(x = date, color = sector_desc)) +
  # --- SMOOTH for share_CHN_export ---
  geom_smooth(data = d_long %>% filter(variable == "share_CHN_export"),
    aes(y = value, linetype = variable),  se = FALSE,   size = 1  ) +
    # --- LINE for teti_weighted_average ---
  geom_line(  data = d_long %>% filter(variable == "teti_weighted_average"),
    aes(y = value, linetype = variable),    size = 1  ) +
  facet_wrap(~ sector_desc, scales = "free_y") +
  scale_linetype_manual(values = c("share_CHN_export" = "solid","teti_weighted_average" = "dotted"),
      labels = c("share_CHN_export" = "% export to China","teti_weighted_average" = "CHM weighted average tariff"  )  ) +
  labs(  x = "Date",  y = "Value",  color = "Sector",   linetype = "Series",  title = "Share of U.S. Exports to China and Chinese Tariffs Over Time by Sector"  ) +
  theme_minimal(base_size = 14) +
  theme(  legend.position = "bottom",   legend.title = element_text(face = "bold"),  plot.title = element_text(face = "bold", hjust = 0.5)  )
p
ggsave( filename = paste0(exp, "US_export_trend/US_export_share_tariff_naics3_ag.png"), plot = p,  width = 10, height = 6, units = "in",  dpi = 300,  bg = "white")


################################################################################
# aggregate at naics 4 digit level 
################################################################################
names(tot_dta_naics4)

dta_naics4 <- tot_dta_naics4 %>% group_by(year, month, naics4) %>% 
  summarise(CHN_export_val_USD = sum(CHN_export_val_USD, na.rm = TRUE),
            tot_export_val_USD = sum(tot_export_val_USD, na.rm = TRUE),
            share_CHN_export = CHN_export_val_USD / tot_export_val_USD*100,
            teti_simple_average = mean(teti_tariff_2, na.rm= TRUE),
            fajgel_simple_average = mean(fajgel_tariff_2, na.rm= TRUE),
            teti_weighted_average = mean((CHN_export_val_USD/tot_export_CHN_naics4)*teti_tariff_2, na.rm= TRUE),
            fajgel_weighted_average = mean((CHN_export_val_USD/tot_export_CHN_naics4)*fajgel_tariff_2, na.rm= TRUE)            )
unique(dta_naics4$naics4)  
names(dta_naics4)



#details on naics code 
# Define 4-digit lookup tables for crop and animal subsectors
naics4_lookup <- c(
  # Crop Production (111)
  "1111" = "Oilseed and Grain Farming",
  "1112" = "Vegetable and Melon Farming",
  "1113" = "Fruit and Tree Nut Farming",
  "1114" = "Greenhouse, Nursery, and Floriculture Production",
  "1119" = "Other Crop Farming",
  
  # Animal Production and Aquaculture (112)
  "1121" = "Cattle Ranching and Farming",
  "1122" = "Hog and Pig Farming",
  "1123" = "Poultry and Egg Production",
  "1124" = "Sheep and Goat Farming",
  "1125" = "Aquaculture",
  "1129" = "Other Animal Production")

# Add sector classification to your merged dataset
dta_naics4 <- dta_naics4 %>%
  mutate(
    # ensure naics4 is treated as numeric or character safely
    naics4_chr = substr(gsub("\\D", "", as.character(naics4)), 1, 4),
    sector = if_else(naics4_chr %in% names(naics4_lookup), "Ag", "NonAg"),
    subsector = case_when(naics4 %in% c(1111, 1112, 1113, 1114, 1119) ~ "Crop",
      naics4 %in% c(1121, 1122, 1123, 1124, 1125, 1129) ~ "Animal",
      TRUE ~ "Other"),
    sector_desc = naics4_lookup[naics4_chr]  )



# Combine Year and Month into a proper Date variable (1st of each month)
dta_naics4 <- dta_naics4 %>%
  mutate( year = as.integer(year),    month = as.integer(month),
          date = make_date(year = year, month = month, day = 1)  )
names(dta_naics4)


# Plot: only Ag sector
ggplot( subset(dta_naics4, subsector %in% c("Crop" ,"Animal")),
        aes(x = date, y = teti_weighted_average, color = as.factor(sector_desc))) +
  geom_line(size = 1) +
  geom_point() +
  labs( x = "Date",    y = "weighted tariff rate",color = "Product Code",
        title = "Share of U.S. Agricultural Exports to China over Time by Product Code"  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom",  legend.title = element_text(face = "bold"),  plot.title = element_text(face = "bold", hjust = 0.5)  )


ggplot( subset(dta_naics4, subsector %in% c("Crop" ,"Animal")),
        aes(x = date, y = CHN_export_val_USD, color = as.factor(sector_desc))) +
  geom_line(size = 1) +
  geom_point() +
  labs(  x = "Date",y = "weighted tariff rate",  color = "Product Code",
         title = "Share of U.S. Agricultural Exports to China over Time by Product Code"  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom",  legend.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold", hjust = 0.5)  )



names(dta_naics3)
# Reshape to long format
d_long <- dta_naics3 %>%
  filter(subsector %in% c("Crop" ,"Animal")) %>%
  pivot_longer(
    cols = c(share_CHN_export, fajgel_weighted_average),
    names_to = "variable",
    values_to = "value"
  )
# with normal geom
# Plot
ggplot(  d_long,  aes(x = date, y = value, color = sector_desc,  linetype = variable)) +
  geom_line(size = 1) +
  facet_wrap(~ sector_desc, scales = "free_y") +
  labs( x = "Date",  y = "Value",   color = "Sector",  linetype = "Series",
        title = "Exports to China and Tariffs Over Time by Sector"  ) +
  scale_linetype_manual(values = c(  "share_CHN_export" = "solid","fajgel_weighted_average" = "dotted"  )) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold", hjust = 0.5)  )

# with geom_smooth
ggplot(d_long, aes(x = date, color = sector_desc)) +
  # --- SMOOTH for share_CHN_export ---
  geom_smooth(data = d_long %>% filter(variable == "share_CHN_export"),
              aes(y = value, linetype = variable),  se = FALSE,   size = 1  ) +
  # --- LINE for fajgel_weighted_average ---
  geom_line(  data = d_long %>% filter(variable == "fajgel_weighted_average"),
              aes(y = value, linetype = variable),    size = 1  ) +
  facet_wrap(~ sector_desc, scales = "free_y") +
  scale_linetype_manual(    values = c( "share_CHN_export" = "solid",  "fajgel_weighted_average" = "dotted" )  ) +
  labs( x = "Date",  y = "Value", color = "Sector", linetype = "Series", title = "Exports to China and Tariffs Over Time by Sector"  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom",  legend.title = element_text(face = "bold"),
        plot.title = element_text(face = "bold", hjust = 0.5)  )


