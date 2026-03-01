




library(tidyr)
library(dplyr)
library(readr)
library(concordance)
library(readr)
library(readxl )
library(dplyr)
library(ggplot2)
library(lubridate)

rm(list=ls())
# Set directory
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git/data")
getwd()

# export directory:
exp <- c("/data/sikeme/TRADE/US_CHN_TradeWar_git/output/summary/")

################################################################################

# Look at overall changes in US exports withing agricultural commodities


################################################################################

# load data 

US_export <- read_csv( "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/trade/schott/monthly_export/US_cleaned_export.csv")

################################################################################

names(US_export)

# get total export 

US_export_tot <- US_export %>%
  group_by(HS10, year,month, sic, naics6, HS6, naics4, naics3) %>%
  summarise(tot_export_val_USD = sum(export_val_USD, na.rm = TRUE))


unique(US_export$ISO3_Code)

# Chinese export 

US_export_CHN <- US_export %>%  filter(ISO3_Code == "CHN") %>%
  rename(CHN_export_val_USD = export_val_USD)

# merge Chinese and total export 

merge_US_export <- full_join(US_export_CHN, US_export_tot)




################################################################################


# get Diane crosswalk HS naics code 
# HS_NAICS <- read_csv("crosswalk/HS6_NAICS_Diane/NAICS_HS_2012.csv")
# # sleect variablel of interest
# HS_NAICS <- HS_NAICS %>% rename(naics3 = naics3_D, naics4 = naics4_D , naics6 = naics6_D)
# HS_NAICS <- HS_NAICS %>%  select(naics6, crop, k, icrop, j)

merge_US_export$naics6 <- as.numeric(merge_US_export$naics6)





################################################################################
# aggregate at naics 3 digit level 
################################################################################
names(merge_US_export)


merge_US_export_naics3 <- merge_US_export %>% group_by(year, month, naics3) %>% 
  summarise(CHN_export_val_USD = sum(CHN_export_val_USD, na.rm = TRUE),
            tot_export_val_USD = sum(tot_export_val_USD, na.rm = TRUE),
            share_CHN_export = CHN_export_val_USD / tot_export_val_USD*100)
unique(merge_US_export_naics3$naics3)  
names(merge_US_export_naics3)


#details on naics code 

# Define lookup table for 3-digit Agriculture NAICS codes
naics3_lookup <- c(
  "111" = "Crop Production",
  "112" = "Animal Production and Aquaculture",
  "113" = "Forestry and Logging",
  "114" = "Fishing, Hunting and Trapping",
  "115" = "Support Activities for Agriculture and Forestry"
)

# Add sector classification to your merged dataset
merge_US_export_naics3 <- merge_US_export_naics3 %>%
  mutate(
    # ensure naics3 is treated as numeric or character safely
    naics3_chr = substr(gsub("\\D", "", as.character(naics3)), 1, 3),
    sector = if_else(naics3_chr %in% names(naics3_lookup), "Ag", "NonAg"),
    sector_desc = naics3_lookup[naics3_chr]  )



# Combine Year and Month into a proper Date variable (1st of each month)
merge_US_export_naics3 <- merge_US_export_naics3 %>%
  mutate( year = as.integer(year),    month = as.integer(month),
    date = make_date(year = year, month = month, day = 1)  )
names(merge_US_export_naics3)


# Plot: only Ag sector
ggplot(
  subset(merge_US_export_naics3, sector == "Ag"),
  aes(x = date, y = share_CHN_export, color = as.factor(sector_desc))) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    x = "Date",
    y = "Share of Exports to China(%)",
    color = "Product Code",
    title = "Share of U.S. Agricultural Exports to China over Time by Product Code"  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5)  )


plot <- ggplot(
  subset(merge_US_export_naics3, sector == "Ag"),
  aes(x = date, y = share_CHN_export, color = as.factor(sector_desc))) +
  geom_smooth(, method = "loess", se = FALSE, linetype = "dashed", size = 0.8) +
  # geom_line(size = 1) +  geom_point() +
  labs(
    x = "Date",
    y = "Share of Exports to China(%)",
    color = "Product Code",
    title = "Share of U.S. Agricultural Exports to China over Time by Product Code"  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 9),
    axis.title = element_text(size = 10),       # smaller axis titles
    axis.text = element_text(size = 9),         # smaller axis tick labels
    plot.title = element_text(face = "bold", hjust = 0.5, size = 10)
  )
plot
ggsave( filename = paste0(exp, "US_export_trend/US_export_share_naics3_ag.png"),  plot = plot,  width = 10, height = 6, units = "in",  dpi = 300,  bg = "white")


        

ggplot(
  subset(merge_US_export_naics3, sector == "Ag"),
  aes(x = date, y = CHN_export_val_USD, color = as.factor(sector_desc))) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    x = "Date",
    y = "Exports to CHina in USD",
    color = "Product Code",
    title = "Share of U.S. Agricultural Exports to China over Time by Product Code"  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 11),  # slightly smaller legend title
    legend.text = element_text(size = 10),                  # slightly smaller legend labels
    plot.title = element_text(face = "bold", hjust = 0.5, size = 12)  # slightly smaller title
  )


plot <- ggplot(
  subset(merge_US_export_naics3, sector == "Ag"),
  aes(x = date, y = tot_export_val_USD, color = as.factor(sector_desc))) +
  geom_smooth(, method = "loess", se = FALSE, linetype = "dashed", size = 0.8) +
  # geom_line(size = 1) +  geom_point() +
  labs(
    x = "Date",
    y = "Total U.S. Exports in USD",
    color = "Product Code",
    title = "Total U.S. Agricultural Exports Over Time by Product Code"  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 9),
    axis.title = element_text(size = 10),       # smaller axis titles
    axis.text = element_text(size = 9),         # smaller axis tick labels
    plot.title = element_text(face = "bold", hjust = 0.5, size = 10)
  )
plot
ggsave( filename = paste0(exp, "US_export_trend/US_tot_export_naics3_ag.png"),
        plot = plot,  width = 10, height = 6, units = "in",  dpi = 300,  bg = "white")



################################################################################
# aggregate at naics 4 digit level 
################################################################################
names(merge_US_export)


merge_US_export_naics4 <- merge_US_export %>% group_by(year, month, naics4, naics3) %>% 
  summarise(CHN_export_val_USD = sum(CHN_export_val_USD, na.rm = TRUE),
            tot_export_val_USD = sum(tot_export_val_USD, na.rm = TRUE),
            share_CHN_export = CHN_export_val_USD / tot_export_val_USD*100)
unique(merge_US_export_naics4$naics4)  
names(merge_US_export_naics4)


#details on naics code 

# Define lookup table for 3-digit Agriculture NAICS codes
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
  "1129" = "Other Animal Production"
)

# Extend your dataset with subsector classification
merge_US_export_naics4 <- merge_US_export_naics4 %>%
  mutate(
    # 3-digit NAICS (existing logic)
    naics3_chr = substr(gsub("\\D", "", as.character(naics3)), 1, 3),
    sector = if_else(naics3_chr %in% names(naics3_lookup), "Ag", "NonAg"),
    sector_desc = naics3_lookup[naics3_chr],
    
    # 4-digit NAICS (for subsectors)
    naics4_chr = substr(gsub("\\D", "", as.character(naics4)), 1, 4),
    subsector_desc = naics4_lookup[naics4_chr],
    
    # broader grouping variable for plotting or analysis
    subsector = case_when(
      substr(naics4_chr, 1, 3) == "111" ~ "Crop",
      substr(naics4_chr, 1, 3) == "112" ~ "Animal",
      TRUE ~ NA_character_
    )
  )



# Combine Year and Month into a proper Date variable (1st of each month)
merge_US_export_naics4 <- merge_US_export_naics4 %>%
  mutate( year = as.integer(year),    month = as.integer(month),
          date = make_date(year = year, month = month, day = 1)  )
names(merge_US_export_naics4)


# Plot: only Ag sector
ggplot(
  subset(merge_US_export_naics4, subsector %in% c("Crop", "Animal")),
  aes(x = date, y = share_CHN_export, color = as.factor(subsector_desc))) +
  geom_line(size = 1) +
  geom_point() +
  facet_wrap(~subsector)+
  labs(
    x = "Date",
    y = "Share of Exports to China(%)",
    color = "Product Code",
    title = "Share of U.S. Agricultural Exports to China over Time by Product Code (NAICS4)"  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5)  )

library(ggplot2)
library(dplyr)

plot <- ggplot(
  subset(merge_US_export_naics4, subsector %in% c("Crop", "Animal")),
  aes(x = date, y = share_CHN_export, color = subsector_desc)) +
  #geom_line(size = 1, alpha = 0.8) +
  #geom_point(alpha = 0.7) +
  # Add a smoothed trend line per subsector (optional: loess for small datasets)
  geom_smooth(aes(group = subsector_desc), method = "loess", se = FALSE, linetype = "dashed", size = 0.8) +
  facet_wrap(~subsector) +
  labs(
    x = "Date",
    y = "Share of Exports to China (%)",
    color = "Product Code (NAICS 4)",
    title = "Share of U.S. Agricultural Exports to China Over Time by Product Code (NAICS 4)"  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 9),
    axis.title = element_text(size = 10),       # smaller axis titles
    axis.text = element_text(size = 9),         # smaller axis tick labels
    plot.title = element_text(face = "bold", hjust = 0.5, size = 10)  ) +
  # Distinguish colors by subsector (Crop vs Animal) using nested palettes
  scale_color_manual(
    values = c(
      # Crop-related NAICS 111x → greens and yellows
      "Oilseed and Grain Farming" = "#7FFF00",
      "Vegetable and Melon Farming" = "#FFA500",
      "Fruit and Tree Nut Farming" = "#41ae76",
      "Greenhouse, Nursery, and Floriculture Production" = "#8B6508",
      "Other Crop Farming" = "#00441b",
      # Animal-related NAICS 112x → blues and purples
      "Cattle Ranching and Farming" = "#551A8B",
      "Hog and Pig Farming" = "#4a74b5",
      "Poultry and Egg Production" = "#40E0D0",
      "Sheep and Goat Farming" = "#8B0A50",
      "Aquaculture" = "#FF69B4",
      "Other Animal Production" = "#0000FF"
    )
  )
plot
ggsave( filename = paste0(exp, "US_export_trend/US_export_share_naics4_ag.png"), plot = plot,  width = 10, height = 6, units = "in",  dpi = 300,  bg = "white")



plot <- ggplot(
  subset(merge_US_export_naics4, subsector %in% c("Crop", "Animal")),
  aes(x = date, y = tot_export_val_USD, color = subsector_desc)) +
  #geom_line(size = 1, alpha = 0.8) +
  #geom_point(alpha = 0.7) +
  # Add a smoothed trend line per subsector (optional: loess for small datasets)
  geom_smooth(aes(group = subsector_desc), method = "loess", se = FALSE, linetype = "dashed", size = 0.8) +
  facet_wrap(~subsector, scales = "free_y") +
  labs(
    x = "Date",
    y = "US total export in USD",
    color = "Product Code (NAICS 4)",
    title = "Total U.S. Agricultural Exports values Over Time by Product Code (NAICS 4)"  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 9),
    legend.text = element_text(size = 9),
    axis.title = element_text(size = 10),       # smaller axis titles
    axis.text = element_text(size = 9),         # smaller axis tick labels
    plot.title = element_text(face = "bold", hjust = 0.5, size = 10)  ) +
  # Distinguish colors by subsector (Crop vs Animal) using nested palettes
  scale_color_manual(
    values = c(
      # Crop-related NAICS 111x → greens and yellows
      "Oilseed and Grain Farming" = "#7FFF00",
      "Vegetable and Melon Farming" = "#FFA500",
      "Fruit and Tree Nut Farming" = "#41ae76",
      "Greenhouse, Nursery, and Floriculture Production" = "#8B6508",
      "Other Crop Farming" = "#00441b",
      # Animal-related NAICS 112x → blues and purples
      "Cattle Ranching and Farming" = "#551A8B",
      "Hog and Pig Farming" = "#4a74b5",
      "Poultry and Egg Production" = "#40E0D0",
      "Sheep and Goat Farming" = "#8B0A50",
      "Aquaculture" = "#FF69B4",
      "Other Animal Production" = "#0000FF"
    )
  )
plot
ggsave( filename = paste0(exp, "US_export_trend/US_tot_export_naics4_ag.png"), plot = plot,  width = 10, height = 6, units = "in",  dpi = 300,  bg = "white")





ggplot(
  subset(merge_US_export_naics3, sector == "Ag"),
  aes(x = date, y = CHN_export_val_USD, color = as.factor(sector_desc))) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    x = "Date",
    y = "Exports to CHina in USD",
    color = "Product Code",
    title = "Share of U.S. Agricultural Exports to China over Time by Product Code"  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5)  )



################################################################################
# aggregate at naics 3 digit level 
################################################################################
names(merge_US_export)


merge_US_export_naics3 <- merge_US_export %>% group_by(year, month, naics3) %>% 
  summarise(CHN_export_val_USD = sum(CHN_export_val_USD, na.rm = TRUE),
            tot_export_val_USD = sum(tot_export_val_USD, na.rm = TRUE),
            share_CHN_export = CHN_export_val_USD / tot_export_val_USD*100)
unique(merge_US_export_naics3$naics3)  
names(merge_US_export_naics3)


#details on naics code 

# Define lookup table for 3-digit Agriculture NAICS codes
naics3_lookup <- c(
  "111" = "Crop Production",
  "112" = "Animal Production and Aquaculture",
  "113" = "Forestry and Logging",
  "114" = "Fishing, Hunting and Trapping",
  "115" = "Support Activities for Agriculture and Forestry"
)

# Add sector classification to your merged dataset
merge_US_export_naics3 <- merge_US_export_naics3 %>%
  mutate(
    # ensure naics3 is treated as numeric or character safely
    naics3_chr = substr(gsub("\\D", "", as.character(naics3)), 1, 3),
    sector = if_else(naics3_chr %in% names(naics3_lookup), "Ag", "NonAg"),
    sector_desc = naics3_lookup[naics3_chr]
  )



# Combine Year and Month into a proper Date variable (1st of each month)
merge_US_export_naics3 <- merge_US_export_naics3 %>%
  mutate( year = as.integer(year),    month = as.integer(month),
          date = make_date(year = year, month = month, day = 1)  )
names(merge_US_export_naics3)


# Plot: only Ag sector
ggplot(
  subset(merge_US_export_naics3, sector == "Ag"),
  aes(x = date, y = share_CHN_export, color = as.factor(sector_desc))) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    x = "Date",
    y = "Share of Exports to China(%)",
    color = "Product Code",
    title = "Share of U.S. Agricultural Exports to China over Time by Product Code"  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5)  )


ggplot(
  subset(merge_US_export_naics3, sector == "Ag"),
  aes(x = date, y = CHN_export_val_USD, color = as.factor(sector_desc))) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    x = "Date",
    y = "Exports to CHina in USD",
    color = "Product Code",
    title = "Share of U.S. Agricultural Exports to China over Time by Product Code"  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5)  )















