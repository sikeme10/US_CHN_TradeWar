

library(tidyr)
library(dplyr)
library(readr)
library(concordance)
library(readr)
library(readxl )
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggplot2)
library(dplyr)
library(patchwork)


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
tariff <- read_csv( "tariff_dta/trade_war_tariffs.csv")

################################################################################
# clean export data 
################################################################################

names(US_export)

# get total export 
US_export_tot <- US_export %>%  group_by(year,month, HS6) %>%
  summarise(tot_export_val_USD = sum(export_val_USD, na.rm = TRUE))
length(unique(US_export_tot$HS6))
unique(US_export$ISO3_Code)

# Chinese export 
US_export_CHN <- US_export %>%  filter(ISO3_Code == "CHN") %>%
  rename(CHN_export_val_USD = export_val_USD) %>% group_by(year,month, HS6) %>% 
  summarise(CHN_export_val_USD = sum(CHN_export_val_USD, na.rm = TRUE))
length(unique(US_export_CHN$HS6))

# merge Chinese and total export 
merge_US_export <- full_join(US_export_CHN, US_export_tot)
colSums(is.na(merge_US_export))
merge_US_export <- merge_US_export %>%mutate(CHN_export_val_USD = coalesce(CHN_export_val_USD, 0))

################################################################################
class(merge_US_export$HS6)
merge_US_export$HS6 <- as.character(merge_US_export$HS6)
unique(nchar(merge_US_export$HS6))
merge_US_export <- merge_US_export %>% mutate(HS6 = str_pad(as.character(HS6), width = 6, side = "left", pad = "0")  )
merge_US_export <- merge_US_export %>%  mutate(hs2 = substr( HS6 ,1,2), hs4 = substr( HS6 ,1,4))


merge_US_export$hs2 <- as.numeric(merge_US_export$hs2)
merge_US_export <- merge_US_export %>% mutate(
  hs_section = case_when(
    hs2 %in% 1:5 ~ 1,
    hs2 %in% 6:14 ~ 2,
    hs2 %in% 15 ~ 3,
    hs2 %in% 16:24 ~ 4,
    hs2 %in% 25:27 ~ 5,
    hs2 %in% 28:38 ~ 6,
    hs2 %in% 39:40 ~ 7,
    hs2 %in% 41:43 ~ 8,
    hs2 %in% 44:46 ~ 9,
    hs2 %in% 47:49 ~ 10,
    hs2 %in% 50:63 ~ 11,
    hs2 %in% 64:67 ~ 12,
    hs2 %in% 68:70 ~ 13,
    hs2 %in% 71 ~ 14,
    hs2 %in% 72:83 ~ 15,
    hs2 %in% 84:85 ~ 16,
    hs2 %in% 86:89 ~ 17,
    hs2 %in% 90:92 ~ 18,
    hs2 %in% 93 ~ 19,
    hs2 %in% 94:96 ~ 20,
    hs2 %in% 97 ~ 21  ),
  sector = case_when(  hs_section %in% 1:4   ~ "Ag",
                       hs_section %in% 5:20  ~ "Manu",
                       TRUE                  ~ "Other"))

################################################################################
# clean tariff data 
################################################################################

names(tariff)
colSums(is.na(tariff))
unique(tariff$year)
unique(tariff$ImporterISO3)
names(US_export_CHN)
unique(US_export_CHN$year)

tariff <- tariff %>% filter(year %in% c(2017:2020)) %>% select(
  hs6, year , month, teti_tariff_2) %>% rename( CHN_tariff = teti_tariff_2)
length(unique(tariff$hs6))
length(unique(US_export_CHN$HS6))

class(merge_US_export$HS6)
merge_US_export$HS6 <- as.numeric(merge_US_export$HS6)
class(tariff$hs6)
merge_US_export <- left_join(merge_US_export, tariff,
                        by = c("HS6" = "hs6", "year" = "year", "month" = "month"))
colSums(is.na(merge_US_export))

# get 2017 US export values to then use as weights at one point 
trade_2017 <- merge_US_export %>% filter(year == 2017) %>% group_by(HS6) %>% 
  summarise(CHN_export_val_2017  = sum(CHN_export_val_USD , na.rm = TRUE ))

merge_US_export <- left_join(merge_US_export, trade_2017)
colSums(is.na(merge_US_export))
merge_US_export <- merge_US_export %>%mutate(CHN_export_val_2017 = coalesce(CHN_export_val_2017, 0)) 

################################################################################
# get share export at sector level 
################################################################################
names(merge_US_export)
unique(merge_US_export$year)



merge_US_export_sector <- merge_US_export %>%

  # 1) compute 2017 weights at HS6 within sector (fixed over time)
  group_by(sector, HS6) %>%
  mutate(CHN_export_val_2017_hs6 = first(na.omit(CHN_export_val_2017))) %>%
  ungroup() %>%
  group_by(sector) %>%  mutate(tot_2017_sector = sum(CHN_export_val_2017_hs6, na.rm = TRUE),
         w_2017 = if_else(tot_2017_sector > 0, CHN_export_val_2017_hs6 / tot_2017_sector,
                          NA_real_)) %>%  ungroup() %>%
  
  # 2) aggregate to sector-year-month and compute weighted tariff
  group_by(year, month, sector) %>%
  summarise(
    CHN_export_val_USD = sum(CHN_export_val_USD, na.rm = TRUE),
    tot_export_val_USD = sum(tot_export_val_USD, na.rm = TRUE),
    share_CHN_export   = 100 * CHN_export_val_USD / tot_export_val_USD,
    
    # weighted mean tariff with 2017 weights (weights are constant within sector)
    CHN_tariff_wmean = weighted.mean(CHN_tariff, w = w_2017, na.rm = TRUE),
    .groups = "drop"  )

merge_US_export_sector <- merge_US_export_sector %>%  mutate(date = ym(paste(year, month))) 

################################################################################
# plots 

df_1819 <- subset(merge_US_export_sector, year %in% 2018:2019)
sector_cols <- c( "Ag"    = "red", "Manu"  = "blue", "Other" = "darkgreen")

p1 <- ggplot(df_1819, aes(x = date, y = share_CHN_export, color = sector)) +
  geom_line(size = 1) +
  scale_color_manual(values = sector_cols) +
  labs( x = "Time",  y = "CHN Export Share (%)",  title = "Share of US Exports to CHN",
    color = "Sector"  ) +
  theme_minimal()

p2 <- ggplot(df_1819, aes(x = date, y = CHN_export_val_USD, color = sector)) +
  geom_line(size = 1) +
  scale_color_manual(values = sector_cols) +
  labs(x = "Time",  y = "Exports (USD)",title = "US Exports to CHN",  color = "Sector"  ) +
  theme_minimal()

p3 <- ggplot(df_1819, aes(x = date, y = CHN_tariff_wmean, color = sector)) +
  geom_line(size = 1, linetype = "dashed", show.legend = FALSE) +
  scale_color_manual(values = sector_cols) +
  labs( x = "Time",  y = "Tariff Rate (%)",  title = "Weighted Tariffs (2017 wts)",  color = "Sector"  ) +
  theme_minimal()


plot <- (p1 | p2 | p3) +  plot_layout(guides = "collect") & 
  theme(legend.position = "bottom",
        # Titles
        plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
        # Axis titles
        # axis.title.x = element_text(size = 10),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 12),
        # Axis tick labels
        axis.text.x = element_text(size = 12,angle = 60, vjust = 0.5, hjust = 1),
        axis.text.y = element_text(size = 12),
        # Legend
        legend.title = element_text(size = 12),
        legend.text  = element_text(size = 12)  )
plot 
ggsave( filename = paste0(exp, "US_Export_tariffs_sector.png"),  plot = plot,  width = 12, height = 6, units = "in",  dpi = 300,  bg = "white")


################################################################################
# At HS section level 
################################################################################


# ------------------------------------------------------------------------------
# 1) Aggregate + weights at HS section level (instead of sector)
# ------------------------------------------------------------------------------

merge_US_export_hssec <- merge_US_export %>%
  # safe "first non-NA" (avoids the length-0 error you hit before)
  group_by(sector, hs_section, HS6) %>%
  mutate(CHN_export_val_2017_hs6 = if_else(
    all(is.na(CHN_export_val_2017)),  NA_real_,  first(CHN_export_val_2017[!is.na(CHN_export_val_2017)])  )) %>%
  ungroup() %>%  group_by(hs_section) %>%
  mutate( tot_2017_hssec = sum(CHN_export_val_2017_hs6, na.rm = TRUE),
          w_2017 = if_else(tot_2017_hssec > 0,
                           CHN_export_val_2017_hs6 / tot_2017_hssec, NA_real_)  ) %>%
  ungroup() %>%  group_by(year, month,sector, hs_section) %>%
  summarise(CHN_export_val_USD = sum(CHN_export_val_USD, na.rm = TRUE),
            tot_export_val_USD = sum(tot_export_val_USD, na.rm = TRUE),
            share_CHN_export   = 100 * CHN_export_val_USD / tot_export_val_USD,
            CHN_tariff_wmean   = weighted.mean(CHN_tariff, w = w_2017, na.rm = TRUE),  .groups = "drop"  ) %>%
  mutate(date = ym(paste(year, month)))



# ------------------------------------------------------------------------------
# 2) Plots  HS section: Ag 
# ------------------------------------------------------------------------------

df_1819 <- subset(merge_US_export_hssec, year %in% 2018:2019 & sector == "Ag")
hs_labels <- c("1"  = "1: Animal Products",  "2"  = "2: Vegetable Products",
               "3"  = "3: Fats & Oils",  "4"  = "4: Food & Beverages & Tobacco")

hs_colors <- c("1"  = "red", "2"  = "darkgreen", "3"  = "darkorange1",  "4"  = "blue")


# If you have many HS sections, a manual palette is painful.
# ggplot will pick distinct colors automatically; keep it simple:
p1 <- ggplot(df_1819, aes(x = date, y = share_CHN_export, color = as.factor(hs_section))) +
  geom_line(size = 1) +
  labs( x = NULL, y = "CHN Export Share (%)", title = "Share of US Exports to CHN",
    color = "HS Section") +
  scale_color_manual( values = hs_colors,  labels = hs_labels,name   = "HS Section"  )+
  theme_minimal()

p2 <- ggplot(df_1819, aes(x = date, y = CHN_export_val_USD, color = as.factor(hs_section))) +
  geom_line(size = 1) +
  labs(  x = NULL, y = "Exports (USD)",  title = "US Exports to CHN",  color = "HS Section"  ) +
  scale_color_manual( values = hs_colors,  labels = hs_labels,name   = "HS Section"  )+
  theme_minimal()

p3 <- ggplot(df_1819, aes(x = date, y = CHN_tariff_wmean, color = as.factor(hs_section))) +
  geom_line(size = 1, linetype = "dashed", show.legend = FALSE) +
  labs(x = NULL, y = "Tariff Rate (%)", title = "Weighted Tariffs (2017 wts)", color = "HS Section"  ) +
  scale_color_manual( values = hs_colors,  labels = hs_labels,name   = "HS Section"  )+
  theme_minimal()

plot <- (p1 | p2 | p3) +
  plot_layout(guides = "collect") &
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 13, face = "bold", hjust = 0.5),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 12),
    axis.text.x = element_text(size = 12, angle = 60, vjust = 0.5, hjust = 1),
    axis.text.y = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text  = element_text(size = 12)  )

plot
ggsave( filename = paste0(exp, "US_Export_tariffs_hssection_Ag.png"),  plot = plot,  width = 12, height = 6, units = "in",  dpi = 300,  bg = "white")






