
################################################################################
# MFP
################################################################################

rm(list=ls())


library(readr)
library(tidyr)
library(dplyr)
library(data.table)
library(stringi)
library(fixest)
library(countrycode)
library(tidyverse)
library(vroom)
library(readr)
library(dplyr)
library(sf)
library(tigris)
library(ggplot2)
library(viridis)
################################################################################

setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git/data")


dta <- read_csv("trade_cost/20250507-ESCAP-WB-tradecosts-dataset.csv")
names(dta)

################################################################################

unique(dta$sectorname) 
unique(dta$sector) 
unique(dta$year)
unique(dta$reporter)
unique(dta$partner)

countries <- c("USA", "CHN")
dta_US <- dta %>% filter((reporter %in% countries) & (partner %in% countries)) %>%
  filter(year %in% c(2012:2021))

dta_US <- dta_US %>% mutate(sect = case_when(
  sector ==  "AB" ~ "Ag-forestry",
  sector ==  "D" ~ "Manufacturing",
  sector ==  "GTT" ~ "Total"))

names(dta_US)

dta_US <- dta_US %>% mutate(geometric_avg_tariff_percent = (geometric_avg_tariff-1) *100)

################################################################################


plot <- ggplot(subset(dta_US, reporter == "CHN")) +
  geom_line( aes(x = year, y = nontariff_tij, color = sect)) +
  geom_line( aes(x = year, y = geometric_avg_tariff_percent, color = sect),  linetype = "dashed") +
  labs(title = "Change in AVE vs. SOE share",  x = "SOE share in 2010",  y = "Change in AVE (FE demean approach)"  ) +
  theme_minimal()
plot

plot <- ggplot(subset(dta_US, reporter == "CHN")) +
  geom_line( aes(x = year, y = nontariff_tij, color = sect)) +
  geom_line( aes(x = year, y = tij, color = sect)) +
  labs(title = "Change in AVE vs. SOE share",  x = "year",  y = "AVE between US and CHina"  ) +
  theme_minimal()
plot


library(ggplot2)

library(ggplot2)

theme_trade <- theme_minimal(base_size = 14, base_family = "Times New Roman") +
  theme(
    panel.spacing.x = unit(1.2, "lines"),
    plot.title = element_text(size = 12, hjust = 0.5),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.text  = element_text(size = 12),
    legend.title = element_text(size = 12)
  )


plot <- ggplot(subset(dta_US, reporter == "CHN"), aes(x = year)) +
  geom_line(aes(y = tij, color = sect, linetype = "Total trade cost"), linewidth = 0.9) +
  geom_line(aes(y = nontariff_tij, color = sect, linetype = "Non-tariff trade cost"), linewidth = 0.9) +
  scale_linetype_manual(values = c("Total trade cost" = "solid", "Non-tariff trade cost" = "dashed")) +
  scale_color_manual(values = c(
    "Ag-forestry"   = "red",
    "Manufacturing" = "blue",
    "Total"         = "black"
  )) +
  labs(
    title = "Bilateral trade costs between the US and China",
    x = "Year",
    y = "Ad valorem equivalent (%)",
    color = "Sector",
    linetype = "Measure",
  ) + theme_trade  

plot


ggsave( filename = file.path("/data/sikeme/TRADE/US_CHN_TradeWar_git/output/summary/trade_cost/trade_cost.png"),
        plot = plot, dpi = 300)

