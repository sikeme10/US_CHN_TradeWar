
################################################################################
#                    Gravity regression analysis: residual approach


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
library(countrycode)
library(Hmisc)
library(haven)
library(sfaR)
library(frontier)

################################################################################
# directory: 
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git")
exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/summary/prelim_reg/"

################################################################################
# 1) Load data 
################################################################################

dta <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/output/summary/prelim_reg/gravity_pois_residual.csv")
names(dta)

################################################################################

# check values:
summary(dta$residual)


# add HS section for each HS6 product code 
class(dta$hs6_H5)
unique(nchar(dta$hs6_H5))
unique(dta$hs2)


dta <- dta %>% mutate(
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
  sector = case_when(
    hs_section %in% 1:4   ~ "Ag",
    hs_section %in% 5:20  ~ "Manu",
    TRUE                  ~ "Other"))
table(dta$sector)
table(dta$hs_section)

# select US residuals
US <- dta %>%   filter(ExporterISO3 == "USA") %>% 
  mutate( trade_war = case_when(
    year < 2018 ~ "pre 2018",
    year %in% c(2018, 2019) ~ "2018-2019",
    year == 2020 ~ "2020"    ),
    trade_war = factor(trade_war, levels = c("pre 2018", "2018-2019", "2020"))  )
# create a time variable:
US <- US %>%  mutate(date = as.Date(paste(year, month, "01", sep = "-"))  )
names(US)


################################################################################
# plot residual distribution and over time 
################################################################################


plot <- ggplot(subset(US, sector == "Ag"), aes(x = residual, fill = factor(trade_war))) + 
  geom_density(alpha = 0.25) +
  labs(title = "Gravity model US residual",
       fill = "Year",
       x = "Residual",
       y = "Density") +
  coord_cartesian(xlim = c(-100000000, 100000000)) +
  theme_minimal(base_size = 14) +   
  theme(panel.background = element_rect(fill = "white", color = NA),     
        plot.background  = element_rect(fill = "white", color = NA))
plot
ggsave(filename = file.path(exp, "plot/", "density_res_US_Ag_2015_2020.png"),plot = plot, width = 8, height = 5, dpi = 300)


plot <- ggplot(subset(US, sector == "Ag"), aes(x = date, y = residual)) +
  geom_smooth(se = TRUE) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "red") + 
  labs( title = "Gravity model US residual",
        x = "Date",   y = "Residual value"  ) +
  theme_minimal(base_size = 14) +   
  theme(panel.background = element_rect(fill = "white", color = NA),     
        plot.background  = element_rect(fill = "white", color = NA))
plot
ggsave(filename = file.path(exp, "plot/", "plot_res_US_Ag_2015_2020.png"),plot = plot, width = 8, height = 5, dpi = 300)


#for manufacturing 

plot <- ggplot(subset(US, sector == "Manu"), aes(x = residual, fill = factor(trade_war))) + 
  geom_density(alpha = 0.25) +
  labs(title = "Gravity model US residual",
       fill = "Year",
       x = "Residual",
       y = "Density") +
  coord_cartesian(xlim = c(-100000000, 100000000)) +
  theme_minimal(base_size = 14) +   
  theme(panel.background = element_rect(fill = "white", color = NA),     
        plot.background  = element_rect(fill = "white", color = NA))
plot
ggsave(filename = file.path(exp, "plot/", "density_res_US_Manu_2015_2020.png"),plot = plot, width = 8, height = 5, dpi = 300)


plot <- ggplot(subset(US, sector == "Manu"), aes(x = date, y = residual)) +
  geom_smooth(se = TRUE) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "red") + 
  labs( title = "Gravity model US residual",
        x = "Date",   y = "Residual value"  ) +
  theme_minimal(base_size = 14) +   
  theme(panel.background = element_rect(fill = "white", color = NA),     
        plot.background  = element_rect(fill = "white", color = NA))
plot
ggsave(filename = file.path(exp, "plot/", "plot_res_US_Manu_2015_2020.png"),plot = plot, width = 8, height = 5, dpi = 300)


################################################################################




