
################################################################################
#                     Stochastic frontier regression anlaysis


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

exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/stochastic/"

################################################################################
# 1) Load data 
################################################################################


dta <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/output/stochastic/sfaR_efficiency.csv")

################################################################################

# check values:
names(dta)
summary(dta$u)
summary(dta$teJLMS)
unique(dta$year)
unique(dta$hs2)

# add indicator on sector and HS section


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

# select US efficiencies 
US <- dta %>%   filter(ExporterISO3 == "USA") %>% 
  mutate( trade_war = case_when(
      year < 2018 ~ "pre 2018",
      year %in% c(2018, 2019) ~ "2018-2019",
      year == 2020 ~ "2020"    ),
    trade_war = factor(trade_war, levels = c("pre 2018", "2018-2019", "2020"))  )
# create a time variable:
US <- US %>%  mutate(date = as.Date(paste(year, month, "01", sep = "-"))  )


AUS <- dta %>%   filter(ExporterISO3 == "AUS") %>% 
  mutate( trade_war = case_when(
    year < 2018 ~ "pre 2018",
    year %in% c(2018, 2019) ~ "2018-2019",
    year == 2020 ~ "2020"    ),
    trade_war = factor(trade_war, levels = c("pre 2018", "2018-2019", "2020"))  )
AUS <- AUS %>%  mutate(date = as.Date(paste(year, month, "01", sep = "-"))  )


CAN <- dta %>%   filter(ExporterISO3 == "CAN") %>% 
  mutate( trade_war = case_when(
    year < 2018 ~ "pre 2018",
    year %in% c(2018, 2019) ~ "2018-2019",
    year == 2020 ~ "2020"    ),
    trade_war = factor(trade_war, levels = c("pre 2018", "2018-2019", "2020"))  )
CAN <- CAN %>%  mutate(date = as.Date(paste(year, month, "01", sep = "-"))  )


BRA <- dta %>%   filter(ExporterISO3 == "BRA") %>% 
  mutate( trade_war = case_when(
    year < 2018 ~ "pre 2018",
    year %in% c(2018, 2019) ~ "2018-2019",
    year == 2020 ~ "2020"    ),
    trade_war = factor(trade_war, levels = c("pre 2018", "2018-2019", "2020"))  )
BRA <- BRA %>%  mutate(date = as.Date(paste(year, month, "01", sep = "-"))  )



# look at change in efficencies over time for US 
# US1 <- US %>% group_by(hs6_H5 , month) %>% 

  

################################################################################

# plot efficiency trend by year 

ggplot(dta, aes(x = teJLMS, fill = factor(year))) + 
  geom_density(alpha = 0.15) +
  labs(fill = "Year",
       x = "JLMS Efficiency (teJLMS)",
       y = "Density") +
  theme_minimal()

###############################################################################
# For agriculture 
###############################################################################

ggplot(subset(US, sector == "Ag"), aes(x = teJLMS, fill = factor(year))) + 
  geom_density(alpha = 0.15) +
  labs(title = "Distribution of Efficiency for US Agricultural Exports (2015–2020)",
       fill = "Year",
       x = "Efficiency (teJLMS)",
       y = "Density") +
  theme_minimal(base_size = 14) +   
  theme(panel.background = element_rect(fill = "white", color = NA),     
        plot.background  = element_rect(fill = "white", color = NA))

plot <- ggplot(subset(US, sector == "Ag"), aes(x = teJLMS, fill = factor(trade_war))) + 
  geom_density(alpha = 0.25) +
  labs(title = "Distribution of Efficiency for US Agricultural Exports (2015–2020)",
       fill = "Year",
       x = "Efficiency (teJLMS)",
       y = "Density") +
  theme_minimal(base_size = 14) +   
  theme(panel.background = element_rect(fill = "white", color = NA),     
        plot.background  = element_rect(fill = "white", color = NA))
plot
ggsave(filename = file.path(exp, "plots/", "density_TE_US_Ag_2015_2020.png"),plot = plot, width = 8, height = 5, dpi = 300)



plot <- ggplot(subset(US, sector == "Ag"), aes(x = date, y = teJLMS)) +
  geom_smooth(se = TRUE) +
  labs( title = "TE Efficiency Over Time – US Agricultural Exports (2015–2020)",
    x = "Date",   y = "Efficiency"  ) +
  theme_minimal(base_size = 14) +   
  theme(panel.background = element_rect(fill = "white", color = NA),     
        plot.background  = element_rect(fill = "white", color = NA))
plot
ggsave(filename = file.path(exp, "plots/", "plot_TE_US_Ag_2015_2020.png"),plot = plot, width = 8, height = 5, dpi = 300)

###############################################################################
# For manufacturing
###############################################################################

ggplot(subset(US, sector == "Manu"), aes(x = teJLMS, fill = factor(year))) + 
  geom_density(alpha = 0.15) +
  labs(title = "Distribution of Efficiency for US Agricultural Exports (2015–2020)",
       fill = "Year",
       x = "Efficiency (teJLMS)",
       y = "Density") +
  theme_minimal(base_size = 14) +   
  theme(panel.background = element_rect(fill = "white", color = NA),     
        plot.background  = element_rect(fill = "white", color = NA))

plot <-ggplot(subset(US, sector == "Manu"), aes(x = teJLMS, fill = factor(trade_war))) + 
  geom_density(alpha = 0.35) +
  labs(title = "Distribution of Efficiency for US Agricultural Exports (2015–2020)",
       fill = "Year",
       x = "Efficiency (teJLMS)",
       y = "Density") +
  theme_minimal(base_size = 14) +   
  theme(panel.background = element_rect(fill = "white", color = NA),     
        plot.background  = element_rect(fill = "white", color = NA))
plot
ggsave(filename = file.path(exp, "plots/", "density_TE_US_Manu_2015_2020.png"),plot = plot, width = 8, height = 5, dpi = 300)



ggplot(subset(US, sector == "Manu"), aes(x = teJLMS)) + 
  geom_density(alpha = 0.25) +
  facet_wrap(~trade_war)+
  labs(title = "Distribution of Efficiency for US Agricultural Exports (2015–2020)",
       fill = "Year",
       x = "JLMS Efficiency (teJLMS)",
       y = "Density") +
  theme_minimal(base_size = 14) +   
  theme(panel.background = element_rect(fill = "white", color = NA),     
        plot.background  = element_rect(fill = "white", color = NA))


plot <- ggplot(subset(US, sector == "Manu"), aes(x = date, y = teJLMS)) +
  geom_smooth(se = TRUE) +
  labs( title = "TE Efficiency Over Time – US Manufacturing Exports (2015–2020)",
        x = "Date",   y = "Efficiency"  ) +
  theme_minimal(base_size = 14) +   
  theme(panel.background = element_rect(fill = "white", color = NA),     
        plot.background  = element_rect(fill = "white", color = NA))
plot
ggsave(filename = file.path(exp, "plots/", "plot_TE_US_Manu_2015_2020.png"),plot = plot, width = 8, height = 5, dpi = 300)



##################################################################################
# check on other countries 
##################################################################################

# function to plot for other countries
plot_te_over_time <- function(data, country_name, sector_name, exp_path) {
  
  # Filter sector
  df <- subset(data, sector == sector_name)
  
  # Build title
  plot_title <- paste0( "TE Efficiency Over Time – ", country_name, 
    " ", sector_name, " Exports (2015–2020)"  )
  
  # Create plot
  p <- ggplot(df, aes(x = date, y = teJLMS)) + geom_smooth(se = TRUE) +
    labs( title = plot_title,
      x = "Date",    y = "Efficiency"   ) +
    theme_minimal(base_size = 14) +   
    theme(panel.background = element_rect(fill = "white", color = NA),     
          plot.background  = element_rect(fill = "white", color = NA))
  
  # Ensure output folder exists
  dir.create(file.path(exp_path, "plots/Robust"), showWarnings = FALSE)
  
  # Build filename
  file_name <- paste0("plot_TE_", country_name, "_", sector_name, "_2015_2020.png")
  
  # Save the plot
  ggsave(  filename = file.path(exp_path, "plots/Robust", file_name),
    plot = p,   width = 8, height = 5, dpi = 300  )
  
  # Return plot object
  return(p)
}


# Australia 
plot_te_over_time(AUS, "AUS", "Ag", exp)
plot_te_over_time(AUS, "AUS", "Manu", exp)

# Canada
plot_te_over_time(CAN, "CAN", "Ag", exp)
plot_te_over_time(CAN, "CAN", "Manu", exp)

# Brazil 
plot_te_over_time(BRA, "BRA", "Ag", exp)
plot_te_over_time(BRA, "BRA", "Manu", exp)






