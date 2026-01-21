



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

exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/Compare_values/"

################################################################################
# 1) Load data 
################################################################################


dta <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/output/stochastic/sfaR_efficiency_average.csv")
dta_mu <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/output/stochastic/sfaR_efficiency_average_mu.csv")
names(dta)
colSums(is.na(dta))
colSums(is.na(dta_mu))

################################################################################
# merge dta 

names(dta)

dta <- dta %>% select(hs2, hs4, hs6_H5, ExporterISO3,ImporterISO3 , year, month , Applied_tariff,  log_tariff , u,teJLMS)

dta_mu <- dta_mu %>%  select(hs2, hs4, hs6_H5, ExporterISO3, year, month ,Applied_tariff, log_tariff , u,teJLMS) %>% rename(
  u_tariff = u, teJLMS_tariff = teJLMS)

dups <- dta %>%   group_by(hs6_H5, ExporterISO3, year, month , log_tariff , u,teJLMS) %>% 
  filter(n() > 1)

# join and export the dta
dta <- full_join(dta, dta_mu)
write_csv(dta, "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/stochastic/sfaR_efficiency_average_merged.csv")



################################################################################
# select US 

US <- dta %>% filter(ExporterISO3 == "USA")
US_mu <- dta_mu %>% filter(ExporterISO3 == "USA")

US <- full_join(US, US_mu)
colSums(is.na(US))
summary(US)

# select the agricultural sector 

US <- US %>% mutate(
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

# select US agricultural sector 
US_Ag <- US %>% filter(sector == "Ag")
################################################################################

# corrplot 

ggplot(US_Ag, aes(x = u, y = u_tariff)) +
  geom_point(alpha = 0.150, size = 1) +
  geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 1) +
  theme_minimal() +
  labs(
    title = "Plot of inefficiency and tariff-adjusted Inefficiency (u) in agricultural Sector",
    x = "u",
    y = "Tariff-adjusted u"  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),  # center + enlarge title
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 12)  )


ggplot(US_Ag, aes(x = u, y = u_tariff)) +
  geom_hex(bins = 40) +
  scale_fill_viridis_c(option = "C") +
  geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 1) +
  theme_minimal()
model <- lm(u_tariff ~ u, data = US_Ag)
summary(model)
summary(US)



# get density of the distribution
US_long <- US_Ag %>%
  pivot_longer(cols = c(u, u_tariff), names_to = "var", values_to = "value") %>%
  mutate(years =case_when(
    year < 2018 ~ "2015-2017",
    year %in% c(2018:2019) ~ "20180-2019",
    year > 2019 ~ "2020"  ))
table(US_long$year,US_long$years )

ggplot(US_long, aes(x = value, color = var, fill = var)) +
  geom_density(alpha = 0.3, linewidth = 1.2) +
  scale_color_manual( values = c("u" = "blue", "u_tariff" = "red"),
    labels = c("u" = "u", "u_tariff" = "Tariff-adjusted u") ) +
  scale_fill_manual(values = c("u" = "blue", "u_tariff" = "red"),  labels = c("u" = "u",
               "u_tariff" = "Tariff-adjusted u")  ) +
  theme_minimal() +
  labs( title = "Density of inefficiency term (u) for agricultural sector:\n with and without tariff",
    x = "Value",
    y = "Density",
    color = "variables",
    fill = "variables"  ) +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
    legend.title = element_text(size = 12),
    legend.text  = element_text(size = 12),
    axis.title   = element_text(size = 12),
    axis.text    = element_text(size = 12)  )


  
ggplot(US_long, aes(x = value, color = factor(years), fill = factor(years))) +
  geom_density(alpha = 0.3, linewidth = 1.2) +
  facet_wrap(  ~ var, scales = "free",labeller = labeller(var = c("u" = "u",
        "u_tariff" = "Tariff-adjusted u"      )    )  ) +
  theme_minimal() +
  labs(
    title = "Density of inefficiency term (u) for agricultural sector: \n with and without tariff",
    x = "Value",
    y = "Density",
    color = "Years",
    fill = "Years"  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    axis.text  = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.text  = element_text(size = 12),
    strip.text = element_text(size = 12)  )

  

ggplot(US, aes(x = teJLMS, y = teJLMS_tariff)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme_minimal()

################################################################################

