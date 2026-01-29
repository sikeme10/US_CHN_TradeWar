
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
library(forcats)
library(polyglotr)
library(stringr)
library(labelled)
library(janitor)
library(readxl)
################################################################################


# directory: 
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git")
exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/IVs/summary"


################################################################################
# 1) Load data 
################################################################################

# Load FE estimates 
dta <- read_csv("data/SOE_dta/SOE_share_2010.csv")
colSums(is.na(dta))
unique(dta$Year)
names(dta)
################################################################################

# get product level 
dta <- dta %>% mutate(hs4 =  str_sub(hs6_H5, 1,4),
                      hs2 =  str_sub(hs6_H5, 1,2))

dta$hs2 <- as.numeric(dta$hs2)
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


################################################################################
# at HS4 level
################################################################################

# check distribution at HS4 

dta_hs4 <- dta %>% group_by(Year, hs4) %>%
  summarise(Trade_value_USD_SOE = sum(Trade_value_USD_SOE, na.rm=TRUE),
            tot_Trade_value_USD = sum(tot_Trade_value_USD, na.rm=TRUE)  ) %>%
  ungroup()
dta_hs4 <- dta_hs4 %>% mutate(share_value_SOE = Trade_value_USD_SOE / tot_Trade_value_USD )

################################################################################
# plot histogram 

library(ggplot2)

plot <- ggplot(dta_hs4, aes(x = share_value_SOE)) +
  geom_histogram(bins = 50, fill = "blue") +
  xlab("Share of SOE import trade value at HS4 level") +
  ylab("Count of HS4 products") +
  ggtitle("Histogram of the distribution across HS4 industries of the share \n 
          of Chinese imports from the US\nimported by SOEs"  ) +
  theme_minimal()
out_path <- file.path(exp, "/SOE_share_distribution_hs4.png")
ggsave(filename = out_path, plot = plot, width = 10, height = 7, dpi = 300)

################################################################################
# at HS2 level
################################################################################

dta_hs2 <- dta %>% group_by(Year, hs2) %>%
  summarise(Trade_value_USD_SOE = sum(Trade_value_USD_SOE, na.rm=TRUE),
            tot_Trade_value_USD = sum(tot_Trade_value_USD, na.rm=TRUE)  ) %>%
  ungroup()
dta_hs2 <- dta_hs2 %>% mutate(share_value_SOE = Trade_value_USD_SOE / tot_Trade_value_USD )


write_csv(dta_hs2, file.path(exp, "SOE_share_by_HS2.csv") )




