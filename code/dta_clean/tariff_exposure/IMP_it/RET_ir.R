


################################################################################
# we create RET at County level

################################################################################


library(readr)
library(tidyr)
library(dplyr)
library(data.table)
library(stringi)
library(fixest)
library(countrycode)
library(tidyverse)
library(vroom)




rm(list=ls())
# Set directory
setwd("/data/sikeme/TRADE/NTM_trade_war/data")
getwd()

exp <- "/data/sikeme/TRADE/NTM_trade_war/data/created_exposure"


################################################################################


# load data:
gamma <- read_csv("/data/sikeme/TRADE/NTM_trade_war/data/created_exposure/RET_i_Chen_CHN.csv")
labor<- read_csv("/data/sikeme/TRADE/NTM_trade_war/data/QCEW/clean_labor_share_2012.csv")

################################################################################


names(gamma)
names(labor)

gamma1 <- gamma %>% select(naics3, NAICS_description,gamma_iju_tau_CHN_tariffs, gamma_iju_tau_NTB )
unique(gamma1$naics3)

# merge the two data
merge_data <- left_join(gamma1, labor, join_by(naics3 == industry_code))
unique(merge_data$naics3)
summary(merge_data$share_labor_ir)
################################################################################
# get RET at County fips level
names(merge_data)
table(merge_data$area_fips)

merge_data <- merge_data %>% mutate(
  RET_tariff_r = gamma_iju_tau_CHN_tariffs*share_labor_ir,
  RET_NTB_r = gamma_iju_tau_NTB*share_labor_ir
  
)

merge_data_crop <- merge_data %>% filter(naics3 = )

################################################################################ 

#maps 

library(tidyverse)
library(tigris)     # for US shapefiles
library(ggplot2)

# Make sure FIPS codes are character and zero-padded
merge_data <- merge_data |>
  mutate(area_fips = str_pad(area_fips, width = 5, pad = "0"))

# Get US county boundaries (you can also use cb = TRUE for simplified shapes)
counties <- counties(cb = TRUE, year = 2012)


map_data <- counties |>
  left_join(merge_data, by = c("GEOID" = "area_fips"))

# Plot the intensity map
ggplot(map_data) +
  geom_sf(aes(fill = share), color = NA) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  labs(
    title = "Share Intensity by County",
    fill = "Share"
  ) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )
