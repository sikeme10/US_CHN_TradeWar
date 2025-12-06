


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
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git/data")
getwd()

exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure"


################################################################################


# load data:
gamma <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/RET_i_Chen_CHN.csv")
labor<- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/clean_labor_share_2012.csv")

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
################################################################################

names(merge_data)
table(merge_data$area_fips)

# create exposure indicator
merge_data <- merge_data %>% mutate(
  RET_tariff_r = gamma_iju_tau_CHN_tariffs*share_labor_ir,
  RET_NTB_r = gamma_iju_tau_NTB*share_labor_ir
)
summary(merge_data$RET_tariff_r)
summary(merge_data$RET_NTB_r)

###############################################################################
# pick industry of interest to get RET_ir
unique(merge_data$naics3)

merge_data_crop <- merge_data %>% filter(naics3 == 111 )
names(merge_data_crop)
summary(merge_data_crop$RET_tariff_r)
summary(merge_data_crop$RET_NTB_r)


################################################################################ 

#maps 
library(tidyverse)
library(tigris)
library(sf)
library(ggplot2)

options(tigris_use_cache = TRUE)

# Data
dta <- merge_data_crop %>%  mutate(area_fips = str_pad(area_fips, width = 5, pad = "0"))

# Counties shapefile with CRS
counties_sf <- tigris::counties(cb = TRUE, year = 2013) %>%
  st_transform(4326)

# Join data to shapes
map_data <- counties_sf %>%
  left_join(dta, by = c("GEOID" = "area_fips"))

# Keep only the lower 48 (drop AK, HI, PR + other territories)
map_data_conus <- map_data %>%
  filter(!STATEFP %in% c(
    "02", # Alaska
    "15", # Hawaii
    "72", # Puerto Rico
    "60", # American Samoa
    "66", # Guam
    "69", # Northern Mariana Islands
    "78"  # US Virgin Islands
  ))

# Plot the intensity map
ggplot(map_data_conus) +
  geom_sf(aes(fill = RET_tariff_r), color = NA) +
  scale_fill_viridis_c(option = "magma", na.value = "grey90", direction = -1) +
  labs(
    title = "Share Intensity by County",
    fill  = "Share"
  ) +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50), expand = FALSE) +
  theme_minimal() +
  theme(
    axis.text  = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )


# Plot the intensity map
ggplot(map_data_conus) +
  geom_sf(aes(fill = RET_NTB_r), color = NA) +
  scale_fill_viridis_c(option = "magma", na.value = "grey90", direction = -1) +
  labs(
    title = "Share Intensity by County",
    fill  = "Share"
  ) +
  coord_sf(xlim = c(-125, -66), ylim = c(24, 50), expand = FALSE) +
  theme_minimal() +
  theme(
    axis.text  = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  )



