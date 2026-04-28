

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
library(readr)
library(dplyr)
library(sf)
library(tigris)
library(ggplot2)
library(viridis)



rm(list=ls())
# Set directory
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git/data")
getwd()

exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/summary/exposure_maps"


################################################################################ 
sectors <- read_csv("crosswalk/HS6_NAICS_Diane/NAICS_industry_2012.csv")
sapply(sectors, class)
dta <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/QCEW_2012_naics6_CZ.csv")

cw_cty_czone_2012 <- read_csv("crosswalk_CZ_county/cw_cty_czone_2012.csv")
names(cw_cty_czone_2012)


output_NAICS6 <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/Census_output/output_NAICS_6.csv")
names(output_NAICS6)
output_NAICS6 <- output_NAICS6 %>% select(NAICS6_2012, NAICS_description)
sapply(output_NAICS6, class)

################################################################################ 

# merge with sector definition from Diane
names(sectors)
sectors <- sectors %>% select(naics, subsector, ag_subsector)
sectors <- sectors %>% distinct() %>% filter(!is.na(naics))
sectors <- sectors %>%  group_by(naics, subsector) %>%
  summarise(ag_subsector = paste(ag_subsector, collapse = ", "), .groups = "drop")
length(unique(sectors$naics))
test <- sectors %>% filter(duplicated(naics) | duplicated(naics, fromLast = TRUE))
sectors <- sectors %>%  group_by(naics) %>%
  filter(if (n_distinct(subsector) > 1) subsector == "crop" else TRUE) %>%  ungroup()
class(sectors$naics)

# merge with NAICS detail info
sectors <- left_join(sectors, output_NAICS6, by = c("naics" = "NAICS6_2012"))


# create subsectors:

sectors <- sectors %>% 
  mutate(sector_group = case_when(
    # Manufacturing — must come BEFORE farming categories
    subsector == "crop" & grepl("milling|mfg|manufacturing|canning|processing|roasting", NAICS_description, ignore.case = TRUE) ~ "Food Manufacturing",
    subsector == "crop" & grepl("Soybean|Oilseed|Dry pea|Wheat|Corn farming|Rice farming|Other grain", NAICS_description, ignore.case = TRUE) ~ "Grain & Oilseed Farming",
    subsector == "crop" & grepl("grove|orchard|vineyard|Berry|strawberry|Tree nut|noncitrus fruit", NAICS_description, ignore.case = TRUE) ~ "Fruit & Nut Farming",
    subsector == "crop" & grepl("Potato|vegetable|melon", NAICS_description, ignore.case = TRUE) ~ "Vegetable Farming",
    subsector == "crop" & grepl("Cotton", NAICS_description, ignore.case = TRUE) ~ "Cotton Farming",
    subsector == "crop" & grepl("textile", NAICS_description, ignore.case = TRUE) ~ "Textile Manufacturing",
    subsector == "crop" & grepl("Tobacco", NAICS_description, ignore.case = TRUE) ~ "Tobacco Farming",
    subsector == "crop" & grepl("Nursery|Floriculture", NAICS_description, ignore.case = TRUE) ~ "Nursery & Floriculture",
    subsector == "crop" & grepl("Sugarcane|Hay|Sugar beet", NAICS_description, ignore.case = TRUE) ~ "Other Crops",
    subsector == "livestock" & grepl("Beef cattle|Cattle feedlots", NAICS_description) ~ "Cattle Farming",
    subsector == "livestock" & grepl("Hog and pig", NAICS_description) ~ "Hog & Pig Farming",
    subsector == "livestock" & grepl("slaughtering|Rendering|byproduct|Poultry processing", NAICS_description, ignore.case = TRUE) ~ "Meat Processing",
    subsector == "livestock" & grepl("Chicken|Turkey|Poultry|Broiler", NAICS_description, ignore.case = TRUE) ~ "Poultry Farming",
    subsector == "livestock" & grepl("Sheep|Goat", NAICS_description) ~ "Sheep & Goat Farming",
    subsector == "livestock" & grepl("Apiculture|Horse|Fur-bearing|All other animal", NAICS_description) ~ "Other Animal Production",
    subsector == "livestock" & grepl("milk|butter|Cheese|dairy|Ice cream", NAICS_description, ignore.case = TRUE) ~ "Dairy Manufacturing",
    subsector == "livestock" & grepl("Leather|hide", NAICS_description, ignore.case = TRUE) ~ "Leather & Hides",
    TRUE ~ "Other"
  ))


################################################################################ 

class(dta$naics)

# Extract 2-digit NAICS sector
dta <- dta %>%  mutate(naics2 = as.integer(naics %/% 10000))

# Classify into three broad sectors
dta <- left_join(dta, sectors)
colSums(is.na(dta))
test <- dta %>% filter(is.na(subsector))
unique(test$naics)
dta <- dta %>% mutate(subsector = if_else(is.na(subsector), "nonag", subsector))
unique(dta$subsector)

# Compute total employment by CZ and sector
sector_emp <- dta %>%  group_by(czone_2012, subsector) %>%
  summarise(emp_sector = sum(emp, na.rm = TRUE), .groups = "drop")

# Compute total employment by CZ and sector
sector_emp <- dta %>%  group_by(czone_2012, subsector) %>%
  summarise(emp_sector = sum(emp, na.rm = TRUE), .groups = "drop")

# Compute total employment by CZ
total_emp <- dta %>%  group_by(czone_2012) %>%
  summarise(emp_total = sum(emp, na.rm = TRUE), .groups = "drop")

# Compute shares
sector_shares <- sector_emp %>%  left_join(total_emp, by = "czone_2012") %>%
  mutate(share = emp_sector / emp_total) %>%
  select(czone_2012, subsector, share) 

################################################################################ 

library(maps)
library(sf)
library(maps)
library(sf)
library(maps)
library(sf)
library(dplyr)
library(ggplot2)
library(viridis)

# ── Step 1: Get county map and extract state/county names ────────────────────
counties_sf <- st_as_sf(map("county", plot = FALSE, fill = TRUE)) %>%
  st_transform(crs = 5070) %>%
  # ID is in format "state,county" — split into two columns
  mutate(
    state_name  = sub(",.*", "", ID),
    county_name = sub(".*,", "", ID)
  )

# ── Step 2: Get name-to-FIPS crosswalk (built into maps package) ─────────────
data(county.fips)  # built-in to maps package
# county.fips has columns: fips, polyname (in "state,county" format)

counties_sf <- counties_sf %>%
  left_join(county.fips, by = c("ID" = "polyname")) %>%
  mutate(cty_fips_2012 = as.numeric(fips))

# ── Step 3: Join your CZ crosswalk ──────────────────────────────────────────
counties_sf <- counties_sf %>%  left_join(cw_cty_czone_2012, by = "cty_fips_2012")

# ── Step 4: Dissolve to CZ level ────────────────────────────────────────────
library(lwgeom) # needed for st_make_valid in some versions

# Fix invalid geometries first, then dissolve
cz_sf <- counties_sf %>%  filter(!is.na(czone_2012)) %>%
  st_make_valid() %>%                          # fix topology issues
  group_by(czone_2012) %>%
  summarise(geometry = st_union(geom), .groups = "drop") %>%
  st_make_valid()                              # fix any remaining issues after union

# ── Step 5: Merge RET data and plot ─────────────────────────────────────────
cz_map <- cz_sf %>%  left_join(sector_shares, by = "czone_2012")

# Check
table(cz_map$year)
names(cz_map)
colSums(is.na(cz_map))
summary(cz_map)
# ── Set sector filter ─────────────────────────────────────────────────────────
cz_map <- cz_map %>% filter(subsector %in% c("crop", "livestock" , "nonag"))
cz_map <- cz_map %>%  mutate(subsector = recode(subsector,    "nonag" = "non-ag"  ))

################################################################################
theme_map_autor <-  theme_minimal(base_size = 10, base_family = "Times New Roman") +
  theme(
    panel.spacing.x  = unit(1.2, "lines"),
    panel.grid       = element_blank(),
    axis.text        = element_blank(),
    axis.ticks       = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    plot.title       = element_text(size = 12, hjust = 0.5),
    strip.text       = element_text(size = 12, face = "bold", family = "Times New Roman"),
    legend.position  = "bottom",
    legend.title     = element_text(size = 11),
    legend.text      = element_text(size = 11),
    legend.key.height = unit(0.45, "cm"),
    legend.key.width  = unit(0.35, "cm")
  )

theme_map <- theme_minimal(base_size = 10, base_family = "Times New Roman") +
  theme(
    panel.spacing.x    = unit(-0.5, "lines"),
    panel.spacing.y    = unit(-0.5, "lines"),
    panel.grid         = element_blank(),
    axis.text          = element_blank(),
    axis.ticks         = element_blank(),
    panel.background   = element_rect(fill = "white", color = NA),
    plot.background    = element_rect(fill = "white", color = NA),
    plot.title         = element_text(size = 9, hjust = 0.5),
    strip.text         = element_text(size = 9, face = "bold", family = "Times New Roman"),
    strip.background   = element_blank(),
    legend.position    = "bottom",
    legend.direction   = "horizontal",
    legend.box         = "vertical",
    legend.title       = element_text(size = 7),
    legend.text        = element_text(size = 7),
    legend.key.height  = unit(0.25, "cm"),
    legend.key.width   = unit(0.4, "cm"),
    legend.spacing.y   = unit(0.2, "cm"),
    legend.margin      = margin(2, 2, 2, 2),
    legend.box.spacing = unit(0.3, "cm"),
    plot.margin        = margin(2, 2, 2, 2)
  )

names(cz_map)
names(cz_map)
summary(cz_map)


library(patchwork)

# Split data
ag_map <- cz_map %>% filter(subsector %in% c("crop", "livestock"))
nonag_map <- cz_map %>% filter(subsector == "non-ag")

summary(ag_map)

# Ag bins and colors
ag_breaks <- c(0, 0.003, 0.006, 0.01, 0.02, 0.06, Inf)
ag_labels <- c("0.00-0.003", "0.003-0.006", "0.006-0.01", "0.01-0.02", 
               "0.02-0.06", "> 0.06")
ag_colors <- c("#FFFFEE", "#FFFFCC", "#FED976", "#FEB24C", "#E31A1C", "#800026")

# Split and bin
ag_map <- cz_map %>% 
  filter(subsector %in% c("crop", "livestock")) %>%
  mutate(share_bin = cut(share, breaks = ag_breaks, labels = ag_labels, include.lowest = TRUE))


# Ag plot
p_ag <- ggplot(ag_map) +
  aes(fill = share_bin) +
  geom_sf(color = "white", linewidth = 0.05) +
  geom_sf(fill = NA, color = "black", linewidth = 0.05) +
  scale_fill_manual(values = ag_colors, name = "labor shares", na.value = "grey80", drop = FALSE) +
  facet_wrap(~subsector) +
  theme_void() +
  theme_map
p_ag

ggsave(filename = file.path(exp, paste0("labor_share_ag.png")),
       plot = plot, width = 8, height = 5, dpi = 300)



summary(nonag_map)
# Non-ag bins and colors
nonag_breaks <- c(0, 0.70, 0.85, 0.93, 0.96, 0.99, Inf)
nonag_labels <- c("< 0.70", "0.70-0.85", "0.85-0.93", "0.93-0.96", 
                  "0.96-0.99", "> 0.99")
nonag_colors <- c("#FFFFEE", "#FFFFCC", "#FED976", "#FEB24C", "#E31A1C", "#800026")
nonag_map <- cz_map %>% 
  filter(subsector == "non-ag") %>%
  mutate(share_bin = cut(share, breaks = nonag_breaks, labels = nonag_labels, include.lowest = TRUE))


# Non-ag plot
p_nonag <- ggplot(nonag_map) +
  aes(fill = share_bin) +
  geom_sf(color = "white", linewidth = 0.05) +
  geom_sf(fill = NA, color = "black", linewidth = 0.05)+ 
  scale_fill_manual(values = nonag_colors, name = NULL, na.value = "grey80", drop = FALSE) +
  facet_wrap(~subsector) +
  theme_void() +
  theme_map

# Combine
plot <- p_ag + p_nonag + plot_layout(widths = c(2, 1))
plot

ggsave(filename = file.path(exp, paste0("labor_share.png")),
       plot = plot, width = 8, height = 3.5, dpi = 300)

