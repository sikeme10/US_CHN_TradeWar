
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


dta <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/MFP/SUB.csv")


exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/summary/exposure_maps"


cw_cty_czone_2012 <- read_csv("crosswalk_CZ_county/cw_cty_czone_2012.csv")
names(cw_cty_czone_2012)

################################################################################ 

names(dta)
dta <- dta %>% mutate(SUB_monthly = SUB/12)
summary(dta)

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
counties_sf <- counties_sf %>%
  left_join(cw_cty_czone_2012, by = "cty_fips_2012")

# ── Step 4: Dissolve to CZ level ────────────────────────────────────────────
library(lwgeom) # needed for st_make_valid in some versions

# Fix invalid geometries first, then dissolve
cz_sf <- counties_sf %>%
  filter(!is.na(czone_2012)) %>%
  st_make_valid() %>%                          # fix topology issues
  group_by(czone_2012) %>%
  summarise(geometry = st_union(geom), .groups = "drop") %>%
  st_make_valid()                              # fix any remaining issues after union

# ── Step 5: Merge RET data and plot ─────────────────────────────────────────
cz_map <- cz_sf %>%  left_join(dta, by = "czone_2012")
cz_map <- cz_map %>% filter(year %in% (2018:2019))
# Check
table(cz_map$year)
names(cz_map)
cz_map <- cz_map %>% filter(!is.na(year))
colSums(is.na(cz_map))
summary(cz_map)

##################################################################################

theme_map_autor <-  theme_minimal(base_size = 14, base_family = "Times New Roman") +
  theme(
    panel.spacing.x  = unit(1.2, "lines"),
    panel.grid       = element_blank(),
    axis.text        = element_blank(),
    axis.ticks       = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    plot.title       = element_text(size = 11, hjust = 0.5),
    strip.text       = element_text(size = 10, face = "bold", family = "Times New Roman"),
    legend.position  = "right",
    legend.title     = element_text(size = 10),
    legend.text      = element_text(size = 10),
    legend.key.height = unit(0.45, "cm"),
    legend.key.width  = unit(0.35, "cm")
  )

theme_map <- theme_minimal(base_size = 10, base_family = "Times New Roman") +
  theme(
    panel.spacing.x   = unit(0.3, "lines"),
    panel.grid        = element_blank(),
    axis.text         = element_blank(),
    axis.ticks        = element_blank(),
    panel.background  = element_rect(fill = "white", color = NA),
    plot.background   = element_rect(fill = "white", color = NA),
    plot.title        = element_text(size = 9, hjust = 0.5),
    strip.text        = element_text(size = 8, face = "bold", family = "Times New Roman"),
    legend.position   = "right",
    legend.title      = element_text(size = 7),
    legend.text       = element_text(size = 7),
    legend.key.height = unit(0.3, "cm"),
    legend.key.width  = unit(0.25, "cm"),
    legend.margin     = margin(0, 0, 0, 0),
    legend.box.spacing = unit(0.1, "cm"),
    plot.margin       = margin(2, 2, 2, 2)
  )


################################################################################

summary(cz_map)

test <- cz_map %>% filter(year == 2019 )
summary(test)

library(ggplot2)
library(dplyr)
library(viridis)
summary(cz_map)

# ── Define the exact bins and colors from the image ──────────────────────────
breaks <- c(0.000, 0.003, 0.022, 0.086, 0.362, 1.521, Inf)
labels <- c("0.000-0.003", "0.003-0.022", "0.022-0.086",
            "0.086-0.362", "0.362-1.521", "> 1.521")
colors <- c("#FEFEBE", "#FDD58B", "#F4A657", "#D95F2B", "#B22421", "#6B0000")


# ── Bin your RET variable ─────────────────────────────────────────────────────
cz_map <- cz_map %>%
  mutate(SUB_monthly_bin = cut(SUB_monthly,
                                     breaks = breaks,
                                     labels = labels,
                                     include.lowest = TRUE))

# ── Plot ──────────────────────────────────────────────────────────────────────
plot <- ggplot(subset(cz_map, year == 2019)) +
  aes(fill = SUB_monthly_bin) +
  geom_sf(color = "white", linewidth = 0.05) +
  geom_sf(fill = NA, color = "black", linewidth = 0.2) + # CZ outlines in black
  scale_fill_manual(values   = colors, name = "IMP Tariff",    na.value = "grey80",    drop     = FALSE  ) +
  labs( title   = "U.S. import tariff exposure") +
  theme_void() +
  theme_map_autor
plot
ggsave(filename = file.path(exp,  "SUB_2019_autor.png"),plot = plot, width = 8, height = 5, dpi = 300)




################################################################################



##################################################################################

# ── Define the exact bins and colors from the image ──────────────────────────

breaks <- c(0, 1, 5, 15, 50, 150, Inf)
labels <- c("0-1", "1-5", "5-15", "15-50", "50-150", "> 150")
colors <- c("#FEFEBE", "#FDD58B", "#F4A657", "#D95F2B", "#B22421", "#6B0000")


# ── Bin your RET variable ─────────────────────────────────────────────────────
cz_map <- cz_map %>%
  mutate( SUB_bin = cut( SUB ,
                         breaks = breaks,
                         labels = labels,
                         include.lowest = TRUE))
                                     

# ── Plot ──────────────────────────────────────────────────────────────────────
plot <- ggplot(cz_map) +
  aes(fill = SUB_bin) +
  geom_sf(color = "white", linewidth = 0.05) +
  geom_sf(fill = NA, color = "black", linewidth = 0.2) + # CZ outlines in black
  scale_fill_manual(values   = colors, name = "SUB",    na.value = "grey80",    drop     = FALSE  ) +
  labs( title   = "Farm subsidies per capita") +
  facet_wrap(~year) +
  theme_void() +
  theme_map
plot
ggsave(filename = file.path(exp,  "SUB.png"),plot = plot, width = 5, height = 2, dpi = 300)











