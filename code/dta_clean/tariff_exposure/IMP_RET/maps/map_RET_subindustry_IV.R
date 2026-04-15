


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

# dta <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6/RET_r_CHN_naics6_IV.csv")
dta <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6/RET_r_subsectors_IV.csv")


summary(dta)
names(dta)
unique(dta$czone_2012)
unique(dta$year)


cw_cty_czone_2012 <- read_csv("crosswalk_CZ_county/cw_cty_czone_2012.csv")
names(cw_cty_czone_2012)
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
cz_map <- cz_sf %>%  left_join(dta, by = "czone_2012")

# Check
table(cz_map$year)
names(cz_map)
cz_map <- cz_map %>% filter(!is.na(year)) %>% filter(year %in% (2018:2019))
colSums(is.na(cz_map))
summary(cz_map)


################################################################################
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
    legend.position  = "bottom",
    legend.title     = element_text(size = 10),
    legend.text      = element_text(size = 10),
    legend.key.height = unit(0.45, "cm"),
    legend.key.width  = unit(0.35, "cm")
  )

theme_map <- theme_minimal(base_size = 10, base_family = "Times New Roman") +
  theme(
    panel.spacing.x    = unit(-0.5, "lines"),  # negative pulls maps together
    panel.spacing.y    = unit(-0.5, "lines"),
    panel.grid         = element_blank(),
    axis.text          = element_blank(),
    axis.ticks         = element_blank(),
    panel.background   = element_rect(fill = "white", color = NA),
    plot.background    = element_rect(fill = "white", color = NA),
    plot.title         = element_text(size = 9, hjust = 0.5),
    strip.text         = element_text(size = 7, face = "bold", family = "Times New Roman"),
    strip.background   = element_blank(),
    legend.position    = "right",
    legend.title       = element_text(size = 4),
    legend.text        = element_text(size = 4),
    legend.key.height  = unit(0.08, "cm"),
    legend.key.width   = unit(0.08, "cm"),
    legend.spacing.y   = unit(0.02, "cm"),
    legend.margin      = margin(0, 0, 0, 0),
    legend.box.spacing = unit(0.02, "cm"),
    plot.margin        = margin(0, 0, 0, 0)
  )


################################################################################
# select sector

unique(cz_map$year)
unique(cz_map$subsector)
names(cz_map)

# ── Set sector filter ─────────────────────────────────────────────────────────
cz_map <- cz_map %>% filter(subsector %in% c("crop", "livestock" , "nonag"))
cz_map <- cz_map %>%  mutate(subsector = recode(subsector,    "nonag" = "non-ag"  ))

################################################################################
# ── tariff─────────────────────────────────────────────────────────────────────

# ── Define bins, labels, colors (2019 single map) ────────────────────────────
breaks <- c(0, 0.01, 0.02, 0.03, 0.04, 0.06, 0.75)
labels <- c("0.00 - 0.01", "0.01 - 0.02", "0.02 - 0.03",
            "0.03 - 0.04", "0.04 - 0.06", "> 0.06")
colors <- c("#FEFEBE", "#FDD58B", "#F4A657", "#D95F2B", "#B22421", "#6B0000")

cz_map <- cz_map %>%
  mutate(RET_tariff_bin = cut(RET_tariff_tot_r,breaks = breaks,
                              labels = labels,include.lowest = TRUE))

# ── Plot 2019 ─────────────────────────────────────────────────────────────────
plot <- ggplot(subset(cz_map, year == 2019 )) +
  aes(fill = RET_tariff_bin) +
  geom_sf(color = "white", linewidth = 0.05) +
  geom_sf(fill = NA, color = "black", linewidth = 0.2) +
  scale_fill_manual(values = colors, name = "RET Tariff", na.value = "grey80", drop = FALSE) +
  labs(title = paste("Retaliatory tariff exposure in 2019 by subsector")) +
  facet_wrap(~subsector) +
  theme_void() +
  theme_map_autor
plot
ggsave(filename = file.path(exp, paste0("RET_tariff_2019_subsector", "_autor.png")),
       plot = plot, width = 8, height = 5, dpi = 300)



# ── Define bins, labels, colors (all years faceted) ───────────────────────────
breaks <- c(0, 0.01, 0.02, 0.03, 0.04, 0.06, Inf)
labels <- c("0.00-0.01", "0.01-0.02", "0.02-0.03",
            "0.03-0.04", "0.04-0.06", "> 0.06")
colors <- c("#FEFEBE", "#FDD58B", "#F4A657", "#D95F2B", "#B22421", "#6B0000")



breaks <- c(0, 0.01, 0.02, 0.03, 0.04, 0.06, 0.50, Inf)
labels <- c("0.00-0.02", "0.02-0.04", "0.04-0.06",
            "0.06-0.08", "0.08-0.12", "0.12-0.50", ">0.50")
colors <- c("#FEFEBE", "#FDD58B", "#F4A657", "#D95F2B", "#B22421", "#6B0000", "#3B0000")

cz_map <- cz_map %>%
  mutate(RET_tariff_bin = cut(RET_tariff_tot_r, breaks = breaks,
                              labels = labels,include.lowest = TRUE))

# ── Plot all years faceted ────────────────────────────────────────────────────
plot <- ggplot(subset(cz_map)) +
  aes(fill = RET_tariff_bin) +
  geom_sf(color = "white", linewidth = 0.05) +
  geom_sf(fill = NA, color = "black", linewidth = 0.05) +
  facet_wrap(~subsector) +
  scale_fill_manual(values = colors, name = "RET Tariff", na.value = "grey80", drop = FALSE) +
  labs(title = paste("Retaliatory tariff exposure by subsector")) +
  theme_void() +
  theme_map
plot
ggsave(filename = file.path(exp, paste0("RET_tariff_subsector.png")),
       plot = plot, width = 5, height = 2, dpi = 300)



plot_2018 <- ggplot(subset(cz_map, year == 2018)) +
  aes(fill = RET_tariff_bin) +
  geom_sf(color = "white", linewidth = 0.05) +
  geom_sf(fill = NA, color = "black", linewidth = 0.05) +
  facet_wrap(~subsector) +
  scale_fill_manual(values = colors, name = "RET Tariff", na.value = "grey80", drop = FALSE) +
  labs(title = paste("Retaliatory tariff exposure by subsector: 2018")) +
  theme_void() +
  theme_map

plot_2019 <- ggplot(subset(cz_map, year == 2019)) +
  aes(fill = RET_tariff_bin) +
  geom_sf(color = "white", linewidth = 0.05) +
  geom_sf(fill = NA, color = "black", linewidth = 0.05) +
  facet_wrap(~subsector) +
  scale_fill_manual(values = colors, name = "RET Tariff", na.value = "grey80", drop = FALSE) +
  labs(title = paste("Retaliatory tariff exposure by subsector: 2019")) +
  theme_void() +
  theme_map
plot <- plot_2018 / plot_2019
plot
ggsave(filename = file.path(exp, paste0("RET_tariff_subsector_year.png")), plot = plot, width = 5, height = 4, dpi = 300)


################################################################################
summary(cz_map)

# ── NTM ───────────────────────────────────────────────────────────────────────
breaks <- c(-Inf, 0, 0.05, 0.1, 0.5, 1.75, Inf)
labels <- c("<0", "0.00-0.05", "0.05-0.10", "0.10-0.50", "0.50-1.50", ">1.50")

breaks <- c(-001, 0.1, 0.02, 0.03, 0.04, 0.06, 0.8)
labels <- c("0.00-0.01", "0.01-0.02", "0.02-0.03",
            "0.03-0.04", "0.04-0.06", "> 0.06")

colors <- c("#FEFEBE", "#FDD58B", "#F4A657", "#D95F2B", "#B22421", "#6B0000")




breaks <- c(-Inf, 0.01, 0.02, 0.03, 0.04, 0.06, 0.50, Inf)
labels <- c("0.00-0.02", "0.02-0.04", "0.04-0.06",
            "0.06-0.08", "0.08-0.12", "0.12-0.50", ">0.50")
colors <- c("#FEFEBE", "#FDD58B", "#F4A657", "#D95F2B", "#B22421", "#6B0000", "#3B0000")


cz_map <- cz_map %>%
  mutate(RET_NTM_bin = cut(RET_NTB_tot_r, breaks = breaks,
                           labels = labels, include.lowest = TRUE))

plot <- ggplot(subset(cz_map)) +
  aes(fill = RET_NTM_bin) +
  geom_sf(color = "white", linewidth = 0.05) +
  geom_sf(fill = NA, color = "black", linewidth = 0.05) +
  scale_fill_manual(values = colors, name = "RET NTM", na.value = "grey80", drop = FALSE) +
  labs(title = paste("Retaliatory NTM exposure by subsector")) +
  facet_wrap(~subsector) +
  theme_void() +
  theme_map
plot

ggsave(filename = file.path(exp, paste0("RET_NTM_subsector.png")),
       plot = plot, width = 5, height = 2, dpi = 300)


# ── NTM IV ────────────────────────────────────────────────────────────────────
breaks <- c(-001, 0.1, 0.02, 0.03, 0.04, 0.06, 0.8)
labels <- c("0.00-0.01", "0.01-0.02", "0.02-0.03",
            "0.03-0.04", "0.04-0.06", "> 0.06")
colors <- c("#FEFEBE", "#FDD58B", "#F4A657", "#D95F2B", "#B22421", "#6B0000")

cz_map <- cz_map %>%
  mutate(RET_NTM_IV_bin = cut(RET_NTB_tot_IV_r, breaks = breaks,
                              labels = labels, include.lowest = TRUE))

plot <- ggplot(subset(cz_map)) +
  aes(fill = RET_NTM_IV_bin) +
  geom_sf(color = "white", linewidth = 0.05) +
  geom_sf(fill = NA, color = "black", linewidth = 0.05) +
  scale_fill_manual(values = colors, name = "RET NTM", na.value = "grey80", drop = FALSE) +
  labs(title = paste("Retaliatory NTM exposure (using IV) by subsector" )) +
  facet_wrap(~subsector) +
  theme_void() +
  theme_map
plot

ggsave(filename = file.path(exp, paste0("RET_NTM_IV_subsector.png")),
       plot = plot, width = 5, height = 2, dpi = 300)

