


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

dta <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6/RET_r_CHN_naics6.csv")

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

# Check
table(cz_map$year)
names(cz_map)
cz_map <- cz_map %>% filter(!is.na(year))
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

ggplot(cz_map) +
  aes(fill = RET_tariff_r) +
  geom_sf(color = "white", linewidth = 0.05) +
  geom_sf(fill = NA, color = "black", linewidth = 0.2) + # CZ outlines in black
  scale_fill_distiller( palette   = "YlOrRd",
    direction = 1, # 1 = yellow (low) → red (high)
    name      = "RET Tariff",  na.value  = "grey80"  ) +
  labs(title   = "(b) Retaliatory tariff exposure" ) +
  facet_wrap(~year) +
  theme_void() +
  theme(
    plot.title        = element_text(hjust = 0.5, size = 11),
    legend.position   = "right",
    legend.key.height = unit(0.8, "cm"),
    legend.key.width  = unit(0.3, "cm"),
    legend.text       = element_text(size = 7),
    legend.title      = element_text(size = 8)
  )

ggplot(cz_map) +
  aes(fill = RET_NTB_r) +
  geom_sf(color = "white", linewidth = 0.05) +
  geom_sf(fill = NA, color = "black", linewidth = 0.2) + # CZ outlines in black
  scale_fill_distiller( palette   = "YlOrRd",
                        direction = 1, # 1 = yellow (low) → red (high)
                        name      = "RET NTM",  na.value  = "grey80"  ) +
  facet_wrap(~year) +
  labs(title   = "(b) Retaliatory NTM exposure") +
  theme_void() +
  theme(
    plot.title        = element_text(hjust = 0.5, size = 11),
    legend.position   = "right",
    legend.key.height = unit(0.8, "cm"),
    legend.key.width  = unit(0.3, "cm"),
    legend.text       = element_text(size = 7),
    legend.title      = element_text(size = 8)
  )


################################################################################

unique(cz_map$year)


library(ggplot2)
library(dplyr)
library(viridis)

# ── Define the exact bins and colors from the image ──────────────────────────
breaks <- c(0, 0.013, 0.022, 0.030, 0.042, 0.061, 0.75)
labels <- c("0.000 - 0.013", "0.013 - 0.022", "0.022 - 0.030",
            "0.030 - 0.042", "0.042 - 0.061", "> 0.061")
colors <- c("#FEFEBE", "#FDD58B", "#F4A657", "#D95F2B", "#B22421", "#6B0000")
# colors <- c("#FFFFFF", "#FEE08B", "#FDAE61", "#F46D43", "#D73027", "#A50026")

# ── Bin your RET variable ─────────────────────────────────────────────────────
cz_map <- cz_map %>%
  mutate(RET_tariff_bin = cut(RET_tariff_r,
                              breaks = breaks,
                              labels = labels,
                              include.lowest = TRUE))

# ── Plot ──────────────────────────────────────────────────────────────────────
plot <- ggplot(subset(cz_map, year == 2019)) +
  aes(fill = RET_tariff_bin) +
  geom_sf(color = "white", linewidth = 0.05) +           # 1st: CZ fill + thin white internal borders
  geom_sf(fill = NA, color = "black", linewidth = 0.2) + # 2nd: CZ outlines in black (on top)
  scale_fill_manual(values   = colors,name     = "RET Tariff",  na.value = "grey80",  drop     = FALSE  ) +
  labs(title   = "Retaliatory tariff exposure in 2019") +
  theme_void() +
  theme_map_autor
plot
ggsave(filename = file.path(exp,  "RET_tariff_2019_autor.png"),plot = plot, width = 8, height = 5, dpi = 300)



# ── Define the exact bins and colors from the image ──────────────────────────
breaks <- c(0, 0.03, 0.06, 0.08, 0.12, 0.15, Inf)
labels <- c("0.00-0.03", "0.03-0.06", "0.06-0.08",
            "0.08-0.12", "0.12-0.15", "> 0.15")
colors <- c("#FEFEBE", "#FDD58B", "#F4A657", "#D95F2B", "#B22421", "#6B0000")
# colors <- c("#FFFFFF", "#FEE08B", "#FDAE61", "#F46D43", "#D73027", "#A50026")

# ── Bin your RET variable ─────────────────────────────────────────────────────
cz_map <- cz_map %>%
  mutate(RET_tariff_bin = cut(RET_tariff_r,
                              breaks = breaks,
                              labels = labels,
                              include.lowest = TRUE))

# ── Plot ──────────────────────────────────────────────────────────────────────
plot <- ggplot(cz_map) +
  aes(fill = RET_tariff_bin) +
  geom_sf(color = "white", linewidth = 0.05) +           # 1st: CZ fill + thin white internal borders
  geom_sf(fill = NA, color = "black", linewidth = 0.2) + # 2nd: CZ outlines in black (on top)
  scale_fill_manual(values   = colors,name     = "RET Tariff",  na.value = "grey80",  drop     = FALSE  ) +
  labs(    title   = "Retaliatory tariff exposure") +
  theme_void() +
  facet_wrap(~year)+
  theme_map
plot
ggsave(filename = file.path(exp,  "RET_tariff.png"),plot = plot,  width = 5, height = 2, dpi = 300)





# ################################################################################
# NTM 

# ── Define the exact bins and colors from the image ──────────────────────────
breaks <- c(0.000, 0.1, 0.2, 0.5, 1, 1.5, 13)
labels <- c("0.0-0.1", "0.1-0.2", "0.2-0.5",
            "0.5-1.0", "1.0-1.5", ">1.5")
colors <- c("#FEFEBE", "#FDD58B", "#F4A657", "#D95F2B", "#B22421", "#6B0000")

cz_map <- cz_map %>%  mutate(RET_NTM_bin = cut(RET_NTB_r,  breaks = breaks,
                                               labels = labels,  include.lowest = TRUE))

# ── Plot NTM ──────────────────────────────────────────────────────────────────────
plot <- ggplot(cz_map) +
  aes(fill = RET_NTM_bin) +
  geom_sf(color = "white", linewidth = 0.05) +           # 1st: CZ fill + thin white internal borders
  geom_sf(fill = NA, color = "black", linewidth = 0.2) +
  scale_fill_manual(values   = colors,name     = "RET NTM",  na.value = "grey80",  drop     = FALSE  ) +
  labs( title   = "Retaliatory NTM exposure") +
  facet_wrap(~year) +
  theme_void() +
  theme_map

plot
ggsave(filename = file.path(exp,  "RET_NTM.png"),plot = plot,  width = 5, height = 2, dpi = 300)


