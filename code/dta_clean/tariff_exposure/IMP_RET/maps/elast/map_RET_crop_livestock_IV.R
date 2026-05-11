


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

exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/summary/exposure_maps/elast/"


################################################################################ 

# dta <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6/RET_r_CHN_naics6_IV.csv")
crop <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6/RET_elast_r_crop_IV.csv")
livestock <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6/RET_elast_r_livestock_IV.csv")

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
create_cz_map <- function(data, cz_sf) {
  
  complete_grid <- expand.grid(
    czone_2012   = unique(cz_sf$czone_2012),
    sector_group = unique(data$sector_group),
    year         = c(2018, 2019),
    stringsAsFactors = FALSE
  )
  
  data_complete <- complete_grid %>%
    left_join(data, by = c("czone_2012", "sector_group", "year")) %>%
    mutate(across(c(RET_i_tariff, RET_i_NTB, RET_i_NTB_IV,
                    RET_tariff_tot_r, RET_NTB_tot_r, RET_NTB_tot_IV_r),
                  ~replace_na(.x, 0))) %>%
    filter(year %in% 2018:2019)
  
  cz_map <- cz_sf %>% left_join(data_complete, by = "czone_2012")
  
  expected <- nrow(cz_sf) * n_distinct(data$sector_group) * 2
  actual   <- nrow(cz_map)
  message("Expected rows: ", expected, " | Actual rows: ", actual,
          if (expected == actual) " -- OK" else " -- MISMATCH, check join")
  
  return(cz_map)
}

library(rmapshaper)
# cz_sf <- ms_simplify(cz_sf, keep = 0.05, check_validity = FALSE)

# Rebuild everything after
cz_map_crop      <- create_cz_map(crop, cz_sf)
cz_map_livestock <- create_cz_map(livestock, cz_sf)

################################################################################


theme_map <- theme_minimal(base_size = 12, base_family = "Times New Roman") +
  theme(
    panel.spacing.x    = unit(-0.5, "lines"),  # negative pulls maps together
    panel.spacing.y    = unit(-0.5, "lines"),
    panel.grid         = element_blank(),
    axis.text          = element_blank(),
    axis.ticks         = element_blank(),
    panel.background   = element_rect(fill = "white", color = NA),
    plot.background    = element_rect(fill = "white", color = NA),
    plot.title         = element_text(size = 10, hjust = 0.5),
    strip.text         = element_text(size = 10, face = "bold", family = "Times New Roman"),
    strip.background   = element_blank(),
    legend.position    = "right",
    legend.title       = element_text(size = 9),
    legend.text        = element_text(size = 9),
    legend.key.height  = unit(0.08, "cm"),
    legend.key.width   = unit(0.08, "cm"),
    legend.spacing.y   = unit(0.02, "cm"),
    legend.margin      = margin(0, 0, 0, 0),
    legend.box.spacing = unit(0.02, "cm"),
    plot.margin        = margin(0, 0, 0, 0)
  )


################################################################################
# for tariff 
################################################################################


# ── Crop─────────────────────────────────────────────────────────────────────

breaks <- c(0, 0.001, 0.005, 0.01, 0.02, 0.03, 0.04, 0.06, Inf)
labels <- c("0-0.001", "0.001-0.005", "0.005-0.01", "0.01-0.02", "0.02-0.03", "0.03-0.04", "0.04-0.06", ">0.06")
colors <- c("#FEFEBE", "#FEF0A0", "#FDD58B", "#F4A657", "#D95F2B", "#B22421", "#6B0000", "#3B0000")





cz_map_crop <- cz_map_crop %>%  mutate(RET_tariff_bin = cut(RET_tariff_tot_r, breaks = breaks,
                              labels = labels, include.lowest = TRUE))

plot <- ggplot(cz_map_crop) +
  aes(fill = RET_tariff_bin) +
  geom_sf(color = "white", linewidth = 0.05) +
  geom_sf(fill = NA, color = "black", linewidth = 0.05) +
  scale_fill_manual(
    values   = colors,
    name     = "RET Tariff",
    na.value = "grey80",
    drop     = FALSE  ) +
  labs(title = "Retaliatory tariff exposure by crop sectors") +
  facet_wrap(~sector_group) +
  theme_void() +
  theme_map
plot
ggsave(filename = file.path(exp, paste0("RET_tariff_crop.png")),plot = plot,,width = 10, height = 4.5, dpi = 300)

# ── Livestock─────────────────────────────────────────────────────────────────────

summary(cz_map_livestock)

breaks <- c(0, 0.001, 0.005, 0.01, 0.02, 0.03, 0.04, 0.06, Inf)
labels <- c("0-0.001", "0.001-0.005", "0.005-0.01", "0.01-0.02", "0.02-0.03", "0.03-0.04", "0.04-0.06", ">0.06")
colors <- c("#FEFEBE", "#FEF0A0", "#FDD58B", "#F4A657", "#D95F2B", "#B22421", "#6B0000", "#3B0000")


cz_map_livestock <- cz_map_livestock %>%  mutate(RET_tariff_bin = cut(RET_tariff_tot_r, breaks = breaks,
                                                            labels = labels, include.lowest = TRUE))

plot <- ggplot(cz_map_livestock) +
  aes(fill = RET_tariff_bin) +
  geom_sf(color = "white", linewidth = 0.05) +
  geom_sf(fill = NA, color = "black", linewidth = 0.05) +
  scale_fill_manual(
    values   = colors,
    name     = "RET Tariff",
    na.value = "grey80",
    drop     = FALSE  ) +
  labs(title = "Retaliatory tariff exposure by livestock sectors") +
  facet_wrap(~sector_group) +
  theme_void() +
  theme_map
plot


ggsave(filename = file.path(exp, paste0("RET_tariff_livestock.png")),plot = plot,width = 10, height = 4.5, dpi = 300)



# 
# cap <- quantile(cz_map$RET_tariff_tot_r, 0.95, na.rm = TRUE)
# 
# plot <- ggplot(subset(cz_map_crop, year == 2019)) +
#   aes(fill = RET_tariff_tot_r) +
#   geom_sf(color = "white", linewidth = 0.05) +
#   geom_sf(fill = NA, color = "black", linewidth = 0.2) +
#   scale_fill_gradientn(
#     colors   = c("#FEFEBE", "#FDD58B", "#F4A657", "#D95F2B", "#B22421", "#6B0000"),
#     name     = "RET Tariff",
#     na.value = "grey80",
#     limits   = c(0, cap),
#     oob      = squish
#   ) +
#   labs(title = "Retaliatory tariff exposure in 2019 by subsector") +
#   facet_wrap(~sector_group) +
#   theme_void() +
#   theme_map
# plot


################################################################################
# NTMs  
################################################################################
summary(cz_map_crop)

# ── NTM ───────────────────────────────────────────────────────────────────────

breaks <- c(0, 0.001, 0.005, 0.01, 0.02, 0.03, 0.04, 0.06, Inf)
labels <- c("0-0.001", "0.001-0.005", "0.005-0.01", "0.01-0.02", "0.02-0.03", "0.03-0.04", "0.04-0.06", ">0.06")
colors <- c("#FEFEBE", "#FEF0A0", "#FDD58B", "#F4A657", "#D95F2B", "#B22421", "#6B0000", "#3B0000")

cz_map_crop <- cz_map_crop %>%  mutate(RET_NTM_bin = cut(RET_NTB_tot_r , breaks = breaks,
                                                         labels = labels, include.lowest = TRUE))

plot <- ggplot(cz_map_crop) +
  aes(fill = RET_NTM_bin) +
  geom_sf(color = "white", linewidth = 0.05) +
  geom_sf(fill = NA, color = "black", linewidth = 0.05) +
  scale_fill_manual(
    values   = colors,
    name     = "RET NTM",
    na.value = "grey80",
    drop     = FALSE  ) +
  labs(title = "Retaliatory NTM exposure by crop sectors") +
  facet_wrap(~sector_group) +
  theme_void() +
  theme_map
plot


ggsave(filename = file.path(exp, paste0("RET_NTM_crop.png")),plot = plot, width = 10, height = 4.5,dpi = 300)

# ── Livestock─────────────────────────────────────────────────────────────────────

summary(cz_map_livestock)

breaks <- c(0, 0.001, 0.005, 0.01, 0.02, 0.03, 0.04, 0.06, Inf)
labels <- c("0-0.001", "0.001-0.005", "0.005-0.01", "0.01-0.02", "0.02-0.03", "0.03-0.04", "0.04-0.06", ">0.06")
colors <- c("#FEFEBE", "#FEF0A0", "#FDD58B", "#F4A657", "#D95F2B", "#B22421", "#6B0000", "#3B0000")


cz_map_livestock <- cz_map_livestock %>%  mutate(RET_NTM_bin = cut(RET_NTB_tot_r, breaks = breaks,
                                                                   labels = labels, include.lowest = TRUE))

plot <- ggplot(cz_map_livestock) +
  aes(fill = RET_NTM_bin) +
  geom_sf(color = "white", linewidth = 0.05) +
  geom_sf(fill = NA, color = "black", linewidth = 0.05) +
  scale_fill_manual(
    values   = colors,
    name     = "RET NTM",
    na.value = "grey80",
    drop     = FALSE  ) +
  labs(title = "Retaliatory NTM exposure by livestock sectors") +
  facet_wrap(~sector_group) +
  theme_void() +
  theme_map
plot


ggsave(filename = file.path(exp, paste0("RET_NTM_livestock.png")),plot = plot, width = 10, height = 4.5,dpi = 300)




################################################################################
# NTMs  IV
################################################################################
summary(cz_map_crop)

# ── NTM ───────────────────────────────────────────────────────────────────────

breaks <- c(0, 0.001, 0.005, 0.01, 0.02, 0.03, 0.04, 0.06, Inf)
labels <- c("0-0.001", "0.001-0.005", "0.005-0.01", "0.01-0.02", "0.02-0.03", "0.03-0.04", "0.04-0.06", ">0.06")
colors <- c("#FEFEBE", "#FEF0A0", "#FDD58B", "#F4A657", "#D95F2B", "#B22421", "#6B0000", "#3B0000")

cz_map_crop <- cz_map_crop %>%  mutate(RET_NTM_bin = cut(RET_NTB_tot_IV_r , breaks = breaks,
                                                            labels = labels, include.lowest = TRUE))

plot <- ggplot(cz_map_crop) +
  aes(fill = RET_NTM_bin) +
  geom_sf(color = "white", linewidth = 0.05) +
  geom_sf(fill = NA, color = "black", linewidth = 0.05) +
  scale_fill_manual(
    values   = colors,
    name     = "RET NTM",
    na.value = "grey80",
    drop     = FALSE  ) +
  labs(title = "Retaliatory NTM exposure (using IV) by crop sectors") +
  facet_wrap(~sector_group) +
  theme_void() +
  theme_map
plot


ggsave(filename = file.path(exp, paste0("RET_NTM_IV_crop.png")),plot = plot, width = 10, height = 4.5,dpi = 300)

# ── Livestock─────────────────────────────────────────────────────────────────────

summary(cz_map_livestock)

breaks <- c(0, 0.001, 0.005, 0.01, 0.02, 0.03, 0.04, 0.06, Inf)
labels <- c("0-0.001", "0.001-0.005", "0.005-0.01", "0.01-0.02", "0.02-0.03", "0.03-0.04", "0.04-0.06", ">0.06")
colors <- c("#FEFEBE", "#FEF0A0", "#FDD58B", "#F4A657", "#D95F2B", "#B22421", "#6B0000", "#3B0000")


cz_map_livestock <- cz_map_livestock %>%  mutate(RET_NTM_bin = cut(RET_NTB_tot_IV_r, breaks = breaks,
                                                                      labels = labels, include.lowest = TRUE))

plot <- ggplot(cz_map_livestock) +
  aes(fill = RET_NTM_bin) +
  geom_sf(color = "white", linewidth = 0.05) +
  geom_sf(fill = NA, color = "black", linewidth = 0.05) +
  scale_fill_manual(
    values   = colors,
    name     = "RET NTM",
    na.value = "grey80",
    drop     = FALSE  ) +
  labs(title = "Retaliatory NTM exposure (using IV) by livestock sectors") +
  facet_wrap(~sector_group) +
  theme_void() +
  theme_map
plot


ggsave(filename = file.path(exp, paste0("RET_NTM_IV_livestock.png")),plot = plot, width = 10, height = 4.5,dpi = 300)

