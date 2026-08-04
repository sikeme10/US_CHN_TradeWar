


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
crop <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6/county/RET_elast_r_crop_IV.csv")
livestock <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6/county/RET_elast_r_livestock_IV.csv")

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
    ag_subsector = unique(data$ag_subsector),
    year         = c(2018, 2019),
    stringsAsFactors = FALSE
  )
  
  data_complete <- complete_grid %>%
    left_join(data, by = c("czone_2012", "ag_subsector", "year")) %>%
    mutate(across(c(RET_i_tariff, RET_i_NTB, RET_i_NTB_IV,
                    RET_tariff_tot_r, RET_NTB_tot_r, RET_NTB_tot_IV_r),
                  ~replace_na(.x, 0))) %>%
    filter(year %in% 2018:2019)
  
  cz_map <- cz_sf %>% left_join(data_complete, by = "czone_2012")
  
  expected <- nrow(cz_sf) * n_distinct(data$ag_subsector) * 2
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
  facet_wrap(~ag_subsector) +
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
  facet_wrap(~ag_subsector) +
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
#   facet_wrap(~ag_subsector) +
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
  facet_wrap(~ag_subsector) +
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
  facet_wrap(~ag_subsector) +
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
  facet_wrap(~ag_subsector) +
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
  facet_wrap(~ag_subsector) +
  theme_void() +
  theme_map
plot


ggsave(filename = file.path(exp, paste0("RET_NTM_IV_livestock.png")),plot = plot, width = 10, height = 4.5,dpi = 300)











################################################################################
# we create RET at County level (crop / livestock sector groups)
################################################################################

library(readr)
library(dplyr)
library(tidyr)
library(sf)
library(tigris)
library(maps)
library(ggplot2)
library(RColorBrewer)

options(tigris_use_cache = TRUE)

rm(list = ls())
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git/data")
getwd()

exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/summary/exposure_maps/elast/county/"
dir.create(exp, recursive = TRUE, showWarnings = FALSE)

################################################################################
crop      <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6/county/RET_elast_r_crop_IV.csv")
livestock <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6/county/RET_elast_r_livestock_IV.csv")

names(crop)
summary(crop)
unique(crop$ag_subsector)
unique(livestock$ag_subsector)

################################################################################
# ── Step 1: county geometry ──────────────────────────────────────────────────
counties_sf <- counties(year = 2013, cb = TRUE) %>%
  mutate(fips = as.numeric(paste0(STATEFP, COUNTYFP))) %>%
  filter(!STATEFP %in% c("02", "15")) %>%
  filter(as.numeric(STATEFP) <= 56) %>%
  st_transform(crs = 5070)

# ── Step 2: state overlay ────────────────────────────────────────────────────
states_sf <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE)) %>%
  st_transform(crs = 5070)

# ── Step 3: sanity checks ────────────────────────────────────────────────────
print(setdiff(unique(crop$fips), counties_sf$fips))
print(length(setdiff(counties_sf$fips, unique(crop$fips))))

################################################################################
# ── Balance the panel and merge onto geometry ────────────────────────────────
create_county_map <- function(data, counties_sf) {
  
  data_complete <- data %>%
    filter(year %in% 2018:2019) %>%
    complete(
      fips         = counties_sf$fips,
      ag_subsector,
      year,
      fill = list(
        RET_i_tariff     = 0, RET_i_NTB        = 0, RET_i_NTB_IV     = 0,
        RET_tariff_tot_r = 0, RET_NTB_tot_r    = 0, RET_NTB_tot_IV_r = 0
      )
    )
  
  county_map <- counties_sf %>% left_join(data_complete, by = "fips")
  
  expected <- nrow(counties_sf) * n_distinct(data$ag_subsector) * 2
  message("Expected rows: ", expected, " | Actual rows: ", nrow(county_map),
          if (expected == nrow(county_map)) " -- OK" else " -- MISMATCH, check join")
  
  county_map %>% filter(!is.na(year))
}

county_map_crop      <- create_county_map(crop, counties_sf)
county_map_livestock <- create_county_map(livestock, counties_sf)

################################################################################
# ── Theme (single definition, used everywhere) ───────────────────────────────
theme_trade_map <- theme_minimal(base_size = 10, base_family = "Times New Roman") +
  theme(
    panel.spacing      = unit(0.4, "lines"),
    plot.title         = element_blank(),
    panel.background   = element_rect(fill = "white", color = NA),
    plot.background    = element_rect(fill = "white", color = NA),
    panel.border       = element_rect(color = "black", fill = NA, linewidth = 0.4),
    panel.grid         = element_blank(),
    strip.background   = element_rect(fill = "grey92", color = "black", linewidth = 0.4),
    strip.text         = element_text(size = 9, face = "bold", color = "black"),
    axis.text          = element_blank(),
    axis.ticks         = element_blank(),
    axis.title         = element_blank(),
    legend.position    = "top",
    legend.text        = element_text(size = 9),
    legend.title       = element_blank(),
    legend.key.height  = unit(0.35, "cm"),
    legend.key.width   = unit(0.55, "cm"),
    legend.spacing.x   = unit(0.15, "cm"),
    legend.box.spacing = unit(0.2, "cm"),
    plot.caption       = element_text(size = 7, color = "grey40", hjust = 1)
  )

################################################################################
# ── Quantile binning helper ──────────────────────────────────────────────────
make_bins <- function(x, n = 6, digits = 4) {
  x <- x[is.finite(x) & x >= 0]
  brks <- unique(quantile(x, probs = seq(0, 1, length.out = n + 1), na.rm = TRUE))
  brks[1] <- 0
  brks[length(brks)] <- Inf
  labs <- paste0(sprintf(paste0("%.", digits, "f"), head(brks, -1)), " – ",
                 c(sprintf(paste0("%.", digits, "f"), brks[2:(length(brks) - 1)]),
                   paste0("> ", sprintf(paste0("%.", digits, "f"), brks[length(brks) - 1]))))
  labs[length(labs)] <- paste0("> ", sprintf(paste0("%.", digits, "f"), brks[length(brks) - 1]))
  list(breaks = brks, labels = labs,
       colors = brewer.pal(length(labs), "YlOrRd"))
}

# ── Build bins from raw data, pooled across crop + livestock so the two ──────
#    figures are directly comparable on color
d19 <- bind_rows(crop, livestock) %>% filter(year == 2019)

b_tariff <- make_bins(d19$RET_tariff_tot_r)
b_ntm    <- make_bins(d19$RET_NTB_tot_r)
b_ntm_iv <- make_bins(d19$RET_NTB_tot_IV_r)

add_bins <- function(m) {
  m %>% mutate(
    RET_tariff_bin = cut(RET_tariff_tot_r, b_tariff$breaks, b_tariff$labels, include.lowest = TRUE),
    RET_NTM_bin    = cut(RET_NTB_tot_r,    b_ntm$breaks,    b_ntm$labels,    include.lowest = TRUE),
    RET_NTM_IV_bin = cut(RET_NTB_tot_IV_r, b_ntm_iv$breaks, b_ntm_iv$labels, include.lowest = TRUE)
  )
}

county_map_crop      <- add_bins(county_map_crop)
county_map_livestock <- add_bins(county_map_livestock)

################################################################################
# ── Plot helper ──────────────────────────────────────────────────────────────
map_plot <- function(data, fill_var, bins, facet_yr = FALSE, nrow = NULL) {
  p <- ggplot(data) +
    aes(fill = .data[[fill_var]]) +
    geom_sf(color = "grey", linewidth = 0.05) +
    geom_sf(data = states_sf, fill = NA, color = "grey20", linewidth = 0.3) +
    scale_fill_manual(values = bins$colors, name = NULL, na.value = "grey85",
                      drop = FALSE, na.translate = FALSE,
                      guide = guide_legend(nrow = 2, byrow = TRUE)) +
    theme_void() + theme_trade_map
  if (facet_yr) {
    p <- p + facet_grid(year ~ ag_subsector)
  } else {
    p <- p + facet_wrap(~ag_subsector, nrow = nrow)
  }
  p
}

################################################################################
# ── Tariff ───────────────────────────────────────────────────────────────────
plot <- map_plot(subset(county_map_crop, year == 2019), "RET_tariff_bin", b_tariff, nrow = 3)
plot
ggsave(file.path(exp, "RET_tariff_crop.png"), plot, width = 10.5, height = 7, dpi = 300)

plot <- map_plot(subset(county_map_livestock, year == 2019), "RET_tariff_bin", b_tariff)
plot
ggsave(file.path(exp, "RET_tariff_livestock.png"), plot, width = 10, height = 6, dpi = 300)

# ── NTM ──────────────────────────────────────────────────────────────────────
plot <- map_plot(subset(county_map_crop, year == 2019), "RET_NTM_bin", b_ntm, nrow = 3)
plot
ggsave(file.path(exp, "RET_NTM_crop.png"), plot,width = 10.5, height = 7, dpi = 300)

plot <- map_plot(subset(county_map_livestock, year == 2019), "RET_NTM_bin", b_ntm)
plot
ggsave(file.path(exp, "RET_NTM_livestock.png"), plot, width = 10, height = 6, dpi = 300)

# ── NTM IV ───────────────────────────────────────────────────────────────────
plot <- map_plot(subset(county_map_crop, year == 2019), "RET_NTM_IV_bin", b_ntm_iv, nrow = 3)
plot
ggsave(file.path(exp, "RET_NTM_IV_crop.png"), plot, width = 10.5, height = 7, dpi = 300)

plot <- map_plot(subset(county_map_livestock, year == 2019), "RET_NTM_IV_bin", b_ntm_iv)
plot
ggsave(file.path(exp, "RET_NTM_IV_livestock.png"), plot, width = 10, height = 6, dpi = 300)
