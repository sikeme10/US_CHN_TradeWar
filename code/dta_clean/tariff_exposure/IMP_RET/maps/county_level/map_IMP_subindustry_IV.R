################################################################################
# we create IMP at County level (subsectors)
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
dta <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6/county/IMP_r_subsectors_IV.csv")

summary(dta)
names(dta)
unique(dta$year)
unique(dta$subsector)

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
missing_geom <- setdiff(unique(dta$fips), counties_sf$fips)
print(missing_geom)

missing_data <- setdiff(counties_sf$fips, unique(dta$fips))
print(length(missing_data))

# ── Step 4: balance the panel — every fips × year × subsector, missing = 0 ───
all_fips <- counties_sf$fips

dta_balanced <- dta %>%
  filter(subsector %in% c("crop", "livestock", "nonag")) %>%
  filter(year %in% 2018:2019) %>%
  complete(
    fips = all_fips,
    year,
    subsector,
    fill = list(
      IMP_tariff_tot_ir  = 0,
      IMP_tariff_sect_ir = 0
    )
  )

# ── Step 5: merge onto geometry ──────────────────────────────────────────────
county_map <- counties_sf %>% left_join(dta_balanced, by = "fips")

table(county_map$year)
colSums(is.na(county_map))

county_map <- county_map %>%
  filter(!is.na(year)) %>%
  filter(year %in% 2018:2019) %>%
  mutate(subsector = recode(subsector,
                            "crop"      = "Crop",
                            "livestock" = "Livestock",
                            "nonag"     = "Non-ag"))

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
make_bins <- function(x, n = 6, digits = 3) {
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

# ── Build bins from the raw data (pre-balancing) ─────────────────────────────
d19 <- dta %>% filter(year == 2019, subsector %in% c("crop", "livestock", "nonag"))

b_tot  <- make_bins(d19$IMP_tariff_tot_ir)
b_sect <- make_bins(d19$IMP_tariff_sect_ir)

county_map <- county_map %>%
  mutate(
    IMP_tot_bin  = cut(IMP_tariff_tot_ir,  b_tot$breaks,  b_tot$labels,  include.lowest = TRUE),
    IMP_sect_bin = cut(IMP_tariff_sect_ir, b_sect$breaks, b_sect$labels, include.lowest = TRUE)
  )

################################################################################
# ── Plot helper ──────────────────────────────────────────────────────────────
map_plot <- function(data, fill_var, bins, facet_yr = FALSE) {
  p <- ggplot(data) +
    aes(fill = .data[[fill_var]]) +
    geom_sf(color = "grey", linewidth = 0.05) +
    geom_sf(data = states_sf, fill = NA, color = "grey20", linewidth = 0.3) +
    scale_fill_manual(values = bins$colors, name = NULL, na.value = "grey85",
                      drop = FALSE, na.translate = FALSE,
                      guide = guide_legend(nrow = 2, byrow = TRUE)) +
    theme_void() + theme_trade_map
  if (facet_yr) p <- p + facet_grid(year ~ subsector) else p <- p + facet_wrap(~subsector)
  p
}

################################################################################
# ── IMP tariff (total) ───────────────────────────────────────────────────────
plot <- map_plot(subset(county_map, year == 2019), "IMP_tot_bin", b_tot)
plot
ggsave(file.path(exp, "IMP_tariff_2019_subsector.png"), plot, width = 8, height = 3.2, dpi = 300)

plot <- map_plot(county_map, "IMP_tot_bin", b_tot, facet_yr = TRUE)
plot
ggsave(file.path(exp, "IMP_tariff_subsector_year.png"), plot, width = 8, height = 5.5, dpi = 300)

# ################################################################################
# # ── IMP tariff (sectoral) ────────────────────────────────────────────────────
# plot <- map_plot(subset(county_map, year == 2019), "IMP_sect_bin", b_sect)
# plot
# ggsave(file.path(exp, "IMP_tariff_sect_2019_subsector.png"), plot, width = 8, height = 3.2, dpi = 300)
# 
# plot <- map_plot(county_map, "IMP_sect_bin", b_sect, facet_yr = TRUE)
# plot
# ggsave(file.path(exp, "IMP_tariff_sect_subsector.png"), plot, width = 8, height = 5.5, dpi = 300)