


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

exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/summary/exposure_maps/elast/county/"


################################################################################ 

dta <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6/county/RET_elast_r_naics6_IV.csv")

names(dta)
summary(dta)
unique(dta$year)

################################################################################
# ── Step 1: county geometry ──────────────────────────────────────────────────
counties_sf <- counties(year = 2013, cb = TRUE) %>%
  mutate(fips = as.numeric(paste0(STATEFP, COUNTYFP))) %>%
  filter(!STATEFP %in% c("02", "15")) %>%              # drop AK, HI
  filter(as.numeric(STATEFP) <= 56) %>%                # drop territories
  st_transform(crs = 5070)


# ── Step 2: state overlay for cleaner visual separation ──────────────────────
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
  filter(year %in% 2018:2019) %>%
  complete(
    fips = all_fips,
    year,
    fill = list(
      RET_tariff_r = 0,
      RET_NTB_r    = 0,
      RET_NTB_IV_r = 0
    )
  )

# ── Step 5: merge onto geometry ──────────────────────────────────────────────
county_map <- counties_sf %>% left_join(dta_balanced, by = "fips")

table(county_map$year)
colSums(is.na(county_map))

table(county_map$year)
county_map <- county_map %>% filter(!is.na(year))
colSums(is.na(county_map))
summary(county_map)

################################################################################
# ── Themes ───────────────────────────────────────────────────────────────────

theme_trade_map <- theme_minimal(base_size = 14, base_family = "Times New Roman") +
  theme(
    panel.spacing     = unit(1.2, "lines"),
    plot.title        = element_text(hjust = 0.5, size = 12),
    panel.background  = element_rect(fill = "white", color = NA),
    plot.background   = element_rect(fill = "white", color = NA),
    panel.border      = element_rect(color = "black", fill = NA, linewidth = 0.6),
    panel.grid        = element_blank(),
    strip.background  = element_rect(fill = "grey92", color = "black", linewidth = 0.6),
    strip.text        = element_text(size = 12, face = "bold", color = "black"),
    axis.text         = element_blank(),
    axis.ticks        = element_blank(),
    axis.title        = element_blank(),
    legend.position   = "top",
    legend.text       = element_text(size = 11),
    legend.title      = element_blank(),
    legend.key.height = unit(0.4, "cm"),
    legend.key.width  = unit(1.0, "cm"),
    plot.caption      = element_text(size = 8, color = "grey40", hjust = 1)
  )

# Single theme, no duplicate definition
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
library(RColorBrewer)

# Computes 6 quantile bins from the raw data (pre-balancing, so zero-fills
# don't distort breaks), truncating the top bin at the 6th quantile.
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

# ── Build bins for each variable from the raw data ───────────────────────────
d19 <- dta %>% filter(year == 2019)

b_tariff <- make_bins(d19$RET_tariff_r)
b_ntm    <- make_bins(d19$RET_NTB_r)

county_map <- county_map %>%
  mutate(
    RET_tariff_bin = cut(RET_tariff_r, b_tariff$breaks, b_tariff$labels, include.lowest = TRUE),
    RET_NTM_bin    = cut(RET_NTB_r,    b_ntm$breaks,    b_ntm$labels,    include.lowest = TRUE)
  )

################################################################################
# ── Plot helper ──────────────────────────────────────────────────────────────
map_plot <- function(data, fill_var, bins, title, facet = FALSE, thm = theme_trade_map) {
  p <- ggplot(data) +
    aes(fill = .data[[fill_var]]) +
    geom_sf(color = "grey", linewidth = 0.05) +
    geom_sf(data = states_sf, fill = NA, color = "grey20", linewidth = 0.3) +
    scale_fill_manual(values = bins$colors, name = NULL, na.value = "grey85",
                      drop = FALSE, guide = guide_legend(nrow = 2, byrow = TRUE)) +
    # labs(title = title) +
    theme_void() + thm
  if (facet) p <- p + facet_wrap(~year)
  p
}

# ── Tariff ───────────────────────────────────────────────────────────────────
plot <- map_plot(subset(county_map, year == 2019), "RET_tariff_bin", b_tariff,
                 "(b) Retaliatory tariff exposure")
plot
ggsave(file.path(exp, "RET_tariff_2019.png"), plot, width = 8, height = 5.2, dpi = 300)

plot <- map_plot(county_map, "RET_tariff_bin", b_tariff,
                 "Retaliatory tariff exposure", facet = TRUE, thm = theme_trade_map)
plot
ggsave(file.path(exp, "RET_tariff.png"), plot, width = 8, height = 3, dpi = 300)

# ── NTM ──────────────────────────────────────────────────────────────────────
plot <- map_plot(subset(county_map, year == 2019), "RET_NTM_bin", b_ntm,
                 "(b) Retaliatory NTM exposure")
plot
ggsave(file.path(exp, "RET_NTM_2019.png"), plot, width = 8, height = 5.2, dpi = 300)

plot <- map_plot(county_map, "RET_NTM_bin", b_ntm,
                 "Retaliatory NTM exposure", facet = TRUE, thm = theme_trade_map)
plot
ggsave(file.path(exp, "RET_NTM.png"), plot, width = 6, height = 3, dpi = 300)



