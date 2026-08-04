################################################################################
# we create IMP at County level
################################################################################

library(readr)
library(tidyr)
library(dplyr)
library(sf)
library(maps)
library(ggplot2)
library(RColorBrewer)

rm(list = ls())
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git/data")
getwd()

exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/summary/exposure_maps/elast/county/"
dir.create(exp, recursive = TRUE, showWarnings = FALSE)

################################################################################
dta <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_exposure/NAICS6/county/IMP_r_naics6.csv")

names(dta)
summary(dta)
unique(dta$year)

################################################################################
# ── Step 1: county geometry ──────────────────────────────────────────────────
counties_sf <- st_as_sf(maps::map("county", plot = FALSE, fill = TRUE)) %>%
  st_transform(crs = 5070) %>%
  mutate(
    state_name  = sub(",.*", "", ID),
    county_name = sub(".*,", "", ID)
  )

data(county.fips)
counties_sf <- counties_sf %>%
  left_join(county.fips, by = c("ID" = "polyname")) %>%
  mutate(fips = as.numeric(fips))

# ── Step 2: state overlay ────────────────────────────────────────────────────
states_sf <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE)) %>%
  st_transform(crs = 5070)

# ── Step 3: sanity checks ────────────────────────────────────────────────────
missing_geom <- setdiff(unique(dta$fips), counties_sf$fips)
print(sort(missing_geom))

missing_data <- setdiff(counties_sf$fips, unique(dta$fips))
print(length(missing_data))

# ── Step 4: balance the panel — every fips × year, missing = 0 ───────────────
all_fips <- counties_sf$fips

dta_balanced <- dta %>%
  filter(year %in% 2018:2019) %>%
  complete(
    fips = all_fips,
    year,
    fill = list(
      IMP_tariff_2015_r = 0,
      IMP_tariff_2017_r = 0
    )
  )

# ── Step 5: merge onto geometry ──────────────────────────────────────────────
county_map <- counties_sf %>% left_join(dta_balanced, by = "fips")

table(county_map$year)
county_map <- county_map %>% filter(!is.na(year)) %>% filter(year %in% 2018:2019)
colSums(is.na(county_map))
summary(county_map)

################################################################################
# ── Theme (single definition, used for all plots) ────────────────────────────
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
d19 <- dta %>% filter(year == 2019)

b_imp2017 <- make_bins(d19$IMP_tariff_2017_r)
b_imp2015 <- make_bins(d19$IMP_tariff_2015_r)

county_map <- county_map %>%
  mutate(
    IMP_2017_bin = cut(IMP_tariff_2017_r, b_imp2017$breaks, b_imp2017$labels, include.lowest = TRUE),
    IMP_2015_bin = cut(IMP_tariff_2015_r, b_imp2015$breaks, b_imp2015$labels, include.lowest = TRUE)
  )

################################################################################
# ── Plot helper ──────────────────────────────────────────────────────────────
map_plot <- function(data, fill_var, bins, facet = FALSE) {
  p <- ggplot(data) +
    aes(fill = .data[[fill_var]]) +
    geom_sf(color = "grey", linewidth = 0.05) +
    geom_sf(data = states_sf, fill = NA, color = "grey20", linewidth = 0.3) +
    scale_fill_manual(values = bins$colors, name = NULL, na.value = "grey85",
                      drop = FALSE, na.translate = FALSE,
                      guide = guide_legend(nrow = 2, byrow = TRUE)) +
    theme_void() + theme_trade_map
  if (facet) p <- p + facet_wrap(~year)
  p
}

################################################################################
# ── IMP 2017 tariff ──────────────────────────────────────────────────────────
plot <- map_plot(subset(county_map, year == 2019), "IMP_2017_bin", b_imp2017)
plot
ggsave(file.path(exp, "IMP_2017_2019.png"), plot, width = 8, height = 5.2, dpi = 300)

plot <- map_plot(county_map, "IMP_2017_bin", b_imp2017, facet = TRUE)
plot
ggsave(file.path(exp, "IMP_2017.png"), plot, width = 8, height = 2.8, dpi = 300)

# ################################################################################
# # ── IMP 2015 tariff ──────────────────────────────────────────────────────────
# plot <- map_plot(subset(county_map, year == 2019), "IMP_2015_bin", b_imp2015)
# plot
# ggsave(file.path(exp, "IMP_2015_2019.png"), plot, width = 8, height = 5.2, dpi = 300)
# 
# plot <- map_plot(county_map, "IMP_2015_bin", b_imp2015, facet = TRUE)
# plot
# ggsave(file.path(exp, "IMP_2015.png"), plot, width = 8, height = 2.8, dpi = 300)