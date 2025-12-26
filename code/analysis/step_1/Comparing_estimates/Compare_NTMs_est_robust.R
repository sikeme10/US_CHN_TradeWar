



################################################################################
#                    Gravity regression analysis: residual approach


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
library(countrycode)
library(Hmisc)
library(haven)
library(sfaR)
library(frontier)

################################################################################
# directory: 
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git")
exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/Compare_values/"

################################################################################
# 1) Load data 
################################################################################

# Load FE estimates 
dta <- read_csv( paste0(exp, "estimates_reduced_form1.csv"))
unique(dta$ExporterISO3)
colSums(is.na(dta))
unique(dta$year)

################################################################################

# Pick countries to plot robustness

################################################################################


library(dplyr)
library(ggplot2)

make_ag_lnAVE_plot <- function(Country, dta, save = FALSE, exp = NULL) {
  
  # 1. Filter country data and build date
  dta_country <- dta %>% 
    filter(ExporterISO3 == Country) %>%
    mutate(date = as.Date(paste(year, month, "01", sep = "-")))
  
  # 2. Agricultural subset
  ag <- dta_country %>% 
    filter(sector == "Ag")
  
  # 3. Sector weights (HS6 -> sector)
  ag_2017_hs6 <- ag %>%
    filter(year == 2017) %>%
    group_by(hs6_H5) %>%
    summarise(
      Trade_value_USD_2017 = sum(Trade_value_USD, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      tot_Trade_value_USD_2017 = sum(Trade_value_USD_2017),
      weight_sector = Trade_value_USD_2017 / tot_Trade_value_USD_2017
    )
  
  ag <- left_join(ag, ag_2017_hs6, by = "hs6_H5")
  
  # 4. HS-section weights (HS6 -> HS-section)
  ag_2017_hs_sect <- ag %>%
    filter(year == 2017) %>%
    group_by(hs_section, hs6_H5) %>%
    summarise(
      Trade_value_USD_2017 = sum(Trade_value_USD, na.rm = TRUE),
      .groups = "drop_last"
    ) %>%
    group_by(hs_section) %>%
    mutate(
      hs_sect_tot_Trade_value_USD_2017 = sum(Trade_value_USD_2017),
      weight_hs_sect = if_else(
        hs_sect_tot_Trade_value_USD_2017 > 0,
        Trade_value_USD_2017 / hs_sect_tot_Trade_value_USD_2017,
        NA_real_
      )
    ) %>%
    ungroup()
  
  ag <- left_join(ag, ag_2017_hs_sect, by = c("hs_section", "hs6_H5"))
  
  # 5. Weighted and simple averages over time
  ag_w <- ag %>%
    filter(year %in% c(2018:2019)) %>%
    group_by(date) %>%
    summarise(
      w_FE        = weighted.mean(diff_ln_AVE_FE,        w = weight_sector, na.rm = TRUE),
      w_sf        = weighted.mean(diff_ln_AVE_u,         w = weight_sector, na.rm = TRUE),
      w_sf_tariff = weighted.mean(diff_ln_AVE_u_tariff,  w = weight_sector, na.rm = TRUE),
      w_tariff    = weighted.mean(diff_log_tariff_2017,  w = weight_sector, na.rm = TRUE),
      s_FE        = mean(diff_ln_AVE_FE,        na.rm = TRUE),
      s_sf        = mean(diff_ln_AVE_u,         na.rm = TRUE),
      s_sf_tariff = mean(diff_ln_AVE_u_tariff,  na.rm = TRUE),
      s_tariff    = mean(diff_log_tariff_2017,  na.rm = TRUE)
    ) %>%
    ungroup()
  
  # 6. Plot
  plot <- ggplot(ag_w) +
    geom_line(aes(x = date, y = w_FE,        color = "FE")) +
    geom_line(aes(x = date, y = w_sf,        color = "sf")) +
    geom_line(aes(x = date, y = w_sf_tariff, color = "sf_tariff")) +
    geom_line(aes(x = date, y = w_tariff,    color = "tariff")) +
    scale_color_manual(
      values = c(  "FE"        = "steelblue",  "sf"        = "darkorange",
        "sf_tariff" = "firebrick",        "tariff"    = "darkgreen"      ),
      labels = c(   "FE"        = "FE",     "sf"        = "SF",
        "sf_tariff" = "Tariff-adjusted SF",      "tariff"    = "Tariff"      ),
      name = "Variables"    ) +
    labs(
      title = paste0(Country, ": Weighted average Δ ln(1+AVE) for agricultural sector (relative to 2017)"),
      x = "Date",
      y = "Weighted Δ ln(1+AVE)"
    ) +
    theme_minimal(base_size = 14) +
    coord_cartesian(ylim = c(-2, 2)) +   # ← Y-axis from -2 to 2
        theme(
      plot.title = element_text(size = 12, hjust = 0.5),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      axis.text.x = element_text(size = 9),
      axis.text.y = element_text(size = 9),
      axis.title.x = element_text(size = 11),
      axis.title.y = element_text(size = 11),
      legend.text  = element_text(size = 10),
      legend.title = element_text(size = 10)
    )
  
  # Show plot in the console
  print(plot)
  
  # 7. Optional saving
  if (save) {
    if (is.null(exp)) {
      warning("save = TRUE but 'exp' is NULL; not saving.")
    } else {
      out_path <- file.path(exp, "plot/Robust/", paste0(Country, "_Compare_ln_AVE_Ag_weight.png"))
      ggsave(filename = out_path, plot = plot, width = 8, height = 5, dpi = 300)
      message("Saved plot to: ", out_path)
    }
  }
  
  invisible(plot)
}

unique(dta$ExporterISO3)
make_ag_lnAVE_plot("BRA", dta)
make_ag_lnAVE_plot("ARG", dta)
make_ag_lnAVE_plot("DEU", dta)
make_ag_lnAVE_plot("RUS", dta)
make_ag_lnAVE_plot("AUS", dta)
make_ag_lnAVE_plot("CAN", dta)
make_ag_lnAVE_plot("UKR", dta)

make_ag_lnAVE_plot("BRA", dta, save = TRUE, exp = exp)
make_ag_lnAVE_plot("ARG", dta, save = TRUE, exp = exp)
make_ag_lnAVE_plot("DEU", dta, save = TRUE, exp = exp)
make_ag_lnAVE_plot("RUS", dta, save = TRUE, exp = exp)
make_ag_lnAVE_plot("AUS", dta, save = TRUE, exp = exp)
make_ag_lnAVE_plot("CAN", dta, save = TRUE, exp = exp)
make_ag_lnAVE_plot("UKR", dta, save = TRUE, exp = exp)

#############################################################
# HS section level 
#############################################################
names(US_ag)
US_ag_w_sect <- US_ag %>% filter(sector == "Ag", year > 2017) %>%
  group_by(date, hs_section) %>% summarise( 
    w_FE        = weighted.mean(diff_ln_AVE_FE,        w = weight_hs_sect,       na.rm = TRUE),
    w_sf        = weighted.mean(diff_ln_AVE_u,         w = weight_hs_sect,       na.rm = TRUE),
    w_sf_tariff = weighted.mean(diff_ln_AVE_u_tariff,  w = weight_hs_sect,       na.rm = TRUE),
    w_chen      = weighted.mean(diff_ln_AVE_chen,      w = weight_hs_sect_Chen,  na.rm = TRUE),
    w_tariff    = weighted.mean(diff_log_tariff_2017,  w = weight_hs_sect,       na.rm = TRUE),
    s_FE        = mean(diff_ln_AVE_FE,         na.rm = TRUE),
    s_sf        = mean(diff_ln_AVE_u,          na.rm = TRUE),
    s_sf_tariff = mean(diff_ln_AVE_u_tariff,   na.rm = TRUE),
    s_chen      = mean(diff_ln_AVE_chen,       na.rm = TRUE),
    s_tariff    = mean(diff_log_tariff_2017,   na.rm = TRUE),
    .groups = "drop"  ) %>%  group_by(hs_section) %>%
  mutate(  w_chen = mean(w_chen, na.rm = TRUE),   s_chen = mean(s_chen, na.rm = TRUE)  )



plot <- ggplot(US_ag_w_sect) +
  geom_line(aes(x = date, y = w_FE,        color = "FE")) +
  geom_line(aes(x = date, y = w_sf,        color = "sf")) +
  geom_line(aes(x = date, y = w_sf_tariff, color = "sf_tariff")) +
  geom_line(aes(x = date, y = w_chen,      color = "Chen_et_al")) +
  geom_line(aes(x = date, y = w_tariff,    color = "tariff")) +
  scale_color_manual(  values = c(  "FE"         = "steelblue",
                                    "sf"         = "darkorange","sf_tariff"  = "firebrick",
                                    "Chen_et_al" = "purple",      "tariff"     = "darkgreen"    ),
                       labels = c(  "FE"         = "FE",
                                    "sf"         = "SF",
                                    "sf_tariff"  = "Tariff-adjusted SF",
                                    "Chen_et_al" = "Chen et al.",   "tariff"     = "Tariff"    ),    name = "Variables"  ) +
  facet_wrap(~hs_section)+
  labs(  title = "Weighted average Δ ln(1+AVE) for agricultural sector by HS section(relative to 2017) ",
         x = "Date",
         y = "Weighted Δ ln(1+AVE)"  ) +
  theme_minimal(base_size = 14)+
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA) ,
    axis.text.x = element_text(size = 9), # Axis text (tick labels)
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 11), # Axis titles
    axis.title.y = element_text(size = 11),
    legend.text  = element_text(size = 10), # Legend text and title
    legend.title = element_text(size = 10)  )
plot
ggsave(filename = file.path(exp, "plot/", "Compare_ln_AVE_Ag_hs_sect_weight.png"),plot = plot, width = 8, height = 5, dpi = 300)


plot <- ggplot(US_ag_w_sect) +
  geom_line(aes(x = date, y = s_FE,        color = "FE")) +
  geom_line(aes(x = date, y = s_sf,        color = "sf")) +
  geom_line(aes(x = date, y = s_sf_tariff, color = "sf_tariff")) +
  geom_line(aes(x = date, y = s_chen,      color = "Chen_et_al")) +
  geom_line(aes(x = date, y = s_tariff,    color = "tariff")) +
  scale_color_manual(  values = c(  "FE"         = "steelblue",
                                    "sf"         = "darkorange","sf_tariff"  = "firebrick",
                                    "Chen_et_al" = "purple",      "tariff"     = "darkgreen"    ),
                       labels = c(  "FE"         = "FE",
                                    "sf"         = "SF",
                                    "sf_tariff"  = "Tariff-adjusted SF",
                                    "Chen_et_al" = "Chen et al.",   "tariff"     = "Tariff"    ),    name = "Variables"  ) +
  facet_wrap(~hs_section)+
  labs(  title = "Simple average Δ ln(1+AVE) for agricultural sector by HS section (relative to 2017) ",
         x = "Date",
         y = "Simple average Δ ln(1+AVE)"  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA) ,
    axis.text.x = element_text(size = 9), # Axis text (tick labels)
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 11), # Axis titles
    axis.title.y = element_text(size = 11),
    legend.text  = element_text(size = 10), # Legend text and title
    legend.title = element_text(size = 10)  )
plot
ggsave(filename = file.path(exp, "plot/", "Compare_ln_AVE_Ag_hs_sect_simple.png"),plot = plot, width = 8, height = 5, dpi = 300)
################################################################################

# if put all countries together ;
make_ag_lnAVE_facet <- function(dta, countries = NULL, save = FALSE, exp = NULL) {
  
  # If no country list is given, use all ExporterISO3 in the data
  if (is.null(countries)) {
    countries <- unique(dta$ExporterISO3)
  }
  
  # 1. Filter data to chosen countries + Ag sector, build date
  ag <- dta %>% 
    filter(
      ExporterISO3 %in% countries,
      sector == "Ag"
    ) %>%
    mutate(
      date = as.Date(paste(year, month, "01", sep = "-"))
    )
  
  # 2. HS6 weights per country (baseline 2017)
  ag_2017_hs6 <- ag %>%
    filter(year == 2017) %>%
    group_by(ExporterISO3, hs6_H5) %>%
    summarise(
      Trade_value_USD_2017 = sum(Trade_value_USD, na.rm = TRUE),
      .groups = "drop_last"
    ) %>%
    group_by(ExporterISO3) %>%
    mutate(
      tot_Trade_value_USD_2017 = sum(Trade_value_USD_2017),
      weight_sector = if_else(
        tot_Trade_value_USD_2017 > 0,
        Trade_value_USD_2017 / tot_Trade_value_USD_2017,
        NA_real_
      )
    ) %>%
    ungroup()
  
  # Join weights back
  ag <- ag %>%
    left_join(ag_2017_hs6, by = c("ExporterISO3", "hs6_H5"))
  
  # (Your HS-section weights could also be made per country if you still need them,
  # but they are not used in the plot, so I’m skipping them here.)
  
  # 3. Weighted & simple averages over time PER COUNTRY
  ag_w <- ag %>%
    filter(year %in% 2018:2019) %>%
    group_by(ExporterISO3, date) %>%
    summarise(
      w_FE        = weighted.mean(diff_ln_AVE_FE,        w = weight_sector, na.rm = TRUE),
      w_sf        = weighted.mean(diff_ln_AVE_u,         w = weight_sector, na.rm = TRUE),
      w_sf_tariff = weighted.mean(diff_ln_AVE_u_tariff,  w = weight_sector, na.rm = TRUE),
      w_tariff    = weighted.mean(diff_log_tariff_2017,  w = weight_sector, na.rm = TRUE),
      s_FE        = mean(diff_ln_AVE_FE,        na.rm = TRUE),
      s_sf        = mean(diff_ln_AVE_u,         na.rm = TRUE),
      s_sf_tariff = mean(diff_ln_AVE_u_tariff,  na.rm = TRUE),
      s_tariff    = mean(diff_log_tariff_2017,  na.rm = TRUE),
      .groups = "drop"
    )
  
  # 4. Plot with facet_wrap by country
  plot <- ggplot(ag_w) +
    geom_line(aes(x = date, y = w_FE,        color = "FE")) +
    geom_line(aes(x = date, y = w_sf,        color = "sf")) +
    geom_line(aes(x = date, y = w_sf_tariff, color = "sf_tariff")) +
    geom_line(aes(x = date, y = w_tariff,    color = "tariff")) +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +   # ← Only show the year
    scale_color_manual(
      values = c(
        "FE"        = "steelblue",
        "sf"        = "darkorange",
        "sf_tariff" = "firebrick",
        "tariff"    = "darkgreen"
      ),
      labels = c(
        "FE"        = "FE",
        "sf"        = "SF",
        "sf_tariff" = "Tariff-adjusted SF",
        "tariff"    = "Tariff"
      ),
      name = "Variables"
    ) +
    labs(
      title = "Weighted average Δ ln(1+AVE) for agricultural sector (relative to 2017)",
      x = "Date",
      y = "Weighted Δ ln(1+AVE)"
    ) +
    coord_cartesian(ylim = c(-1.5, 1.5)) +
    facet_wrap(~ ExporterISO3, scales = "fixed") +
    theme_minimal(base_size = 14) +
    theme(
      panel.spacing.x = unit(1.2, "lines"),   # ← MORE HORIZONTAL SPACE
      plot.title = element_text(size = 12, hjust = 0.5),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      axis.text.x = element_text(size = 9),
      axis.text.y = element_text(size = 9),
      axis.title.x = element_text(size = 11),
      axis.title.y = element_text(size = 11),
      legend.text  = element_text(size = 10),
      legend.title = element_text(size = 10)
    )
  
  print(plot)
  
  # 5. Optional saving
  if (save) {
    if (is.null(exp)) {
      warning("save = TRUE but 'exp' is NULL; not saving.")
    } else {
      out_path <- file.path(exp, "plot/Robust/", "AllCountries_Compare_ln_AVE_Ag_weight.png")
      ggsave(filename = out_path, plot = plot, width = 10, height = 7, dpi = 300)
      message("Saved plot to: ", out_path)
    }
  }
  
  invisible(plot)
}
countries_vec <- c("BRA", "MEX", "AUS", "CAN", "UKR", "RUS")

make_ag_lnAVE_facet(dta, countries = countries_vec)

# To save:
make_ag_lnAVE_facet(dta, countries = countries_vec, save = TRUE, exp = exp)




################################################################################

# correlations of change in ln(1+AVE)

################################################################################
names(US_ag)

# If we look at correlation between FE and efficiency 
names(US)
US_changes_2017 <- US %>% filter(year %in% c(2018:2020))




plot <- ggplot(US, aes(x = diff_ln_AVE_FE, y = diff_ln_AVE_u_tariff)) +
  geom_point(alpha = 0.15, size = 1) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linewidth = 1) +
  geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 1,linetype = "dotted", ) +
  theme_minimal() +
  labs(
    title = "Plot of FE and Tariff-adjusted Inefficiency (u) in Agricultural Sector",
    x = "FE",
    y = "Tariff-adjusted u"  ) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title = element_text(size = 12),
    axis.text  = element_text(size = 12)
  )
plot


plot <- ggplot(US_ag, aes(x = diff_ln_AVE_u, y = diff_ln_AVE_u_tariff)) +
  geom_point(alpha = 0.20, size = 1) +
  geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 1,linetype = "dotted", ) +
  theme_minimal() +
  labs(
    title = "Plot of US Δ ln(1+AVE) obtained from SF and Tariff-adjusted SF",
    x = "SF",
    y = "Tariff-adjusted SF"  ) +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA) ,
    axis.text.x = element_text(size = 10), # Axis text (tick labels)
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12), # Axis titles
    axis.title.y = element_text(size = 12) )
plot
ggsave(filename = file.path(exp, "plot/", "Plot_stochastic_corr.png"),plot = plot, width = 9, height = 5, dpi = 300)

################################################################################

# Get correlation plot

US_ag1 <- US_ag %>% filter(year %in% c(2018,2019))

vars <- US_ag1[, c("diff_ln_AVE_chen", "diff_ln_AVE_FE", "diff_ln_AVE_u", "diff_ln_AVE_u_tariff")]

vars <- vars %>%  rename(  Chen_et_al = diff_ln_AVE_chen,  FE = diff_ln_AVE_FE,
    SF = diff_ln_AVE_u,  SF_tariff  = diff_ln_AVE_u_tariff )

cor_mat <- cor(vars, use = "complete.obs")

# open PNG device
png(paste0(exp, "plot/corrplot.png"), width = 1200, height = 1000, res = 150)

corrplot(cor_mat, method = "circle", addCoef.col = "black")

dev.off()
 