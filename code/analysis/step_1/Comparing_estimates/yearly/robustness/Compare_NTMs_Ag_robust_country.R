



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
exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/Compare_values/yearly/robust"

################################################################################
# 1) Load data 
################################################################################

# Load FE estimates 
dta <- read_csv( paste0(exp, "/estimates_reduced_form_base_2015.csv"))
unique(dta$ExporterISO3)
colSums(is.na(dta))
unique(dta$year)
names(dta)
################################################################################


# create a theme for ggplot 
theme_trade <- theme_minimal(base_size = 14) +
  theme(panel.spacing.x = unit(1.2, "lines"),
    plot.title = element_text(size = 11, hjust = 0.5),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    legend.text  = element_text(size = 10),
    legend.title = element_text(size = 10)  )


################################################################################
# 1) Pick countries to plot robustness
################################################################################


library(dplyr)
library(ggplot2)

make_ag_lnAVE_plot <- function(Country, dta, save = FALSE, exp = NULL) {
  
  # 1. Filter country data and build date
  dta_country <- dta %>% filter(ExporterISO3 == Country)
  
  # 2. Agricultural subset
  ag <- dta_country %>% 
    filter(sector == "Ag")
  
  # 3. Sector weights (HS6 -> sector)
  ag_2015_hs6 <- ag %>%    filter(year == 2015) %>%
    group_by(hs6_H5) %>%
    summarise(Trade_value_USD_2015 = sum(Trade_value_USD, na.rm = TRUE),
      .groups = "drop"    ) %>%
    mutate( tot_Trade_value_USD_2015 = sum(Trade_value_USD_2015),
                 weight_sector = Trade_value_USD_2015 / tot_Trade_value_USD_2015    )
  
  ag <- left_join(ag, ag_2015_hs6, by = "hs6_H5")
  
  # 4. HS-section weights (HS6 -> HS-section)
  ag_2015_hs_sect <- ag %>%    filter(year == 2015) %>%
    group_by(hs_section, hs6_H5) %>%
    summarise(Trade_value_USD_2015 = sum(Trade_value_USD, na.rm = TRUE), .groups = "drop_last"    ) %>%
    group_by(hs_section) %>%
    mutate( hs_sect_tot_Trade_value_USD_2015 = sum(Trade_value_USD_2015),
            weight_hs_sect = if_else(hs_sect_tot_Trade_value_USD_2015 > 0,
        Trade_value_USD_2015 / hs_sect_tot_Trade_value_USD_2015,  NA_real_  ) ) %>% ungroup()
  
  ag <- left_join(ag, ag_2015_hs_sect, by = c("hs_section", "hs6_H5"))
  
  # 5. Weighted and simple averages over time
  ag_w <- ag %>%  filter(year %in% c(2016:2019)) %>%
    group_by(year) %>%
    summarise(
      w_FE        = weighted.mean(diff_ln_AVE_FE,        w = weight_sector, na.rm = TRUE),
      w_FE_bench  = weighted.mean(diff_ln_AVE_FE_bench,  w = weight_sector, na.rm = TRUE),
      w_sf        = weighted.mean(diff_ln_AVE_u,         w = weight_sector, na.rm = TRUE),# sf
      w_sf_bench  = weighted.mean(diff_ln_AVE_u_bench,   w = weight_sector, na.rm = TRUE),
      w_sf_tariff = weighted.mean(diff_ln_AVE_u_tariff,  w = weight_sector, na.rm = TRUE),
      w_sf_tariff_bench = weighted.mean(diff_ln_AVE_u_tariff_bench,  w = weight_sector, na.rm = TRUE), 

      w_tariff    = weighted.mean(diff_log_tariff_2015,  w = weight_sector, na.rm = TRUE),

      s_FE        = mean(diff_ln_AVE_FE,         na.rm = TRUE),
      s_FE_bench  = mean(diff_ln_AVE_FE_bench,   na.rm = TRUE),
      s_sf        = mean(diff_ln_AVE_u,          na.rm = TRUE),
      s_sf_bench  = mean(diff_ln_AVE_u_bench,    na.rm = TRUE),
      s_sf_tariff = mean(diff_ln_AVE_u_tariff,   na.rm = TRUE),
      s_sf_tariff_bench = mean(diff_ln_AVE_u_tariff_bench, na.rm = TRUE), 
      s_tariff    = mean(diff_log_tariff_2015,  na.rm = TRUE)    ) %>%    ungroup()
  
  # 6. Plot
  plot <- ggplot(ag_w) +
    geom_line(aes(x = year, y = w_FE,              color = "FE")) +
    geom_line(aes(x = year, y = w_sf,              color = "sf")) +
    geom_line(aes(x = year, y = w_sf_tariff,       color = "sf_tariff")) +
    # benchmark lines
    geom_line(aes(x = year, y = w_FE_bench,        color = "FE_bench"),linetype = "dashed") +
    geom_line(aes(x = year, y = w_sf_bench,        color = "sf_bench"),linetype = "dashed") +
    geom_line(aes(x = year, y = w_sf_tariff_bench, color = "sf_tariff_bench"), linetype = "dashed") +
    scale_color_manual(
      values = c("FE"  = "blue", "sf" = "darkorange", "sf_tariff" = "red",
                  "FE_bench" = "blue", "sf_bench" = "darkorange",  "sf_tariff_bench" = "red" ),
      labels = c(  "FE" = "FE",    "sf" = "SF",    "sf_tariff" = "Tariff-adjusted SF",
                   "FE_bench" = "FE (benchmark)",  "sf_bench" = "SF (benchmark)",
                   "sf_tariff_bench" = "Tariff-adjusted SF (benchmark)" ),    name = "Variables"  ) +
    labs( title = paste0(Country, ": Weighted average Δ ln(1+AVE) for agricultural sector (relative to 2015)"),
      x = "year",      y = "Weighted Δ ln(1+AVE)"    ) +
    theme_trade
  
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




################################################################################
# 2) if put all countries together in facet wrap
################################################################################

make_ag_lnAVE_facet <- function(dta, countries = NULL, save = FALSE, exp = NULL) {
  
  # If no country list is given, use all ExporterISO3 in the data
  if (is.null(countries)) {
    countries <- unique(dta$ExporterISO3)
  }
  
  # 1. Filter data to chosen countries + Ag sector, build year
  ag <- dta %>%   filter(ExporterISO3 %in% countries,   sector == "Ag"    )
  
  # 2. HS6 weights per country (baseline 2015)
  ag_2015_hs6 <- ag %>%    filter(year == 2015) %>%
    group_by(ExporterISO3, hs6_H5) %>%
    summarise(Trade_value_USD_2015 = sum(Trade_value_USD, na.rm = TRUE),
      .groups = "drop_last"    ) %>%
    group_by(ExporterISO3) %>%
    mutate( tot_Trade_value_USD_2015 = sum(Trade_value_USD_2015),
      weight_sector = if_else( tot_Trade_value_USD_2015 > 0,
        Trade_value_USD_2015 / tot_Trade_value_USD_2015,  NA_real_  )) %>%  ungroup()
  
  # Join weights back
  ag <- ag %>%  left_join(ag_2015_hs6, by = c("ExporterISO3", "hs6_H5"))
  
  # (Your HS-section weights could also be made per country if you still need them,
  # but they are not used in the plot, so I’m skipping them here.)
  
  # 3. Weighted & simple averages over time PER COUNTRY
  ag_w <- ag %>% filter(year %in% 2016:2020) %>%
    group_by(ExporterISO3, year) %>%
    summarise(
      w_FE        = weighted.mean(diff_ln_AVE_FE,        w = weight_sector, na.rm = TRUE),
      w_FE_bench  = weighted.mean(diff_ln_AVE_FE_bench,  w = weight_sector, na.rm = TRUE),
      w_sf        = weighted.mean(diff_ln_AVE_u,         w = weight_sector, na.rm = TRUE),# sf
      w_sf_bench  = weighted.mean(diff_ln_AVE_u_bench,   w = weight_sector, na.rm = TRUE),
      w_sf_tariff = weighted.mean(diff_ln_AVE_u_tariff,  w = weight_sector, na.rm = TRUE),
      w_sf_tariff_bench = weighted.mean(diff_ln_AVE_u_tariff_bench,  w = weight_sector, na.rm = TRUE), 
      
      w_tariff    = weighted.mean(diff_log_tariff_2015,  w = weight_sector, na.rm = TRUE),
      
      s_FE        = mean(diff_ln_AVE_FE,         na.rm = TRUE),
      s_FE_bench  = mean(diff_ln_AVE_FE_bench,   na.rm = TRUE),
      s_sf        = mean(diff_ln_AVE_u,          na.rm = TRUE),
      s_sf_bench  = mean(diff_ln_AVE_u_bench,    na.rm = TRUE),
      s_sf_tariff = mean(diff_ln_AVE_u_tariff,   na.rm = TRUE),
      s_sf_tariff_bench = mean(diff_ln_AVE_u_tariff_bench, na.rm = TRUE), 
      s_tariff    = mean(diff_log_tariff_2015,  na.rm = TRUE) ,  .groups = "drop"  )
  
  # 4. Plot with facet_wrap by country
  plot <- ggplot(ag_w) +
    geom_line(aes(x = year, y = w_FE,              color = "FE")) +
    geom_line(aes(x = year, y = w_sf,              color = "sf")) +
    geom_line(aes(x = year, y = w_sf_tariff,       color = "sf_tariff")) +
    # benchmark lines
    geom_line(aes(x = year, y = w_FE_bench,        color = "FE_bench"),linetype = "dashed") +
    geom_line(aes(x = year, y = w_sf_bench,        color = "sf_bench"),linetype = "dashed") +
    geom_line(aes(x = year, y = w_sf_tariff_bench, color = "sf_tariff_bench"), linetype = "dashed") +
    scale_color_manual(
      values = c("FE"  = "blue", "sf" = "darkorange", "sf_tariff" = "red",
                 "FE_bench" = "blue", "sf_bench" = "darkorange",  "sf_tariff_bench" = "red" ),
      labels = c(  "FE" = "FE",    "sf" = "SF",    "sf_tariff" = "Tariff-adjusted SF",
                   "FE_bench" = "FE (benchmark)",  "sf_bench" = "SF (benchmark)",
                   "sf_tariff_bench" = "Tariff-adjusted SF (benchmark)" ),    name = "Variables"  ) +
    labs( title = "Weighted average Δ ln(1+AVE) for agricultural sector (relative to 2015)",
      x = "Date",  y = "Weighted Δ ln(1+AVE)"  ) +
    coord_cartesian(ylim = c(-5, 5)) +
    facet_wrap(~ ExporterISO3, scales = "fixed") +
    theme_trade
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
# 3) if compare other countries to US 
################################################################################

# 1. Filter data to chosen countries + Ag sector, build year
ag <- dta %>%   filter( sector == "Ag"    )

# 2. HS6 weights per country (baseline 2015)
ag_2015_hs6 <- ag %>%    filter(year == 2015) %>%
  group_by(ExporterISO3, hs6_H5) %>%
  summarise(Trade_value_USD_2015 = sum(Trade_value_USD, na.rm = TRUE),
            .groups = "drop_last"    ) %>%
  group_by(ExporterISO3) %>%
  mutate( tot_Trade_value_USD_2015 = sum(Trade_value_USD_2015),
          weight_sector = if_else( tot_Trade_value_USD_2015 > 0,
                                   Trade_value_USD_2015 / tot_Trade_value_USD_2015,  NA_real_  )) %>%  ungroup()
ag <- ag %>%  left_join(ag_2015_hs6, by = c("ExporterISO3", "hs6_H5"))


# 3. Weighted & simple averages over time PER COUNTRY
ag_w <- ag %>% filter(year %in% 2016:2020) %>%
  group_by(ExporterISO3, year) %>%
  summarise(
    w_FE        = weighted.mean(diff_ln_AVE_FE,        w = weight_sector, na.rm = TRUE),
    w_FE_bench  = weighted.mean(diff_ln_AVE_FE_bench,  w = weight_sector, na.rm = TRUE),
    w_sf        = weighted.mean(diff_ln_AVE_u,         w = weight_sector, na.rm = TRUE),# sf
    w_sf_bench  = weighted.mean(diff_ln_AVE_u_bench,   w = weight_sector, na.rm = TRUE),
    w_sf_tariff = weighted.mean(diff_ln_AVE_u_tariff,  w = weight_sector, na.rm = TRUE),
    w_sf_tariff_bench = weighted.mean(diff_ln_AVE_u_tariff_bench,  w = weight_sector, na.rm = TRUE), 
    
    w_tariff    = weighted.mean(diff_log_tariff_2015,  w = weight_sector, na.rm = TRUE),
    
    s_FE        = mean(diff_ln_AVE_FE,         na.rm = TRUE),
    s_FE_bench  = mean(diff_ln_AVE_FE_bench,   na.rm = TRUE),
    s_sf        = mean(diff_ln_AVE_u,          na.rm = TRUE),
    s_sf_bench  = mean(diff_ln_AVE_u_bench,    na.rm = TRUE),
    s_sf_tariff = mean(diff_ln_AVE_u_tariff,   na.rm = TRUE),
    s_sf_tariff_bench = mean(diff_ln_AVE_u_tariff_bench, na.rm = TRUE), 
    s_tariff    = mean(diff_log_tariff_2015,  na.rm = TRUE) ,  .groups = "drop"  )

ag_w <- ag_w %>%  mutate(is_US = ExporterISO3 == "USA")

# pick countries
countries_vec <- unique(ag_w$ExporterISO3)
#countries_vec <- c("USA", "BRA", "MEX", "AUS", "CAN", "UKR", "RUS")

plot_FE <- ag_w %>%
  filter(ExporterISO3 %in% countries_vec) %>%
  ggplot(aes(x = year, y = w_FE, group = ExporterISO3, color = is_US)) +
  geom_line(linewidth = 0.5) +
  scale_color_manual( values = c(`TRUE` = "red", `FALSE` = "grey70"),
    breaks = c(TRUE, FALSE),  labels = c("US", "Other countries"),
    name = NULL  ) +
  theme_trade +
  labs(title = "Weighted average Δ ln(1+AVE) (FE model) in the Agricultural sector",
    x = "Year",  y = "Weighted Δ ln(1+AVE)" )

plot_FE
ggsave(filename = file.path(exp, "/plot/", "change_ln_AVE_US_others_Ag_FE.png"),
       plot = plot_FE, width = 10, height = 7, dpi = 300)


# for FE with benchmark 
plot_FE_bench<- ag_w %>%
  filter(ExporterISO3 %in% countries_vec) %>%
  ggplot(aes(x = year, y = w_FE_bench, group = ExporterISO3, color = is_US)) +
  geom_line(linewidth = 0.5) +
  scale_color_manual( values = c(`TRUE` = "red", `FALSE` = "grey70"),
                      breaks = c(TRUE, FALSE),  labels = c("US", "Other countries"),
                      name = NULL  ) +
  theme_trade +
  labs(title = "Weighted average Δ ln(1+AVE) (FE model with benchmark) \n in the Agricultural sector",
       x = "Year",  y = "Weighted Δ ln(1+AVE)" )
plot_FE_bench
ggsave(filename = file.path(exp, "/plot/", "change_ln_AVE_US_others_Ag_FE_bench.png"),
       plot = plot_FE_bench, width = 10, height = 7, dpi = 300)

test <- ag_w %>% group_by(ExporterISO3) %>%
  filter(any(w_FE_bench > 5, na.rm = TRUE)) %>%
  ungroup()

# for tariffs
plot_tariff <- ag_w %>%
  filter(ExporterISO3 %in% countries_vec) %>%
  ggplot(aes(x = year, y = w_tariff, group = ExporterISO3, color = is_US)) +
  geom_line(linewidth = 0.5) +
  scale_color_manual( values = c(`TRUE` = "red", `FALSE` = "grey70"),
                      breaks = c(TRUE, FALSE),  labels = c("US", "Other countries"),
                      name = NULL  ) +
  theme_trade +
  labs(title = "Weighted average Δ ln(1+tariff) in the Agricultural sector",
       x = "Year",  y = "Weighted Δ ln(1+tariff)" )

plot_tariff
ggsave(filename = file.path(exp, "/plot/", "change_ln_AVE_US_others_Ag_tariff.png"),
       plot = plot_tariff, width = 10, height = 7, dpi = 300)


# check how stable is the benchmark FE value  over time 
names(dta)
summary(dta$FE_bench)
 # get at HS4 level FE

FE_bench_hs4 <- dta %>%
  filter(sector == "Ag") %>%
  group_by(year, hs4) %>%
  summarise(FE_bench_hs4 = mean(FE_bench, na.rm = TRUE), .groups = "drop_last") %>%
  ungroup()

plot_bemchmark <- FE_bench_hs4 %>%
  ggplot(aes(x = year, y = FE_bench_hs4, color= hs4)) +
  geom_line(linewidth = 0.5) +
  theme_trade +
  labs(title = "Benchmark FE value in the Agricultural sector",
       x = "Year",  y = "Benchmark FE" )

plot_bemchmark


################################################################################

# correlations of change in ln(1+AVE)

################################################################################
names(US_ag)

# If we look at correlation between FE and efficiency 
names(US)
US_changes_2015 <- US %>% filter(year %in% c(2016:2020))




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

US_ag1 <- US_ag %>% filter(year %in% c(2016,2019))

vars <- US_ag1[, c("diff_ln_AVE_chen", "diff_ln_AVE_FE", "diff_ln_AVE_u", "diff_ln_AVE_u_tariff")]

vars <- vars %>%  rename(  Chen_et_al = diff_ln_AVE_chen,  FE = diff_ln_AVE_FE,
    SF = diff_ln_AVE_u,  SF_tariff  = diff_ln_AVE_u_tariff )

cor_mat <- cor(vars, use = "complete.obs")

# open PNG device
png(paste0(exp, "plot/corrplot.png"), width = 1200, height = 1000, res = 150)

corrplot(cor_mat, method = "circle", addCoef.col = "black")

dev.off()
 