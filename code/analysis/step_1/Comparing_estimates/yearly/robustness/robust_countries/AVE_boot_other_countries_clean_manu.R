

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
dta <- read_csv( paste0(exp, "/estimates_reduced_form_base_2015_FE_boot.csv"))
unique(dta$ExporterISO3)
colSums(is.na(dta))
unique(dta$year)
names(dta)
################################################################################

# aggregate so that drop the draws 
dta <- dta %>% group_by(year, sector, hs_section, hs2, hs4, hs6_H5, ExporterISO3, ImporterISO3) %>%
  summarise(
    # keep "levels" as first (should be identical across draws)
    across(c(Trade_value_USD, Applied_tariff, log_tariff, diff_log_tariff_2015),~ dplyr::first(.x)  ),
    # average the draw-varying objects
    across(c(diff_ln_AVE_FE, diff_ln_AVE_FE_log, diff_ln_AVE_FE_bench, diff_ln_AVE_FE_log_bench,
        diff_ln_AVE_FE_wmean, diff_ln_AVE_FE_log_wmean),  ~ mean(.x, na.rm = TRUE)),  .groups = "drop"  )
write_csv(dta, paste0(exp, "/estimates_reduced_form_base_2015_FE_boot_summarised.csv") )


dta <- read_csv(paste0(exp, "/estimates_reduced_form_base_2015_FE_boot_summarised.csv"))


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
# 3) if compare other countries to US 
################################################################################

# 1. Filter data to chosen countries + Manu sector, build year
manu <- dta %>%   filter( sector == "Manu"    )

# 2. HS6 weights per country (baseline 2015)
manu_2015_hs6 <- manu %>%    filter(year == 2015) %>%
  group_by(ExporterISO3, hs6_H5) %>%
  summarise(Trade_value_USD_2015 = sum(Trade_value_USD, na.rm = TRUE),
            .groups = "drop_last"    ) %>%
  group_by(ExporterISO3) %>%
  mutate( tot_Trade_value_USD_2015 = sum(Trade_value_USD_2015),
          weight_sector = if_else( tot_Trade_value_USD_2015 > 0,
                                   Trade_value_USD_2015 / tot_Trade_value_USD_2015,  NA_real_  )) %>%  ungroup()
manu <- manu %>%  left_join(manu_2015_hs6, by = c("ExporterISO3", "hs6_H5"))


# 3. Weighted & simple averages over time PER COUNTRY
manu_w <- manu %>% filter(year %in% 2016:2020) %>%
  group_by(ExporterISO3, year) %>%
  summarise(
    w_FE        = weighted.mean(diff_ln_AVE_FE,        w = weight_sector, na.rm = TRUE),
    w_FE_bench  = weighted.mean(diff_ln_AVE_FE_bench,  w = weight_sector, na.rm = TRUE),
    w_FE_wmean  = weighted.mean(diff_ln_AVE_FE_wmean,  w = weight_sector, na.rm = TRUE), 
    
    w_tariff    = weighted.mean(diff_log_tariff_2015,  w = weight_sector, na.rm = TRUE),
    
    s_FE        = mean(diff_ln_AVE_FE,         na.rm = TRUE),
    s_FE_bench  = mean(diff_ln_AVE_FE_bench,   na.rm = TRUE),
    s_FE_wmean  = mean(diff_ln_AVE_FE_wmean,   na.rm = TRUE),
    
    s_tariff    = mean(diff_log_tariff_2015,  na.rm = TRUE) ,  .groups = "drop"  )

manu_w <- manu_w %>%  mutate(is_US = ExporterISO3 == "USA")


# pick countries
countries_vec <- unique(manu_w$ExporterISO3)
#countries_vec <- c("USA", "BRA", "MEX", "AUS", "CAN", "UKR", "RUS")


################################################################################

plot_FE <- manu_w %>%
  filter(ExporterISO3 %in% countries_vec) %>%
  ggplot(aes(x = year, y = w_FE, group = ExporterISO3, color = is_US)) +
  geom_line(linewidth = 0.5) +
  scale_color_manual( values = c(`TRUE` = "red", `FALSE` = "grey70"),
                      breaks = c(TRUE, FALSE),  labels = c("US", "Other countries"),
                      name = NULL  ) +
  theme_trade +
  labs(title = "Weighted average Δ ln(1+AVE) (FE model) in the Manufacturing sector",
       x = "Year",  y = "Weighted Δ ln(1+AVE)" )

plot_FE
ggsave(filename = file.path(exp, "/plot/other_countries/", "change_ln_AVE_US_others_Manu_FE.png"),
       plot = plot_FE, width = 10, height = 7, dpi = 300)

################################################################################

# for FE with benchmark 
plot_FE_bench<- manu_w %>%
  filter(ExporterISO3 %in% countries_vec) %>%
  ggplot(aes(x = year, y = w_FE_bench, group = ExporterISO3, color = is_US)) +
  geom_line(linewidth = 0.5) +
  scale_color_manual( values = c(`TRUE` = "red", `FALSE` = "grey70"),
                      breaks = c(TRUE, FALSE),  labels = c("US", "Other countries"),
                      name = NULL  ) +
  theme_trade +
  labs(title = "Weighted average Δ ln(1+AVE) (FE model with benchmark) \n in the Manufacturing sector",
       x = "Year",  y = "Weighted Δ ln(1+AVE)" )
plot_FE_bench
ggsave(filename = file.path(exp, "/plot/other_countries/", "change_ln_AVE_US_others_Manu_FE_bench.png"),
       plot = plot_FE_bench, width = 10, height = 7, dpi = 300)
# check countries witj high values 
test <- manu_w %>% group_by(ExporterISO3) %>% filter(any(w_FE_bench > 3, na.rm = TRUE)) %>%  ungroup()
unique(test$ExporterISO3)


# check how stable is the benchmark FE value  over time 
names(dta)
summary(dta$FE_bench)
# get at HS4 level FE
FE_bench_hs4 <- dta %>%  filter(sector == "Manu") %>% group_by(year, hs4) %>%  
  summarise(FE_bench_hs4 = mean(FE_bench, na.rm = TRUE), .groups = "drop_last") %>%
  ungroup()
plot_bemchmark <- FE_bench_hs4 %>%  ggplot(aes(x = year, y = FE_bench_hs4, color= hs4)) +
  geom_line(linewidth = 0.5) +  theme_trade +
  labs(title = "Benchmark FE value in the Manufacturing sector", x = "Year",  y = "Benchmark FE" )

plot_bemchmark

################################################################################

# for FE with demean 
plot_FE_wmean<- manu_w %>%
  filter(ExporterISO3 %in% countries_vec) %>%
  ggplot(aes(x = year, y = w_FE_wmean, group = ExporterISO3, color = is_US)) +
  geom_line(linewidth = 0.5) +
  scale_color_manual( values = c(`TRUE` = "red", `FALSE` = "grey70"),
                      breaks = c(TRUE, FALSE),  labels = c("US", "Other countries"),
                      name = NULL  ) +
  theme_trade +
  labs(title = "Weighted average Δ ln(1+AVE) (FE model demeaned) \n in the Manufacturing sector",
       x = "Year",  y = "Weighted Δ ln(1+AVE)" )
plot_FE_wmean
ggsave(filename = file.path(exp, "/plot/other_countries/", "change_ln_AVE_US_others_Manu_FE_wmean.png"),
       plot = plot_FE_wmean, width = 10, height = 7, dpi = 300)


################################################################################

# for tariffs
plot_tariff <- manu_w %>%
  filter(ExporterISO3 %in% countries_vec) %>%
  ggplot(aes(x = year, y = w_tariff, group = ExporterISO3, color = is_US)) +
  geom_line(linewidth = 0.5) +
  scale_color_manual( values = c(`TRUE` = "red", `FALSE` = "grey70"),
                      breaks = c(TRUE, FALSE),  labels = c("US", "Other countries"),
                      name = NULL  ) +
  theme_trade +
  labs(title = "Weighted average Δ ln(1+tariff) in the Manufacturing sector",
       x = "Year",  y = "Weighted Δ ln(1+tariff)" )

plot_tariff
ggsave(filename = file.path(exp, "/plot/other_countries/", "change_ln_AVE_US_others_Manu_tariff.png"),
       plot = plot_tariff, width = 10, height = 7, dpi = 300)










