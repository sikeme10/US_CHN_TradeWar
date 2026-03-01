

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
dta <- read_csv( paste0(exp, "/estimates_reduced_form_base_2015_FE_boot_100.csv"))
unique(dta$ExporterISO3)
colSums(is.na(dta))
unique(dta$year)
names(dta)
################################################################################

# aggregate so that drop the draws 
dta <- dta %>% group_by(year, sector, hs_section, hs2, hs4, ExporterISO3, ImporterISO3) %>%
  summarise(
    # keep "levels" as first (should be identical across draws)
    across(c(Trade_value_USD),~ dplyr::first(.x)  ),
    # average the draw-varying objects
    across(c(diff_ln_AVE_FE, diff_ln_AVE_FE_log, diff_ln_AVE_FE_bench, diff_ln_AVE_FE_log_bench,
        diff_ln_AVE_FE_wmean, diff_ln_AVE_FE_log_wmean),  ~ mean(.x, na.rm = TRUE)),  .groups = "drop"  )
write_csv(dta, paste0(exp, "/estimates_reduced_form_base_2015_FE_boot_hs4_summarised.csv") )

dta <- read_csv(paste0(exp, "/estimates_reduced_form_base_2015_FE_boot_hs4_summarised.csv"))



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
quant <- 0.01

FE_q   <- quantile(dta$diff_ln_AVE_FE,        quant,      na.rm = TRUE)
FE_qH  <- quantile(dta$diff_ln_AVE_FE,        1 - quant,  na.rm = TRUE)
FE_b_q <- quantile(dta$diff_ln_AVE_FE_bench,  quant,      na.rm = TRUE)
FE_b_qH<- quantile(dta$diff_ln_AVE_FE_bench,  1 - quant,  na.rm = TRUE)
FE_w_q <- quantile(dta$diff_ln_AVE_FE_wmean,  quant,      na.rm = TRUE)
FE_w_qH<- quantile(dta$diff_ln_AVE_FE_wmean,  1 - quant,  na.rm = TRUE)

cat("FE [1%,99%]:", FE_q, FE_qH,"| Bench [1%,99%]:", FE_b_q, FE_b_qH, "| Wmean [1%,99%]:", FE_w_q, FE_w_qH, "\n")

dta <- dta %>%
  mutate(diff_ln_AVE_FE        = ifelse(diff_ln_AVE_FE        < FE_q   | diff_ln_AVE_FE        > FE_qH,  NA, diff_ln_AVE_FE),
         diff_ln_AVE_FE_bench  = ifelse(diff_ln_AVE_FE_bench  < FE_b_q | diff_ln_AVE_FE_bench  > FE_b_qH,NA, diff_ln_AVE_FE_bench),
         diff_ln_AVE_FE_wmean  = ifelse(diff_ln_AVE_FE_wmean  < FE_w_q | diff_ln_AVE_FE_wmean  > FE_w_qH,NA, diff_ln_AVE_FE_wmean)  )



################################################################################
# 3) if compare other countries to US 
################################################################################

# 1. Filter data to chosen countries + Ag sector, build year
ag <- dta %>%   filter( sector == "Ag"    )

# 2. HS6 weights per country (baseline 2015)
ag_2015_hs6 <- ag %>%    filter(year == 2015) %>%
  group_by(ExporterISO3, hs4) %>%
  summarise(Trade_value_USD_2015 = sum(Trade_value_USD, na.rm = TRUE),
            .groups = "drop_last"    ) %>%
  group_by(ExporterISO3) %>%
  mutate( tot_Trade_value_USD_2015 = sum(Trade_value_USD_2015),
          weight_sector = if_else( tot_Trade_value_USD_2015 > 0,
                                   Trade_value_USD_2015 / tot_Trade_value_USD_2015,  NA_real_  )) %>%  ungroup()
ag <- ag %>%  left_join(ag_2015_hs6, by = c("ExporterISO3", "hs4"))


# 3. Weighted & simple averages over time PER COUNTRY
ag_w <- ag %>% filter(year %in% 2016:2020) %>%
  group_by(ExporterISO3, year) %>%
  summarise(
    w_FE        = weighted.mean(diff_ln_AVE_FE,        w = weight_sector, na.rm = TRUE),
    w_FE_bench  = weighted.mean(diff_ln_AVE_FE_bench,  w = weight_sector, na.rm = TRUE),
    w_FE_wmean  = weighted.mean(diff_ln_AVE_FE_wmean,  w = weight_sector, na.rm = TRUE), 
    
    # w_tariff    = weighted.mean(diff_log_tariff_2015,  w = weight_sector, na.rm = TRUE),
    
    s_FE        = mean(diff_ln_AVE_FE,         na.rm = TRUE),
    s_FE_bench  = mean(diff_ln_AVE_FE_bench,   na.rm = TRUE),
    s_FE_wmean  = mean(diff_ln_AVE_FE_wmean,   na.rm = TRUE))
    
    # s_tariff    = mean(diff_log_tariff_2015,  na.rm = TRUE) ,  .groups = "drop"  )

ag_w <- ag_w %>%  mutate(is_US = ExporterISO3 == "USA")


# pick countries
countries_vec <- unique(ag_w$ExporterISO3)
#countries_vec <- c("USA", "BRA", "MEX", "AUS", "CAN", "UKR", "RUS")


################################################################################

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
ggsave(filename = file.path(exp, "/plot/other_countries/", "change_ln_AVE_US_others_Ag_FE.png"),
       plot = plot_FE, width = 10, height = 7, dpi = 300)

################################################################################

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
ggsave(filename = file.path(exp, "/plot/other_countries/", "change_ln_AVE_US_others_Ag_FE_bench.png"),
       plot = plot_FE_bench, width = 10, height = 7, dpi = 300)
# check countries witj high values 
test <- ag_w %>% group_by(ExporterISO3) %>% filter(any(w_FE_bench > 3, na.rm = TRUE)) %>%  ungroup()
unique(test$ExporterISO3)


# check how stable is the benchmark FE value  over time 
names(dta)
summary(dta$FE_bench)
# get at HS4 level FE
FE_bench_hs4 <- dta %>%  filter(sector == "Ag") %>% group_by(year, hs4) %>%  
  summarise(FE_bench_hs4 = mean(FE_bench, na.rm = TRUE), .groups = "drop_last") %>%
  ungroup()
plot_bemchmark <- FE_bench_hs4 %>%  ggplot(aes(x = year, y = FE_bench_hs4, color= hs4)) +
  geom_line(linewidth = 0.5) +  theme_trade +
  labs(title = "Benchmark FE value in the Agricultural sector", x = "Year",  y = "Benchmark FE" )

plot_bemchmark

################################################################################

# for FE with demean 
plot_FE_wmean<- ag_w %>%
  filter(ExporterISO3 %in% countries_vec) %>%
  ggplot(aes(x = year, y = w_FE_wmean, group = ExporterISO3, color = is_US)) +
  geom_line(linewidth = 0.5) +
  scale_color_manual( values = c(`TRUE` = "red", `FALSE` = "grey70"),
                      breaks = c(TRUE, FALSE),  labels = c("US", "Other countries"),
                      name = NULL  ) +
  theme_trade +
  labs(title = "Weighted average Δ ln(1+AVE) (FE model demeaned) \n in the Agricultural sector",
       x = "Year",  y = "Weighted Δ ln(1+AVE)" )
plot_FE_wmean
ggsave(filename = file.path(exp, "/plot/other_countries/", "change_ln_AVE_US_others_Ag_FE_wmean.png"),
       plot = plot_FE_wmean, width = 10, height = 7, dpi = 300)


# ################################################################################
# 
# # for tariffs
# plot_tariff <- ag_w %>%
#   filter(ExporterISO3 %in% countries_vec) %>%
#   ggplot(aes(x = year, y = w_tariff, group = ExporterISO3, color = is_US)) +
#   geom_line(linewidth = 0.5) +
#   scale_color_manual( values = c(`TRUE` = "red", `FALSE` = "grey70"),
#                       breaks = c(TRUE, FALSE),  labels = c("US", "Other countries"),
#                       name = NULL  ) +
#   theme_trade +
#   labs(title = "Weighted average Δ ln(1+tariff) in the Agricultural sector",
#        x = "Year",  y = "Weighted Δ ln(1+tariff)" )
# 
# plot_tariff
# ggsave(filename = file.path(exp, "/plot/other_countries/", "change_ln_AVE_US_others_Ag_tariff.png"),
#        plot = plot_tariff, width = 10, height = 7, dpi = 300)
# 
# 
# 
# 






