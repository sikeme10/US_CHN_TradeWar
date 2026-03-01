



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
exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/Compare_values/yearly/robust/"

################################################################################
# 1) Load data 
################################################################################

US <- read_csv(paste0(exp, "US_ln_NTMs_base_2017_FE_boot.csv"))

################################################################################

# For US ag: 

# create simple average change and weighted average change in 
US_ag <- US %>% filter(sector == "Ag")
length(unique(US_ag$hs6_H5))
names(US_ag)
unique(US_ag$year)
unique(US_ag$hs2)
unique(US_ag$hs_section)
length(unique(US_ag$hs6_H5))



# 1) Base 2017 (draw==1) totals at hs6 (this is the "atom" you use everywhere)
base2017_hs6 <- US_ag %>% filter(year == 2017, draw == 1) %>% group_by(hs6_H5, hs_section, hs2, hs4) %>%
  summarise(Trade_value_USD_2017 = sum(Trade_value_USD, na.rm = TRUE), .groups = "drop")

# 2) Build weights for each aggregation level, then bind into ONE table

# sector level weight: HS6 to sector 
w_sector <- base2017_hs6 %>%  group_by(hs6_H5) %>%
  summarise(Trade_value_USD_2017 = sum(Trade_value_USD_2017), .groups = "drop") %>%
  mutate(tot_Trade_value_USD_2017 = sum(Trade_value_USD_2017),
         weight_sector = if_else(tot_Trade_value_USD_2017 > 0, Trade_value_USD_2017 / tot_Trade_value_USD_2017, 0)) %>%
  select(hs6_H5, weight_sector)

# HS section level weight: HS6 to HS section 
w_hs_sect <- base2017_hs6 %>%  group_by(hs_section, hs6_H5) %>%
  summarise(Trade_value_USD_2017 = sum(Trade_value_USD_2017), .groups = "drop_last") %>%
  mutate(hs_sect_tot_Trade_value_USD_2017 = sum(Trade_value_USD_2017),
         weight_hs_sect = if_else(hs_sect_tot_Trade_value_USD_2017 > 0,
                                  Trade_value_USD_2017 / hs_sect_tot_Trade_value_USD_2017, 0) ) %>%
  ungroup() %>% select(hs_section, hs6_H5, weight_hs_sect)

# HS2 level weight: HS6 to HS2 
w_hs2 <- base2017_hs6 %>%  group_by(hs2, hs6_H5) %>%
  summarise(Trade_value_USD_2017 = sum(Trade_value_USD_2017), .groups = "drop_last") %>%
  mutate(hs2_tot_Trade_value_USD_2017 = sum(Trade_value_USD_2017),
         weight_hs2 = if_else(hs2_tot_Trade_value_USD_2017 > 0,
                              Trade_value_USD_2017 / hs2_tot_Trade_value_USD_2017, 0)) %>%
  ungroup() %>% select(hs2, hs6_H5, weight_hs2)

# HS4 level weight: HS6 to HS4
w_hs4 <- base2017_hs6 %>% group_by(hs4, hs6_H5) %>%
  summarise(Trade_value_USD_2017 = sum(Trade_value_USD_2017), .groups = "drop_last") %>%
  mutate(hs4_tot_Trade_value_USD_2017 = sum(Trade_value_USD_2017),
         weight_hs4 = if_else(hs4_tot_Trade_value_USD_2017 > 0,
                              Trade_value_USD_2017 / hs4_tot_Trade_value_USD_2017, 0)) %>%
  ungroup() %>% select(hs4, hs6_H5, weight_hs4)

# 3) Single join (instead of 4 joins)
US_ag <- US_ag %>% left_join(w_sector,  by = "hs6_H5") %>%
  left_join(w_hs_sect, by = c("hs_section", "hs6_H5")) %>%
  left_join(w_hs2,     by = c("hs2", "hs6_H5")) %>%
  left_join(w_hs4,     by = c("hs4", "hs6_H5"))



################################################################################
# 3) Add Chen et al estimates
################################################################################
# add Chen et al. estimations 
Chen <- read_csv("data/chen_NTB_tariff/hs2_agriculture_manufacturing_clean.csv")
names(Chen)


Chen <- Chen %>% select(-Country, - ISO3_Code) %>% rename(hs2 = HS2 , Chen_US_import_share = US_import_share,
                                                          diff_log_tariff_Chen = tau_tariff_CHN, 
                                                          diff_ln_AVE_chen = tau_NTB)
US_ag$hs2 <- as.numeric(US_ag$hs2)
US_ag <- left_join(US_ag,Chen)
names(US_ag)

# create weights for Chen et al at sector level and HS section

# Ag sector level: create weight in trade (hs6/total US export)
# weights for hs6 to aggregate to sector level
US_ag_2017_hs2_chen <- US_ag %>%
  filter(year == 2017 & draw == 1 ) %>%  group_by(hs2) %>%
  summarise(Trade_value_USD_2017 = sum(Trade_value_USD, na.rm = TRUE),.groups = "drop"  ) %>%
  mutate(  chen_tot_Trade_value_USD_2017 = sum(Trade_value_USD_2017, na.rm = TRUE),
    weight_sector_chen = if_else( chen_tot_Trade_value_USD_2017 > 0,Trade_value_USD_2017 / chen_tot_Trade_value_USD_2017,
      NA_real_    )  ) %>% select(-Trade_value_USD_2017 )
US_ag <-left_join(US_ag,US_ag_2017_hs2_chen )


# HS section level : weights for hs2 to aggregate to HS-section
US_ag_2017_hs_sect_chen <- US_ag %>%  
  filter(year == 2017 & draw == 1 ) %>%
  group_by(hs_section, hs2) %>%
  summarise(Trade_value_USD_2017 = sum(Trade_value_USD, na.rm = TRUE),
    .groups = "drop_last"  ) %>%
  group_by(hs_section) %>%
  mutate( Chen_hs_sect_tot_Trade_value_USD_2017 = sum(Trade_value_USD_2017, na.rm = TRUE),
    weight_hs_sect_Chen = if_else(Chen_hs_sect_tot_Trade_value_USD_2017 > 0,Trade_value_USD_2017 / Chen_hs_sect_tot_Trade_value_USD_2017,
      NA_real_    )  ) %>%  ungroup() %>% select(-Trade_value_USD_2017 )
US_ag <-left_join(US_ag, US_ag_2017_hs_sect_chen )

################################################################################

US_ag <- US_ag %>% filter(year>2017)

################################################################################
# 4) Plot change in ln(1+AVE)
################################################################################


  # create a theme for ggplot 
theme_trade <- theme_minimal(base_size = 14) +
  theme(  panel.spacing.x = unit(1.2, "lines"),
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
# a) sector level 
################################################################################

# trade weighted and simple average trade costs :
names(US_ag)

US_ag_w <- US_ag %>%  
  filter(sector == "Ag") %>%  
  group_by(year, draw) %>%
  summarise( 
    w_FE        = weighted.mean(diff_ln_AVE_FE,           w = weight_sector, na.rm = TRUE),
    w_FE_bench  = weighted.mean(diff_ln_AVE_FE_bench,     w = weight_sector, na.rm = TRUE), 
    w_FE_mean   = weighted.mean(diff_ln_AVE_FE_wmean,       w = weight_sector, na.rm = TRUE),
    
    # --- added: log FE versions ---
    w_FE_log        = weighted.mean(diff_ln_AVE_FE_log,       w = weight_sector, na.rm = TRUE),
    w_FE_log_bench  = weighted.mean(diff_ln_AVE_FE_log_bench, w = weight_sector, na.rm = TRUE),
    w_FE_log_mean   = weighted.mean(diff_ln_AVE_FE_log_wmean,       w = weight_sector, na.rm = TRUE),

    w_chen      = weighted.mean(diff_ln_AVE_chen,         w = weight_sector_chen, na.rm = TRUE),
    w_tariff    = weighted.mean(diff_log_tariff_2017,     w = weight_sector, na.rm = TRUE),
    
    s_FE        = mean(diff_ln_AVE_FE,        na.rm = TRUE),
    s_FE_bench  = mean(diff_ln_AVE_FE_bench,  na.rm = TRUE),
    s_FE_mean   = mean(diff_ln_AVE_FE_wmean,  na.rm = TRUE),
    
    # --- added: log FE versions ---
    s_FE_log        = mean(diff_ln_AVE_FE_log,       na.rm = TRUE),
    s_FE_log_bench  = mean(diff_ln_AVE_FE_log_bench, na.rm = TRUE),
    s_FE_log_mean   = mean(diff_ln_AVE_FE_log_wmean, na.rm = TRUE),
    
    s_chen      = mean(diff_ln_AVE_chen,       na.rm = TRUE), # chen
    s_tariff    = mean(diff_log_tariff_2017,   na.rm = TRUE)  ) %>% 
  ungroup() %>%  mutate(W_chen = mean(w_chen, na.rm = TRUE), s_chen = mean(s_chen, na.rm = TRUE)  )


US_ag_q <- US_ag_w %>%  
  group_by(year) %>%  summarise(
    # ---- FE (levels) ----
    FE_mean = mean(w_FE, na.rm = TRUE),
    FE_lo   = quantile(w_FE, 0.025, na.rm = TRUE),
    FE_hi   = quantile(w_FE, 0.975, na.rm = TRUE),
    
    # ---- FE benchmark (levels) ----
    FEb_mean = mean(w_FE_bench, na.rm = TRUE),
    FEb_lo   = quantile(w_FE_bench, 0.025, na.rm = TRUE),
    FEb_hi   = quantile(w_FE_bench, 0.975, na.rm = TRUE),
    
    # ---- FE demeaned (levels) ----
    FEm_mean = mean(w_FE_mean, na.rm = TRUE),
    FEm_lo   = quantile(w_FE_mean, 0.025, na.rm = TRUE),
    FEm_hi   = quantile(w_FE_mean, 0.975, na.rm = TRUE),
    
    # ---- FE (log version) ----
    FE_log_mean = mean(w_FE_log, na.rm = TRUE),
    FE_log_lo   = quantile(w_FE_log, 0.025, na.rm = TRUE),
    FE_log_hi   = quantile(w_FE_log, 0.975, na.rm = TRUE),
    
    # ---- FE benchmark (log version) ----
    FEb_log_mean = mean(w_FE_log_bench, na.rm = TRUE),
    FEb_log_lo   = quantile(w_FE_log_bench, 0.025, na.rm = TRUE),
    FEb_log_hi   = quantile(w_FE_log_bench, 0.975, na.rm = TRUE),
    
    # ---- FE demeaned (log version) ----
    FEm_log_mean = mean(w_FE_log_mean, na.rm = TRUE),
    FEm_log_lo   = quantile(w_FE_log_mean, 0.025, na.rm = TRUE),
    FEm_log_hi   = quantile(w_FE_log_mean, 0.975, na.rm = TRUE),
    
    # ---- other series ----
    chen_mean   = mean(w_chen, na.rm = TRUE),
    tariff_mean = mean(w_tariff, na.rm = TRUE),
    
    .groups = "drop"  )




plot <- ggplot(US_ag_q, aes(x = year)) +
  ## FE CI
  # geom_ribbon(  aes(ymin = FE_lo, ymax = FE_hi), fill = "blue", alpha = 0.2  ) +
  geom_line(aes(y = FE_mean, color = "FE"),    linewidth = 1  ) +
  ## Tariff
  geom_line(aes(y = tariff_mean, color = "tariff"),   linewidth = 1  ) +
  ## Benchmark
  geom_line(  aes(y = FEb_mean, color = "FE_bench"),  linetype = "dashed",  linewidth = 1  ) +
  ## demeaned
  geom_line(  aes(y = FEm_mean, color = "FE_demeaned"),  linetype = "dashed",  linewidth = 1  ) +
  scale_color_manual(  values = c( "FE" = "blue","tariff" = "darkgreen", "FE_bench" = "blue", 
                                   "FE_demeaned" = "purple"),    name = "Variables"  ) +
  labs(  title = "Weighted average Δ ln(1+AVE), Agriculture (relative to 2017)",
    x = "Year",    y = "Weighted Δ ln(1+AVE)"  ) +
  theme_trade

plot
ggsave(filename = file.path(exp, "plot/", "Compare_ln_AVE_Ag_weight_2017_boot.png"),plot = plot, width = 8, height = 5, dpi = 300)

library(dplyr)
library(tidyr)
library(ggplot2)

# -------------------------
# 1) FE (no log): long for facet (bench vs non-bench)
# -------------------------
US_ag_FE_plot <- US_ag_q %>%
  transmute(year,  tariff_mean,  nb_mean = FE_mean,   nb_lo = FE_lo,   nb_hi = FE_hi,
    b_mean  = FEb_mean,  b_lo  = FEb_lo,  b_hi  = FEb_hi,
    m_mean  = FEm_mean,  m_lo  = FEm_lo,  m_hi  = FEm_hi  ) %>%
  pivot_longer(cols = c(nb_mean, nb_lo, nb_hi, b_mean,  b_lo,  b_hi,
             m_mean,  m_lo,  m_hi), names_to = c("bench", "stat"),
    names_pattern = "(nb|b|m)_(mean|lo|hi)"  ) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate( bench = recode(bench,  nb = "FE", b  = "Benchmark",m  = "Demean")  )

plot_FE <- ggplot(US_ag_FE_plot, aes(x = year)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "blue", alpha = 0.2) +
  geom_line(aes(y = mean, color = "FE"), linewidth = 1) +
  geom_line(aes(y = tariff_mean, color = "tariff"), linewidth = 1) +
  facet_wrap(~ bench, nrow = 1) +
  scale_color_manual( values = c("FE" = "blue", "tariff" = "darkgreen"),name = "Variables"  ) +
  labs(title = "Weighted average Δ ln(1+AVE), Agriculture (relative to 2017) — FE model",
    x = "Year",  y = "Weighted Δ ln(1+AVE)" ) +
  theme_trade
plot_FE

# -------------------------
# 2) FE_log: long for facet (bench vs non-bench)
# -------------------------
US_ag_FElog_plot <- US_ag_q %>%
  transmute(year, tariff_mean,nb_mean = FE_log_mean,   nb_lo = FE_log_lo,   nb_hi = FE_log_hi,
    b_mean  = FEb_log_mean,  b_lo  = FEb_log_lo,  b_hi  = FEb_log_hi,
    m_mean  = FEm_log_mean,  m_lo  = FEm_log_lo,  m_hi  = FEm_log_hi  ) %>%
  pivot_longer( cols = c(nb_mean, nb_lo, nb_hi,  b_mean,  b_lo,  b_hi,
             m_mean,  m_lo,  m_hi),  names_to = c("bench", "stat"),
    names_pattern = "(nb|b|m)_(mean|lo|hi)"  ) %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  mutate(    ben = recode(bench, nb = "FE",    b  = "Benchmark",   m  = "Demean")  )

plot_FE_log <- ggplot(US_ag_FElog_plot, aes(x = year)) +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = "blue", alpha = 0.2) +
  geom_line(aes(y = mean, color = "FE_log"), linewidth = 1) +
  geom_line(aes(y = tariff_mean, color = "tariff"), linewidth = 1) +
  facet_wrap(~ bench, nrow = 1) +
  scale_color_manual(values = c("FE_log" = "blue", "tariff" = "darkgreen"),
    name = "Variables"  ) +
  labs( title = "Weighted average Δ ln(1+AVE), Agriculture (relative to 2017) — FE_log",
    x = "Year",    y = "Weighted Δ ln(1+AVE)"  ) +
  theme_trade
plot_FE_log

# Print them
plot_FE
ggsave(filename = file.path(exp, "plot/", "Compare_ln_AVE_Ag_weight_2017_boot.png"),plot = plot_FE, width = 8, height = 5, dpi = 300)

plot_FE_log
ggsave(filename = file.path(exp, "plot/", "Compare_ln_AVE_Ag_weight_2017_boot_loglin.png"),plot = plot_FE_log, width = 8, height = 5, dpi = 300)




################################################################################
# a) HS section level
################################################################################
# get trade weighted values 
names(US_ag)
US_ag_w_sect <- US_ag %>% filter(sector == "Ag") %>%
  group_by(year, draw, hs_section) %>%
  summarise( 
    w_FE        = weighted.mean(diff_ln_AVE_FE,           w = weight_hs_sect, na.rm = TRUE),
    w_FE_bench  = weighted.mean(diff_ln_AVE_FE_bench,     w = weight_hs_sect, na.rm = TRUE),
    w_FE_mean   = weighted.mean(diff_ln_AVE_FE_wmean,       w = weight_hs_sect, na.rm = TRUE),
    
    # added (log)
    w_FE_log        = weighted.mean(diff_ln_AVE_FE_log,       w = weight_hs_sect, na.rm = TRUE),
    w_FE_log_bench  = weighted.mean(diff_ln_AVE_FE_log_bench, w = weight_hs_sect, na.rm = TRUE),
    w_FE_log_mean   = weighted.mean(diff_ln_AVE_FE_log_wmean,       w = weight_hs_sect, na.rm = TRUE),
    
    w_chen      = weighted.mean(diff_ln_AVE_chen,         w = weight_hs_sect_Chen, na.rm = TRUE),
    w_tariff    = weighted.mean(diff_log_tariff_2017,     w = weight_hs_sect, na.rm = TRUE),
    
    s_FE        = mean(diff_ln_AVE_FE,        na.rm = TRUE),
    s_FE_bench  = mean(diff_ln_AVE_FE_bench,  na.rm = TRUE),
    s_FE_mean   = mean(diff_ln_AVE_FE_wmean,  na.rm = TRUE),
    
    # added (log)
    s_FE_log        = mean(diff_ln_AVE_FE_log,       na.rm = TRUE),
    s_FE_log_bench  = mean(diff_ln_AVE_FE_log_bench, na.rm = TRUE),
    s_FE_log_mean   = mean(diff_ln_AVE_FE_log_wmean, na.rm = TRUE),
    
    s_chen      = mean(diff_ln_AVE_chen,      na.rm = TRUE),
    s_tariff    = mean(diff_log_tariff_2017,  na.rm = TRUE),
    
    .groups = "drop"  ) %>%  
  group_by(hs_section) %>% mutate(w_chen = mean(w_chen, na.rm = TRUE),s_chen = mean(s_chen, na.rm = TRUE)  ) %>%
  ungroup()

# aggregate at hs section
US_ag_q_sect <- US_ag_w_sect %>%    group_by(year, hs_section) %>%
  summarise(
    # FE (non-benchmark)
    FE_mean = mean(w_FE, na.rm = TRUE),
    FE_lo   = quantile(w_FE, 0.025, na.rm = TRUE),
    FE_hi   = quantile(w_FE, 0.975, na.rm = TRUE),
    
    # FE (benchmark) + CI
    FEb_mean = mean(w_FE_bench, na.rm = TRUE),
    FEb_lo   = quantile(w_FE_bench, 0.025, na.rm = TRUE),
    FEb_hi   = quantile(w_FE_bench, 0.975, na.rm = TRUE),
    
    # ---- FE demeaned (levels) ----
    FEm_mean = mean(w_FE_mean, na.rm = TRUE),
    FEm_lo   = quantile(w_FE_mean, 0.025, na.rm = TRUE),
    FEm_hi   = quantile(w_FE_mean, 0.975, na.rm = TRUE),
    
    # FE_log (non-benchmark)
    FE_log_mean = mean(w_FE_log, na.rm = TRUE),
    FE_log_lo   = quantile(w_FE_log, 0.025, na.rm = TRUE),
    FE_log_hi   = quantile(w_FE_log, 0.975, na.rm = TRUE),
    
    # FE_log (benchmark) + CI
    FEb_log_mean = mean(w_FE_log_bench, na.rm = TRUE),
    FEb_log_lo   = quantile(w_FE_log_bench, 0.025, na.rm = TRUE),
    FEb_log_hi   = quantile(w_FE_log_bench, 0.975, na.rm = TRUE),
    
    # ---- FE demeaned (log version) ----
    FEm_log_mean = mean(w_FE_log_mean, na.rm = TRUE),
    FEm_log_lo   = quantile(w_FE_log_mean, 0.025, na.rm = TRUE),
    FEm_log_hi   = quantile(w_FE_log_mean, 0.975, na.rm = TRUE),
    
    tariff_mean = mean(w_tariff, na.rm = TRUE),
    
    # Chen (optional CI)
    chen_mean = mean(w_chen, na.rm = TRUE),
    chen_lo   = quantile(w_chen, 0.025, na.rm = TRUE),
    chen_hi   = quantile(w_chen, 0.975, na.rm = TRUE),
    
    .groups = "drop"  )


# plot poisson model 
plot <- ggplot(US_ag_q_sect, aes(x = year)) +
  # FE CI band
  geom_ribbon(aes(ymin = FE_lo, ymax = FE_hi), fill = "blue", alpha = 0.2) +
  geom_line(aes(y = FE_mean, color = "FE"), linewidth = 1) +
  # Mean FE (NEW)
  geom_ribbon(aes(ymin = FEm_lo, ymax = FEm_hi), fill = "orange", alpha = 0.15) +
  geom_line(aes(y = FEm_mean, color = "FE_mean"),       linewidth = 1) +
  # Tariff
  geom_line(aes(y = tariff_mean, color = "tariff"), linewidth = 1) +
  # Benchmark (dashed)
  geom_ribbon(aes(ymin = FEb_lo, ymax = FEb_hi),       fill = "purple", alpha = 0.08) +
  geom_line(aes(y = FEb_mean, color = "FE_bench"),      linetype = "dashed", linewidth = 1) +
  # Chen
  geom_line(aes(y = chen_mean, color = "Chen_et_al"), linewidth = 1) +
  
  scale_color_manual(
    values = c(  "FE" = "blue",   "FE_mean" = "orange",
      "tariff" = "darkgreen",   "FE_bench" = "purple","Chen_et_al" = "red"    ),
    labels = c(   "FE" = "FE","FE_mean" = "FE (demean)",
      "tariff" = "Tariff",  "FE_bench" = "FE (benchmark)","Chen_et_al" = "Chen et al."),
    name = "Variables"  ) +
  facet_wrap(~ hs_section, scales = "free_y") +
  labs(  title = "Weighted average Δ ln(1+AVE), Agriculture by HS section (relative to 2017)",
    x = "Year",    y = "Weighted Δ ln(1+AVE)"  ) +
  theme_trade
plot
ggsave(filename = file.path(exp, "plot/", "Compare_ln_AVE_Ag_hs_sect_weight_2017_boot.png"),plot = plot, width = 9, height = 5, dpi = 300)


plot <- ggplot(US_ag_q_sect, aes(x = year)) +
  # FE_log CI band
  geom_ribbon(aes(ymin = FE_log_lo, ymax = FE_log_hi),          fill = "blue", alpha = 0.2) +
  geom_line(aes(y = FE_log_mean, color = "FE_log"),            linewidth = 1) +
  # Mean FE_log (NEW)
  geom_ribbon(aes(ymin = FEm_log_lo, ymax = FEm_log_hi),        fill = "orange", alpha = 0.15) +
  geom_line(aes(y = FEm_log_mean, color = "FE_log_mean"),            linewidth = 1) +
  # Tariff
  geom_line(aes(y = tariff_mean, color = "tariff"),            linewidth = 1) +
  # Benchmark (dashed)
  geom_ribbon(aes(ymin = FEb_log_lo, ymax = FEb_log_hi),    fill = "purple", alpha = 0.2) +
  geom_line(aes(y = FEb_log_mean, color = "FE_log_bench"),  linetype = "dashed", linewidth = 1) +
  # Chen
  geom_line(aes(y = chen_mean, color = "Chen_et_al"),        linewidth = 1) +
  
  scale_color_manual(  values = c(    "FE_log"       = "blue",  "FE_log_mean"  = "orange",
      "FE_log_bench" = "purple",  "tariff"       = "darkgreen",    "Chen_et_al"   = "red"    ),
    labels = c(    "FE_log"       = "FE (log)",    "FE_log_mean"  = "FE (log, demean)",
      "FE_log_bench" = "FE (log, benchmark)",     "tariff"       = "Tariff",    "Chen_et_al"   = "Chen et al."    ),
    name = "Variables"  ) +
  facet_wrap(~ hs_section, scales = "free_y") +
  labs(   title = "Weighted average Δ ln(1+AVE), Agriculture by HS section (log FE, relative to 2017)",
    x = "Year",   y = "Weighted Δ ln(1+AVE)") +
  theme_trade
plot
ggsave(filename = file.path(exp, "plot/", "Compare_ln_AVE_Ag_hs_sect_weight_loglin_2017_boot.png"),  plot = plot,
  width = 9, height = 5, dpi = 300)



################################################################################
# a) HS 2 level level
################################################################################


US_ag_w_hs2 <- US_ag %>%  filter(sector == "Ag") %>%  group_by(year, draw,hs2) %>%
  summarise( 
    w_FE        = weighted.mean(diff_ln_AVE_FE,        w = weight_hs2, na.rm = TRUE),
    w_FE_bench  = weighted.mean(diff_ln_AVE_FE_bench,  w = weight_hs2, na.rm = TRUE), 
    w_FE_mean   = weighted.mean(diff_ln_AVE_FE_wmean,  w = weight_hs2, na.rm = TRUE),
    
    w_chen      = weighted.mean(diff_ln_AVE_chen,      w = weight_hs2, na.rm = TRUE),
    w_tariff    = weighted.mean(diff_log_tariff_2017,  w = weight_hs2, na.rm = TRUE),
    
    s_FE        = mean(diff_ln_AVE_FE,         na.rm = TRUE),
    s_FE_bench  = mean(diff_ln_AVE_FE_bench,   na.rm = TRUE),
    s_FE_mean  = mean(diff_ln_AVE_FE_wmean,   na.rm = TRUE),
    
    s_chen      = mean(diff_ln_AVE_chen,       na.rm = TRUE),# chen
    s_tariff    = mean(diff_log_tariff_2017,   na.rm = TRUE)  ) %>% ungroup() 


US_ag_w_hs2_band <- US_ag_w_hs2 %>%  group_by(year, hs2) %>%
  summarise(FE_mean = mean(w_FE, na.rm = TRUE),
            FE_lo   = quantile(w_FE, 0.025, na.rm = TRUE),
            FE_hi   = quantile(w_FE, 0.975, na.rm = TRUE),
            
            FEbench_mean = mean(w_FE_bench, na.rm = TRUE),
            FEbench_lo   = quantile(w_FE_bench, 0.025, na.rm = TRUE),
            FEbench_hi   = quantile(w_FE_bench, 0.975, na.rm = TRUE),
            
            FEmean_mean = mean(w_FE_mean, na.rm = TRUE),
            FEmean_lo   = quantile(w_FE_mean, 0.025, na.rm = TRUE),
            FEmean_hi   = quantile(w_FE_mean, 0.975, na.rm = TRUE),
            
            chen_mean = mean(w_chen, na.rm = TRUE),
            tariff_mean = mean(w_tariff, na.rm = TRUE),    .groups = "drop"  )



unique(US_ag_w_hs2$hs2)
HS <- c( 2, 8,10, 12,15, 16, 22,23)
HS <- c(1:9)
HS <- c(11:23)

hs2_names <- c(
  "2"  = "Meat & edible meat offal (HS 02)",
  "8"  = "Edible fruit & nuts (HS 08)",
  "10" = "Cereals (HS 10)",
  "12" = "Oil seeds & oleaginous fruits (HS 12)",
  "15" = "Animal & veg fats/oils (HS 15)",
  "16" = "Prep. of meat, fish etc. (HS 16)",
  "22" = "Beverages, spirits & vinegar (HS 22)",
  "23" = "Residues & animal feed (HS 23)")
plot <- ggplot(subset(US_ag_w_hs2, hs2 %in% HS)) +
  geom_line(aes(x = date, y = w_FE,        color = "FE")) +
  geom_line(aes(x = date, y = w_sf,        color = "sf")) +
  geom_line(aes(x = date, y = w_sf_tariff, color = "sf_tariff")) +
  geom_line(aes(x = date, y = w_tariff,    color = "tariff")) +
  scale_color_manual( values = c(    "FE"        = "steelblue",
      "sf"        = "darkorange",      "sf_tariff" = "firebrick",      "tariff"    = "darkgreen"    ),    
      labels = c(     "FE"        = "FE",      "sf"        = "SF",      "sf_tariff" = "Tariff-adjusted SF",
      "tariff"    = "Tariff"    ),    name = "Variables"  ) +
  facet_wrap(  ~ hs2,  scales   = "free_y",  labeller = as_labeller(hs2_names)  ) +
  labs( title = "Weighted average Δ ln(1+AVE) for agricultural sector by HS section (relative to 2017)",
    x = "Date",    y = "Weighted Δ ln(1+AVE)"  ) +
  theme_trade
plot
ggsave(filename = file.path(exp, "plot/", "Compare_ln_AVE_Ag_hs2_weighted.png"),plot = plot, width = 8, height = 5, dpi = 300)



# get a summary table (only interested in mean change between 2018 and 2019

summary <- US_ag_w_hs2 %>%  filter(year %in% c(2018,2019)) %>% group_by(hs2) %>%
  summarise(FE_mean = mean(w_FE, na.rm = TRUE),
            FE_med = median(w_FE, na.rm = TRUE),
            FE_lo   = quantile(w_FE, 0.025, na.rm = TRUE),
            FE_hi   = quantile(w_FE, 0.975, na.rm = TRUE),
            
            FEbench_mean = mean(w_FE_bench, na.rm = TRUE),
            FEbench_med = median(w_FE_bench, na.rm = TRUE),
            FEbench_lo   = quantile(w_FE_bench, 0.025, na.rm = TRUE),
            FEbench_hi   = quantile(w_FE_bench, 0.975, na.rm = TRUE),
            
            FEmean_mean = mean(w_FE_mean, na.rm = TRUE),
            FEmean_med = median(w_FE_mean, na.rm = TRUE),
            FEmean_lo   = quantile(w_FE_mean, 0.025, na.rm = TRUE),
            FEmean_hi   = quantile(w_FE_mean, 0.975, na.rm = TRUE),
            
            chen_mean = mean(w_chen, na.rm = TRUE),
            tariff_mean = mean(w_tariff, na.rm = TRUE),    .groups = "drop"  )



hs2_desc <- data.frame(
  hs2 = sprintf("%02d", 1:24),
  description = c(
    "Live animals",
    "Meat and edible meat offal",
    "Fish and seafood",
    "Dairy, eggs, honey",
    "Other animal products",
    "Live trees and plants",
    "Vegetables",
    "Fruits and nuts",
    "Coffee, tea, spices",
    "Cereals",
    "Milling products; malt; starch",
    "Oil seeds and oleaginous fruits",
    "Gums, resins, vegetable saps",
    "Other vegetable materials",
    "Animal & vegetable fats and oils",
    "Prepared meat, fish",
    "Sugar and sugar confectionery",
    "Cocoa and cocoa preparations",
    "Preparations of cereals & bakery",
    "Processed vegetables, fruit, nuts",
    "Misc. edible preparations",
    "Beverages",
    "Animal feed, residues",
    "Tobacco and substitutes"  ),  stringsAsFactors = FALSE)
hs2_desc$hs2 <- as.numeric(hs2_desc$hs2)
summary <- summary %>%  left_join(hs2_desc, by = "hs2")

library(writexl)
write_xlsx( list("hs2_summary" = summary),  path = file.path(exp, "hs2_summary_Ag_boot.xlsx"))










 