################################################################################
# Gravity regression analysis: residual approach (parameterized)
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
library(Hmisc)
library(haven)
library(sfaR)
library(frontier)


################################################################################
# USER CHOICES (change these only)
################################################################################
BASE_YEAR <- 2017
# SECTOR    <- "Ag"   # e.g. "Ag", "Manuf", etc.
SECTOR    <- "Manu" 


# nice label used in titles/filenames
sector_label_map <- c("Ag" = "Agriculture", "Manuf" = "Manufacturing")
SECTOR_LABEL <- unname(sector_label_map[SECTOR])
if (is.na(SECTOR_LABEL)) SECTOR_LABEL <- SECTOR  # fallback

# directories
ROOT <- "/data/sikeme/TRADE/US_CHN_TradeWar_git"
setwd(ROOT)

OUT_PLOT_DIR <- file.path(ROOT, "output/Compare_values/yearly/robust/elast/plot", as.character(BASE_YEAR), "boot")
dir.create(OUT_PLOT_DIR, recursive = TRUE, showWarnings = FALSE)

################################################################################
# 1) Load data
################################################################################
US <- read_csv(file.path(ROOT,paste0("output/Compare_values/yearly/robust/elast/US_ln_NTMs_base_", BASE_YEAR, "_FE_boot_hs4_elast.csv") ))
sectors_hs4 <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/crosswalk/clean_HS4_sub_sector_edit.csv")


################################################################################
# 2) Drop extreme values (1st/99th percentile)
################################################################################
quant <- 0.05

FE_q   <- quantile(US$diff_ln_AVE_FE,        quant,      na.rm = TRUE)
FE_qH  <- quantile(US$diff_ln_AVE_FE,        1 - quant,  na.rm = TRUE)
FE_b_q <- quantile(US$diff_ln_AVE_FE_bench,  quant,      na.rm = TRUE)
FE_b_qH<- quantile(US$diff_ln_AVE_FE_bench,  1 - quant,  na.rm = TRUE)
FE_w_q <- quantile(US$diff_ln_AVE_FE_wmean,  quant,      na.rm = TRUE)
FE_w_qH<- quantile(US$diff_ln_AVE_FE_wmean,  1 - quant,  na.rm = TRUE)

cat("FE [1%,99%]:", FE_q, FE_qH,"| Bench [1%,99%]:", FE_b_q, FE_b_qH, "| Wmean [1%,99%]:", FE_w_q, FE_w_qH, "\n")

US1 <- US %>%
  mutate(diff_ln_AVE_FE        = ifelse(diff_ln_AVE_FE        < FE_q   | diff_ln_AVE_FE        > FE_qH,  NA, diff_ln_AVE_FE),
         diff_ln_AVE_FE_bench  = ifelse(diff_ln_AVE_FE_bench  < FE_b_q | diff_ln_AVE_FE_bench  > FE_b_qH,NA, diff_ln_AVE_FE_bench),
         diff_ln_AVE_FE_wmean  = ifelse(diff_ln_AVE_FE_wmean  < FE_w_q | diff_ln_AVE_FE_wmean  > FE_w_qH,NA, diff_ln_AVE_FE_wmean),
         
         diff_ln_AVE_FE_Chen        = ifelse(diff_ln_AVE_FE_Chen        < FE_q   | diff_ln_AVE_FE_Chen        > FE_qH,  NA, diff_ln_AVE_FE_Chen),
         diff_ln_AVE_FE_bench_Chen  = ifelse(diff_ln_AVE_FE_bench_Chen  < FE_b_q | diff_ln_AVE_FE_bench_Chen  > FE_b_qH,NA, diff_ln_AVE_FE_bench_Chen),
         diff_ln_AVE_FE_wmean_Chen  = ifelse(diff_ln_AVE_FE_wmean_Chen  < FE_w_q | diff_ln_AVE_FE_wmean_Chen  > FE_w_qH,NA, diff_ln_AVE_FE_wmean_Chen)         )



################################################################################
# 3) Add susbector 
################################################################################

sectors_hs4$hs4 <- as.numeric(sectors_hs4$hs4)
US1$hs4 <- as.numeric(US1$hs4)

# join
US1 <- left_join(US1, sectors_hs4) 
colSums(is.na(US1))
test <- US1 %>% filter(is.na(subsector))
unique(test$hs4)
colSums(is.na(US1))

US_dta <- US1

################################################################################
# 4) Construct weights based on BASE_YEAR (draw == 1)
################################################################################
names(US_dta)
unique(US_dta$draw)

base_hs6 <- US_dta %>%  filter(year == BASE_YEAR & draw ==1) %>%
  group_by( hs_section, hs2, hs4,subsector) %>%
  summarise(Trade_value_USD_base = sum(Trade_value_USD, na.rm = TRUE), .groups = "drop")

# Sector-level weights (HS6 share of sector total)
w_sector <- base_hs6 %>%  group_by(hs4) %>%
  summarise(Trade_value_USD_base = sum(Trade_value_USD_base), .groups = "drop") %>%
  mutate(tot = sum(Trade_value_USD_base),
         weight_sector = if_else(tot > 0, Trade_value_USD_base / tot, 0)) %>%
  select(hs4, weight_sector)

# hs_sectionr weights
w_hs_sect <- base_hs6 %>%  group_by(subsector, hs4) %>%
  summarise(Trade_value_USD_base = sum(Trade_value_USD_base), .groups = "drop_last") %>%
  mutate(tot = sum(Trade_value_USD_base),
         weight_hs_sect = if_else(tot > 0, Trade_value_USD_base / tot, 0)) %>%
  ungroup() %>%  select(subsector, hs4, weight_hs_sect)


US_dta <- US_dta %>%  left_join(w_sector,  by = "hs4") %>%
  left_join(w_hs_sect, by = c("subsector", "hs4"))

################################################################################
# 5) Add Chen et al estimates + Chen weights (based on BASE_YEAR)
################################################################################

Chen <- read_csv(file.path(ROOT, "data/chen_NTB_tariff/hs2_agriculture_manufacturing_clean.csv"))

Chen <- Chen %>%  select(-Country, -ISO3_Code) %>%
  rename( hs2 = HS2, Chen_US_import_share = US_import_share, 
          diff_log_tariff_Chen = tau_tariff_CHN,diff_ln_AVE_chen     = tau_NTB  )

US_dta$hs2 <- as.numeric(US_dta$hs2)
US_dta <- left_join(US_dta, Chen, by = "hs2")

# Chen sector-level weights (HS2 share of sector total) using BASE_YEAR
w_sector_chen <- US_dta %>%  filter(year == BASE_YEAR & draw == 1) %>%  group_by(hs2) %>%
  summarise(Trade_value_USD_base = sum(Trade_value_USD, na.rm = TRUE), .groups = "drop") %>%
  mutate(tot = sum(Trade_value_USD_base, na.rm = TRUE),
         weight_sector_chen = if_else(tot > 0, Trade_value_USD_base / tot, NA_real_)) %>%
  select(hs2, weight_sector_chen)

US_dta <- left_join(US_dta, w_sector_chen, by = "hs2")

# Chen HS-section weights (HS2 share within subsector) using BASE_YEAR
w_hs_sect_chen <- US_dta %>%  filter(year == BASE_YEAR & draw == 1) %>%
  group_by(subsector, hs2) %>%  summarise(Trade_value_USD_base = sum(Trade_value_USD, na.rm = TRUE), .groups = "drop_last") %>%
  group_by(subsector) %>%  mutate(tot = sum(Trade_value_USD_base, na.rm = TRUE),
                                  weight_hs_sect_Chen = if_else(tot > 0, Trade_value_USD_base / tot, NA_real_)) %>%
  ungroup() %>%  select(subsector, hs2, weight_hs_sect_Chen)

US_dta <- left_join(US_dta, w_hs_sect_chen, by = c("subsector", "hs2"))


################################################################################
# Keep only post-base years
################################################################################
US_dta <- US_dta %>% filter(year > BASE_YEAR)
unique(US_dta$year)

# pick tariff var: 
TARIFF_VAR <- paste0("diff_log_tariff_", BASE_YEAR)

################################################################################
# Theme
################################################################################
# - panel.border added so every facet panel is boxed
# - strip.background/strip.text tidied so facet labels sit cleanly inside the box
# - plot.title set to element_blank() so labs(title = ...) calls below are
#   suppressed even if a title string is still passed
theme_trade <- theme_minimal(base_size = 14, base_family = "Times New Roman") +
  theme(
    panel.spacing.x  = unit(1.2, "lines"),
    plot.title       = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.6),
    strip.background = element_rect(fill = "grey92", color = "black", linewidth = 0.6),
    strip.text       = element_text(size = 12, face = "bold", color = "black"),
    axis.text.x  = element_text(size = 11),
    axis.text.y  = element_text(size = 11),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "top",
    legend.text  = element_text(size = 12),
    # legend.title = element_text(size = 12)  
    legend.title = element_blank())

### PLOT THEME


# ---- consistent legend keys across ALL plots ----
legend_breaks <- c("FE", "FE_demeaned", "FE_bench", "tariff", "Chen_et_al")

legend_labels <- c(  "FE"          = "NTM (FE)",
                     "FE_demeaned" = "NTM (demeaned)",
                     "FE_bench"    = "NTM (benchmark)",
                     "tariff"      = "Tariff",
                     "Chen_et_al"  = "Chen et al.")

# Revised palette: Okabe-Ito colorblind-safe set, minus the yellow swatch,
# chosen so saturation/lightness are matched across series (no single
# color reads as louder or "hotter" than the rest, which is what was
# happening with the old orange). Chen et al. stays black/dashed so it
# reads as an external benchmark rather than another estimated series.
legend_colors <- c(  "FE"          = "#D55E00",   # blue
                     "FE_demeaned" = "#0072B2",   # vermillion (muted, not orange/yellow)
                     "FE_bench"    = "#009E73",   # teal green
                     "tariff"      = "#7F7F7F",   # neutral gray
                     "Chen_et_al"  = "#7B3294")   # black




################################################################################
# A) Subsector
################################################################################
US_dta_w <- US_dta %>%  group_by(year, draw, subsector) %>%
  summarise(  w_FE        = weighted.mean(diff_ln_AVE_FE,               w = weight_sector, na.rm = TRUE),
              w_FE_bench  = weighted.mean(diff_ln_AVE_FE_bench,         w = weight_sector, na.rm = TRUE),
              w_FE_mean   = weighted.mean(diff_ln_AVE_FE_wmean,         w = weight_sector, na.rm = TRUE),
              
              w_FE_log        = weighted.mean(diff_ln_AVE_FE_log,       w = weight_sector, na.rm = TRUE),
              w_FE_log_bench  = weighted.mean(diff_ln_AVE_FE_log_bench, w = weight_sector, na.rm = TRUE),
              w_FE_log_mean   = weighted.mean(diff_ln_AVE_FE_log_wmean, w = weight_sector, na.rm = TRUE),
              
              w_FE_Chen        = weighted.mean(diff_ln_AVE_FE_Chen,               w = weight_sector, na.rm = TRUE),
              w_FE_bench_Chen  = weighted.mean(diff_ln_AVE_FE_bench_Chen,         w = weight_sector, na.rm = TRUE),
              w_FE_mean_Chen   = weighted.mean(diff_ln_AVE_FE_wmean_Chen,         w = weight_sector, na.rm = TRUE),
              
              w_FE_log_Chen        = weighted.mean(diff_ln_AVE_FE_log_Chen,       w = weight_sector, na.rm = TRUE),
              w_FE_log_bench_Chen  = weighted.mean(diff_ln_AVE_FE_log_bench_Chen, w = weight_sector, na.rm = TRUE),
              w_FE_log_mean_Chen   = weighted.mean(diff_ln_AVE_FE_log_wmean_Chen, w = weight_sector, na.rm = TRUE),
              
              w_chen     = weighted.mean(diff_ln_AVE_chen,              w = weight_sector_chen, na.rm = TRUE),
              w_tariff   = weighted.mean(.data[[TARIFF_VAR]], w = weight_sector, na.rm = TRUE),
              .groups = "drop"  ) # %>%  mutate(w_chen = mean(w_chen, na.rm = TRUE)  )

US_dta_w_sect <- US_dta_w %>%
  group_by(year, subsector) %>%
  summarise(
    FE_mean  = mean(w_FE, na.rm = TRUE),
    FE_lo    = quantile(w_FE, 0.025, na.rm = TRUE),
    FE_hi    = quantile(w_FE, 0.975, na.rm = TRUE),
    
    FEb_mean = mean(w_FE_bench, na.rm = TRUE),
    FEb_lo   = quantile(w_FE_bench, 0.025, na.rm = TRUE),
    FEb_hi   = quantile(w_FE_bench, 0.975, na.rm = TRUE),
    
    FEm_mean = mean(w_FE_mean, na.rm = TRUE),
    FEm_lo   = quantile(w_FE_mean, 0.025, na.rm = TRUE),
    FEm_hi   = quantile(w_FE_mean, 0.975, na.rm = TRUE),
    
    FE_log_mean  = mean(w_FE_log, na.rm = TRUE),
    FE_log_lo    = quantile(w_FE_log, 0.025, na.rm = TRUE),
    FE_log_hi    = quantile(w_FE_log, 0.975, na.rm = TRUE),
    
    FEb_log_mean = mean(w_FE_log_bench, na.rm = TRUE),
    FEb_log_lo   = quantile(w_FE_log_bench, 0.025, na.rm = TRUE),
    FEb_log_hi   = quantile(w_FE_log_bench, 0.975, na.rm = TRUE),
    
    FEm_log_mean = mean(w_FE_log_mean, na.rm = TRUE),
    FEm_log_lo   = quantile(w_FE_log_mean, 0.025, na.rm = TRUE),
    FEm_log_hi   = quantile(w_FE_log_mean, 0.975, na.rm = TRUE),
    
    FE_mean_Chen  = mean(w_FE_Chen, na.rm = TRUE),
    FE_lo_Chen    = quantile(w_FE_Chen, 0.025, na.rm = TRUE),
    FE_hi_Chen    = quantile(w_FE_Chen, 0.975, na.rm = TRUE),
    
    FEb_mean_Chen = mean(w_FE_bench_Chen, na.rm = TRUE),
    FEb_lo_Chen   = quantile(w_FE_bench_Chen, 0.025, na.rm = TRUE),
    FEb_hi_Chen   = quantile(w_FE_bench_Chen, 0.975, na.rm = TRUE),
    
    FEm_mean_Chen = mean(w_FE_mean_Chen, na.rm = TRUE),
    FEm_lo_Chen   = quantile(w_FE_mean_Chen, 0.025, na.rm = TRUE),
    FEm_hi_Chen   = quantile(w_FE_mean_Chen, 0.975, na.rm = TRUE),
    
    FE_log_mean_Chen  = mean(w_FE_log_Chen, na.rm = TRUE),
    FE_log_lo_Chen    = quantile(w_FE_log_Chen, 0.025, na.rm = TRUE),
    FE_log_hi_Chen    = quantile(w_FE_log_Chen, 0.975, na.rm = TRUE),
    
    FEb_log_mean_Chen = mean(w_FE_log_bench_Chen, na.rm = TRUE),
    FEb_log_lo_Chen   = quantile(w_FE_log_bench_Chen, 0.025, na.rm = TRUE),
    FEb_log_hi_Chen   = quantile(w_FE_log_bench_Chen, 0.975, na.rm = TRUE),
    
    FEm_log_mean_Chen = mean(w_FE_log_mean_Chen, na.rm = TRUE),
    FEm_log_lo_Chen   = quantile(w_FE_log_mean_Chen, 0.025, na.rm = TRUE),
    FEm_log_hi_Chen   = quantile(w_FE_log_mean_Chen, 0.975, na.rm = TRUE),
    
    chen_mean   = mean(w_chen, na.rm = TRUE),
    tariff_mean = mean(w_tariff, na.rm = TRUE),
    
    .groups = "drop"
  )

# If you only want Chen for 2018-2019 as before:
US_dta_w_sect <- US_dta_w_sect %>%mutate(chen_mean = if_else(year %in% c(2018:2019), chen_mean, NA_real_))


# Add BASE_YEAR row (zeros) for each subsector
cols0 <- setdiff(names(US_dta_w_sect), c("year", "subsector"))
add_base <- US_dta_w_sect %>%  distinct(subsector) %>%
  mutate(year = BASE_YEAR) %>%
  mutate(!!!setNames(rep(list(0), length(cols0)), cols0))

US_dta_w_sect <- bind_rows(US_dta_w_sect, add_base) %>%  arrange(subsector, year)

unique(US_dta_w_sect$subsector)
US_dta_w_sect <- US_dta_w_sect %>% filter(subsector %in% c("crop", "livestock", "nonag"))


# HS section descriptive labels - capitalized for the paper
subsector_labels <- c(
  "crop" = "Crop",
  "livestock" = "Livestock",
  "nonag" = "Non-ag")

US_dta_w_sect <- US_dta_w_sect %>%
  mutate(hs_section_lab = if_else(
    subsector %in% names(subsector_labels),
    subsector_labels[subsector],    as.character(subsector)     )  )




################################################################################
# PPML 
################################################################################
# plot Hs4 level elast
# NOTE: title removed from labs() (kept the string commented out for reference)

p_hs_sect <- ggplot(US_dta_w_sect, aes(x = year)) +
  geom_line(aes(y = FE_mean,     color = "FE"),          linewidth = 1) +
  geom_line(aes(y = FEm_mean,    color = "FE_demeaned"), linewidth = 1) +
  geom_line(aes(y = FEb_mean,    color = "FE_bench"),    linewidth = 1) +
  geom_line(aes(y = tariff_mean, color = "tariff"),      linewidth = 1) +
  # geom_line(aes(y = chen_mean,   color = "Chen_et_al"),  linetype = "dashed", linewidth = 1) +
  scale_color_manual(values = legend_colors,
                     breaks = legend_breaks,
                     labels = legend_labels,
                     name   = "Variables") +
  facet_wrap(~ hs_section_lab) +
  labs(# title = paste0("Weighted average ln(1+AVE) by sectors (relative to ", BASE_YEAR, ") using Soderbery level elasticities"),
    x = "Year",   y = "Weighted \u0394 ln(1+AVE)"  ) +
  theme_trade
p_hs_sect

ggsave(  filename = file.path(OUT_PLOT_DIR, paste0("/Compare_ln_AVE_subsector_weight_HS4_elast" ,".png")),
         plot = p_hs_sect, width = 10, height = 6, dpi = 300)


# bis for presentation;
p_hs_sect <- ggplot(US_dta_w_sect, aes(x = year)) +
  geom_line(aes(y = FEm_mean,    color = "FE_demeaned"), linewidth = 1) +
  geom_line(aes(y = tariff_mean, color = "tariff"),      linewidth = 1) +
  # geom_line(aes(y = chen_mean,  color = "Chen_et_al"),  linetype = "dashed", linewidth = 1) +
  scale_color_manual(values = legend_colors,
                     breaks = legend_breaks,
                     labels = legend_labels,
                     name   = "Variables"  ) +
  facet_wrap(~ hs_section_lab) +
  labs(# title = paste0("Weighted average ln(1+AVE) by sectors (relative to ", BASE_YEAR, ")"),
    x = "Year",   y = "Weighted \u0394 ln(1+AVE)"  ) +
  theme_trade
p_hs_sect

ggsave(  filename = file.path(OUT_PLOT_DIR, paste0("Compare_ln_AVE_subsector_weight_HS4_elast_prez" ,".png")),
         plot = p_hs_sect, width = 11, height = 6, dpi = 300)


################################################################################
# plot chen et al elastciities

p_hs_sect <- ggplot(US_dta_w_sect, aes(x = year)) +
  geom_line(aes(y = FE_mean,     color = "FE"),          linewidth = 1) +
  geom_line(aes(y = FEm_mean,    color = "FE_demeaned"), linewidth = 1) +
  geom_line(aes(y = FEb_mean,    color = "FE_bench"),    linewidth = 1) +
  geom_line(aes(y = tariff_mean, color = "tariff"),      linewidth = 1) +
  geom_line(aes(y = chen_mean,   color = "Chen_et_al"),  linetype = "dashed", linewidth = 1) +
  scale_color_manual(values = legend_colors,
                     breaks = legend_breaks,
                     labels = legend_labels,
                     name   = "Variables") +
  facet_wrap(~ hs_section_lab) +
  labs(# title = paste0("Weighted average ln(1+AVE) by sectors (relative to ", BASE_YEAR, ") using Soderbery level elasticities"),
    x = "Year",   y = "Weighted \u0394 ln(1+AVE)"  ) +
  theme_trade
p_hs_sect

ggsave(  filename = file.path(OUT_PLOT_DIR, paste0("Compare_ln_AVE_subsector_weight_chen_elast" ,".png")),
         plot = p_hs_sect, width = 10, height = 6, dpi = 300)


# bis for presentation;
p_hs_sect <- ggplot(US_dta_w_sect, aes(x = year)) +
  geom_line(aes(y = FEm_mean,    color = "FE_demeaned"), linewidth = 1) +
  geom_line(aes(y = tariff_mean, color = "tariff"),      linewidth = 1) +
  geom_line(aes(y = chen_mean,  color = "Chen_et_al"),  linetype = "dashed", linewidth = 1) +
  scale_color_manual(values = legend_colors,
                     breaks = legend_breaks,
                     labels = legend_labels,
                     name   = "Variables"  ) +
  facet_wrap(~ hs_section_lab) +
  labs(# title = paste0("Weighted average ln(1+AVE) by sectors (relative to ", BASE_YEAR, ")"),
    x = "Year",   y = "Weighted \u0394 ln(1+AVE)"  ) +
  theme_trade
p_hs_sect

ggsave(  filename = file.path(OUT_PLOT_DIR, paste0("Compare_ln_AVE_subsector_weight_chen_elast_prez" ,".png")),
         plot = p_hs_sect, width = 11, height = 6, dpi = 300)



################################################################################
# Log 
################################################################################
# plot Hs4 level elast

p_hs_sect <- ggplot(US_dta_w_sect, aes(x = year)) +
  geom_line(aes(y = FE_log_mean,     color = "FE"),          linewidth = 1) +
  geom_line(aes(y = FEm_log_mean,    color = "FE_demeaned"), linewidth = 1) +
  geom_line(aes(y = FEb_log_mean,    color = "FE_bench"),    linewidth = 1) +
  geom_line(aes(y = tariff_mean, color = "tariff"),      linewidth = 1) +
  geom_line(aes(y = chen_mean,   color = "Chen_et_al"),  linetype = "dashed", linewidth = 1) +
  scale_color_manual(values = legend_colors,
                     breaks = legend_breaks,
                     labels = legend_labels,
                     name   = "Variables") +
  facet_wrap(~ hs_section_lab) +
  labs(# title = paste0("Weighted average ln(1+AVE) by sectors (relative to ", BASE_YEAR, ") \n using HS4 level elasticities"),
    x = "Year",   y = "Weighted \u0394 ln(1+AVE)"  ) +
  theme_trade
p_hs_sect

ggsave(  filename = file.path(OUT_PLOT_DIR, paste0("Compare_ln_AVE_subsector_weight_log_HS4_elast" ,".png")),
         plot = p_hs_sect, width = 10, height = 6, dpi = 300)


# bis for presentation;
p_hs_sect <- ggplot(US_dta_w_sect, aes(x = year)) +
  geom_line(aes(y = FEm_log_mean,    color = "FE_demeaned"), linewidth = 1) +
  geom_line(aes(y = tariff_mean, color = "tariff"),      linewidth = 1) +
  # geom_line(aes(y = chen_mean,  color = "Chen_et_al"),  linetype = "dashed", linewidth = 1) +
  scale_color_manual(values = legend_colors,
                     breaks = legend_breaks,
                     labels = legend_labels,
                     name   = "Variables"  ) +
  facet_wrap(~ hs_section_lab) +
  labs(# title = paste0("Weighted average ln(1+AVE) by sectors (log specification, relative to ", BASE_YEAR, ")"),
    x = "Year",   y = "Weighted \u0394 ln(1+AVE)"  ) +
  theme_trade
p_hs_sect

ggsave(  filename = file.path(OUT_PLOT_DIR, paste0("Compare_ln_AVE_subsector_weight_log_HS4_elast_prez" ,".png")),
         plot = p_hs_sect, width = 11, height = 6, dpi = 300)


################################################################################
# plot chen et al elastciities
################################################################################
# with log
p_hs_sect <- ggplot(US_dta_w_sect, aes(x = year)) +
  geom_line(aes(y = FE_log_mean_Chen,     color = "FE"),          linewidth = 1) +
  geom_line(aes(y = FEm_log_mean_Chen,    color = "FE_demeaned"), linewidth = 1) +
  geom_line(aes(y = FEb_log_mean_Chen,    color = "FE_bench"),    linewidth = 1) +
  geom_line(aes(y = tariff_mean, color = "tariff"),      linewidth = 1) +
  geom_line(aes(y = chen_mean,   color = "Chen_et_al"),  linetype = "dashed", linewidth = 1) +
  scale_color_manual(values = legend_colors,
                     breaks = legend_breaks,
                     labels = legend_labels,
                     name   = "Variables") +
  facet_wrap(~ hs_section_lab) +
  labs(# title = paste0("Weighted average ln(1+AVE) by sectors (relative to ", BASE_YEAR, ") \n using Chen et al. elasticities (log specification)"),
    x = "Year",   y = "Weighted \u0394 ln(1+AVE)"  ) +
  theme_trade
p_hs_sect

ggsave(  filename = file.path(OUT_PLOT_DIR, paste0("Compare_ln_AVE_subsector_weight_log_chen_elast" ,".png")),
         plot = p_hs_sect, width = 10, height = 6, dpi = 300)


# with ppml

p_hs_sect <- ggplot(US_dta_w_sect, aes(x = year)) +
  geom_line(aes(y = FEm_mean,    color = "FE_demeaned"), linewidth = 1) +
  geom_line(aes(y = FEm_mean_Chen,    color = "Chen_et_al"),    linewidth = 1) +
  geom_line(aes(y = tariff_mean, color = "tariff"),      linewidth = 1) +
  # geom_line(aes(y = chen_mean,   color = "Chen_et_al"),  linetype = "dashed", linewidth = 1) +
  scale_color_manual(values = legend_colors,
                     breaks = legend_breaks,
                     labels = legend_labels,
                     name   = "Variables") +
  facet_wrap(~ hs_section_lab) +
  labs(# title = paste0("Weighted average ln(1+AVE) by sectors (relative to ", BASE_YEAR, ") using Soderbery level elasticities"),
    x = "Year",   y = "Weighted \u0394 ln(1+AVE)"  ) +
  theme_trade
p_hs_sect

ggsave(  filename = file.path(OUT_PLOT_DIR, paste0("/Compare_ln_AVE_subsector_weight_HS4_chen_sodeberry_elast" ,".png")),
         plot = p_hs_sect, width = 10, height = 6, dpi = 300)


# 
# 
# 
# 
# ################################################################################
# # correlation between measures
# ################################################################################
# library(ggcorrplot)
# 
# 
# # across three specifications 
# corr_vars <- c( "diff_ln_AVE_FE",  "diff_ln_AVE_FE_bench",
#   "diff_ln_AVE_FE_wmean",  "diff_log_tariff_2017",  "diff_ln_AVE_chen")
# 
# corr_matrix <- US_dta %>%  select(all_of(corr_vars)) %>%  cor(use = "pairwise.complete.obs")
# 
# corr_labels <- c(
#   "FE",
#   "FE_bench",
#   "FE_wmean",
#   "tariff_2017",
#   "AVE_chen")
# 
# rownames(corr_matrix) <- corr_labels
# colnames(corr_matrix) <- corr_labels
# 
# ggcorrplot(corr_matrix,
#            method   = "square",
#            type     = "lower",
#            lab      = TRUE,
#            lab_size = 3,
#            colors   = c("#d73027", "white", "#4575b4"),
#            title    = "Correlation across AVE measures",
#            ggtheme  = theme_minimal(base_size = 11))
# 
# 
# 
# # for weighted mean
# corr_matrix <- US_dta %>%
#   select(all_of(corr_vars)) %>%
#   cor(use = "pairwise.complete.obs")
# 
# corr_labels <- c(
#   "FE_wmean",
#   "FE_log_wmean",
#   "FE_wmean_Chen",
#   "FE_log_wmean_Chen",
#   "tariff_2017",
#   "AVE_chen"
# )
# 
# rownames(corr_matrix) <- corr_labels
# colnames(corr_matrix) <- corr_labels
# 
# ggcorrplot(corr_matrix,
#            method   = "square",
#            type     = "lower",
#            lab      = TRUE,
#            lab_size = 3,
#            colors   = c("#d73027", "white", "#4575b4"),
#            title    = "Correlation across AVE measures",
#            ggtheme  = theme_minimal(base_size = 11))
# 
# 
# ################################################################################
# 
# 
# p1 <- quantile(US_dta$diff_ln_AVE_FE_wmean,      0.01, na.rm = TRUE)
# p99 <- quantile(US_dta$diff_ln_AVE_FE_wmean,     0.99, na.rm = TRUE)
# p1_chen <- quantile(US_dta$diff_ln_AVE_FE_wmean_Chen,  0.01, na.rm = TRUE)
# p99_chen <- quantile(US_dta$diff_ln_AVE_FE_wmean_Chen, 0.99, na.rm = TRUE)
# 
# ggplot(US_dta %>% filter(
#   diff_ln_AVE_FE_wmean      >= p1    & diff_ln_AVE_FE_wmean      <= p99,
#   diff_ln_AVE_FE_wmean_Chen >= p1_chen & diff_ln_AVE_FE_wmean_Chen <= p99_chen
# ),
# aes(x = diff_ln_AVE_FE_wmean, y = diff_ln_AVE_FE_wmean_Chen)) +
#   geom_point(alpha = 0.3, size = 0.1) +
#   geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
#   labs(
#     x = "AVE FE (own elasticities, demeaned)",
#     y = "AVE FE (Chen elasticities, demeaned)",
#     title = "Comparison of AVE estimates: own vs Chen elasticities"  ) +
#   theme_minimal()
# 
# 
# library(patchwork)
# 
# # Helper function to avoid repeating code
# plot_comparison <- function(data, x_var, y_var, x_lab, y_lab, title) {
#   p1    <- quantile(data[[x_var]], 0.01, na.rm = TRUE)
#   p99   <- quantile(data[[x_var]], 0.99, na.rm = TRUE)
#   p1_y  <- quantile(data[[y_var]], 0.01, na.rm = TRUE)
#   p99_y <- quantile(data[[y_var]], 0.99, na.rm = TRUE)
#   
#   data %>%
#     filter(.data[[x_var]] >= p1   & .data[[x_var]] <= p99,
#            .data[[y_var]] >= p1_y & .data[[y_var]] <= p99_y) %>%
#     ggplot(aes(x = .data[[x_var]], y = .data[[y_var]])) +
#     geom_point(alpha = 0.3, size = 0.8) +
#     geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
#     labs(x = x_lab, y = y_lab, title = title) +
#     theme_minimal()
# }
# 
# p1 <- plot_comparison(US_dta,
#                       x_var = "diff_ln_AVE_FE",
#                       y_var = "diff_ln_AVE_FE_Chen",
#                       x_lab = "AVE FE (HS4 elasticities)",
#                       y_lab = "AVE FE (Chen elasticities)",
#                       title = "No demeaning")
# 
# p2 <- plot_comparison(US_dta,
#                       x_var = "diff_ln_AVE_FE_bench",
#                       y_var = "diff_ln_AVE_FE_bench_Chen",
#                       x_lab = "AVE FE bench (HS4 elasticities)",
#                       y_lab = "AVE FE bench (Chen elasticities)",
#                       title = "Benchmark demeaning")
# 
# p3 <- plot_comparison(US_dta,
#                       x_var = "diff_ln_AVE_FE_wmean",
#                       y_var = "diff_ln_AVE_FE_wmean_Chen",
#                       x_lab = "AVE FE wmean (HS4 elasticities)",
#                       y_lab = "AVE FE wmean (Chen elasticities)",
#                       title = "Weighted mean demeaning")
# 
# plot <- p1 | p2 | p3
# plot
# 
# ggsave(  filename = file.path(OUT_PLOT_DIR, paste0("Compare_elastcities" ,".png")),
#          plot = p_hs_sect, width = 11, height = 6, dpi = 300)
# 
# 
# 
# 
# 
# 
# 
