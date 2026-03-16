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

OUT_PLOT_DIR <- file.path(ROOT, "output/Compare_values/yearly/plot", as.character(BASE_YEAR))
dir.create(OUT_PLOT_DIR, recursive = TRUE, showWarnings = FALSE)

################################################################################
# 1) Load data
################################################################################
US <- read_csv(file.path(ROOT,paste0("output/Compare_values/yearly/robust/US_ln_NTMs_base_", BASE_YEAR, "_FE_boot_hs4.csv") ))


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
         diff_ln_AVE_FE_wmean  = ifelse(diff_ln_AVE_FE_wmean  < FE_w_q | diff_ln_AVE_FE_wmean  > FE_w_qH,NA, diff_ln_AVE_FE_wmean)  )



################################################################################
# 3) Filter to chosen sector
################################################################################
US_dta <- US1 %>% filter(sector == SECTOR)

################################################################################
# 4) Construct weights based on BASE_YEAR (draw == 1)
################################################################################
names(US_dta)


base_hs6 <- US_dta %>%  filter(year == BASE_YEAR) %>%
  group_by( hs_section, hs2, hs4) %>%
  summarise(Trade_value_USD_base = sum(Trade_value_USD, na.rm = TRUE), .groups = "drop")

# Sector-level weights (HS6 share of sector total)
w_sector <- base_hs6 %>%  group_by(hs4) %>%
  summarise(Trade_value_USD_base = sum(Trade_value_USD_base), .groups = "drop") %>%
  mutate(tot = sum(Trade_value_USD_base),
         weight_sector = if_else(tot > 0, Trade_value_USD_base / tot, 0)) %>%
  select(hs4, weight_sector)

# HS section weights
w_hs_sect <- base_hs6 %>%  group_by(hs_section, hs4) %>%
  summarise(Trade_value_USD_base = sum(Trade_value_USD_base), .groups = "drop_last") %>%
  mutate(tot = sum(Trade_value_USD_base),
         weight_hs_sect = if_else(tot > 0, Trade_value_USD_base / tot, 0)) %>%
  ungroup() %>%
  select(hs_section, hs4, weight_hs_sect)

# HS2 weights
w_hs2 <- base_hs6 %>%
  group_by(hs2, hs4) %>%
  summarise(Trade_value_USD_base = sum(Trade_value_USD_base), .groups = "drop_last") %>%
  mutate(tot = sum(Trade_value_USD_base),
         weight_hs2 = if_else(tot > 0, Trade_value_USD_base / tot, 0)) %>%
  ungroup() %>%
  select(hs2, hs4, weight_hs2)



US_dta <- US_dta %>%
  left_join(w_sector,  by = "hs4") %>%
  left_join(w_hs_sect, by = c("hs_section", "hs4")) %>%
  left_join(w_hs2,     by = c("hs2", "hs4"))

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
w_sector_chen <- US_dta %>%  filter(year == BASE_YEAR, draw == 1) %>%  group_by(hs2) %>%
  summarise(Trade_value_USD_base = sum(Trade_value_USD, na.rm = TRUE), .groups = "drop") %>%
  mutate(tot = sum(Trade_value_USD_base, na.rm = TRUE),
         weight_sector_chen = if_else(tot > 0, Trade_value_USD_base / tot, NA_real_)) %>%
  select(hs2, weight_sector_chen)

US_dta <- left_join(US_dta, w_sector_chen, by = "hs2")

# Chen HS-section weights (HS2 share within hs_section) using BASE_YEAR
w_hs_sect_chen <- US_dta %>%  filter(year == BASE_YEAR, draw == 1) %>%
  group_by(hs_section, hs2) %>%  summarise(Trade_value_USD_base = sum(Trade_value_USD, na.rm = TRUE), .groups = "drop_last") %>%
  group_by(hs_section) %>%  mutate(tot = sum(Trade_value_USD_base, na.rm = TRUE),
                                   weight_hs_sect_Chen = if_else(tot > 0, Trade_value_USD_base / tot, NA_real_)) %>%
  ungroup() %>%  select(hs_section, hs2, weight_hs_sect_Chen)

US_dta <- left_join(US_dta, w_hs_sect_chen, by = c("hs_section", "hs2"))

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
theme_trade <- theme_minimal(base_size = 14, base_family = "Times New Roman") +
  theme(
    panel.spacing.x = unit(1.2, "lines"),
    plot.title = element_text(size = 11, hjust = 0.5),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    legend.text  = element_text(size = 10),
    legend.title = element_text(size = 10)
  )

### PLOT THEME
{
  blue <- "#1f4e79"  # darker, prints well
  
  theme_paper <- function(base_size = 12, base_family = "Latin Modern Roman") {
    theme_bw(base_size = base_size, base_family = base_family) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "grey85", linewidth = 0.3),
        panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
        axis.title = element_text(),
        plot.margin = margin(5.5, 8, 5.5, 5.5),
        axis.title.x = element_text(margin = margin(t = 8)),
        panel.border = element_rect(
          linewidth = 0.6,
          color = "black"
        ),
        axis.ticks = element_line(
          linewidth = 0.6,
          color = "black"
        ),
        axis.ticks.length = unit(3.5, "pt")
      )
  }
  }### PLOT THEME
{
  blue <- "#1f4e79"  # darker, prints well
  
  theme_paper <- function(base_size = 12, base_family = "Latin Modern Roman") {
    theme_bw(base_size = base_size, base_family = base_family) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(color = "grey85", linewidth = 0.3),
        panel.grid.major.y = element_line(color = "grey85", linewidth = 0.3),
        axis.title = element_text(),
        plot.margin = margin(5.5, 8, 5.5, 5.5),
        axis.title.x = element_text(margin = margin(t = 8)),
        panel.border = element_rect(
          linewidth = 0.6,
          color = "black"
        ),
        axis.ticks = element_line(
          linewidth = 0.6,
          color = "black"
        ),
        axis.ticks.length = unit(3.5, "pt")
      )
  }
}

# ---- consistent legend keys across ALL plots ----
legend_breaks <- c("FE", "FE_demeaned", "FE_bench", "tariff", "Chen_et_al")

legend_labels <- c(  "FE"          = "FE",
                     "FE_demeaned" = "FE (demeaned)",
                     "FE_bench"    = "FE (benchmark)",
                     "tariff"      = "Tariff",
                     "Chen_et_al"  = "Chen et al.")

legend_colors <- c(  "FE"          = "blue",
                     "FE_demeaned" = "orange",
                     "FE_bench"    = "purple",
                     "tariff"      = "darkgreen",
                     "Chen_et_al"  = "red")



################################################################################
# A) Sector level plot
################################################################################
US_dta_w <- US_dta %>%  group_by(year, draw) %>%
  summarise(  w_FE        = weighted.mean(diff_ln_AVE_FE,               w = weight_sector, na.rm = TRUE),
              w_FE_bench  = weighted.mean(diff_ln_AVE_FE_bench,         w = weight_sector, na.rm = TRUE),
              w_FE_mean   = weighted.mean(diff_ln_AVE_FE_wmean,         w = weight_sector, na.rm = TRUE),
              
              w_FE_log        = weighted.mean(diff_ln_AVE_FE_log,       w = weight_sector, na.rm = TRUE),
              w_FE_log_bench  = weighted.mean(diff_ln_AVE_FE_log_bench, w = weight_sector, na.rm = TRUE),
              w_FE_log_mean   = weighted.mean(diff_ln_AVE_FE_log_wmean, w = weight_sector, na.rm = TRUE),
              
              w_chen     = weighted.mean(diff_ln_AVE_chen,              w = weight_sector_chen, na.rm = TRUE),
              w_tariff   = weighted.mean(.data[[TARIFF_VAR]], w = weight_sector, na.rm = TRUE),
              .groups = "drop"  ) %>%  mutate(w_chen = mean(w_chen, na.rm = TRUE)  )

US_dta_q <- US_dta_w %>%  group_by(year) %>%
  summarise(FE_mean  = mean(w_FE, na.rm = TRUE),
            FE_lo    = quantile(w_FE, 0.025, na.rm = TRUE),
            FE_hi    = quantile(w_FE, 0.975, na.rm = TRUE),
            
            FEb_mean = mean(w_FE_bench, na.rm = TRUE),
            FEb_lo   = quantile(w_FE_bench, 0.025, na.rm = TRUE),
            FEb_hi   = quantile(w_FE_bench, 0.975, na.rm = TRUE),
            
            FEm_mean = mean(w_FE_mean, na.rm = TRUE),
            FEm_lo   = quantile(w_FE_mean, 0.025, na.rm = TRUE),
            FEm_hi   = quantile(w_FE_mean, 0.975, na.rm = TRUE),
            
            FE_log_mean  = mean(w_FE_log, na.rm = TRUE),
            FEb_log_mean = mean(w_FE_log_bench, na.rm = TRUE),
            FEm_log_mean = mean(w_FE_log_mean, na.rm = TRUE),
            
            chen_mean   = mean(w_chen, na.rm = TRUE),
            tariff_mean = mean(w_tariff, na.rm = TRUE),
            
            .groups = "drop"  )

# If you only want Chen for 2018-2019 as before:
US_dta_q <- US_dta_q %>%mutate(chen_mean = if_else(year %in% c(2018:2019), chen_mean, NA_real_))

# Add BASE_YEAR row as zero baseline
US_dta_q <- bind_rows(US_dta_q, as_tibble(setNames(as.list(c(BASE_YEAR, rep(0, ncol(US_dta_q) - 1))), names(US_dta_q)))) %>% arrange(year)

p_sector <- ggplot(US_dta_q, aes(x = year)) +
  geom_line(aes(y = FE_mean,    color = "FE"),          linewidth = 1) +
  geom_line(aes(y = FEm_mean,   color = "FE_demeaned"), linewidth = 1) +
  geom_line(aes(y = FEb_mean,   color = "FE_bench"),    linewidth = 1) +
  geom_line(aes(y = tariff_mean,color = "tariff"),      linewidth = 1) +
  geom_line(aes(y = chen_mean,  color = "Chen_et_al"),  linetype = "dashed", linewidth = 1) +
  scale_color_manual(values = legend_colors,
                     breaks = legend_breaks,
                     labels = legend_labels,
                     name   = "Variables" ) +
  labs( title = paste0("Weighted average Δ ln(1+AVE) in ", SECTOR_LABEL, " (relative to ", BASE_YEAR, ")"),
        x = "Year", y = "Weighted Δ ln(1+AVE)") +
  theme_paper()
p_sector

ggsave( filename = file.path(OUT_PLOT_DIR, paste0("Compare_ln_AVE_", SECTOR, "_sector_weight_boot", ".png")),
        plot = p_sector, width = 8, height = 6, dpi = 300)



# bis for presentation;
p_sector <- ggplot(US_dta_q, aes(x = year)) +
  geom_line(aes(y = FEm_mean,   color = "FE_demeaned"), linewidth = 1) +
  geom_line(aes(y = tariff_mean,color = "tariff"),      linewidth = 1) +
  # geom_line(aes(y = chen_mean,  color = "Chen_et_al"),  linetype = "dashed", linewidth = 1) +
  scale_color_manual(values = legend_colors,
                     breaks = legend_breaks,
                     labels = legend_labels,
                     name   = "Variables" ) +
  labs( title = paste0("Weighted average Δ ln(1+AVE) in ", SECTOR_LABEL, " (relative to ", BASE_YEAR, ")"),
        x = "Year", y = "Weighted Δ ln(1+AVE)") +
  theme_trade
p_sector
ggsave( filename = file.path(OUT_PLOT_DIR, paste0("Compare_ln_AVE_", SECTOR, "_sector_weight_boot_prez", ".png")),
        plot = p_sector, width = 8, height = 6, dpi = 300)


################################################################################
# B) HS section level plot
################################################################################
US_dta_w_sect <- US_dta %>%  group_by(year, draw, hs_section) %>%
  summarise(w_FE        = weighted.mean(diff_ln_AVE_FE,               w = weight_hs_sect, na.rm = TRUE),
            w_FE_bench  = weighted.mean(diff_ln_AVE_FE_bench,         w = weight_hs_sect, na.rm = TRUE),
            w_FE_mean   = weighted.mean(diff_ln_AVE_FE_wmean,         w = weight_hs_sect, na.rm = TRUE),
            
            w_FE_log        = weighted.mean(diff_ln_AVE_FE_log,       w = weight_hs_sect, na.rm = TRUE),
            w_FE_log_bench  = weighted.mean(diff_ln_AVE_FE_log_bench, w = weight_hs_sect, na.rm = TRUE),
            w_FE_log_mean   = weighted.mean(diff_ln_AVE_FE_log_wmean, w = weight_hs_sect, na.rm = TRUE),
            
            w_chen     = weighted.mean(diff_ln_AVE_chen,              w = weight_hs_sect_Chen, na.rm = TRUE),
            w_tariff   = weighted.mean(.data[[TARIFF_VAR]], w = weight_hs_sect, na.rm = TRUE),            
            .groups = "drop"  ) %>%
  group_by(hs_section) %>%  mutate(w_chen = mean(w_chen, na.rm = TRUE)) %>%
  ungroup()

US_dta_q_sect <- US_dta_w_sect %>%  group_by(year, hs_section) %>%
  summarise(FE_mean  = mean(w_FE, na.rm = TRUE),
            FE_lo    = quantile(w_FE, 0.025, na.rm = TRUE),
            FE_hi    = quantile(w_FE, 0.975, na.rm = TRUE),
            
            FEb_mean = mean(w_FE_bench, na.rm = TRUE),
            FEb_lo   = quantile(w_FE_bench, 0.025, na.rm = TRUE),
            FEb_hi   = quantile(w_FE_bench, 0.975, na.rm = TRUE),
            
            FEm_mean = mean(w_FE_mean, na.rm = TRUE),
            FEm_lo   = quantile(w_FE_mean, 0.025, na.rm = TRUE),
            FEm_hi   = quantile(w_FE_mean, 0.975, na.rm = TRUE),
            
            tariff_mean = mean(w_tariff, na.rm = TRUE),
            chen_mean   = mean(w_chen, na.rm = TRUE),
            
            .groups = "drop") %>%  mutate(chen_mean = if_else(year %in% c(2018:2019), chen_mean, NA_real_))

# Add BASE_YEAR row (zeros) for each hs_section
cols0 <- setdiff(names(US_dta_q_sect), c("year", "hs_section"))
add_base <- US_dta_q_sect %>%
  distinct(hs_section) %>%
  mutate(year = BASE_YEAR) %>%
  mutate(!!!setNames(rep(list(0), length(cols0)), cols0))

US_dta_q_sect <- bind_rows(US_dta_q_sect, add_base) %>%
  arrange(hs_section, year)

# HS section descriptive labels
hs_section_labels <- c(
  "1" = "Animal products",
  "2" = "Vegetable products",
  "3" = "Animal/vegetable fats & oils",
  "4" = "Prepared foodstuffs/beverages/tobacco")

US_dta_q_sect <- US_dta_q_sect %>%
  mutate(hs_section_lab = if_else(
    hs_section %in% names(hs_section_labels),
    hs_section_labels[hs_section],
    as.character(hs_section)   # keep others unchanged
  )  )
    


p_hs_sect <- ggplot(US_dta_q_sect, aes(x = year)) +
  geom_line(aes(y = FE_mean,     color = "FE"),          linewidth = 1) +
  geom_line(aes(y = FEm_mean,    color = "FE_demeaned"), linewidth = 1) +
  geom_line(aes(y = FEb_mean,    color = "FE_bench"),    linewidth = 1) +
  geom_line(aes(y = tariff_mean, color = "tariff"),      linewidth = 1) +
  geom_line(aes(y = chen_mean,   color = "Chen_et_al"),  linetype = "dashed", linewidth = 1) +
  scale_color_manual(values = legend_colors,
                     breaks = legend_breaks,
                     labels = legend_labels,
                     name   = "Variables") +
  facet_wrap(~ hs_section_lab, scales = "free_y") +
  labs(
    title = paste0("Weighted average Δ ln(1+AVE) in ", SECTOR_LABEL,
                   " by HS section (relative to ", BASE_YEAR, ")"),
    x = "Year",
    y = "Weighted Δ ln(1+AVE)"
  ) +
  theme_trade

p_hs_sect

ggsave(  filename = file.path(OUT_PLOT_DIR, paste0("Compare_ln_AVE_", SECTOR, "_hs_section_weight_boot" ,".png")),
         plot = p_hs_sect, width = 9, height = 6, dpi = 300)


# bis for presentation;
p_hs_sect <- ggplot(US_dta_q_sect, aes(x = year)) +
  geom_line(aes(y = FEm_mean,    color = "FE_demeaned"), linewidth = 1) +
  geom_line(aes(y = tariff_mean, color = "tariff"),      linewidth = 1) +
  # geom_line(aes(y = chen_mean,  color = "Chen_et_al"),  linetype = "dashed", linewidth = 1) +
  scale_color_manual(values = legend_colors,
                     breaks = legend_breaks,
                     labels = legend_labels,
                     name   = "Variables"  ) +
  facet_wrap(~ hs_section_lab) +
  labs(title = paste0("Weighted average Δ ln(1+AVE) in ", SECTOR_LABEL, " by HS section (relative to ", BASE_YEAR, ")"),
       x = "Year", y = "Weighted Δ ln(1+AVE)"  ) +
  theme_trade

p_hs_sect

ggsave(  filename = file.path(OUT_PLOT_DIR, paste0("Compare_ln_AVE_", SECTOR, "_hs_section_weight_boot_prez" ,".png")),
         plot = p_hs_sect, width = 9, height = 6, dpi = 300)

