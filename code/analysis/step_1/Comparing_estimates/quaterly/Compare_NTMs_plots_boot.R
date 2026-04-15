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
SECTOR    <- "Ag"   # e.g. "Ag", "Manuf", etc.
# SECTOR    <- "Manu" 


# nice label used in titles/filenames
sector_label_map <- c("Ag" = "Agriculture", "Manuf" = "Manufacturing")
SECTOR_LABEL <- unname(sector_label_map[SECTOR])
if (is.na(SECTOR_LABEL)) SECTOR_LABEL <- SECTOR  # fallback

# directories
ROOT <- "/data/sikeme/TRADE/US_CHN_TradeWar_git"
setwd(ROOT)

OUT_PLOT_DIR <- file.path(ROOT, "output/Compare_values/quaterly/plot", as.character(BASE_YEAR))
dir.create(OUT_PLOT_DIR, recursive = TRUE, showWarnings = FALSE)

################################################################################
# 1) Load data
################################################################################
US <- read_csv(file.path(ROOT,paste0("output/Compare_values/quaterly/US_ln_NTMs_base_", BASE_YEAR, "_FE.csv") ))
table(US$year)

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

# US1 <- US
table(US1$year)
table(US1$quarter)
summary(US1)

# balance the panel 

################################################################################
# 3) Filter to chosen sector
################################################################################
US_dta <- US1 %>% filter(sector == SECTOR)
summary(US1)
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
w_sector_chen <- US_dta %>%  filter(year == BASE_YEAR) %>%  group_by(hs2) %>%
  summarise(Trade_value_USD_base = sum(Trade_value_USD, na.rm = TRUE), .groups = "drop") %>%
  mutate(tot = sum(Trade_value_USD_base, na.rm = TRUE),
         weight_sector_chen = if_else(tot > 0, Trade_value_USD_base / tot, NA_real_)) %>%
  select(hs2, weight_sector_chen)

US_dta <- left_join(US_dta, w_sector_chen, by = "hs2")

# Chen HS-section weights (HS2 share within hs_section) using BASE_YEAR
w_hs_sect_chen <- US_dta %>%  filter(year == BASE_YEAR) %>%
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
unique(US_dta$quarter)
names(US_dta)

# pick tariff var: 
TARIFF_VAR <- paste0("diff_log_tariff")

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
US_dta_w <- US_dta %>%  group_by(year, quarter) %>%
  summarise(  FE_mean   = weighted.mean(diff_ln_AVE_FE,               w = weight_sector, na.rm = TRUE),
              FEb_mean  = weighted.mean(diff_ln_AVE_FE_bench,         w = weight_sector, na.rm = TRUE),
              FEm_mean  = weighted.mean(diff_ln_AVE_FE_wmean,         w = weight_sector, na.rm = TRUE),
              
              chen_mean     = weighted.mean(diff_ln_AVE_chen,              w = weight_sector_chen, na.rm = TRUE),
              tariff_mean   = weighted.mean(.data[[TARIFF_VAR]], w = weight_sector, na.rm = TRUE),
              .groups = "drop"  ) %>%  mutate(chen_mean = mean(chen_mean, na.rm = TRUE)  )


# If you only want Chen for 2018-2019 as before:
US_dta_q <- US_dta_w %>%mutate(chen_mean = if_else(year %in% c(2018:2019), chen_mean, NA_real_))


# create a date variable:
US_dta_q <- US_dta_q |>  mutate(date = as.Date(paste(year, quarter * 3 - 2, "01", sep = "-"), "%Y-%m-%d"))

base_rows <- tibble( year        = BASE_YEAR,  quarter     = 1:4,
  date        = as.Date(paste(BASE_YEAR, (1:4) * 3 - 2, "01", sep = "-")),
  FE_mean     = 0,  FEb_mean    = 0,  FEm_mean    = 0,  chen_mean   = 0,  tariff_mean = 0)

US_dta_q <- bind_rows(US_dta_q, base_rows) |> arrange(date)

p_sector <- ggplot(US_dta_q, aes(x = date)) +
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
  theme_trade
p_sector

ggsave( filename = file.path(OUT_PLOT_DIR, paste0("Compare_ln_AVE_", SECTOR, "_sector_weight_boot", ".png")),
        plot = p_sector, width = 8, height = 6, dpi = 300)



# bis for presentation;
p_sector <- ggplot(US_dta_q, aes(x = date)) +
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
US_dta_q_sect <- US_dta %>%  group_by(year, quarter, hs_section) %>%
  summarise(FE_mean        = weighted.mean(diff_ln_AVE_FE,               w = weight_hs_sect, na.rm = TRUE),
            FEb_mean  = weighted.mean(diff_ln_AVE_FE_bench,         w = weight_hs_sect, na.rm = TRUE),
            FEm_mean   = weighted.mean(diff_ln_AVE_FE_wmean,         w = weight_hs_sect, na.rm = TRUE),
            
            chen_mean     = weighted.mean(diff_ln_AVE_chen,              w = weight_hs_sect_Chen, na.rm = TRUE),
            tariff_mean   = weighted.mean(.data[[TARIFF_VAR]], w = weight_hs_sect, na.rm = TRUE),            
            .groups = "drop"  ) %>%
  group_by(hs_section) %>%  mutate(chen_mean = mean(chen_mean, na.rm = TRUE)) %>%
  ungroup()





#  create a date variable:
US_dta_q_sect <- US_dta_q_sect |>  mutate(date = as.Date(paste(year, quarter * 3 - 2, "01", sep = "-"), "%Y-%m-%d"))

base_rows <- tibble( year        = BASE_YEAR,  quarter     = 1:4,
                     date        = as.Date(paste(BASE_YEAR, (1:4) * 3 - 2, "01", sep = "-")),
                     FE_mean     = 0,  FEb_mean    = 0,  FEm_mean    = 0,  chen_mean   = 0,  tariff_mean = 0)

US_dta_q_sect <- bind_rows(US_dta_q_sect, base_rows) |> arrange(date)

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

p_hs_sect <- ggplot(US_dta_q_sect, aes(x = date)) +
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
    y = "Weighted Δ ln(1+AVE)"  ) +
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

