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
BASE_YEAR <- 2015
# SECTOR    <- "Ag"   # e.g. "Ag", "Manuf", etc.
SECTOR    <- "Manu" 


# nice label used in titles/filenames
sector_label_map <- c("Ag" = "Agriculture", "Manuf" = "Manufacturing")
SECTOR_LABEL <- unname(sector_label_map[SECTOR])
if (is.na(SECTOR_LABEL)) SECTOR_LABEL <- SECTOR  # fallback

# directories
ROOT <- "/data/sikeme/TRADE/US_CHN_TradeWar_git"
setwd(ROOT)

OUT_PLOT_DIR <- file.path(ROOT, "output/Compare_values/yearly/robust/elast/plot", as.character(BASE_YEAR))
dir.create(OUT_PLOT_DIR, recursive = TRUE, showWarnings = FALSE)

################################################################################
# 1) Load data
################################################################################
US <- read_csv(file.path(ROOT,paste0("output/Compare_values/yearly/robust/elast/US_ln_NTMs_", BASE_YEAR, "_FE_hs4_elast.csv") ))
sectors <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/crosswalk/HS6_NAICS_Diane/HS6_HS5_revision_industry_2012.csv")

names(US)


################################################################################
# 3) Filter to chosen sector
################################################################################
names(sectors)
names(US)

# merge with subsector
sectors$hs4 <- as.character(substr(sectors$HS6, 1, 4))

sectors %>%  group_by(hs4) %>%  summarise(n_j = n_distinct(j)) %>%  filter(n_j > 1)

sectors <- sectors %>%  group_by(hs4) %>%
  mutate(j = case_when(
    n_distinct(j) > 1 & any(j == "crop")      & any(j == "nonag")     ~ "crop",
    n_distinct(j) > 1 & any(j == "livestock")  & any(j == "nonag")     ~ "livestock",
    n_distinct(j) > 1 & any(j == "mining")     & any(j == "nonag")     ~ "nonag",
    n_distinct(j) > 1 & any(j == "forestry")   & any(j == "nonag")     ~ "nonag",
    n_distinct(j) > 1 & any(j == "crop")       & any(j == "livestock") ~ "crop",
    TRUE ~ j  )) %>%  ungroup()

# verify no more conflicts
sectors %>%  group_by(hs4) %>%  summarise(n_j = n_distinct(j)) %>%  filter(n_j > 1)

sectors_hs4 <- sectors %>%  group_by(hs4, j) %>%
  summarise(crop = paste(unique(crop[!is.na(crop)]), collapse = ", "),  .groups = "drop"  )
length(unique(sectors_hs4$hs4))
length(unique(US$hs4))
US$hs4 <- as.numeric(US$hs4)
sectors_hs4$hs4 <- as.numeric(sectors_hs4$hs4)

US1 <- left_join(US, sectors_hs4) %>% rename(subsector = j)
colSums(is.na(US1))
test <- US1 %>% filter(is.na(subsector))
unique(test$hs4)


# for the NAs had to change them manually:
unique(US1$subsector)
US1 <- US1 %>%
  mutate(subsector = case_when(
    # Livestock
    hs4 %in% c("0207", "0405", "0408", "2307", "2308", "2309",
               "4107", "4112", "4113") ~ "livestock",
    # Crop
    hs4 %in% c("0601", "0602", "0801", "0901", "0902", "0903",
               "0905", "0906", "0907", "0908", "0909", "1002",
               "1106", "1107", "1202", "1203", "1204", "1213",
               "1404", "1508", "1509", "1511", "1513", "1516",
               "1517", "1518", "1520", "1702", "1704", "1801",
               "1802", "1803", "1804", "1805", "1806", "1902",
               "1903", "1905", "2002", "2003", "2006", "2007",
               "2102", "2104", "2106", "2203", "2204", "2205",
               "2206", "2208", "2209", "2305", "3826") ~ "crop",
    
    # Nonag
    hs4 %in% c("0308", "2201", "2503", "2848", "3504", "3823",
               "3824", "3825", "4010", "4807", "5603", "6115",
               "6908", "7217", "7508", "7907", "7920", "8469",
               "8471", "8486", "8528", "8548", "9620", "9700",
               "9800", "9801", "9804", "9805") ~ "nonag",
    
    # Keep existing non-NA values
    TRUE ~ subsector  ))
colSums(is.na(US1))

US_dta <- US1

################################################################################
# 4) Construct weights based on BASE_YEAR (draw == 1)
################################################################################
names(US_dta)


base_hs6 <- US_dta %>%  filter(year == BASE_YEAR) %>%
  group_by(subsector, hs4) %>%
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
w_sector_chen <- US_dta %>%  filter(year == BASE_YEAR) %>%  group_by(hs2) %>%
  summarise(Trade_value_USD_base = sum(Trade_value_USD, na.rm = TRUE), .groups = "drop") %>%
  mutate(tot = sum(Trade_value_USD_base, na.rm = TRUE),
         weight_sector_chen = if_else(tot > 0, Trade_value_USD_base / tot, NA_real_)) %>%
  select(hs2, weight_sector_chen)

US_dta <- left_join(US_dta, w_sector_chen, by = "hs2")

# Chen HS-section weights (HS2 share within subsector) using BASE_YEAR
w_hs_sect_chen <- US_dta %>%  filter(year == BASE_YEAR) %>%
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
theme_trade <- theme_minimal(base_size = 14, base_family = "Times New Roman") +
  theme(
    panel.spacing.x = unit(1.2, "lines"),
    plot.title = element_text(size = 12, hjust = 0.5),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_text(size = 11),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.text  = element_text(size = 12),
    legend.title = element_text(size = 12)
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
# B) HS section level plot
################################################################################
names(US_dta)


US_dta_w_sect <- US_dta %>% group_by(year, subsector) %>%
  summarise(
    FE_mean        = weighted.mean(diff_ln_AVE_FE,                w = weight_hs_sect,      na.rm = TRUE),
    FEb_mean       = weighted.mean(diff_ln_AVE_FE_bench,          w = weight_hs_sect,      na.rm = TRUE),
    FEm_mean       = weighted.mean(diff_ln_AVE_FE_wmean,          w = weight_hs_sect,      na.rm = TRUE),
    FE_log_mean    = weighted.mean(diff_ln_AVE_FE_log,            w = weight_hs_sect,      na.rm = TRUE),
    FEb_log_mean   = weighted.mean(diff_ln_AVE_FE_log_bench,      w = weight_hs_sect,      na.rm = TRUE),
    FEm_log_mean   = weighted.mean(diff_ln_AVE_FE_log_wmean,      w = weight_hs_sect,      na.rm = TRUE),
    # Chen elasticities
    FE_mean_Chen          = weighted.mean(diff_ln_AVE_FE_Chen,          w = weight_hs_sect_Chen, na.rm = TRUE),
    FEb_mean_Chen         = weighted.mean(diff_ln_AVE_FE_bench_Chen,    w = weight_hs_sect_Chen, na.rm = TRUE),
    FEm_mean_Chen         = weighted.mean(diff_ln_AVE_FE_wmean_Chen,    w = weight_hs_sect_Chen, na.rm = TRUE),
    FE_log_mean_Chen      = weighted.mean(diff_ln_AVE_FE_log_Chen,      w = weight_hs_sect_Chen, na.rm = TRUE),
    FEb_log_mean_Chen     = weighted.mean(diff_ln_AVE_FE_log_bench_Chen,w = weight_hs_sect_Chen, na.rm = TRUE),
    FEm_log_mean_Chen     = weighted.mean(diff_ln_AVE_FE_log_wmean_Chen,w = weight_hs_sect_Chen, na.rm = TRUE),
    # original chen_mean
    chen_mean      = weighted.mean(diff_ln_AVE_chen,              w = weight_hs_sect_Chen, na.rm = TRUE),
    tariff_mean    = weighted.mean(.data[[TARIFF_VAR]],           w = weight_hs_sect,      na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(subsector) %>%
  mutate(chen_mean = mean(chen_mean, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(chen_mean = if_else(year %in% c(2018:2019), chen_mean, NA_real_))





# Add BASE_YEAR row (zeros) for each subsector
cols0 <- setdiff(names(US_dta_w_sect), c("year", "subsector"))
add_base <- US_dta_w_sect %>%  distinct(subsector) %>%
  mutate(year = BASE_YEAR) %>%
  mutate(!!!setNames(rep(list(0), length(cols0)), cols0))

US_dta_w_sect <- bind_rows(US_dta_w_sect, add_base) %>%  arrange(subsector, year)

unique(US_dta_w_sect$subsector)

# HS section descriptive labels
subsector_labels <- c(
  "crop" = "crop",
  "forestry" = "forestry",
  "livestock" = "livestock",
  "mining" = "mining",
  "nonag" = "non-ag")

US_dta_w_sect <- US_dta_w_sect %>%
  mutate(hs_section_lab = if_else(
    subsector %in% names(subsector_labels),
    subsector_labels[subsector],    as.character(subsector)     )  )

US_dta_w_sect <- US_dta_w_sect %>% filter(subsector %in% c("crop", "livestock", "nonag"))


################################################################################
# PPML 
################################################################################
# plot Hs4 level elast

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
  labs(title = paste0("Weighted average Δ ln(1+AVE) by sectors (relative to ", BASE_YEAR, ") \n using HS4 level elasticities"),
       x = "Year",   y = "Weighted Δ ln(1+AVE)"  ) +
  theme_trade
p_hs_sect

ggsave(  filename = file.path(OUT_PLOT_DIR, paste0("Compare_ln_AVE_subsector_weight_HS4_elast" ,".png")),
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
  labs(title = paste0("Weighted average Δ ln(1+AVE) by sectors (relative to ", BASE_YEAR, ")"),
       x = "Year",   y = "Weighted Δ ln(1+AVE)"  ) +
  theme_trade
p_hs_sect

ggsave(  filename = file.path(OUT_PLOT_DIR, paste0("Compare_ln_AVE_subsector_weight_HS4_elast_prez" ,".png")),
         plot = p_hs_sect, width = 11, height = 6, dpi = 300)


################################################################################
# plot chen et al elastciities

p_hs_sect <- ggplot(US_dta_w_sect, aes(x = year)) +
  geom_line(aes(y = FE_mean_Chen,     color = "FE"),          linewidth = 1) +
  geom_line(aes(y = FEm_mean_Chen,    color = "FE_demeaned"), linewidth = 1) +
  geom_line(aes(y = FEb_mean_Chen,    color = "FE_bench"),    linewidth = 1) +
  geom_line(aes(y = tariff_mean, color = "tariff"),      linewidth = 1) +
  geom_line(aes(y = chen_mean,   color = "Chen_et_al"),  linetype = "dashed", linewidth = 1) +
  scale_color_manual(values = legend_colors,
                     breaks = legend_breaks,
                     labels = legend_labels,
                     name   = "Variables") +
  facet_wrap(~ hs_section_lab) +
  labs(title = paste0("Weighted average Δ ln(1+AVE) by sectors (relative to ", BASE_YEAR, ") \n using Chen et al. elasticities"),
       x = "Year",   y = "Weighted Δ ln(1+AVE)"  ) +
  theme_trade
p_hs_sect

ggsave(  filename = file.path(OUT_PLOT_DIR, paste0("Compare_ln_AVE_subsector_weight_chen_elast" ,".png")),
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
  labs(title = paste0("Weighted average Δ ln(1+AVE) by sectors (relative to ", BASE_YEAR, ")"),
       x = "Year",   y = "Weighted Δ ln(1+AVE)"  ) +
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
  labs(title = paste0("Weighted average Δ ln(1+AVE) by sectors (relative to ", BASE_YEAR, ") \n using HS4 level elasticities"),
       x = "Year",   y = "Weighted Δ ln(1+AVE)"  ) +
  theme_trade
p_hs_sect

ggsave(  filename = file.path(OUT_PLOT_DIR, paste0("Compare_ln_AVE_subsector_weight_log_HS4_elast" ,".png")),
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
  labs(title = paste0("Weighted average Δ ln(1+AVE) by sectors (relative to ", BASE_YEAR, ")"),
       x = "Year",   y = "Weighted Δ ln(1+AVE)"  ) +
  theme_trade
p_hs_sect

ggsave(  filename = file.path(OUT_PLOT_DIR, paste0("Compare_ln_AVE_subsector_weight_log_HS4_elast_prez" ,".png")),
         plot = p_hs_sect, width = 11, height = 6, dpi = 300)


################################################################################
# plot chen et al elastciities

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
  labs(title = paste0("Weighted average Δ ln(1+AVE) by sectors (relative to ", BASE_YEAR, ") \n using Chen et al. elasticities"),
       x = "Year",   y = "Weighted Δ ln(1+AVE)"  ) +
  theme_trade
p_hs_sect

ggsave(  filename = file.path(OUT_PLOT_DIR, paste0("Compare_ln_AVE_subsector_weight_log_chen_elast" ,".png")),
         plot = p_hs_sect, width = 10, height = 6, dpi = 300)


# # bis for presentation;
# p_hs_sect <- ggplot(US_dta_w_sect, aes(x = year)) +
#   geom_line(aes(y = FEm_mean,    color = "FE_demeaned"), linewidth = 1) +
#   geom_line(aes(y = tariff_mean, color = "tariff"),      linewidth = 1) +
#   # geom_line(aes(y = chen_mean,  color = "Chen_et_al"),  linetype = "dashed", linewidth = 1) +
#   scale_color_manual(values = legend_colors,
#                      breaks = legend_breaks,
#                      labels = legend_labels,
#                      name   = "Variables"  ) +
#   facet_wrap(~ hs_section_lab) +
#   labs(title = paste0("Weighted average Δ ln(1+AVE) by sectors (relative to ", BASE_YEAR, ")"),
#        x = "Year",   y = "Weighted Δ ln(1+AVE)"  ) +
#   theme_trade
# p_hs_sect
# 
# ggsave(  filename = file.path(OUT_PLOT_DIR, paste0("Compare_ln_AVE_subsector_weight_chen_elast_prez" ,".png")),
#          plot = p_hs_sect, width = 11, height = 6, dpi = 300)
# 
################################################################################

################################################################################


names(US_dta)
summary(US_dta)


p1 <- quantile(US_dta$diff_ln_AVE_FE_wmean,      0.01, na.rm = TRUE)
p99 <- quantile(US_dta$diff_ln_AVE_FE_wmean,     0.99, na.rm = TRUE)
p1_chen <- quantile(US_dta$diff_ln_AVE_FE_wmean_Chen,  0.01, na.rm = TRUE)
p99_chen <- quantile(US_dta$diff_ln_AVE_FE_wmean_Chen, 0.99, na.rm = TRUE)

ggplot(US_dta %>% filter(
  diff_ln_AVE_FE_wmean      >= p1    & diff_ln_AVE_FE_wmean      <= p99,
  diff_ln_AVE_FE_wmean_Chen >= p1_chen & diff_ln_AVE_FE_wmean_Chen <= p99_chen
),
aes(x = diff_ln_AVE_FE_wmean, y = diff_ln_AVE_FE_wmean_Chen)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  labs(
    x = "AVE FE (own elasticities, demeaned)",
    y = "AVE FE (Chen elasticities, demeaned)",
    title = "Comparison of AVE estimates: own vs Chen elasticities"  ) +
  theme_minimal()


library(patchwork)

# Helper function to avoid repeating code
plot_comparison <- function(data, x_var, y_var, x_lab, y_lab, title) {
  p1    <- quantile(data[[x_var]], 0.01, na.rm = TRUE)
  p99   <- quantile(data[[x_var]], 0.99, na.rm = TRUE)
  p1_y  <- quantile(data[[y_var]], 0.01, na.rm = TRUE)
  p99_y <- quantile(data[[y_var]], 0.99, na.rm = TRUE)
  
  data %>%
    filter(.data[[x_var]] >= p1   & .data[[x_var]] <= p99,
           .data[[y_var]] >= p1_y & .data[[y_var]] <= p99_y) %>%
    ggplot(aes(x = .data[[x_var]], y = .data[[y_var]])) +
    geom_point(alpha = 0.3, size = 0.8) +
    geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
    labs(x = x_lab, y = y_lab, title = title) +
    theme_minimal()
}

p1 <- plot_comparison(US_dta,
                      x_var = "diff_ln_AVE_FE",
                      y_var = "diff_ln_AVE_FE_Chen",
                      x_lab = "AVE FE (HS4 elasticities)",
                      y_lab = "AVE FE (Chen elasticities)",
                      title = "No demeaning")

p2 <- plot_comparison(US_dta,
                      x_var = "diff_ln_AVE_FE_bench",
                      y_var = "diff_ln_AVE_FE_bench_Chen",
                      x_lab = "AVE FE bench (HS4 elasticities)",
                      y_lab = "AVE FE bench (Chen elasticities)",
                      title = "Benchmark demeaning")

p3 <- plot_comparison(US_dta,
                      x_var = "diff_ln_AVE_FE_wmean",
                      y_var = "diff_ln_AVE_FE_wmean_Chen",
                      x_lab = "AVE FE wmean (HS4 elasticities)",
                      y_lab = "AVE FE wmean (Chen elasticities)",
                      title = "Weighted mean demeaning")

plot <- p1 | p2 | p3
plot

ggsave(  filename = file.path(OUT_PLOT_DIR, paste0("Compare_elastcities" ,".png")),
         plot = p_hs_sect, width = 11, height = 6, dpi = 300)





