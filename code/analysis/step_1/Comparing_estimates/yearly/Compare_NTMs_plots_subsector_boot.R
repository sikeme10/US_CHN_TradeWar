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
SECTOR    <- "subsectors" 


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

sectors <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/crosswalk/HS6_NAICS_Diane/HS6_HS5_revision_industry_2012.csv")

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
names(sectors)
names(US1)

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
length(unique(US1$hs4))

US1 <- left_join(US1, sectors_hs4) %>% rename(subsector = j)
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
w_sector_chen <- US_dta %>%  filter(year == BASE_YEAR, draw == 1) %>%  group_by(hs2) %>%
  summarise(Trade_value_USD_base = sum(Trade_value_USD, na.rm = TRUE), .groups = "drop") %>%
  mutate(tot = sum(Trade_value_USD_base, na.rm = TRUE),
         weight_sector_chen = if_else(tot > 0, Trade_value_USD_base / tot, NA_real_)) %>%
  select(hs2, weight_sector_chen)

US_dta <- left_join(US_dta, w_sector_chen, by = "hs2")

# Chen HS-section weights (HS2 share within subsector) using BASE_YEAR
w_hs_sect_chen <- US_dta %>%  filter(year == BASE_YEAR, draw == 1) %>%
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
# A) Sector level plot
################################################################################
# US_dta_w <- US_dta %>%  group_by(year, draw) %>%
#   summarise(  w_FE        = weighted.mean(diff_ln_AVE_FE,               w = weight_sector, na.rm = TRUE),
#               w_FE_bench  = weighted.mean(diff_ln_AVE_FE_bench,         w = weight_sector, na.rm = TRUE),
#               w_FE_mean   = weighted.mean(diff_ln_AVE_FE_wmean,         w = weight_sector, na.rm = TRUE),
#               
#               w_FE_log        = weighted.mean(diff_ln_AVE_FE_log,       w = weight_sector, na.rm = TRUE),
#               w_FE_log_bench  = weighted.mean(diff_ln_AVE_FE_log_bench, w = weight_sector, na.rm = TRUE),
#               w_FE_log_mean   = weighted.mean(diff_ln_AVE_FE_log_wmean, w = weight_sector, na.rm = TRUE),
#               
#               w_chen     = weighted.mean(diff_ln_AVE_chen,              w = weight_sector_chen, na.rm = TRUE),
#               w_tariff   = weighted.mean(.data[[TARIFF_VAR]], w = weight_sector, na.rm = TRUE),
#               .groups = "drop"  ) %>%  mutate(w_chen = mean(w_chen, na.rm = TRUE)  )
# 
# US_dta_q <- US_dta_w %>%  group_by(year) %>%
#   summarise(FE_mean  = mean(w_FE, na.rm = TRUE),
#             FE_lo    = quantile(w_FE, 0.025, na.rm = TRUE),
#             FE_hi    = quantile(w_FE, 0.975, na.rm = TRUE),
#             
#             FEb_mean = mean(w_FE_bench, na.rm = TRUE),
#             FEb_lo   = quantile(w_FE_bench, 0.025, na.rm = TRUE),
#             FEb_hi   = quantile(w_FE_bench, 0.975, na.rm = TRUE),
#             
#             FEm_mean = mean(w_FE_mean, na.rm = TRUE),
#             FEm_lo   = quantile(w_FE_mean, 0.025, na.rm = TRUE),
#             FEm_hi   = quantile(w_FE_mean, 0.975, na.rm = TRUE),
#             
#             FE_log_mean  = mean(w_FE_log, na.rm = TRUE),
#             FEb_log_mean = mean(w_FE_log_bench, na.rm = TRUE),
#             FEm_log_mean = mean(w_FE_log_mean, na.rm = TRUE),
#             
#             chen_mean   = mean(w_chen, na.rm = TRUE),
#             tariff_mean = mean(w_tariff, na.rm = TRUE),
#             
#             .groups = "drop"  )
# 
# # If you only want Chen for 2018-2019 as before:
# US_dta_q <- US_dta_q %>%mutate(chen_mean = if_else(year %in% c(2018:2019), chen_mean, NA_real_))
# 
# # Add BASE_YEAR row as zero baseline
# US_dta_q <- bind_rows(US_dta_q, as_tibble(setNames(as.list(c(BASE_YEAR, rep(0, ncol(US_dta_q) - 1))), names(US_dta_q)))) %>% arrange(year)
# 
# p_sector <- ggplot(US_dta_q, aes(x = year)) +
#   geom_line(aes(y = FE_mean,    color = "FE"),          linewidth = 1) +
#   geom_line(aes(y = FEm_mean,   color = "FE_demeaned"), linewidth = 1) +
#   geom_line(aes(y = FEb_mean,   color = "FE_bench"),    linewidth = 1) +
#   geom_line(aes(y = tariff_mean,color = "tariff"),      linewidth = 1) +
#   geom_line(aes(y = chen_mean,  color = "Chen_et_al"),  linetype = "dashed", linewidth = 1) +
#   scale_color_manual(values = legend_colors,
#                      breaks = legend_breaks,
#                      labels = legend_labels,
#                      name   = "Variables" ) +
#   labs( title = paste0("Weighted average Δ ln(1+AVE) in ", SECTOR_LABEL, " (relative to ", BASE_YEAR, ")"),
#         x = "Year", y = "Weighted Δ ln(1+AVE)") +
#   theme_trade
# p_sector
# 
# ggsave( filename = file.path(OUT_PLOT_DIR, paste0("Compare_ln_AVE_", SECTOR, "_sector_weight_boot", ".png")),
#         plot = p_sector, width = 8, height = 6, dpi = 300)
# 
# 
# 
# # bis for presentation;
# p_sector <- ggplot(US_dta_q, aes(x = year)) +
#   geom_line(aes(y = FEm_mean,   color = "FE_demeaned"), linewidth = 1) +
#   geom_line(aes(y = tariff_mean,color = "tariff"),      linewidth = 1) +
#   # geom_line(aes(y = chen_mean,  color = "Chen_et_al"),  linetype = "dashed", linewidth = 1) +
#   scale_color_manual(values = legend_colors,
#                      breaks = legend_breaks,
#                      labels = legend_labels,
#                      name   = "Variables" ) +
#   labs( title = paste0("Weighted average Δ ln(1+AVE) in ", SECTOR_LABEL, " (relative to ", BASE_YEAR, ")"),
#         x = "Year", y = "Weighted Δ ln(1+AVE)") +
#   theme_trade
# p_sector
# ggsave( filename = file.path(OUT_PLOT_DIR, paste0("Compare_ln_AVE_", SECTOR, "_sector_weight_boot_prez", ".png")),
#         plot = p_sector, width = 8, height = 6, dpi = 300)


################################################################################
# B) HS section level plot
################################################################################
US_dta_w_sect <- US_dta %>%  group_by(year, draw, subsector) %>%
  summarise(w_FE        = weighted.mean(diff_ln_AVE_FE,               w = weight_hs_sect, na.rm = TRUE),
            w_FE_bench  = weighted.mean(diff_ln_AVE_FE_bench,         w = weight_hs_sect, na.rm = TRUE),
            w_FE_mean   = weighted.mean(diff_ln_AVE_FE_wmean,         w = weight_hs_sect, na.rm = TRUE),
            
            w_FE_log        = weighted.mean(diff_ln_AVE_FE_log,       w = weight_hs_sect, na.rm = TRUE),
            w_FE_log_bench  = weighted.mean(diff_ln_AVE_FE_log_bench, w = weight_hs_sect, na.rm = TRUE),
            w_FE_log_mean   = weighted.mean(diff_ln_AVE_FE_log_wmean, w = weight_hs_sect, na.rm = TRUE),
            
            w_chen     = weighted.mean(diff_ln_AVE_chen,              w = weight_hs_sect_Chen, na.rm = TRUE),
            w_tariff   = weighted.mean(.data[[TARIFF_VAR]], w = weight_hs_sect, na.rm = TRUE),            
            .groups = "drop"  ) %>%
  group_by(subsector) %>%  mutate(w_chen = mean(w_chen, na.rm = TRUE)) %>%
  ungroup()

US_dta_q_sect <- US_dta_w_sect %>%  group_by(year, subsector) %>%
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

# Add BASE_YEAR row (zeros) for each subsector
cols0 <- setdiff(names(US_dta_q_sect), c("year", "subsector"))
add_base <- US_dta_q_sect %>%
  distinct(subsector) %>%
  mutate(year = BASE_YEAR) %>%
  mutate(!!!setNames(rep(list(0), length(cols0)), cols0))

US_dta_q_sect <- bind_rows(US_dta_q_sect, add_base) %>%  arrange(subsector, year)

unique(US_dta_q_sect$subsector)

# HS section descriptive labels
subsector_labels <- c(
  "crop" = "crop",
  "forestry" = "forestry",
  "livestock" = "livestock",
  "mining" = "mining",
  "nonag" = "non-ag")

US_dta_q_sect <- US_dta_q_sect %>%
  mutate(hs_section_lab = if_else(
    subsector %in% names(subsector_labels),
    subsector_labels[subsector],    as.character(subsector)     )  )
    
US_dta_q_sect <- US_dta_q_sect %>% filter(subsector %in% c("crop", "livestock", "nonag"))

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
  facet_wrap(~ hs_section_lab) +
  labs(title = paste0("Weighted average Δ ln(1+AVE) by sectors (relative to ", BASE_YEAR, ")"),
       x = "Year",   y = "Weighted Δ ln(1+AVE)"  ) +
  theme_trade
p_hs_sect

ggsave(  filename = file.path(OUT_PLOT_DIR, paste0("Compare_ln_AVE_", SECTOR, "_subsector_weight_boot" ,".png")),
         plot = p_hs_sect, width = 10, height = 6, dpi = 300)


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
  labs(title = paste0("Weighted average Δ ln(1+AVE) by sectors (relative to ", BASE_YEAR, ")"),
       x = "Year",   y = "Weighted Δ ln(1+AVE)"  ) +
  theme_trade
p_hs_sect

ggsave(  filename = file.path(OUT_PLOT_DIR, paste0("Compare_ln_AVE_", SECTOR, "_subsector_weight_boot_prez" ,".png")),
         plot = p_hs_sect, width = 11, height = 6, dpi = 300)

