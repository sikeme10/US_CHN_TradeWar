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

OUT_PLOT_DIR <- file.path(ROOT, "output/Compare_values/yearly/plot", as.character(BASE_YEAR))
dir.create(OUT_PLOT_DIR, recursive = TRUE, showWarnings = FALSE)


exp_dir <- file.path(ROOT, "output/Compare_values/yearly")
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
US_dta <- US1 
# US_dta <- US1 %>% filter(sector == SECTOR)

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
# 4b) Construct baseline trade (BASE_YEAR) at HS4 level
################################################################################
trade_hs2 <- US_dta %>%  group_by(year, draw, hs2) %>%
  summarise(Trade_hs2 = sum(Trade_value_USD, na.rm = TRUE), .groups="drop")

base_hs2 <- trade_hs2 %>% filter(year == BASE_YEAR & draw == 1) %>%
  select(hs2, Trade_base_hs2 = Trade_hs2)

trade_hs2 <- trade_hs2 %>%left_join(base_hs2, by="hs2") %>%
  mutate( pct_trade_change_hs2 = if_else(   year > BASE_YEAR & !is.na(Trade_base_hs2) & Trade_base_hs2 > 0,
      100 * (Trade_hs2 - Trade_base_hs2) / Trade_base_hs2,      NA_real_  )  )
trade_hs2_post <- trade_hs2 %>% filter(year > BASE_YEAR)
summary(trade_hs2_post)


trade_hs4 <- US_dta %>%  group_by(year, hs4) %>% filter(draw == 1) %>%
  summarise(Trade_hs4 = sum(Trade_value_USD, na.rm = TRUE), .groups="drop")

base_hs4 <- trade_hs4 %>% filter(year == BASE_YEAR) %>%
  select(hs4, Trade_base_hs4 = Trade_hs4)

trade_hs4 <- trade_hs4 %>%left_join(base_hs4, by="hs4") %>%
  mutate( pct_trade_change_hs4 = if_else(   year > BASE_YEAR & !is.na(Trade_base_hs4) & Trade_base_hs4 > 0,
                                            100 * (Trade_hs4 - Trade_base_hs4) / Trade_base_hs4,      NA_real_  )  )
trade_hs4_post <- trade_hs4 %>% filter(year > BASE_YEAR)
summary(trade_hs4_post)



test <- US_dta %>%
  filter(year %in% 2018:2019) %>%  group_by(year, hs2, hs4) %>%
  summarise(FE_mean   = mean(diff_ln_AVE_FE, na.rm = TRUE),
            FE_median = median(diff_ln_AVE_FE, na.rm = TRUE),
            FEm_mean   = mean(diff_ln_AVE_FE_wmean, na.rm = TRUE),
            FEm_median = median(diff_ln_AVE_FE_wmean, na.rm = TRUE),
            FEm_q25    = quantile(diff_ln_AVE_FE_wmean, 0.25, na.rm = TRUE),
            FEm_q75    = quantile(diff_ln_AVE_FE_wmean, 0.75, na.rm = TRUE),
            .groups = "drop"  ) %>%  filter(hs2 == 4)
test <- test %>%  left_join(trade_hs4_post, by = c("year", "hs4"))

################################################################################
# Keep only post-base years
################################################################################
US_dta <- US_dta %>% filter(year > BASE_YEAR)
unique(US_dta$year)

# pick tariff var: 
TARIFF_VAR <- paste0("diff_log_tariff_", BASE_YEAR)




# ################################################################################
# # a) HS 2 level level
# ################################################################################


names(US_dta)
US_all_w_hs2 <- US_dta %>%   group_by(year,draw, hs2) %>% 
  summarise(  w_FE        = weighted.mean(diff_ln_AVE_FE,           w = weight_hs2, na.rm = TRUE),
              w_FE_bench  = weighted.mean(diff_ln_AVE_FE_bench,     w = weight_hs2, na.rm = TRUE),
              w_FE_mean   = weighted.mean(diff_ln_AVE_FE_wmean,       w = weight_hs2, na.rm = TRUE),
              
              # added (log)
              w_FE_log        = weighted.mean(diff_ln_AVE_FE_log,       w = weight_hs2, na.rm = TRUE),
              w_FE_log_bench  = weighted.mean(diff_ln_AVE_FE_log_bench, w = weight_hs2, na.rm = TRUE),
              w_FE_log_mean   = weighted.mean(diff_ln_AVE_FE_log_wmean, w = weight_hs2, na.rm = TRUE),
              
              w_chen      = weighted.mean(diff_ln_AVE_chen,         w = weight_hs2, na.rm = TRUE),
              w_tariff    = weighted.mean(diff_log_tariff_2017,     w = weight_hs2, na.rm = TRUE),
              
              
              s_FE        = mean(diff_ln_AVE_FE,        na.rm = TRUE),
              s_FE_bench  = mean(diff_ln_AVE_FE_bench,  na.rm = TRUE),
              s_FE_mean   = mean(diff_ln_AVE_FE_wmean,  na.rm = TRUE),
              
              # added (log)
              s_FE_log        = mean(diff_ln_AVE_FE_log,       na.rm = TRUE),
              s_FE_log_bench  = mean(diff_ln_AVE_FE_log_bench, na.rm = TRUE),
              s_FE_log_mean   = mean(diff_ln_AVE_FE_log_wmean, na.rm = TRUE),
              
              s_chen      = mean(diff_ln_AVE_chen,      na.rm = TRUE),
              s_tariff    = mean(diff_log_tariff_2017,  na.rm = TRUE),.groups = "drop") %>%
  left_join(trade_hs2_post %>% select(year, draw, hs2, pct_trade_change_hs2),
            by = c("year", "draw", "hs2")) %>%
  group_by(hs2) %>% mutate(w_chen = mean(w_chen, na.rm = TRUE), s_chen = mean(s_chen, na.rm = TRUE))


# aggregate at hs section
US_all_q_hs2 <- US_all_w_hs2 %>%  filter (year %in% c(2018:2019)) %>% group_by(hs2) %>%
  summarise( # FE (non-benchmark)
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
    
    perc_change_trade_mean = mean(pct_trade_change_hs2, na.rm = TRUE),
    
    # Chen (optional CI)
    chen_mean = mean(w_chen, na.rm = TRUE))

library(writexl)
write_xlsx( list("hs2_summary" = US_all_q_hs2),  path = file.path(OUT_PLOT_DIR, paste0("hs2_summary_all_", quant , ".xlsx")))



################################################################################

# try to get correlation plot across variables 

names(US_all_q_hs2)
cor_data <- US_all_q_hs2 %>% select(FE_mean,FEb_mean, FEm_mean, FE_log_mean,FEb_log_mean, FEm_log_mean, tariff_mean, chen_mean) %>%
  drop_na()%>%
  rename(FE = FE_mean,FE_Bench = FEb_mean,  FE_Demean = FEm_mean,  FE_Log = FE_log_mean,
         FE_Log_Bench = FEb_log_mean, FE_Log_Demean = FEm_log_mean, Tariff = tariff_mean,  Chen= chen_mean  )
library(corrplot)
# Compute correlation matrix
cor_mat <- cor(cor_data, use = "complete.obs")

# Plot
save_corrplot <- function(filename, cor_mat) {
  png(filename, width = 2000, height = 2000, res = 300)
  on.exit(dev.off())
  
  corrplot(cor_mat,
           method = "circle",
           type = "full",
           order = "hclust",
           addCoef.col = "black",
           tl.col = "black",
           tl.srt = 45)
}

save_corrplot(paste0(OUT_PLOT_DIR, "/correlation_plot_US_all_hs2.png"), cor_mat)



