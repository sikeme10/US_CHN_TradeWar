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
names(US_dta)

################################################################################
# 4) Construct weights based on BASE_YEAR (draw == 1)
################################################################################
names(US_dta)

base_hs6 <- US_dta %>%  filter(year == BASE_YEAR) %>%
  group_by( hs_section, hs2, hs4) %>%
  summarise(Trade_value_USD_base = sum(Trade_value_USD, na.rm = TRUE), .groups = "drop")

# HS2 weights
w_hs2 <- base_hs6 %>%
  group_by(hs2, hs4) %>%
  summarise(Trade_value_USD_base = sum(Trade_value_USD_base), .groups = "drop_last") %>%
  mutate(tot = sum(Trade_value_USD_base),
         weight_hs2 = if_else(tot > 0, Trade_value_USD_base / tot, 0)) %>%
  ungroup() %>%
  select(hs2, hs4, weight_hs2)

US_dta <- US_dta %>%  left_join(w_hs2,     by = c("hs2", "hs4"))

################################################################################
# 4b) Construct baseline trade (BASE_YEAR) at HS4 level
################################################################################

# percentage change in trade at HS2 level
trade_hs2 <- US_dta %>%  group_by(year, draw, hs2) %>%
  summarise(Trade_hs2 = sum(Trade_value_USD, na.rm = TRUE), .groups="drop")

base_hs2 <- trade_hs2 %>% filter(year == BASE_YEAR & draw == 1) %>%
  select(hs2, Trade_base_hs2 = Trade_hs2)

trade_hs2 <- trade_hs2 %>%left_join(base_hs2, by="hs2") %>%
  mutate( pct_trade_change_hs2 = if_else(   year > BASE_YEAR & !is.na(Trade_base_hs2) & Trade_base_hs2 > 0,
                                            100 * (Trade_hs2 - Trade_base_hs2) / Trade_base_hs2,      NA_real_  )  )
trade_hs2_post <- trade_hs2 %>% filter(year > BASE_YEAR)
summary(trade_hs2_post)


# percentage change in trade at HS4 level
trade_hs4 <- US_dta %>%  group_by(year, hs4) %>% filter(draw == 1) %>%
  summarise(Trade_hs4 = sum(Trade_value_USD, na.rm = TRUE), .groups="drop")

base_hs4 <- trade_hs4 %>% filter(year == BASE_YEAR) %>%
  select(hs4, Trade_base_hs4 = Trade_hs4)

trade_hs4 <- trade_hs4 %>%left_join(base_hs4, by="hs4") %>%
  mutate( pct_trade_change_hs4 = if_else(   year > BASE_YEAR & !is.na(Trade_base_hs4) & Trade_base_hs4 > 0,
                                            100 * (Trade_hs4 - Trade_base_hs4) / Trade_base_hs4,      NA_real_  )  )
trade_hs4_post <- trade_hs4 %>% filter(year > BASE_YEAR)
summary(trade_hs4_post)


################################################################################
# 5) Add Chen et al estimates + Chen weights (based on BASE_YEAR)
################################################################################
Chen <- read_csv(file.path(ROOT, "data/chen_NTB_tariff/hs2_agriculture_manufacturing_clean.csv"))

Chen <- Chen %>%  select(-Country, -ISO3_Code) %>%
  rename( hs2 = HS2, Chen_US_import_share = US_import_share, 
          diff_log_tariff_Chen = tau_tariff_CHN,diff_ln_AVE_chen     = tau_NTB  )

US_dta$hs2 <- as.numeric(US_dta$hs2)
US_dta <- left_join(US_dta, Chen, by = "hs2")

################################################################################
# Keep only post-base years
################################################################################
US_dta <- US_dta %>% filter(year > BASE_YEAR)
unique(US_dta$year)

# pick tariff var: 
TARIFF_VAR <- paste0("diff_log_tariff_", BASE_YEAR)

################################################################################
# get hs4 commodity description
################################################################################
library(tradestatistics)
library(tibble)

hs4 <- as_tibble(ots_commodities_short)
sapply(hs4, class)

hs4 <- hs4 %>%
  mutate( hs2 = if_else(nchar(commodity_code) == 2, commodity_code, NA_character_),
          hs4 = if_else(nchar(commodity_code) == 4, commodity_code, NA_character_),
          hs2_description = if_else(nchar(commodity_code) == 2, commodity_name, NA_character_),
          hs4_description = if_else(nchar(commodity_code) == 4, commodity_name, NA_character_)  )
hs4 <- hs4 %>%
  mutate(hs2 = if_else(nchar(commodity_code) == 2, commodity_code, NA_character_),
         hs4 = if_else(nchar(commodity_code) == 4, commodity_code, NA_character_),
         hs2_description = if_else(nchar(commodity_code) == 2, commodity_name, NA_character_),
         hs4_description = if_else(nchar(commodity_code) == 4, commodity_name, NA_character_)  ) %>%
  fill(hs2, hs2_description, .direction = "down") %>%
  filter(nchar(commodity_code) == 4)  %>% select(-commodity_code, -commodity_name) %>% mutate(hs2 =  as.numeric(hs2))

sapply(US_dta, class)

US_dta <- left_join(US_dta,hs4)

# how many hs2 have multiple subsectors
US_dta %>%  group_by(hs2) %>%  summarise(n_subsector = n_distinct(subsector)) %>%
  filter(n_subsector > 1)

# print those observations
test <- US_dta %>%  group_by(hs2) %>%  filter(n_distinct(subsector) > 1) %>%
  distinct(hs2, subsector) %>%  arrange(hs2)


# create hs2-level subsector by pasting unique values
hs2_subsector <- US_dta %>%  group_by(hs2) %>%
  summarise(subsector = paste(unique(subsector), collapse = ", "), .groups = "drop")

# merge back
US_dta <- US_dta %>%  select(-subsector) %>%  left_join(hs2_subsector, by = "hs2")

# ################################################################################
# # a) HS 2 level level
# ################################################################################


names(US_dta)
US_all_w_hs2 <- US_dta %>%   group_by(year,draw, hs2, subsector, hs2_description) %>% 
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
US_all_q_hs2 <- US_all_w_hs2 %>%  filter (year %in% c(2018:2019)) %>% group_by(hs2, subsector, hs2_description) %>%
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
write_xlsx( list("hs2_summary" = US_all_q_hs2),  path = file.path(OUT_PLOT_DIR, paste0("hs2_summary_subsector_all_", quant , ".xlsx")))



# ################################################################################
# # a) HS 4 level level
# ################################################################################


names(US_dta)
US_all_w_hs4 <- US_dta %>%   group_by(year, hs4, subsector) %>% 
  summarise(  w_FE        = mean(diff_ln_AVE_FE,    na.rm = TRUE   ),
              w_FE_bench  = mean(diff_ln_AVE_FE_bench,   na.rm = TRUE),
              w_FE_mean   = mean(diff_ln_AVE_FE_wmean,  na.rm = TRUE),
              
              # added (log)
              w_FE_log        = mean(diff_ln_AVE_FE_log,   na.rm = TRUE    ),
              w_FE_log_bench  = mean(diff_ln_AVE_FE_log_bench,   na.rm = TRUE),
              w_FE_log_mean   = mean(diff_ln_AVE_FE_log_wmean,   na.rm = TRUE),
              
              w_tariff    = mean(diff_log_tariff_2017,  na.rm = TRUE),
              
              
              s_FE        = mean(diff_ln_AVE_FE,        na.rm = TRUE),
              s_FE_bench  = mean(diff_ln_AVE_FE_bench,  na.rm = TRUE),
              s_FE_mean   = mean(diff_ln_AVE_FE_wmean,  na.rm = TRUE),
              
              # added (log)
              s_FE_log        = mean(diff_ln_AVE_FE_log,       na.rm = TRUE),
              s_FE_log_bench  = mean(diff_ln_AVE_FE_log_bench, na.rm = TRUE),
              s_FE_log_mean   = mean(diff_ln_AVE_FE_log_wmean, na.rm = TRUE),
              
              s_tariff    = mean(diff_log_tariff_2017,  na.rm = TRUE),.groups = "drop") %>%
  left_join(trade_hs4_post %>% select(year, hs4, pct_trade_change_hs4), by = c("year", "hs4")) 


# aggregate at hs section
US_all_q_hs4 <- US_all_w_hs4 %>%  filter (year %in% c(2018:2019)) %>% group_by(hs4,subsector) %>%
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
    
    perc_change_trade_mean = mean(pct_trade_change_hs4, na.rm = TRUE))





################################################################################

library(writexl)
write_xlsx( list("hs4_summary" = US_all_q_hs4),  path = file.path(OUT_PLOT_DIR, paste0("hs4_summary_all_", quant , ".xlsx")))

US_all_q_hs4


