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


# directories
ROOT <- "/data/sikeme/TRADE/US_CHN_TradeWar_git"
setwd(ROOT)

OUT_PLOT_DIR <- file.path(ROOT, "output/Compare_values/yearly/robust/elast/plot", as.character(BASE_YEAR), "boot")
dir.create(OUT_PLOT_DIR, recursive = TRUE, showWarnings = FALSE)

################################################################################
# 1) Load data
################################################################################
US <- read_csv(file.path(ROOT,paste0("output/Compare_values/yearly/robust/elast/US_ln_NTMs_base_", BASE_YEAR, "_FE_boot_hs4_elast.csv") ))
sectors <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/crosswalk/clean_HS6_naics6_2012.csv")


################################################################################
# 2) Drop extreme values (1st/99th percentile)
################################################################################
quant <- 0.01

FE_q    <- quantile(US$diff_ln_AVE_FE,               quant,     na.rm = TRUE)
FE_qH   <- quantile(US$diff_ln_AVE_FE,           1 - quant,     na.rm = TRUE)
FE_b_q  <- quantile(US$diff_ln_AVE_FE_bench,         quant,     na.rm = TRUE)
FE_b_qH <- quantile(US$diff_ln_AVE_FE_bench,     1 - quant,     na.rm = TRUE)
FE_w_q  <- quantile(US$diff_ln_AVE_FE_wmean,         quant,     na.rm = TRUE)
FE_w_qH <- quantile(US$diff_ln_AVE_FE_wmean,     1 - quant,     na.rm = TRUE)

FE_c_q  <- quantile(US$diff_ln_AVE_FE_Chen,          quant,     na.rm = TRUE)
FE_c_qH <- quantile(US$diff_ln_AVE_FE_Chen,      1 - quant,     na.rm = TRUE)
FE_bc_q <- quantile(US$diff_ln_AVE_FE_bench_Chen,    quant,     na.rm = TRUE)
FE_bc_qH<- quantile(US$diff_ln_AVE_FE_bench_Chen, 1- quant,     na.rm = TRUE)
FE_wc_q <- quantile(US$diff_ln_AVE_FE_wmean_Chen,    quant,     na.rm = TRUE)
FE_wc_qH<- quantile(US$diff_ln_AVE_FE_wmean_Chen, 1 - quant,   na.rm = TRUE)

cat("FE [5%,95%]:",       FE_q,   FE_qH,
    "| Bench [5%,95%]:",  FE_b_q, FE_b_qH,
    "| Wmean [5%,95%]:",  FE_w_q, FE_w_qH, "\n")

cat("FE_Chen [5%,95%]:",       FE_c_q,  FE_c_qH,
    "| Bench_Chen [5%,95%]:",  FE_bc_q, FE_bc_qH,
    "| Wmean_Chen [5%,95%]:",  FE_wc_q, FE_wc_qH, "\n")

US1 <- US %>%
  mutate(
    diff_ln_AVE_FE            = ifelse(diff_ln_AVE_FE            < FE_q    | diff_ln_AVE_FE            > FE_qH,   NA, diff_ln_AVE_FE),
    diff_ln_AVE_FE_bench      = ifelse(diff_ln_AVE_FE_bench      < FE_b_q  | diff_ln_AVE_FE_bench      > FE_b_qH, NA, diff_ln_AVE_FE_bench),
    diff_ln_AVE_FE_wmean      = ifelse(diff_ln_AVE_FE_wmean      < FE_w_q  | diff_ln_AVE_FE_wmean      > FE_w_qH, NA, diff_ln_AVE_FE_wmean),
    diff_ln_AVE_FE_Chen       = ifelse(diff_ln_AVE_FE_Chen       < FE_c_q  | diff_ln_AVE_FE_Chen       > FE_c_qH, NA, diff_ln_AVE_FE_Chen),
    diff_ln_AVE_FE_bench_Chen = ifelse(diff_ln_AVE_FE_bench_Chen < FE_bc_q | diff_ln_AVE_FE_bench_Chen > FE_bc_qH,NA, diff_ln_AVE_FE_bench_Chen),
    diff_ln_AVE_FE_wmean_Chen = ifelse(diff_ln_AVE_FE_wmean_Chen < FE_wc_q | diff_ln_AVE_FE_wmean_Chen > FE_wc_qH,NA, diff_ln_AVE_FE_wmean_Chen)
  )


################################################################################
# 3) Filter to chosen sector
################################################################################
names(sectors)
names(US)
unique(sectors$subsector)

# merge with subsector
sectors$HS6 <-  as.character(sectors$HS6)
unique(nchar(sectors$HS6))
sectors$HS6 <- ifelse(nchar(sectors$HS6) == 5, paste0("0", sectors$HS6), sectors$HS6)
sectors$hs4 <- as.character(substr(sectors$HS6, 1, 4)) 
sectors <- sectors %>% select(-naics, -naics_description)

test <- sectors %>%  group_by(hs4) %>%  summarise(n_subsector = n_distinct(subsector)) %>%  filter(n_subsector > 1)

sectors <- sectors %>%
  group_by(hs4) %>%
  mutate(
    non_na_sub = if (all(is.na(subsector))) NA_character_ else first(na.omit(subsector)),
    subsector = case_when(
      any(is.na(subsector)) & n_distinct(subsector, na.rm = TRUE) == 1 ~ non_na_sub,
      n_distinct(subsector) > 1 & any(subsector == "crop")      & any(subsector == "nonag")     ~ "crop",
      n_distinct(subsector) > 1 & any(subsector == "livestock")  & any(subsector == "nonag")     ~ "livestock",
      n_distinct(subsector) > 1 & any(subsector == "mining")     & any(subsector == "nonag")     ~ "nonag",
      n_distinct(subsector) > 1 & any(subsector == "forestry")   & any(subsector == "nonag")     ~ "nonag",
      n_distinct(subsector) > 1 & any(subsector == "crop")       & any(subsector == "livestock") ~ "crop",
      TRUE ~ subsector    )) %>%
  select(-non_na_sub) %>%  ungroup()
# verify no more conflicts
test <- sectors %>%  group_by(hs4) %>%  summarise(n_subsector = n_distinct(subsector)) %>%  filter(n_subsector > 1)

# for the NAs had to change them manually:
sectors <- sectors %>%
  mutate(subsector = case_when(
    # Livestock
    hs4 %in% c("0102", "0105","0207", "0405", "0407","0408", "2307", "2308", "2309",
               "4107", "4112", "4113") ~ "livestock",
    # Crop
    hs4 %in% c("0601", "0602", "0801", "0901", "0902", "0903",
               "0905", "0906", "0907", "0908", "0909", "1002",
               "1106", "1107", "1202", "1203", "1204", "1213",
               "1404", "1508", "1509", "1511", "1513", "1516","1510",
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
    # Nonag (anything above HS chapter 24)
    is.na(subsector) & as.numeric(hs4) > 2400 ~ "nonag",
    # Keep existing non-NA values
    TRUE ~ subsector  ))

sectors_hs4 <- sectors %>%  group_by(hs4, subsector) %>%
  summarise(ag_subsector = paste(unique(ag_subsector[!is.na(ag_subsector)]), collapse = ", "),  .groups = "drop"  )
length(unique(sectors_hs4$hs4))
length(unique(US1$hs4))
class(US1$hs4)
US1$hs4 <- as.numeric(US1$hs4)
sectors_hs4$hs4 <- as.numeric(sectors_hs4$hs4)

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

base_hs6 <- US_dta %>%  filter(year == BASE_YEAR & draw ==1) %>%
  group_by(hs2, hs4) %>%
  summarise(Trade_value_USD_base = sum(Trade_value_USD, na.rm = TRUE), .groups = "drop")

# HS2 weights
w_hs2 <- base_hs6 %>%
  group_by(hs2, hs4) %>%
  summarise(Trade_value_USD_base = sum(Trade_value_USD_base), .groups = "drop_last") %>%
  mutate(tot = sum(Trade_value_USD_base),
         weight_hs2 = if_else(tot > 0, Trade_value_USD_base / tot, 0)) %>%
  ungroup() %>%  select(hs2, hs4, weight_hs2)

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
  mutate(pct_trade_change_hs2 = if_else(year > BASE_YEAR & !is.na(Trade_base_hs2) & Trade_base_hs2 > 0,
                                         100 * (Trade_hs2 - Trade_base_hs2) / Trade_base_hs2, NA),
         trade_change_hs2 = if_else(year > BASE_YEAR & !is.na(Trade_base_hs2) & Trade_base_hs2 > 0,
                                        (Trade_hs2 - Trade_base_hs2), NA),)
trade_hs2_post <- trade_hs2 %>% filter(year > BASE_YEAR)
trade_hs2_post <- trade_hs2_post %>% mutate(pct_trade_change_hs2_bis = if_else(pct_trade_change_hs2 >0, NA, pct_trade_change_hs2))

summary(trade_hs2_post)


# percentage change in trade at HS4 level
trade_hs4 <- US_dta %>%  group_by(year, hs4) %>% filter(draw == 1) %>%
  summarise(Trade_hs4 = sum(Trade_value_USD, na.rm = TRUE), .groups="drop")

base_hs4 <- trade_hs4 %>% filter(year == BASE_YEAR) %>%
  select(hs4, Trade_base_hs4 = Trade_hs4)

trade_hs4 <- trade_hs4 %>%left_join(base_hs4, by="hs4") %>%
  mutate(pct_trade_change_hs4 = if_else(year > BASE_YEAR & !is.na(Trade_base_hs4) & Trade_base_hs4 > 0,
                                        100 * (Trade_hs4 - Trade_base_hs4) / Trade_base_hs4,NA),
         trade_change_hs4 = if_else(year > BASE_YEAR & !is.na(Trade_base_hs4) & Trade_base_hs4 > 0,
                                    (Trade_hs4 - Trade_base_hs4),NA))
trade_hs4_post <- trade_hs4 %>% filter(year > BASE_YEAR)
trade_hs4_post <- trade_hs4_post %>% mutate(pct_trade_change_hs4_bis = if_else(pct_trade_change_hs4 >0, NA, pct_trade_change_hs4))

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
sapply(hs4, class)

hs4$hs4 <- as.numeric(hs4$hs4)

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
              
              w_chen      =  mean(diff_ln_AVE_chen,   na.rm = TRUE),
              w_tariff    = weighted.mean(diff_log_tariff_2017,     w = weight_hs2, na.rm = TRUE),
              w_tariff_chen    = mean(diff_log_tariff_Chen,   na.rm = TRUE),
              
              # weighted means - Chen elasticity variants
              w_FE_Chen           = weighted.mean(diff_ln_AVE_FE_Chen,            w = weight_hs2, na.rm = TRUE),
              w_FE_bench_Chen     = weighted.mean(diff_ln_AVE_FE_bench_Chen,      w = weight_hs2, na.rm = TRUE),
              w_FE_mean_Chen      = weighted.mean(diff_ln_AVE_FE_wmean_Chen,      w = weight_hs2, na.rm = TRUE),
              w_FE_log_Chen       = weighted.mean(diff_ln_AVE_FE_log_Chen,        w = weight_hs2, na.rm = TRUE),
              w_FE_log_bench_Chen = weighted.mean(diff_ln_AVE_FE_log_bench_Chen,  w = weight_hs2, na.rm = TRUE),
              w_FE_log_mean_Chen  = weighted.mean(diff_ln_AVE_FE_log_wmean_Chen,  w = weight_hs2, na.rm = TRUE),
              
              .groups = "drop") %>%
  left_join(trade_hs2_post %>% select(year, draw, hs2, pct_trade_change_hs2,pct_trade_change_hs2_bis, trade_change_hs2  ),
            by = c("year", "draw", "hs2")) %>%
  group_by(hs2) %>% mutate(w_chen = mean(w_chen, na.rm = TRUE))


# aggregate at hs section
US_all_q_hs2 <- US_all_w_hs2 %>%    filter(year %in% c(2019)) %>% 
  group_by(hs2, subsector, hs2_description) %>%
  summarise(
    # FE (non-benchmark)
    FE_mean = mean(w_FE, na.rm = TRUE),
    FE_lo   = quantile(w_FE, 0.025, na.rm = TRUE),
    FE_hi   = quantile(w_FE, 0.975, na.rm = TRUE),
    
    # FE (benchmark)
    FEb_mean = mean(w_FE_bench, na.rm = TRUE),
    FEb_lo   = quantile(w_FE_bench, 0.025, na.rm = TRUE),
    FEb_hi   = quantile(w_FE_bench, 0.975, na.rm = TRUE),
    
    # FE demeaned (levels)
    FEm_mean = mean(w_FE_mean, na.rm = TRUE),
    FEm_lo   = quantile(w_FE_mean, 0.025, na.rm = TRUE),
    FEm_hi   = quantile(w_FE_mean, 0.975, na.rm = TRUE),
    
    # FE_log (non-benchmark)
    FE_log_mean = mean(w_FE_log, na.rm = TRUE),
    FE_log_lo   = quantile(w_FE_log, 0.025, na.rm = TRUE),
    FE_log_hi   = quantile(w_FE_log, 0.975, na.rm = TRUE),
    
    # FE_log (benchmark)
    FEb_log_mean = mean(w_FE_log_bench, na.rm = TRUE),
    FEb_log_lo   = quantile(w_FE_log_bench, 0.025, na.rm = TRUE),
    FEb_log_hi   = quantile(w_FE_log_bench, 0.975, na.rm = TRUE),
    
    # FE demeaned (log)
    FEm_log_mean = mean(w_FE_log_mean, na.rm = TRUE),
    FEm_log_lo   = quantile(w_FE_log_mean, 0.025, na.rm = TRUE),
    FEm_log_hi   = quantile(w_FE_log_mean, 0.975, na.rm = TRUE),
    
    # ===== Chen elasticity variants =====
    
    # FE Chen (non-benchmark)
    FE_Chen_mean = mean(w_FE_Chen, na.rm = TRUE),
    FE_Chen_lo   = quantile(w_FE_Chen, 0.025, na.rm = TRUE),
    FE_Chen_hi   = quantile(w_FE_Chen, 0.975, na.rm = TRUE),
    
    # FE Chen (benchmark)
    FEb_Chen_mean = mean(w_FE_bench_Chen, na.rm = TRUE),
    FEb_Chen_lo   = quantile(w_FE_bench_Chen, 0.025, na.rm = TRUE),
    FEb_Chen_hi   = quantile(w_FE_bench_Chen, 0.975, na.rm = TRUE),
    
    # FE Chen demeaned (levels)
    FEm_Chen_mean = mean(w_FE_mean_Chen, na.rm = TRUE),
    FEm_Chen_lo   = quantile(w_FE_mean_Chen, 0.025, na.rm = TRUE),
    FEm_Chen_hi   = quantile(w_FE_mean_Chen, 0.975, na.rm = TRUE),
    
    # FE_log Chen (non-benchmark)
    FE_log_Chen_mean = mean(w_FE_log_Chen, na.rm = TRUE),
    FE_log_Chen_lo   = quantile(w_FE_log_Chen, 0.025, na.rm = TRUE),
    FE_log_Chen_hi   = quantile(w_FE_log_Chen, 0.975, na.rm = TRUE),
    
    # FE_log Chen (benchmark)
    FEb_log_Chen_mean = mean(w_FE_log_bench_Chen, na.rm = TRUE),
    FEb_log_Chen_lo   = quantile(w_FE_log_bench_Chen, 0.025, na.rm = TRUE),
    FEb_log_Chen_hi   = quantile(w_FE_log_bench_Chen, 0.975, na.rm = TRUE),
    
    # FE_log Chen demeaned
    FEm_log_Chen_mean = mean(w_FE_log_mean_Chen, na.rm = TRUE),
    FEm_log_Chen_lo   = quantile(w_FE_log_mean_Chen, 0.025, na.rm = TRUE),
    FEm_log_Chen_hi   = quantile(w_FE_log_mean_Chen, 0.975, na.rm = TRUE),
    
    # ===== other variables =====
    tariff_mean = mean(w_tariff, na.rm = TRUE),
    perc_change_trade_mean = mean(pct_trade_change_hs2, na.rm = TRUE),
    perc_change_trade_mean_bis =  mean(pct_trade_change_hs2_bis, na.rm = TRUE),
    trade_change_hs2 = mean(trade_change_hs2, na.rm = TRUE),
    chen_mean = mean(w_chen, na.rm = TRUE),
    chen_tariff_mean = mean(w_tariff_chen, na.rm = TRUE),
    .groups = "drop"
  )

library(writexl)
write_xlsx( list("hs2_summary" = US_all_q_hs2),  path = file.path(OUT_PLOT_DIR, paste0("hs2_summary_subsector_all_", quant , ".xlsx")))


##### correlation matrix

corr_vars <- c("FE_mean", "FEb_mean", "FEm_mean", "tariff_mean","chen_tariff_mean",  
               "chen_mean","perc_change_trade_mean","perc_change_trade_mean_bis")

# corr_vars <- c("FE_log_mean", "FEb_log_mean", "FEm_log_mean", "tariff_mean","chen_tariff_mean",  
#                "chen_mean","perc_change_trade_mean","perc_change_trade_mean_bis","trade_change_hs2")
# corr_vars <- c("FE_Chen_mean", "FEb_Chen_mean", "FEm_Chen_mean", "tariff_mean","chen_tariff_mean",  
#                "chen_mean","perc_change_trade_mean","perc_change_trade_mean_bis","trade_change_hs2")


corr_matrix <- US_all_q_hs2 %>%  ungroup() %>%
  select(all_of(corr_vars)) %>%
  cor(use = "pairwise.complete.obs")

corr_labels <- c("FE", "FE_bench", "FE_wmean", "Tariff", "Tariff (Chen)","Chen","% Trade","%change (-)")

rownames(corr_matrix) <- corr_labels
colnames(corr_matrix) <- corr_labels

p_hs2 <-ggcorrplot(corr_matrix,
                   method   = "square",
                   type     = "lower",
                   lab      = TRUE,
                   lab_size = 3.5,
                   colors   = c("#d73027", "white", "#4575b4"),
                   title    = "Correlation: AVE, tariff, and trade changes at HS2 digit level(2017 Vs 2019)",
                   ggtheme  = theme_minimal(base_size = 11))
p_hs2
ggsave(filename = "corr_AVE_tariff_trade_2019_HS2.png",  path     = OUT_PLOT_DIR,  plot     = p_hs2,  width    = 7,  height   = 6,  dpi      = 300,
       bg       = "white")





names(US_all_q_hs2)

corr_vars <- c("FE_log_Chen_mean", "FEb_log_Chen_mean", "FEm_log_Chen_mean",  "tariff_mean","chen_tariff_mean",  "chen_mean","perc_change_trade_mean","perc_change_trade_mean_bis")

corr_matrix <- US_all_q_hs2 %>%
  ungroup() %>%
  select(all_of(corr_vars)) %>%
  cor(use = "pairwise.complete.obs")
corr_labels <- c("FE log_Chen", "FE_bench log_Chen", "FE_wmean log_Chen","Tariff", "Tariff (Chen)","Chen","Trade change","Trade change (-)" )



rownames(corr_matrix) <- corr_labels
colnames(corr_matrix) <- corr_labels

p_chen <- ggcorrplot(corr_matrix,
                     method   = "square",
                     type     = "lower",
                     lab      = TRUE,
                     lab_size = 3.5,
                     colors   = c("#d73027", "white", "#4575b4"),
                     title    = "Correlation: AVE, tariff, and trade changes at HS2 digit level(2017 Vs 2019)",
                     ggtheme  = theme_minimal(base_size = 11))
p_chen
# ggsave("corr_AVE_tariff_trade_2019.png", plot = p, width = 7, height = 6, dpi = 300)


# ################################################################################
# # a) HS 4 level level
# ################################################################################


names(US_dta)
US_all_w_hs4 <- US_dta %>%   group_by(year, hs4, subsector) %>%   filter(year %in% c(2019)) %>% 
  summarise(  w_FE        = mean(diff_ln_AVE_FE,    na.rm = TRUE   ),
              w_FE_bench  = mean(diff_ln_AVE_FE_bench,   na.rm = TRUE),
              w_FE_mean   = mean(diff_ln_AVE_FE_wmean,  na.rm = TRUE),
              
              # added (log)
              w_FE_log        = mean(diff_ln_AVE_FE_log,   na.rm = TRUE    ),
              w_FE_log_bench  = mean(diff_ln_AVE_FE_log_bench,   na.rm = TRUE),
              w_FE_log_mean   = mean(diff_ln_AVE_FE_log_wmean,   na.rm = TRUE),
              
              w_tariff    = mean(diff_log_tariff_2017,  na.rm = TRUE),
              .groups = "drop") %>%
  left_join(trade_hs4_post %>% select(year, hs4, pct_trade_change_hs4,pct_trade_change_hs4_bis ), by = c("year", "hs4")) %>% filter(year %in% c(2019))

names(US_all_w_hs4)


corr_vars <- c("w_FE", "w_FE_bench", "w_FE_mean", "w_tariff", "pct_trade_change_hs4", "pct_trade_change_hs4_bis")
corr_vars <- c("w_FE_log", "w_FE_log_bench", "w_FE_log_mean", "w_tariff", "pct_trade_change_hs4", "pct_trade_change_hs4_bis")

corr_matrix <- US_all_w_hs4 %>%  ungroup() %>%
  select(all_of(corr_vars)) %>%
  cor(use = "pairwise.complete.obs")

corr_labels <- c("FE", "FE_bench", "FE_wmean", "Tariff", "Trade change", "Trade change (-)")

rownames(corr_matrix) <- corr_labels
colnames(corr_matrix) <- corr_labels

p_hs4 <-ggcorrplot(corr_matrix,
                   method   = "square",
                   type     = "lower",
                   lab      = TRUE,
                   lab_size = 3.5,
                   colors   = c("#d73027", "white", "#4575b4"),
                   title    = "Correlation: AVE, tariff, and trade changes at HS4 digit level(2017 Vs 2019)",
                   ggtheme  = theme_minimal(base_size = 11))
p_hs4
ggsave(filename = "corr_AVE_tariff_trade_2019_HS4.png",  path     = OUT_PLOT_DIR,  plot     = p_hs4,  width    = 7,  height   = 6,  dpi      = 300,
       bg       = "white")



################################################################################

library(writexl)
write_xlsx( list("hs4_summary" = US_all_q_hs4),  path = file.path(OUT_PLOT_DIR, paste0("hs4_summary_all_", quant , ".xlsx")))

US_all_q_hs4


