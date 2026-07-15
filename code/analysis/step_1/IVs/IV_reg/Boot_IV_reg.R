################################################################################
# Gravity regression analysis: residual approach (parameterized)
################################################################################

rm(list=ls()); gc()

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
# directory: 
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git")
exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/Compare_values/yearly/robust/"

################################################################################
# 1) Load data 
################################################################################

# US_2015 <- read_csv(paste0(exp, "US_ln_NTMs_base_2015_FE_boot_hs4.csv"))
US_2017 <- read_csv(paste0(exp, "US_ln_NTMs_base_2017_FE_boot_hs4.csv"))


tariff_dta <- read_csv(paste0("/data/sikeme/TRADE/US_CHN_TradeWar_git/output/Compare_values/yearly/robust/US_ln_NTMs_base_2015_FE_boot_hs4.csv"))

# Load SOE data
SOE <- read_csv("data/SOE_dta/SOE_share_2010.csv")
US_import_share <- read_csv("data/SOE_dta/US_import_share_2010.csv")
names(US_import_share)
################################################################################
# filter year 
# dta <- dta %>% filter(year %in% c(2018,2019))
names(SOE)
SOE <- SOE %>% select(-Year)


SOE <- SOE %>% mutate(hs4 =  str_sub(hs6_H5, 1,4), hs2 =  str_sub(hs6_H5, 1,2))

# for SOE:
SOE_hs4 <- SOE %>% group_by(hs4) %>%
  summarise(Trade_value_USD_SOE = sum(Trade_value_USD_SOE, na.rm=TRUE),
            tot_Trade_value_USD = sum(tot_Trade_value_USD, na.rm=TRUE)  ) %>%
  ungroup()
SOE_hs4 <- SOE_hs4 %>% mutate(share_value_SOE = Trade_value_USD_SOE / tot_Trade_value_USD ) %>%
  select(hs4, share_value_SOE)


SOE_hs6 <- SOE %>% group_by(hs4, hs6_H5) %>%
  summarise(Trade_value_USD_SOE = sum(Trade_value_USD_SOE, na.rm=TRUE),
            tot_Trade_value_USD = sum(tot_Trade_value_USD, na.rm=TRUE)  ) %>%
  ungroup()
SOE_hs6 <- SOE_hs6 %>% mutate(share_value_SOE = Trade_value_USD_SOE / tot_Trade_value_USD ) %>%
  select(hs4, hs6_H5, share_value_SOE)



################################################################################
# aggregate tariff data at hs4 level to include in the regression
################################################################################



library(dplyr)
library(fixest)
library(purrr)
library(rlang)

## ---- 0. Pick the specification -----------------------------------------
dep_var   <- "diff_ln_AVE_FE_wmean"   # swap for diff_ln_AVE_FE_bench / diff_ln_AVE_FE
tariff_var <- "diff_log_tariff_2017"

## ---- 1. Fixed pieces (do NOT vary across draws) ------------------------
# Tariffs are identical across draws: take one draw, drop the draw column
tariff_merge <- tariff_dta %>%  filter(year %in% 2018:2019, draw == 1) %>%
  select(year, hs4, all_of(tariff_var))

# SOE_hs4 is already hs4 + share_value_SOE, constant across draws

## ---- 2. Build the analysis panel once, for all draws -------------------
analysis <- US_2017 %>%
  filter(year %in% 2018:2019) %>%
  select(draw, year, hs2, hs4, sector, all_of(dep_var)) %>%
  left_join(tariff_merge, by = c("year", "hs4")) %>%
  left_join(SOE_hs4,      by = "hs4")

# sanity check: usable rows per draw should be stable across draws
analysis %>%
  filter(complete.cases(
    .data[[dep_var]], .data[[tariff_var]], share_value_SOE, sector, year)) %>%
  count(draw) %>% pull(n) %>% summary()

## ---- 3. One regression per draw ----------------------------------------
fml <- as.formula(paste0(
  dep_var, " ~ ", tariff_var,
   " + as.factor(year)*share_value_SOE + as.factor(year) + as.factor(sector)"
  # " + share_value_SOE "
  
))

run_one_draw <- function(d) {
  df <- analysis %>% filter(draw == d)
  
  m <- feols(fml, data = df)
  
  # coefficients (every term, with bootstrap-ready estimate + analytic SE)
  ct <- as.data.frame(coeftable(m))
  coefs <- tibble(
    draw     = d,
    term     = rownames(ct),
    estimate = ct[["Estimate"]],
    se       = ct[["Std. Error"]]
  )
  
  # predicted AVE aligned back to every row (NA where covariates missing)
  df$predicted_ave <- predict(m, newdata = df)
  preds <- df %>%
    mutate(actual_ave = .data[[dep_var]]) %>%
    select(draw, year, hs2, hs4, sector, actual_ave, predicted_ave)
  
  list(coefs = coefs, preds = preds)
}

## ---- 4. Run over all draws ---------------------------------------------
draws <- sort(unique(analysis$draw))
res   <- map(draws, run_one_draw)

coef_boot <- bind_rows(map(res, "coefs"))   # 500 * n_terms rows
pred_boot <- bind_rows(map(res, "preds"))   # 500 * n_hs4_rows rows  <-- feeds step 2

## ---- 5. Summaries ------------------------------------------------------
# Coefficient distribution across draws (sd here is your bootstrap SE)
coef_summary <- coef_boot %>%
  group_by(term) %>%
  summarise(
    mean_est = mean(estimate),
    boot_se  = sd(estimate),
    p2.5     = quantile(estimate, 0.05),
    p97.5    = quantile(estimate, 0.95),
    .groups  = "drop"
  )
coef_summary

# Coefficient distribution across draws (sd here is your bootstrap SE)
coef_summary <- coef_boot %>%
  group_by(term) %>%
  summarise(
    mean_est = mean(estimate),
    boot_se  = sd(estimate),
    p2.5     = quantile(estimate, 0.05),
    p97.5    = quantile(estimate, 0.95),
    .groups  = "drop"
  )
coef_summary


# Predicted AVE per product, summarised (for plotting / inspection only)
pred_summary <- pred_boot %>%
  group_by(hs4, year) %>%
  summarise(
    mean_pred = mean(predicted_ave, na.rm = TRUE),
    boot_se   = sd(predicted_ave,   na.rm = TRUE),
    .groups   = "drop"
  )

###################################################################################

dep_var    <- "diff_ln_AVE_FE_wmean"
tariff_var <- "diff_log_tariff_2017"
instr      <- "share_value_SOE"

fml <- as.formula(paste0(
  dep_var, " ~ ", tariff_var,  " + ", instr, " + as.factor(year) + as.factor(sector)"
  
))

## =======================================================================
## PROPOSITION 1: First-stage strength (F / t^2 on the excluded instrument)
## =======================================================================
# With a single excluded instrument, first-stage F = (t-stat on instrument)^2.
# Cluster SEs the same way as your main spec (you cluster by year in step 2).

first_stage_F <- function(df, cluster = "hs4") {
  m  <- feols(fml, data = df, cluster = cluster)
  ct <- coeftable(m)
  t  <- ct[instr, "t value"]
  tibble(
    coef    = ct[instr, "Estimate"],
    se      = ct[instr, "Std. Error"],
    t_stat  = t,
    F_stat  = t^2,          # single-instrument F
    n       = nobs(m)
  )
}

# On one draw
fs_one <- first_stage_F(filter(analysis, draw == 1))
fs_one

# Across all draws: is the instrument reliably relevant, or only sometimes?
fs_boot <- map_dfr(sort(unique(analysis$draw)), function(d) {
  out <- tryCatch(first_stage_F(filter(analysis, draw == d)),
                  error = function(e) NULL)
  if (!is.null(out)) mutate(out, draw = d) else NULL
})

fs_boot %>%
  summarise(
    median_F = median(F_stat),
    p10_F    = quantile(F_stat, 0.10),
    p90_F    = quantile(F_stat, 0.90),
    share_F_above_10 = mean(F_stat > 10),   # fraction of draws clearing the rule of thumb
    share_F_above_23 = mean(F_stat > 23.1)  # Stock-Yogo-ish stricter bar
  )
fs_boot



## =======================================================================
## PROPOSITION 2: Influence / tail dependence
## Refit dropping high-SOE products. If the relationship dies, it was a
## few leverage points, not identification.
## =======================================================================

refit_drop <- function(df, cutoff = NULL, top_frac = NULL, floor = NULL) {
  d <- df
  if (!is.null(cutoff))   d <- filter(d, share_value_SOE <= cutoff)
  if (!is.null(top_frac)) {
    thr <- quantile(d$share_value_SOE, 1 - top_frac, na.rm = TRUE)
    d   <- filter(d, share_value_SOE <= thr)
  }
  if (!is.null(floor))    d <- filter(d, share_value_SOE >= floor)
  
  m  <- feols(fml, data = d, cluster = "hs4")
  ct <- coeftable(m)
  tibble(
    coef_SOE = ct[instr, "Estimate"],
    se_SOE   = ct[instr, "Std. Error"],
    t_SOE    = ct[instr, "t value"],
    F_SOE    = ct[instr, "t value"]^2,
    n        = nobs(m)
  )
}

df1 <- filter(analysis, draw == 1)

influence_table <- bind_rows(
  refit_drop(df1)                          %>% mutate(sample = "full"),
  refit_drop(df1, cutoff = 0.5)            %>% mutate(sample = "drop SOE > 0.5"),
  refit_drop(df1, cutoff = 0.25)           %>% mutate(sample = "drop SOE > 0.25"),
  refit_drop(df1, top_frac = 0.05)         %>% mutate(sample = "drop top 5%"),
  refit_drop(df1, top_frac = 0.10)         %>% mutate(sample = "drop top 10%"),
  refit_drop(df1, floor = 0.25)            %>% mutate(sample = "drop SOE < 0.25"),
  refit_drop(df1, floor = 0.10)            %>% mutate(sample = "drop SOE < 0.10"),
  refit_drop(df1, floor = 0.05)            %>% mutate(sample = "drop SOE < 0.05"),
  refit_drop(df1, floor = 0.05)            %>% mutate(sample = "drop SOE < 0.01"),
  ) %>%
  select(sample, n, coef_SOE, se_SOE, t_SOE, F_SOE)
influence_table



#################################################################################

# wth import_shares
#################################################################################

library(dplyr)
library(fixest)
library(purrr)
library(tibble)

dep_var    <- "diff_ln_AVE_FE_wmean"
tariff_var <- "diff_log_tariff_2017"

## ---- Build the HS4 US import share (2010), constant across draws --------
US_imp_hs4 <- US_import_share %>%
  filter(ISO3 == "USA") %>%          # confirm the US rows; drop if file is already US-only
  select(hs4, share_import_value) %>%
  distinct()

## ---- Rebuild analysis panel with BOTH product-level shifters ------------
analysis2 <- US_2017 %>%
  filter(year %in% 2018:2019) %>%
  select(draw, year, hs2, hs4, sector, all_of(dep_var)) %>%
  left_join(tariff_merge, by = c("year", "hs4")) %>%
  left_join(SOE_hs4,      by = "hs4") %>%
  left_join(US_imp_hs4,   by = "hs4")

# check coverage: how many HS4 fail to match a US import share?
analysis2 %>% summarise(
  n            = n(),
  miss_usshare = mean(is.na(share_import_value)),
  miss_soe     = mean(is.na(share_value_SOE))
)

## ---- Specification: both shifters in the AVE regression -----------------
fml2 <- as.formula(paste0(
  dep_var, " ~ ", tariff_var,
  " + share_value_SOE + share_import_value",
  " + as.factor(year) + as.factor(sector)"
))

run_one_draw <- function(d, data, fml) {
  df <- data %>% filter(draw == d)
  m  <- feols(fml, data = df, cluster = ~hs4)
  ct <- as.data.frame(coeftable(m))
  tibble(draw = d, term = rownames(ct),
         estimate = ct[["Estimate"]], se = ct[["Std. Error"]],
         t = ct[["t value"]])
}

draws     <- sort(unique(analysis2$draw))
coef_boot <- map_dfr(draws, run_one_draw, data = analysis2, fml = fml2)

## ---- Bootstrap summary, both shifters -----------------------------------
coef_summary <- coef_boot %>%
  group_by(term) %>%
  summarise(
    mean_est = mean(estimate),
    boot_se  = sd(estimate),
    p2.5     = quantile(estimate, 0.025),
    p97.5    = quantile(estimate, 0.975),
    share_sig = mean(abs(t) > 1.96),     # fraction of draws where term is individually significant
    .groups  = "drop"
  )
coef_summary

## ---- Instrument-specific first-stage F for each shifter, across draws ----
## (squared t on each shifter; this is the number that matters, not model F)
fs_strength <- coef_boot %>%
  filter(term %in% c("share_value_SOE", "share_import_value")) %>%
  mutate(F = t^2) %>%
  group_by(term) %>%
  summarise(
    median_F        = median(F),
    p10_F           = quantile(F, 0.10),
    p90_F           = quantile(F, 0.90),
    share_F_above10 = mean(F > 10),
    .groups = "drop"
  )
fs_strength

