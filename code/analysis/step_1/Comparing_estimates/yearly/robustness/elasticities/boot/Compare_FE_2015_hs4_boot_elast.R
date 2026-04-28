

################################################################################
#                    Gravity regression analysis: residual approach


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
library(countrycode)
library(Hmisc)
library(haven)
library(sfaR)
library(frontier)

################################################################################
# directory: 
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git")
exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/Compare_values/yearly/robust/elast/"

################################################################################


################################################################################
# 1) Load data
################################################################################

# load trade data
library(data.table)
library(readr)
library(dplyr)

# --- Load ---
trade    <- fread("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_gravity/dta_CHN_gravity_yearly.csv")
fe_boot  <- fread("/data/sikeme/TRADE/US_CHN_TradeWar_git/output/FE/yearly/boot/gravity_pois_FE_boot_cluster_fixef.csv")
fe_log   <- fread("/data/sikeme/TRADE/US_CHN_TradeWar_git/output/FE/yearly/boot/gravity_logOLS_FE_boot_cluster_fixef_drop0.csv")

# Strip "_NNN" from anywhere in fe_id (matches underscore followed by digits)
fe_boot[, fe_id := gsub("_[0-9]+", "", fe_id)]
fe_log[,  fe_id := gsub("_[0-9]+", "", fe_id)]

# not clustered:
# fe_boot  <- fread("/data/sikeme/TRADE/US_CHN_TradeWar_git/output/FE/yearly/boot/gravity_pois_FE_boot_fixef.csv")
# fe_log   <- fread("/data/sikeme/TRADE/US_CHN_TradeWar_git/output/FE/yearly/boot/gravity_logOLS_FE_boot_fixef_drop0.csv")
#
# test1 <- fe_boot %>% filter(grepl("CHN", fe_id))
# test <- fe_log %>% filter(grepl("CHN", fe_id))
# rm(test1, test); gc()


setnames(fe_log, "FE", "FE_log")
fe_log <- fe_log[!is.na(FE_log)]

# D <- length(unique(fe_boot$draw))
# D
# D <-10
# #
# fe_boot <- fe_boot %>% filter(draw %in% c(1:D))
# fe_log <- fe_log %>% filter(draw %in% c(1:D))

D_total <- length(unique(fe_boot$draw))
D_total

################################################################################
# 2) Aggregate trade — runs ONCE
################################################################################

trade$hs6_H5 <- sprintf("%06d", trade$hs6_H5)
trade[, `:=`(hs2 = substr(hs6_H5, 1, 2), hs4 = substr(hs6_H5, 1, 4))]

trade_hs4 <- trade[, .(Trade_value_USD = sum(Trade_value_USD, na.rm = TRUE)),
                   by = .(year, hs2, hs4, ExporterISO3, ImporterISO3)]
trade_hs4[, fe_id := paste(year, ExporterISO3, hs4, sep = ".")]
trade_hs4[, hs2 := as.numeric(hs2)]

# HS section / sector — same for every draw, do it ONCE on trade_hs4
trade_hs4[, hs_section := fcase(
  hs2 %in% 1:5,   1,  hs2 %in% 6:14,  2,  hs2 == 15,      3,
  hs2 %in% 16:24, 4,  hs2 %in% 25:27, 5,  hs2 %in% 28:38, 6,
  hs2 %in% 39:40, 7,  hs2 %in% 41:43, 8,  hs2 %in% 44:46, 9,
  hs2 %in% 47:49, 10, hs2 %in% 50:63, 11, hs2 %in% 64:67, 12,
  hs2 %in% 68:70, 13, hs2 == 71,      14, hs2 %in% 72:83, 15,
  hs2 %in% 84:85, 16, hs2 %in% 86:89, 17, hs2 %in% 90:92, 18,
  hs2 == 93,      19, hs2 %in% 94:96, 20, hs2 == 97,      21
)]
trade_hs4[, sector := fcase(
  hs_section %in% 1:4,  "Ag",
  hs_section %in% 5:20, "Manu",
  default = "Other"
)]

# HS4 trade share weights — draw-independent
tot_hs4 <- trade_hs4[ExporterISO3 != "USA",
                     .(tot_hs4_trade = sum(Trade_value_USD, na.rm = TRUE)),
                     by = .(year, hs4)]
trade_hs4[tot_hs4, tot_hs4_trade := i.tot_hs4_trade, on = c("year", "hs4")]
trade_hs4[, share_hs4_trade := fifelse(
  !is.na(tot_hs4_trade) & tot_hs4_trade != 0,
  Trade_value_USD / tot_hs4_trade, 0
)]
rm(tot_hs4, trade); gc()

################################################################################
# 3) Elasticities — also draw-independent, attach to trade_hs4 ONCE
################################################################################

elast <- fread("data/elast/clean_Elasticities_Soderbery2018.csv")
trade_hs4[, hs4 := as.numeric(hs4)]
elast[, hs4 := as.numeric(hs4)]

trade_hs4 <- merge(trade_hs4, elast,
                   by = c("ImporterISO3", "ExporterISO3", "hs4"),
                   all.x = TRUE)

# Fill NAs with hs4-level mean
hs4_mean_elast <- trade_hs4[!is.na(elasticities),
                            .(elasticities_mean = mean(elasticities, na.rm = TRUE)),
                            by = hs4]
trade_hs4[hs4_mean_elast,
          elasticities := fifelse(is.na(elasticities), i.elasticities_mean, elasticities),
          on = "hs4"]
rm(hs4_mean_elast); gc()

trade_hs4[, CHEN_elasticities := fcase(
  sector == "Ag",    3,
  sector == "Manu",  1.97,
  sector == "Other", 5
)]



################################################################################
# 4) DRAW-BATCHED PROCESSING
################################################################################

batch_size <- 50     # try 25 first; lower to 10 if memory still tight
batches    <- split(1:D_total, ceiling(seq_along(1:D_total) / batch_size))

out_dir <- paste0(exp, "draw_batches/")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

for (b in seq_along(batches)) {
  # b <- 1
  draws_in_batch <- batches[[b]]
  message(sprintf("Batch %d/%d: draws %d–%d",
                  b, length(batches),
                  min(draws_in_batch), max(draws_in_batch)))

  # ---- Subset FE tables to just this batch ----
  fe_boot_b <- fe_boot[draw %in% draws_in_batch]
  fe_log_b  <- fe_log[draw %in% draws_in_batch]
  fe_all_b  <- merge(fe_boot_b, fe_log_b,
                     by = c("fe_id", "draw", "hs2"), all = TRUE)
  rm(fe_boot_b, fe_log_b); gc()

  D_b <- length(draws_in_batch)

  # ---- Build trade_with_draws for THIS batch only ----
  twd <- trade_hs4[rep(1:.N, each = D_b)]
  twd[, draw := rep(draws_in_batch, times = nrow(trade_hs4))]
  fe_all_b[, hs2 := as.numeric(hs2)]

  dta <- merge(twd, fe_all_b,
               by = c("fe_id", "hs2", "draw"),
               all.x = TRUE, allow.cartesian = TRUE)
  rm(twd, fe_all_b); gc()

  # ---- Benchmarks (depends only on this batch's draws) ----
  bench <- dta[ExporterISO3 != "USA",
               .(FE_bench = {
                 m <- suppressWarnings(max(FE, na.rm = TRUE))
                 fifelse(is.infinite(m), NA_real_, m)
               },
               FE_log_bench = {
                 m <- suppressWarnings(max(FE_log, na.rm = TRUE))
                 fifelse(is.infinite(m), NA_real_, m)
               },
               FE_wmean = {
                 m <- weighted.mean(FE, w = share_hs4_trade, na.rm = TRUE)
                 fifelse(is.nan(m), NA_real_, m)
               },
               FE_log_wmean = {
                 m <- weighted.mean(FE_log, w = share_hs4_trade, na.rm = TRUE)
                 fifelse(is.nan(m), NA_real_, m)
               }),
               by = .(year, draw, hs4)]

  dta[bench, `:=`(FE_bench     = i.FE_bench,
                  FE_log_bench = i.FE_log_bench,
                  FE_wmean     = i.FE_wmean,
                  FE_log_wmean = i.FE_log_wmean),
      on = c("year", "draw", "hs4")]
  rm(bench); gc()

  # ---- 2015 baselines (per draw) ----
  pre17 <- dta[year == 2015,
               .(FE_pre_2015           = mean(FE,           na.rm = TRUE),
                 FE_pre_2015_bench     = mean(FE_bench,     na.rm = TRUE),
                 FE_pre_2015_wmean     = mean(FE_wmean,     na.rm = TRUE),
                 FE_log_pre_2015       = mean(FE_log,       na.rm = TRUE),
                 FE_log_pre_2015_bench = mean(FE_log_bench, na.rm = TRUE),
                 FE_log_pre_2015_wmean = mean(FE_log_wmean, na.rm = TRUE)),
               by = .(ExporterISO3, draw, hs4)]

  dta[pre17, `:=`(FE_pre_2015           = i.FE_pre_2015,
                  FE_pre_2015_bench     = i.FE_pre_2015_bench,
                  FE_pre_2015_wmean     = i.FE_pre_2015_wmean,
                  FE_log_pre_2015       = i.FE_log_pre_2015,
                  FE_log_pre_2015_bench = i.FE_log_pre_2015_bench,
                  FE_log_pre_2015_wmean = i.FE_log_pre_2015_wmean),
      on = c("ExporterISO3", "draw", "hs4")]
  rm(pre17); gc()

  # ---- Differences vs 2015 ----
  dta[, `:=`(
    diff_FE_2015 = fifelse(
      year > 2015 & !is.na(FE) & !is.na(FE_pre_2015),
      FE - FE_pre_2015, NA_real_),
    diff_FE_2015_bench = fifelse(
      year > 2015 & !is.na(FE) & !is.na(FE_pre_2015),
      (FE - FE_bench) - (FE_pre_2015 - FE_pre_2015_bench), NA_real_),
    diff_FE_2015_wmean = fifelse(
      year > 2015 & !is.na(FE) & !is.na(FE_pre_2015),
      (FE - FE_wmean) - (FE_pre_2015 - FE_pre_2015_wmean), NA_real_),
    diff_FE_log_2015 = fifelse(
      year > 2015 & !is.na(FE_log) & !is.na(FE_log_pre_2015),
      FE_log - FE_log_pre_2015, NA_real_),
    diff_FE_log_2015_bench = fifelse(
      year > 2015 & !is.na(FE_log) & !is.na(FE_log_pre_2015),
      (FE_log - FE_log_bench) - (FE_log_pre_2015 - FE_log_pre_2015_bench), NA_real_),
    diff_FE_log_2015_wmean = fifelse(
      year > 2015 & !is.na(FE_log) & !is.na(FE_log_pre_2015),
      (FE_log - FE_log_wmean) - (FE_log_pre_2015 - FE_log_pre_2015_wmean), NA_real_)
  )]

  # ---- AVE construction ----
  dta[, `:=`(
    ln_AVE_FE              = (1 / (1 - elasticities)) * FE,
    diff_ln_AVE_FE         = fifelse(year %in% 2016:2020, (1 / (1 - elasticities)) * diff_FE_2015,           NA_real_),
    diff_ln_AVE_FE_log     = fifelse(year %in% 2016:2020, (1 / (1 - elasticities)) * diff_FE_log_2015,       NA_real_),
    diff_ln_AVE_FE_bench   = fifelse(year %in% 2016:2020, (1 / (1 - elasticities)) * diff_FE_2015_bench,     NA_real_),
    diff_ln_AVE_FE_log_bench = fifelse(year %in% 2016:2020, (1 / (1 - elasticities)) * diff_FE_log_2015_bench, NA_real_),
    diff_ln_AVE_FE_wmean   = fifelse(year %in% 2016:2020, (1 / (1 - elasticities)) * diff_FE_2015_wmean,     NA_real_),
    diff_ln_AVE_FE_log_wmean = fifelse(year %in% 2016:2020, (1 / (1 - elasticities)) * diff_FE_log_2015_wmean, NA_real_),

    ln_AVE_FE_Chen              = (1 / (1 - CHEN_elasticities)) * FE,
    diff_ln_AVE_FE_Chen         = fifelse(year %in% 2016:2020, (1 / (1 - CHEN_elasticities)) * diff_FE_2015,           NA_real_),
    diff_ln_AVE_FE_log_Chen     = fifelse(year %in% 2016:2020, (1 / (1 - CHEN_elasticities)) * diff_FE_log_2015,       NA_real_),
    diff_ln_AVE_FE_bench_Chen   = fifelse(year %in% 2016:2020, (1 / (1 - CHEN_elasticities)) * diff_FE_2015_bench,     NA_real_),
    diff_ln_AVE_FE_log_bench_Chen = fifelse(year %in% 2016:2020, (1 / (1 - CHEN_elasticities)) * diff_FE_log_2015_bench, NA_real_),
    diff_ln_AVE_FE_wmean_Chen   = fifelse(year %in% 2016:2020, (1 / (1 - CHEN_elasticities)) * diff_FE_2015_wmean,     NA_real_),
    diff_ln_AVE_FE_log_wmean_Chen = fifelse(year %in% 2016:2020, (1 / (1 - CHEN_elasticities)) * diff_FE_log_2015_wmean, NA_real_)

  )]

  # ---- Write batch to disk ----
  fwrite(dta, sprintf("%sbatch_%03d.csv", out_dir, b))
  rm(dta); gc()
}

rm(fe_boot, fe_log, trade_hs4); gc()

################################################################################
# 5) Tariff aggregation (once), then merge into each batch
################################################################################
out_dir <- paste0(exp, "draw_batches/")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)


# ---- Aggregate tariffs to HS4 ONCE, before touching any batch files ----
tariffs <- fread("/data/sikeme/TRADE/US_CHN_TradeWar_git/output/Compare_values/yearly/robust/estimates_log_tariff_FE_boot.csv",
                 select = c("year", "hs4", "hs6_H5", "ExporterISO3", "ImporterISO3",
                            "Trade_value_USD", "Applied_tariff", "log_tariff",
                            "log_tariff_pre_2015", "diff_log_tariff_2015",
                            "log_tariff_pre_2017", "diff_log_tariff_2017"),
                 integer64 = "double")

w_hs4_2015 <- tariffs[year == 2015,
                      .(Trade_value_USD = sum(Trade_value_USD, na.rm = TRUE)),
                      by = .(hs4, hs6_H5)]
w_hs4_2015[, tot := sum(Trade_value_USD), by = hs4]
w_hs4_2015[, weight_hs4_2015 := fifelse(tot > 0, Trade_value_USD / tot, 0)]
w_hs4_2015 <- w_hs4_2015[, .(hs4, hs6_H5, weight_hs4_2015)]

w_hs4_2017 <- tariffs[year == 2017,
                      .(Trade_value_USD = sum(Trade_value_USD, na.rm = TRUE)),
                      by = .(hs4, hs6_H5)]
w_hs4_2017[, tot := sum(Trade_value_USD), by = hs4]
w_hs4_2017[, weight_hs4_2017 := fifelse(tot > 0, Trade_value_USD / tot, 0)]
w_hs4_2017 <- w_hs4_2017[, .(hs4, hs6_H5, weight_hs4_2017)]

tariffs[w_hs4_2015, weight_hs4_2015 := i.weight_hs4_2015, on = c("hs4", "hs6_H5")]
tariffs[w_hs4_2017, weight_hs4_2017 := i.weight_hs4_2017, on = c("hs4", "hs6_H5")]
rm(w_hs4_2015, w_hs4_2017); gc()

tariffs_hs4 <- tariffs[,
                       .(
                         diff_log_tariff_2015 = if (first(year) > 2015)
                           weighted.mean(diff_log_tariff_2015, w = weight_hs4_2015, na.rm = TRUE) else NA_real_,
                         diff_log_tariff_2017 = if (first(year) > 2017)
                           weighted.mean(diff_log_tariff_2017, w = weight_hs4_2017, na.rm = TRUE) else NA_real_
                       ),
                       by = .(year, hs4, ExporterISO3, ImporterISO3)
]
tariffs_hs4[, hs4 := as.numeric(hs4)]
rm(tariffs); gc()

################################################################################
# 6) Merge tariffs into each batch, write merged batch to a new directory
################################################################################
# test
merged_dir <- paste0(exp, "draw_batches_merged/")
dir.create(merged_dir, showWarnings = FALSE, recursive = TRUE)
#
# batch_files <- list.files(out_dir, pattern = "^batch_.*\\.csv$", full.names = TRUE)
# dta <- rbindlist(lapply(batch_files, fread), use.names = TRUE)
# colSums(is.na(dta))


################################################################################
batch_files <- list.files(out_dir, pattern = "^batch_.*\\.csv$", full.names = TRUE)

for (i in seq_along(batch_files)) {
  message(sprintf("Merging tariffs into batch %d/%d", i, length(batch_files)))

  bf <- fread(batch_files[i], integer64 = "double")
  bf[, hs4 := as.numeric(hs4)]

  bf <- merge(bf, tariffs_hs4,
              by = c("year", "hs4", "ExporterISO3", "ImporterISO3"),
              all.x = TRUE)
  # NOTE: switched to all.x = TRUE (was all = TRUE).
  # Keep every row from the batch; tariff rows that don't match a batch row
  # would have been added with NAs everywhere else, which inflates output.
  # If you actually want full-join behavior, switch back to all = TRUE.

  out_path <- file.path(merged_dir, basename(batch_files[i]))
  fwrite(bf, out_path)

  rm(bf); gc()
}

rm(tariffs_hs4); gc()


################################################################################
# 7) Combine merged batches into final output (streaming where possible)
################################################################################

# Final combined file path
final_path <- paste0(exp, "estimates_reduced_form_base_2015_FE_boot_elast.csv")

merged_files <- list.files(merged_dir, pattern = "^batch_.*\\.csv$", full.names = TRUE)

# Streaming concatenation: write first batch with header, append the rest
fwrite(fread(merged_files[1], integer64 = "double"), final_path)

for (i in 2:length(merged_files)) {
  fwrite(fread(merged_files[i], integer64 = "double"),
         final_path,
         append = TRUE)
}


################################################################################
# 7) Combine merged batches into final output (streaming where possible)
################################################################################
# List the files written by section 6
merged_files <- list.files(merged_dir, pattern = "^batch_.*\\.csv$", full.names = TRUE)

US_chunks <- vector("list", length(merged_files))
for (i in seq_along(merged_files)) {
  chunk <- fread(merged_files[i], integer64 = "double")
  US_chunks[[i]] <- chunk[ExporterISO3 == "USA"]
  rm(chunk); gc()
}
US <- rbindlist(US_chunks, use.names = TRUE)
rm(US_chunks); gc()

fwrite(US, paste0(exp, "US_ln_NTMs_base_2015_FE_boot_hs4_elast.csv"))


rm(test); gc()


################################################################################
# 8) US subset — read final file and filter
################################################################################

# Use cmd-line filter so we never load the full file into RAM.
# This works on Linux/Mac. If on Windows, fall back to chunked reading.
US <- fread(cmd = sprintf("awk -F, 'NR==1 || $X==\"USA\"' %s", final_path))
# Replace $X with the column number of ExporterISO3. To find it:
# names(fread(final_path, nrows = 0))

# OR simpler/portable: read in chunks
# (uncomment if awk isn't available)
#
# US_chunks <- list()
# for (i in seq_along(merged_files)) {
#   chunk <- fread(merged_files[i], integer64 = "double")
#   US_chunks[[i]] <- chunk[ExporterISO3 == "USA"]
#   rm(chunk); gc()
# }
# US <- rbindlist(US_chunks, use.names = TRUE)
# rm(US_chunks); gc()

fwrite(US, paste0(exp, "US_ln_NTMs_base_2015_FE_boot_hs4_elast.csv"))
rm(US); gc()


test <- fread(paste0(exp, "US_ln_NTMs_base_2015_FE_boot_hs4_elast.csv"))
