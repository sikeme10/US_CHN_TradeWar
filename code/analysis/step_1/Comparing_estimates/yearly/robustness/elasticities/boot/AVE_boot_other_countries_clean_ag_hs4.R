rm(list = ls())
library(data.table)

out_dir  <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/Compare_values/yearly/robust/elast/"
base_year <- 2017

################################################################################

keys <- c("year","sector","hs_section","hs2","hs4","ExporterISO3","ImporterISO3")
avg_cols <- c("diff_ln_AVE_FE","diff_ln_AVE_FE_log",
              "diff_ln_AVE_FE_bench","diff_ln_AVE_FE_log_bench",
              "diff_ln_AVE_FE_wmean","diff_ln_AVE_FE_log_wmean")

dta <- fread(
  file.path(out_dir, sprintf("estimates_reduced_form_base_%d_FE_boot_elast.csv", base_year)),
  select = c(keys, "Trade_value_USD", avg_cols),
  nThread = getDTthreads()
)

# aggregate over draws, in place, no copy of the full table
out <- dta[, c(.(Trade_value_USD = first(Trade_value_USD)),
               lapply(.SD, mean, na.rm = TRUE)),
           by = keys, .SDcols = avg_cols]

rm(dta); gc()

fwrite(out, file.path(out_dir,
                      sprintf("estimates_reduced_form_base_%d_FE_boot_hs4_summarised.csv", base_year)))
################################################################################
################################################################################

# or with removing extreme values:
rm(list = ls())
library(data.table)

out_dir   <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/Compare_values/yearly/robust/elast/"
base_year <- 2017
quant     <- 0.05
################################################################################
keys <- c("year","sector","hs_section","hs2","hs4","ExporterISO3","ImporterISO3")
avg_cols <- c("diff_ln_AVE_FE","diff_ln_AVE_FE_log",
              "diff_ln_AVE_FE_bench","diff_ln_AVE_FE_log_bench",
              "diff_ln_AVE_FE_wmean","diff_ln_AVE_FE_log_wmean")

dta <- fread(
  file.path(out_dir, sprintf("estimates_reduced_form_base_%d_FE_boot_elast.csv", base_year)),
  select = c(keys, "Trade_value_USD", avg_cols),
  nThread = getDTthreads()
)

# trim extreme values per exporter, before averaging over draws
dta[, (avg_cols) := lapply(.SD, function(x) {
  lo <- quantile(x, quant,     na.rm = TRUE)
  hi <- quantile(x, 1 - quant, na.rm = TRUE)
  fifelse(x < lo | x > hi, NA_real_, x)
}), by = ExporterISO3, .SDcols = avg_cols]

# aggregate over draws, in place, no copy of the full table
out <- dta[, c(.(Trade_value_USD = first(Trade_value_USD)),
               lapply(.SD, mean, na.rm = TRUE)),
           by = keys, .SDcols = avg_cols]

rm(dta); gc()

fwrite(out, file.path(out_dir,
                      sprintf("estimates_reduced_form_base_%d_FE_boot_hs4_summarised.csv", base_year)))


################################################################################
year <-2015
out_dir  <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/Compare_values/yearly/robust/elast/"

dta <- read_csv(paste0(out_dir, "/estimates_reduced_form_base_", year, "_FE_boot_hs4_summarised.csv"))

################################################################################
# create a theme for ggplot
theme_trade <- theme_minimal(base_size = 14) +
  theme(panel.spacing.x = unit(1.2, "lines"),
        plot.title = element_text(size = 11, hjust = 0.5),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA),
        axis.text.x = element_text(size = 9),
        axis.text.y = element_text(size = 9),
        axis.title.x = element_text(size = 11),
        axis.title.y = element_text(size = 11),
        legend.text  = element_text(size = 10),
        legend.title = element_text(size = 10)  )


################################################################################
# 3) Filter to chosen sector
################################################################################


sectors_hs4 <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/crosswalk/clean_HS4_sub_sector_edit.csv")


sectors_hs4$hs4 <- as.numeric(sectors_hs4$hs4)
dta$hs4 <- as.numeric(dta$hs4)

# join
dta <- left_join(dta, sectors_hs4) 
colSums(is.na(dta))
test <- dta %>% filter(is.na(subsector))
unique(test$hs4)
colSums(is.na(dta))



# --- 1. DATA ONLY -------------------------------------------------------
make_subsector_data <- function(dta, sub_name) {
  
  ag <- dta %>% filter(subsector == sub_name)
  
  ag_2015_hs4 <- ag %>%
    filter(year == 2015) %>%
    group_by(ExporterISO3, hs4) %>%
    summarise(Trade_value_USD_2015 = sum(Trade_value_USD, na.rm = TRUE),
              .groups = "drop_last") %>%
    group_by(ExporterISO3) %>%
    mutate(
      tot_Trade_value_USD_2015 = sum(Trade_value_USD_2015),
      weight_sector = if_else(tot_Trade_value_USD_2015 > 0,
                              Trade_value_USD_2015 / tot_Trade_value_USD_2015,
                              NA_real_)
    ) %>%
    ungroup()
  
  ag <- ag %>% left_join(ag_2015_hs4, by = c("ExporterISO3", "hs4"))
  
  ag_w <- ag %>%
    filter(year %in% 2016:2020) %>%
    group_by(ExporterISO3, year) %>%
    summarise(
      w_FE       = weighted.mean(diff_ln_AVE_FE,       w = weight_sector, na.rm = TRUE),
      w_FE_bench = weighted.mean(diff_ln_AVE_FE_bench, w = weight_sector, na.rm = TRUE),
      w_FE_wmean = weighted.mean(diff_ln_AVE_FE_wmean, w = weight_sector, na.rm = TRUE),
      s_FE       = mean(diff_ln_AVE_FE,        na.rm = TRUE),
      s_FE_bench = mean(diff_ln_AVE_FE_bench,  na.rm = TRUE),
      s_FE_wmean = mean(diff_ln_AVE_FE_wmean,  na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(is_US = ExporterISO3 == "USA",
           subsector = sub_name)
  
  ag_w
}

subsectors <- c("crop", "livestock", "nonag")

data_all <- lapply(subsectors, function(s) make_subsector_data(dta, s)) %>%
  bind_rows() %>%
  mutate(subsector = str_to_title(subsector))

# --- 2. PLOT: all three subsectors side by side, US vs others -----------
plot_all <- data_all %>%
  ggplot(aes(x = year, y = w_FE_wmean, group = ExporterISO3, color = is_US)) +
  geom_line(linewidth = 0.5) +
  facet_wrap(~ subsector) +
  scale_color_manual(
    values = c(`TRUE` = "red", `FALSE` = "grey70"),
    breaks = c(TRUE, FALSE),
    labels = c("US", "Other countries"),
    name = NULL
  ) +
  labs(x = "Year", y = "Weighted Δ ln(1+AVE)") +
  theme_trade

plot_all





################################################################################
# 3) if compare other countries to US
################################################################################
unique(dta$subsector)
# 1. Filter data to chosen countries + Ag sector, build year
ag <- dta %>%   filter( subsector == "crop"    )

# 2. HS6 weights per country (baseline 2015)
ag_2015_hs6 <- ag %>%    filter(year == 2015) %>%
  group_by(ExporterISO3, hs4) %>%
  summarise(Trade_value_USD_2015 = sum(Trade_value_USD, na.rm = TRUE),
            .groups = "drop_last"    ) %>%
  group_by(ExporterISO3) %>%
  mutate( tot_Trade_value_USD_2015 = sum(Trade_value_USD_2015),
          weight_sector = if_else( tot_Trade_value_USD_2015 > 0,
                                   Trade_value_USD_2015 / tot_Trade_value_USD_2015,  NA_real_  )) %>%  ungroup()
ag <- ag %>%  left_join(ag_2015_hs6, by = c("ExporterISO3", "hs4"))


# 3. Weighted & simple averages over time PER COUNTRY
ag_w <- ag %>% filter(year %in% 2016:2020) %>%
  group_by(ExporterISO3, year) %>%
  summarise(
    w_FE        = weighted.mean(diff_ln_AVE_FE,        w = weight_sector, na.rm = TRUE),
    w_FE_bench  = weighted.mean(diff_ln_AVE_FE_bench,  w = weight_sector, na.rm = TRUE),
    w_FE_wmean  = weighted.mean(diff_ln_AVE_FE_wmean,  w = weight_sector, na.rm = TRUE),

    # w_tariff    = weighted.mean(diff_log_tariff_2015,  w = weight_sector, na.rm = TRUE),

    s_FE        = mean(diff_ln_AVE_FE,         na.rm = TRUE),
    s_FE_bench  = mean(diff_ln_AVE_FE_bench,   na.rm = TRUE),
    s_FE_wmean  = mean(diff_ln_AVE_FE_wmean,   na.rm = TRUE))

    # s_tariff    = mean(diff_log_tariff_2015,  na.rm = TRUE) ,  .groups = "drop"  )

ag_w <- ag_w %>%  mutate(is_US = ExporterISO3 == "USA")


# pick countries
countries_vec <- unique(ag_w$ExporterISO3)
#countries_vec <- c("USA", "BRA", "MEX", "AUS", "CAN", "UKR", "RUS")




################################################################################
ag_w <- ag_w %>% filter()


# for FE with demean
plot_FE_wmean<- ag_w %>%
  filter(ExporterISO3 %in% countries_vec) %>%
  ggplot(aes(x = year, y = w_FE_wmean, group = ExporterISO3, color = is_US)) +
  geom_line(linewidth = 0.5) +
  scale_color_manual( values = c(`TRUE` = "red", `FALSE` = "grey70"),
                      breaks = c(TRUE, FALSE),  labels = c("US", "Other countries"),
                      name = NULL  ) +
  theme_trade +
  labs(title = "Weighted average Δ ln(1+AVE) (FE model demeaned) \n in the Agricultural sector",
       x = "Year",  y = "Weighted Δ ln(1+AVE)" )
plot_FE_wmean


ggsave(filename = file.path(exp, "/plot/other_countries/", "change_ln_AVE_US_others_Ag_FE_wmean.png"),
       plot = plot_FE_wmean, width = 10, height = 7, dpi = 300)

test <- ag_w %>% filter(w_FE_wmean > 5)
test <- ag_w %>% filter(w_FE_wmean < -0.5)



# ################################################################################
# 
# # for tariffs
# plot_tariff <- ag_w %>%
#   filter(ExporterISO3 %in% countries_vec) %>%
#   ggplot(aes(x = year, y = w_tariff, group = ExporterISO3, color = is_US)) +
#   geom_line(linewidth = 0.5) +
#   scale_color_manual( values = c(`TRUE` = "red", `FALSE` = "grey70"),
#                       breaks = c(TRUE, FALSE),  labels = c("US", "Other countries"),
#                       name = NULL  ) +
#   theme_trade +
#   labs(title = "Weighted average Δ ln(1+tariff) in the Agricultural sector",
#        x = "Year",  y = "Weighted Δ ln(1+tariff)" )
# 
# plot_tariff
# ggsave(filename = file.path(exp, "/plot/other_countries/", "change_ln_AVE_US_others_Ag_tariff.png"),
#        plot = plot_tariff, width = 10, height = 7, dpi = 300)
# 
# 
# 
# 






