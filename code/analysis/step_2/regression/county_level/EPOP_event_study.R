################################################################################
# EPOP regression
################################################################################

rm(list = ls())

library(readr)
library(dplyr)
library(tidyr)
library(tibble)
library(fixest)
library(ggplot2)

################################################################################
# Load data
################################################################################
dta <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/Pop_reg_dta/merge_elast_subsector_county.csv")
names(dta)
table(dta$year)

out_dir <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/step_2/regression/elast/county/epop/"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

sub_dta <- dta %>% filter(year %in% 2016:2019)
table(sub_dta$year)

# Verify the columns run_es assumes actually exist before running anything
required_cols <- c("share_crop", "share_livestock", "share_nonag", "share_forestry",
                   "emp_2012", "fips", "change_EPOP_2017_bis", "change_EPOP_2017_2016")
setdiff(required_cols, names(sub_dta))   # should be character(0)

################################################################################
# Theme
################################################################################
theme_trade <- theme_minimal(base_size = 14, base_family = "Times New Roman") +
  theme(
    panel.spacing    = unit(1.2, "lines"),
    plot.title       = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.6),
    panel.grid       = element_blank(),
    strip.background = element_rect(fill = "grey92", color = "black", linewidth = 0.6),
    strip.text       = element_text(size = 12, face = "bold", color = "black"),
    axis.text.x      = element_text(size = 11),
    axis.text.y      = element_text(size = 11),
    axis.title.x     = element_text(size = 12),
    axis.title.y     = element_text(size = 12),
    legend.position  = "bottom",
    legend.text      = element_text(size = 12),
    legend.title     = element_blank()
  )

################################################################################
# Regression
################################################################################
run_es <- function(data, exposure_vars,
                   year_fe      = TRUE,
                   sector_year  = TRUE,
                   pretrend     = FALSE,
                   pretrend_var = "change_EPOP_2017_2016",
                   outcome      = "change_EPOP_2017_bis",
                   shares       = c("share_crop", "share_livestock",
                                    "share_nonag", "share_forestry"),
                   cluster_var  = "fips",
                   weight_var   = "emp_2012",
                   base_year    = 2017) {
  
  d <- data[data$year != base_year, , drop = FALSE]
  d$t_trend <- d$year - base_year
  
  rhs <- paste0("i(year, ", exposure_vars, ")")
  if (sector_year) rhs <- c(rhs, paste0("i(year, ", shares, ")"))
  if (pretrend)    rhs <- c(rhs, paste0(pretrend_var, ":t_trend"))
  rhs <- paste(rhs, collapse = " + ")
  
  fe  <- if (year_fe) " | year" else ""
  fml <- as.formula(paste0(outcome, " ~ ", rhs, fe))
  
  feols(fml, data = d,
        weights = as.formula(paste0("~", weight_var)),
        cluster = as.formula(paste0("~", cluster_var)))
}

################################################################################
# Extract year-specific coefficients for plotting
################################################################################
make_plot_df <- function(reg, vars, spec_label, base_year = 2017) {
  ct <- coeftable(reg) |> as.data.frame() |> rownames_to_column("term")
  ci <- confint(reg)   |> as.data.frame() |> rownames_to_column("term")
  ci <- setNames(ci, c("term", "ci_low", "ci_high"))
  tab <- left_join(ct, ci, by = "term")
  
  bind_rows(lapply(vars, function(v) {
    rows <- tab |>
      filter(grepl(paste0("year::\\d{4}:", v, "$"), term)) |>
      mutate(year     = as.numeric(sub(".*year::(\\d{4}):.*", "\\1", term)),
             variable = v) |>
      select(year, est = Estimate, ci_low, ci_high, variable)
    anchor <- data.frame(year = base_year, est = 0,
                         ci_low = 0, ci_high = 0, variable = v)
    bind_rows(rows, anchor)
  })) |>
    mutate(spec = spec_label)
}

################################################################################
# Run specs: no FE / year FE + sector x year / + pretrend
################################################################################
SPEC_NONE <- "No FE"
SPEC_FE   <- "FE: year + sector x year"
SPEC_PRE  <- "All FE + pretrend"

es_specs <- function(data, exposure_vars, labels,
                     include_pretrend = TRUE,
                     pretrend_var = "change_EPOP_2017_2016",
                     base_year = 2017) {
  
  m_none   <- run_es(data, exposure_vars, year_fe = FALSE, sector_year = FALSE,
                     pretrend = FALSE, base_year = base_year)
  m_sector <- run_es(data, exposure_vars, year_fe = TRUE, sector_year = TRUE,
                     pretrend = FALSE, base_year = base_year)
  
  parts <- list(
    make_plot_df(m_none,   exposure_vars, SPEC_NONE, base_year),
    make_plot_df(m_sector, exposure_vars, SPEC_FE,   base_year)
  )
  models      <- list(no_fe = m_none, sector = m_sector)
  spec_levels <- c(SPEC_NONE, SPEC_FE)
  
  if (include_pretrend) {
    m_pre <- run_es(data, exposure_vars, year_fe = TRUE, sector_year = TRUE,
                    pretrend = TRUE, pretrend_var = pretrend_var,
                    base_year = base_year)
    parts <- c(parts, list(make_plot_df(m_pre, exposure_vars, SPEC_PRE, base_year)))
    models$pretrend <- m_pre
    spec_levels     <- c(spec_levels, SPEC_PRE)
  }
  
  df <- bind_rows(parts) |>
    mutate(variable = recode(variable, !!!labels),
           variable = factor(variable, levels = unname(labels)),
           spec     = factor(spec, levels = spec_levels))
  
  # Guard: catch any spec label that failed to match its factor level
  if (any(is.na(df$spec))) {
    warning("Some spec labels did not match spec_levels and will not be plotted.")
  }
  
  list(models = models, df = df)
}

################################################################################
# Plot
################################################################################
make_plot <- function(plot_df, x_breaks = 2016:2019) {
  pd <- position_dodge(width = 0.4)
  ggplot(plot_df, aes(x = year, y = est, color = spec, group = spec)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_line(position = pd, linewidth = 0.4, alpha = 0.7) +
    geom_point(size = 2, position = pd) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high),
                  width = 0.2, position = pd) +
    facet_wrap(~variable) +
    scale_x_continuous(breaks = x_breaks) +
    labs(x = "Year", y = "Coefficient", color = NULL) +
    theme_trade
}

################################################################################
# Run
################################################################################
res_tar <- es_specs(
  sub_dta,
  c("IMP_tariff_r_2019", "RET_tariff_r_2019"),
  c("IMP_tariff_r_2019" = "IMP Tariff", "RET_tariff_r_2019" = "RET Tariff"),
  include_pretrend = TRUE)

plot_tar <- make_plot(res_tar$df)
plot_tar
ggsave(file.path(out_dir, "event_study_tariffs_specs.png"),
       plot_tar, width = 9, height = 4, dpi = 300)

res_ntm <- es_specs(
  sub_dta,
  c("IMP_tariff_r_2019", "RET_tariff_r_2019", "RET_NTB_r_2019"),
  c("IMP_tariff_r_2019" = "IMP Tariff", "RET_tariff_r_2019" = "RET Tariff",
    "RET_NTB_r_2019"    = "RET NTM"),
  include_pretrend = TRUE)

plot_ntm <- make_plot(res_ntm$df)
plot_ntm
ggsave(file.path(out_dir, "event_study_tariffs_NTM_specs.png"),
       plot_ntm, width = 11, height = 4, dpi = 300)

res_ntm_iv <- es_specs(
  sub_dta,
  c("IMP_tariff_r_2019", "RET_tariff_r_2019", "RET_NTB_IV_r_2019"),
  c("IMP_tariff_r_2019"  = "IMP Tariff", "RET_tariff_r_2019" = "RET Tariff",
    "RET_NTB_IV_r_2019"  = "RET NTM (IV)"),
  include_pretrend = TRUE)

plot_ntm_iv <- make_plot(res_ntm_iv$df)
plot_ntm_iv
ggsave(file.path(out_dir, "event_study_tariffs_NTM_IV_specs.png"),
       plot_ntm_iv, width = 11, height = 4, dpi = 300)

################################################################################
# Check the pretrend spec for dropped collinear terms
################################################################################
summary(res_tar$models$pretrend)