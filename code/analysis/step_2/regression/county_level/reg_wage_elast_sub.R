
################################################################################
# EPOP regression
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
library(readr)
library(dplyr)
library(sf)
library(tigris)
library(ggplot2)
library(viridis)

################################################################################
# LOad data 
dta <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/Pop_reg_dta/merge_elast_subsector_county.csv")
names(dta)

exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/step_2/regression/elast/county/wage/"
if (!dir.exists(exp)) dir.create(exp, recursive = TRUE)

################################################################################
# event study Regressions
################################################################################

sub_dta <- dta %>% filter(year %in% c(2016:2019))
table(sub_dta$year)
names(sub_dta)


################################################################################
#################################################################################

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
    legend.title = element_text(size = 10)  )

library(fixest)
library(dplyr)
library(ggplot2)
library(tibble)

# ---- one event study, with switches for the two controls ----
# year_fe = TRUE adds the nonparametric time effects (delta_t in ABDH eq 5).
# sector_year = TRUE adds year x sector-share interactions.
run_es <- function(data, exposure_vars,
                   year_fe     = TRUE,
                   sector_year = FALSE,
                   outcome     = "change_log_wage_total_2017_bis",
                   shares      = c("share_crop","share_livestock","share_nonag","share_forestry"),
                   cluster_var = "fips",
                   weight_var  = "emp_2012",
                   drop_year   = 2017) {
  
  d <- data[data$year != drop_year, , drop = FALSE]
  
  rhs <- paste0("i(year, ", exposure_vars, ")")
  if (sector_year) rhs <- c(rhs, paste0("i(year, ", shares, ")"))
  rhs <- paste(rhs, collapse = " + ")
  
  fe  <- if (year_fe) " | year" else ""
  # if no year FE, keep an intercept so the per-year slopes are identified sensibly
  fml <- as.formula(paste0(outcome, " ~ ", rhs, fe))
  
  feols(fml, data = d,
        weights = as.formula(paste0("~", weight_var)),
        cluster = as.formula(paste0("~", cluster_var)))
}

# ---- tidy year-specific coefs for the exposure vars only ----
make_plot_df <- function(reg, vars, spec_label, base_year = 2017) {
  ct <- coeftable(reg) |> as.data.frame() |> rownames_to_column("term")
  ci <- confint(reg)    |> as.data.frame() |> rownames_to_column("term")
  ci <- setNames(ci, c("term", "ci_low", "ci_high"))
  tab <- left_join(ct, ci, by = "term")
  
  bind_rows(lapply(vars, function(v) {
    rows <- tab |>
      filter(grepl(paste0("year::\\d{4}:", v, "$"), term)) |>
      mutate(year     = as.numeric(sub(".*year::(\\d{4}):.*", "\\1", term)),
             variable = v) |>
      select(year, est = Estimate, ci_low, ci_high, variable)
    anchor <- data.frame(year = base_year, est = 0, ci_low = 0, ci_high = 0, variable = v)
    bind_rows(rows, anchor)
  })) |>
    mutate(spec = spec_label)
}

# ---- run the three specs for a given exposure set ----
es_three_specs <- function(data, exposure_vars, labels) {
  m_none   <- run_es(data, exposure_vars, year_fe = FALSE, sector_year = FALSE)
  m_yearfe <- run_es(data, exposure_vars, year_fe = TRUE,  sector_year = FALSE)
  m_sector <- run_es(data, exposure_vars, year_fe = TRUE,  sector_year = TRUE)
  
  df <- bind_rows(
    make_plot_df(m_none,   exposure_vars, "no FE"),
    make_plot_df(m_yearfe, exposure_vars, "year FE"),
    make_plot_df(m_sector, exposure_vars, "year FE + sector x year")
  ) |>
    mutate(variable = recode(variable, !!!labels),
           variable = factor(variable, levels = unname(labels)),
           spec     = factor(spec, levels = c("no FE","year FE","year FE + sector x year")))
  
  list(models = list(no_fe = m_none, year_fe = m_yearfe, sector = m_sector), df = df)
}

# ---- overlay plot ----
make_plot <- function(plot_df) {
  pd <- position_dodge(width = 0.4)
  ggplot(plot_df, aes(x = year, y = est, color = spec, group = spec)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    geom_point(size = 2, position = pd) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2, position = pd) +
    facet_wrap(~variable) +
    scale_x_continuous(breaks = 2016:2019) +
    labs(x = "Year", y = "Coefficient", color = NULL) +
    theme_trade +
    theme(legend.position = "bottom")
}

# ---- run it for the tariffs-only set ----
res_tar <- es_three_specs(
  sub_dta,
  c("IMP_tariff_r_2019", "RET_tariff_r_2019"),
  c("IMP_tariff_r_2019" = "IMP Tariff", "RET_tariff_r_2019" = "RET Tariff"))

plot_tar <- make_plot(res_tar$df)
plot_tar
ggsave(paste0(exp, "event_study_tariffs_3specs.png"), plot_tar, width = 9, height = 4, dpi = 300)

# ---- same for tariffs + NTM IV ----
res_ntm_iv <- es_three_specs(
  sub_dta,
  c("IMP_tariff_r_2019", "RET_tariff_r_2019", "RET_NTB_IV_r_2019"),
  c("IMP_tariff_r_2019" = "IMP Tariff", "RET_tariff_r_2019" = "RET Tariff",
    "RET_NTB_IV_r_2019" = "RET NTM (IV)"))

plot_ntm_iv <- make_plot(res_ntm_iv$df)
plot_ntm_iv



# ---- same for tariffs + NTM IV ----
res_ntm <- es_three_specs(
  sub_dta,
  c("IMP_tariff_r_2019", "RET_tariff_r_2019", "RET_NTB_r_2019"),
  c("IMP_tariff_r_2019" = "IMP Tariff", "RET_tariff_r_2019" = "RET Tariff",
    "RET_NTB_r_2019" = "RET NTM "))

plot_ntm <- make_plot(res_ntm$df)
plot_ntm
ggsave(paste0(exp, "event_study_tariffs_NTM_3specs.png"), plot_ntm, width = 11, height = 4, dpi = 300)



################################################################################
# main sppecification
################################################################################


#with NTMs IV
reg1 <- feols(change_log_wage_total_2017_bis ~ IMP_tariff_r + RET_tariff_r + RET_NTB_IV_r |
                year, data = sub_dta, weights = ~emp_2012, cluster ~fips)

reg2 <- feols(change_log_wage_total_2017_bis ~ IMP_tariff_r + RET_tariff_r + RET_NTB_IV_r +
                SUB + as.factor(year):share_crop + as.factor(year):share_livestock + as.factor(year):share_nonag +
                as.factor(year):share_forestry   |
                year, data = sub_dta, weights = ~emp_2012, cluster ~fips)

reg3 <- feols(change_log_wage_total_2017_bis ~ IMP_tariff_r + RET_tariff_r + RET_NTB_IV_r +
                SUB + as.factor(year):share_crop + as.factor(year):share_livestock + as.factor(year):share_nonag +
                as.factor(year):share_forestry +
                as.factor(division):as.factor(year) |
                year, data = sub_dta, weights = ~emp_2012, cluster ~fips)

reg4 <- feols(change_log_wage_total_2017_bis ~ IMP_tariff_r + RET_tariff_r + RET_NTB_IV_r +
                SUB + as.factor(year):share_crop + as.factor(year):share_livestock + as.factor(year):share_nonag +
                as.factor(year):share_forestry +
                as.factor(division):as.factor(year) + year:change_EPOP_2017_2016 |
                year, data = sub_dta, weights = ~emp_2012,  cluster ~fips)

tbl <- etable(reg1, reg2, reg3, reg4,
              headers = c("(1)", "(2)", "(3)", "(4)"),
              drop    = c("share_ag_mining", "share_mfg", "share_other",
                          "division", "change_EPOP_2017_2016"),
              digits  = 4,
              fitstat = ~ n + r2 + ar2 + f + f.p + rmse,
              extralines = list(
                "-^Year FE"                        = list("Yes", "Yes", "Yes", "Yes"),
                "-^Sector x year-month FE"               = list("No",  "Yes", "Yes", "Yes"),
                "-^Census division x year-month FE"      = list("No",  "No",  "Yes", "Yes"),
                "-^Pre-trend CZ outcome 2017"            = list("No",  "No",  "No",  "Yes")
              ))

tbl
write_csv(as.data.frame(tbl), paste0(exp, "reg_main_NTM_IV_controls.csv"))




# with NTMs
summary(sub_dta$change_EPOP_2017_bis)
reg1 <- feols(change_log_wage_total_2017_bis ~ IMP_tariff_r + RET_tariff_r + RET_NTB_r |
                year, data = sub_dta, weights = ~emp_2012,  cluster ~fips)

reg2 <- feols(change_log_wage_total_2017_bis ~ IMP_tariff_r + RET_tariff_r + RET_NTB_r +
                SUB + as.factor(year):share_crop + as.factor(year):share_livestock + as.factor(year):share_nonag +
                as.factor(year):share_forestry   | 
                year, data = sub_dta, weights = ~emp_2012, cluster = ~fips)

reg3 <- feols(change_log_wage_total_2017_bis ~ IMP_tariff_r + RET_tariff_r + RET_NTB_r +
                SUB + as.factor(year):share_crop + as.factor(year):share_livestock + as.factor(year):share_nonag +
                as.factor(year):share_forestry  |division^year +    year
              , data = sub_dta, weights = ~emp_2012, cluster = ~fips)

reg4 <- feols(change_log_wage_total_2017_bis ~ IMP_tariff_r + RET_tariff_r + RET_NTB_r +
                SUB + as.factor(year):share_crop + as.factor(year):share_livestock + as.factor(year):share_nonag +
                as.factor(year):share_forestry + year:change_EPOP_2017_2016 | division^year +    year,
              data = sub_dta, weights = ~emp_2012, cluster = ~fips)

tbl <- etable(reg1, reg2, reg3, reg4,
              headers = c("(1)", "(2)", "(3)", "(4)"),
              drop    = c("share_ag_mining", "share_mfg", "share_other",
                          "division", "change_EPOP_2017_2016"),
              digits  = 4,
              fitstat = ~ n + r2 + ar2 + f + f.p + rmse,
              extralines = list(
                "-^Year FE"                        = list("Yes", "Yes", "Yes", "Yes"),
                "-^Sector x year-month FE"               = list("No",  "Yes", "Yes", "Yes"),
                "-^Census division x year-month FE"      = list("No",  "No",  "Yes", "Yes"),
                "-^Pre-trend CZ outcome 2017"            = list("No",  "No",  "No",  "Yes")
              ))

tbl
write_csv(as.data.frame(tbl), paste0(exp, "reg_main_NTM_controls.csv"))

################################################################################
# by subsector
################################################################################


sub_dta <- dta %>% filter(year %in% c(2017:2019))
names(sub_dta)

reg <- feols(change_log_wage_total_2017_bis ~
               IMP_tariff_tot_ir_crop + IMP_tariff_tot_ir_forestry +
               IMP_tariff_tot_ir_livestock + IMP_tariff_tot_ir_nonag +
               RET_tariff_tot_r_crop + RET_tariff_tot_r_forestry +
               RET_tariff_tot_r_livestock + RET_tariff_tot_r_nonag +
               SUB + share_forestry*year + share_crop*year + share_livestock*year +
               share_nonag*year + share_other*year +
               as.factor(division)*year + year*change_EPOP_2017_2016 +
               as.factor(year),
             data = sub_dta, weights = ~emp_2012)

summary(reg)

# year as a dummy 

reg1 <- feols(change_log_wage_total_2017_bis ~   
                IMP_tariff_tot_ir_crop + IMP_tariff_tot_ir_forestry +
                IMP_tariff_tot_ir_livestock + IMP_tariff_tot_ir_nonag + # IMP
                RET_tariff_tot_r_crop + RET_tariff_tot_r_forestry +
                RET_tariff_tot_r_livestock + RET_tariff_tot_r_nonag +  # tariff 
                SUB + as.factor(year):share_crop + as.factor(year):share_livestock + as.factor(year):share_nonag +
                as.factor(year):share_forestry  + year:change_EPOP_2017_2016 |
                division^year + year, data = sub_dta, weights = ~emp_2012, cluster = ~fips)

reg2 <-  feols(change_log_wage_total_2017_bis ~ 
                 IMP_tariff_tot_ir_crop + IMP_tariff_tot_ir_forestry +
                 IMP_tariff_tot_ir_livestock + IMP_tariff_tot_ir_nonag + # IMP
                 RET_tariff_tot_r_crop + RET_tariff_tot_r_forestry +
                 RET_tariff_tot_r_livestock + RET_tariff_tot_r_nonag +  # tariff 
                 RET_NTB_tot_r_crop + RET_NTB_tot_r_forestry +
                 RET_NTB_tot_r_livestock +  RET_NTB_tot_r_nonag + # NTM
                 SUB + as.factor(year):share_crop + as.factor(year):share_livestock + as.factor(year):share_nonag +
                 as.factor(year):share_forestry + year:change_EPOP_2017_2016 |
                 division^year + year, data = sub_dta, weights = ~emp_2012, cluster = ~fips)

reg3 <-  feols(change_log_wage_total_2017_bis ~ 
                 IMP_tariff_tot_ir_crop + IMP_tariff_tot_ir_forestry +
                 IMP_tariff_tot_ir_livestock + IMP_tariff_tot_ir_nonag + # IMP
                 RET_tariff_tot_r_crop + RET_tariff_tot_r_forestry +
                 RET_tariff_tot_r_livestock + RET_tariff_tot_r_nonag +  # tariff 
                 RET_NTB_tot_IV_r_crop + RET_NTB_tot_IV_r_forestry +
                 RET_NTB_tot_IV_r_livestock + RET_NTB_tot_IV_r_nonag + # NTM IV 
                 SUB + as.factor(year):share_crop + as.factor(year):share_livestock + as.factor(year):share_nonag +
                 as.factor(year):share_forestry + year:change_EPOP_2017_2016 |
                 division^year + year, data = sub_dta, weights = ~emp_2012, cluster = ~fips)
tbl <- etable(reg1, reg2, reg3,
              headers = c("No NTM", "With NTM", "With NTM IV"),
              drop    = c("share_ag_mining", "share_mfg", "share_other",
                          "division", "change_EPOP_2017_2016"),
              order   = c("IMP_tariff_tot_ir_crop", "IMP_tariff_tot_ir_forestry",
                          "IMP_tariff_tot_ir_livestock", "IMP_tariff_tot_ir_mining",
                          "IMP_tariff_tot_ir_nonag",
                          "RET_tariff_tot_r_crop", "RET_tariff_tot_r_forestry",
                          "RET_tariff_tot_r_livestock", "RET_tariff_tot_r_mining",
                          "RET_tariff_tot_r_nonag",
                          "RET_NTB_tot_r_crop", "RET_NTB_tot_r_forestry",
                          "RET_NTB_tot_r_livestock", "RET_NTB_tot_r_mining",
                          "RET_NTB_tot_r_nonag",
                          "RET_NTB_tot_IV_r_crop", "RET_NTB_tot_IV_r_forestry",
                          "RET_NTB_tot_IV_r_livestock", "RET_NTB_tot_IV_r_mining",
                          "RET_NTB_tot_IV_r_nonag"),
              digits  = 4,
              fitstat = ~ n + r2 + ar2 + f + f.p + rmse)


tbl
write_csv(as.data.frame(tbl), paste0(exp, "reg_main_subsector.csv"))

summary(sub_dta)



