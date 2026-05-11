
################################################################################
# 
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
dta <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/Pop_reg_dta/merge_elast_subsector.csv")
names(dta)

exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/step_2/regression/elast/"


################################################################################
names(dta)

reg <- lm(EPOP ~ as.factor(year),          data = dta)
summary(reg)
reg <- lm(employment ~ as.factor(year),          data = dta)
summary(reg)





library(etable)
library(fixest)

sub_dta <- dta %>% filter(year %in% c(2018:2019))

reg1 <- feols(change_EPOP_2017 ~ IMP_tariff_r + RET_tariff_r + RET_NTB_r,
              data = sub_dta)


reg2 <- feols(change_EPOP_2017 ~ IMP_tariff_r + RET_tariff_r + RET_NTB_r + SUB,
           data = sub_dta)

reg3 <- feols(change_EPOP_2017 ~ IMP_tariff_r + RET_tariff_r + RET_NTB_r + as.factor(year),
           data = sub_dta)

reg4 <- feols(change_EPOP_2017 ~ IMP_tariff_r + RET_tariff_r + RET_NTB_r + SUB + as.factor(year),
           data = sub_dta)

etable(reg1, reg2, reg3, reg4,
       extralines = list("Year FE" = c("No", "No", "Yes", "Yes"),
                         "Subsidy Control" = c("No", "Yes", "No", "Yes")),
       se.below = TRUE,
       signif.code = c("***"=0.01, "**"=0.05, "*"=0.1),
       title = "Effect on Change in EPOP (2017 Baseline)")



# with IV
names(sub_dta)

sub_dta <- dta %>% filter(year %in% c(2018:2019))
reg_es <- feols(change_EPOP_2017 ~ IMP_tariff_r + RET_tariff_r + RET_NTB_IV_r + SUB +  year | czone_2012,
                data = sub_dta, weights = ~emp_2012)
reg_es



sub_dta <- dta %>%  filter(year %in% c(2016:2019)) %>%
  mutate(year_rel = year - 2017)  # relative time, 0 = 2017

reg_es <- feols(change_EPOP_2017_bis ~ IMP_tariff_r + RET_tariff_r + RET_NTB_IV_r + SUB +  year | czone,
                data = sub_dta, weights = ~emp_2012)
reg_es

#################################################################################
# pretrend
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


################################################################################
# event study Regressions
################################################################################


sub_dta <- dta %>% filter(year %in% c(2016:2019))
table(sub_dta$year)
################################################################################
# Regressions


# 1) Tariffs only
reg_tar <- feols(change_EPOP_2017_bis ~ as.factor(year)*IMP_tariff_r_2019 + as.factor(year)*RET_tariff_r_2019 +
                   SUB + as.factor(year), data = sub_dta, weights = ~emp_2012)

# reg_tar <- feols(change_EPOP_2017_bis ~ as.factor(year)*IMP_tariff_r_2019 + as.factor(year)*RET_tariff_r_2019 +
#                    SUB + share_ag_mining*year + year*share_mfg + share_other*year +
#                    as.factor(year), data = sub_dta, weights = ~emp_2012)

# 2) Tariffs + NTM
reg_ntm <- feols(change_EPOP_2017_bis ~ as.factor(year)*IMP_tariff_r_2019 + as.factor(year)*RET_tariff_r_2019 +
                   as.factor(year)*RET_NTB_r_2019 +  
                   as.factor(year), data = sub_dta, weights = ~emp_2012)

# 3) IMP + NTM IV only
reg_ntm_iv <- feols(change_EPOP_2017_bis ~ as.factor(year)*IMP_tariff_r_2019 + as.factor(year)*RET_NTB_IV_r_2019 +
                      SUB +  as.factor(year), data = sub_dta, weights = ~emp_2012)

# 4) Tariffs + NTM IV
reg_tar_ntm_iv <- feols(change_EPOP_2017_bis ~ as.factor(year)*IMP_tariff_r_2019 + as.factor(year)*RET_tariff_r_2019 +
                          as.factor(year)*RET_NTB_IV_r_2019 + SUB + as.factor(year), data = sub_dta, weights = ~emp_2012)

################################################################################
# Table
################################################################################

tbl <- etable(reg_tar, reg_ntm, reg_ntm_iv, reg_tar_ntm_iv,
              headers = c("Tariffs", "Tariffs + NTM", "IMP + NTM IV", "Tariffs + NTM IV"),
              digits  = 4,
              fitstat = ~ n + r2 + ar2 + f + f.p + rmse)
tbl


tbl <- etable(reg_tar, reg_ntm, reg_tar_ntm_iv,
              headers = c("Tariffs", "Tariffs + NTM", "Tariffs + NTM IV"),
              digits  = 4,
              fitstat = ~ n + r2 + ar2 + f + f.p + rmse)
tbl
write_csv(tbl, paste0(exp,"NTBs_SOE_tariff.csv"))

################################################################################
# Event study plots
################################################################################

make_plot_df <- function(reg, vars) {
  coefs <- coeftable(reg) %>%
    as.data.frame() %>%
    tibble::rownames_to_column("term")
  
  extract_coefs <- function(var) {
    base     <- coefs %>% filter(term == var)
    base_est <- base$Estimate
    base_se  <- base$`Std. Error`
    
    interactions <- coefs %>%
      filter(grepl(paste0("as.factor\\(year\\)\\d{4}:", var), term)) %>%
      mutate(year      = as.numeric(gsub(".*year\\)(\\d{4}).*", "\\1", term)),
             est_level = Estimate + base_est,
             se_level  = sqrt(`Std. Error`^2 + base_se^2),
             ci_low    = est_level - 1.96 * se_level,
             ci_high   = est_level + 1.96 * se_level) %>%
      select(year, est_level, se_level, ci_low, ci_high)
    
    base_row <- data.frame(year = 2016, est_level = base_est, se_level = base_se,
                           ci_low  = base_est - 1.96 * base_se,
                           ci_high = base_est + 1.96 * base_se)
    bind_rows(base_row, interactions) %>% mutate(variable = var)
  }
  
  bind_rows(lapply(vars, extract_coefs))
}

make_plot <- function(plot_df, labels) {
  plot_df <- plot_df %>% mutate(variable = recode(variable, !!!labels))
  ggplot(plot_df, aes(x = year, y = est_level)) +
    geom_point(size = 2) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    facet_wrap(~variable) +
    scale_x_continuous(breaks = 2016:2019) +
    labs(x = "Year", y = "Coefficient") +
    theme_trade
}

# Plot 1: Tariffs only
plot1 <- make_plot(
  make_plot_df(reg_tar, c("IMP_tariff_r_2019", "RET_tariff_r_2019")),
  c("IMP_tariff_r_2019" = "IMP Tariff", "RET_tariff_r_2019" = "RET Tariff"))
plot1
ggsave(paste0(exp, "event_study_tariffs.png"), plot1, width = 8, height = 4, dpi = 300)

# Plot 2: Tariffs + NTM
plot2 <- make_plot(
  make_plot_df(reg_ntm, c("IMP_tariff_r_2019", "RET_tariff_r_2019", "RET_NTB_r_2019")),
  c("IMP_tariff_r_2019" = "IMP Tariff", "RET_tariff_r_2019" = "RET Tariff", "RET_NTB_r_2019" = "RET NTM"))
plot2
ggsave(paste0(exp, "event_study_tariffs_NTM.png"), plot2, width = 8, height = 4, dpi = 300)

# Plot 3: IMP + NTM IV
plot3 <- make_plot(
  make_plot_df(reg_ntm_iv, c("IMP_tariff_r_2019", "RET_NTB_IV_r_2019")),
  c("IMP_tariff_r_2019" = "IMP Tariff", "RET_NTB_IV_r_2019" = "RET NTM (IV)"))
plot3
ggsave(paste0(exp, "event_study_NTM_IV.png"), plot3, width = 8, height = 4, dpi = 300)

# Plot 4: Tariffs + NTM IV
plot4 <- make_plot(
  make_plot_df(reg_tar_ntm_iv, c("IMP_tariff_r_2019", "RET_tariff_r_2019", "RET_NTB_IV_r_2019")),
  c("IMP_tariff_r_2019" = "IMP Tariff", "RET_tariff_r_2019" = "RET Tariff", "RET_NTB_IV_r_2019" = "RET NTM (IV)"))
plot4
ggsave(paste0(exp, "event_study_tariffs_NTM_IV.png"), plot4, width = 8, height = 4, dpi = 300)







#################################################################################
# mains pecification
#################################################################################
sub_dta <- dta %>% filter(year %in% c(2017:2019))
names(sub_dta)

reg <- feols(change_EPOP_2017_bis ~
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

reg <- feols(change_EPOP_2017_bis ~ IMP_tariff_r + RET_tariff_r+
               SUB + share_ag_mining*year + year*share_mfg + share_other*year +
               as.factor(division)*year + year*change_EPOP_2017_2016+
               as.factor(year), data = sub_dta, weights = ~emp_2012)
summary(reg)

# year as a dummy 

reg1 <- feols(change_EPOP_2017_bis ~   
                IMP_tariff_tot_ir_crop + IMP_tariff_tot_ir_forestry +
                IMP_tariff_tot_ir_livestock + IMP_tariff_tot_ir_nonag + # IMP
                RET_tariff_tot_r_crop + RET_tariff_tot_r_forestry +
                RET_tariff_tot_r_livestock + RET_tariff_tot_r_nonag +  # tariff 
                SUB + as.factor(year):share_crop + as.factor(year):share_livestock + as.factor(year):share_nonag +
                as.factor(year):share_forestry +
                as.factor(division):as.factor(year) + year:change_EPOP_2017_2016 |
                year, data = sub_dta, weights = ~emp_2012)

reg2 <-  feols(change_EPOP_2017_bis ~ 
                 IMP_tariff_tot_ir_crop + IMP_tariff_tot_ir_forestry +
                 IMP_tariff_tot_ir_livestock + IMP_tariff_tot_ir_nonag + # IMP
                 RET_tariff_tot_r_crop + RET_tariff_tot_r_forestry +
                 RET_tariff_tot_r_livestock + RET_tariff_tot_r_nonag +  # tariff 
                 RET_NTB_tot_r_crop + RET_NTB_tot_r_forestry +
                 RET_NTB_tot_r_livestock +  RET_NTB_tot_r_nonag + # NTM
                 SUB + as.factor(year):share_crop + as.factor(year):share_livestock + as.factor(year):share_nonag +
                 as.factor(year):share_forestry +
                 as.factor(division):as.factor(year) + year:change_EPOP_2017_2016 |
                 year, data = sub_dta, weights = ~emp_2012)

reg3 <-  feols(change_EPOP_2017_bis ~ 
                 IMP_tariff_tot_ir_crop + IMP_tariff_tot_ir_forestry +
                 IMP_tariff_tot_ir_livestock + IMP_tariff_tot_ir_nonag + # IMP
                 RET_tariff_tot_r_crop + RET_tariff_tot_r_forestry +
                 RET_tariff_tot_r_livestock + RET_tariff_tot_r_nonag +  # tariff 
                 RET_NTB_tot_IV_r_crop + RET_NTB_tot_IV_r_forestry +
                 RET_NTB_tot_IV_r_livestock + RET_NTB_tot_IV_r_nonag + # NTM IV 
                 SUB + as.factor(year):share_crop + as.factor(year):share_livestock + as.factor(year):share_nonag +
                 as.factor(year):share_forestry +
                 as.factor(division):as.factor(year) + year:change_EPOP_2017_2016 |
                 year, data = sub_dta, weights = ~emp_2012)

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


# when adding controls:

reg1 <- feols(change_EPOP_2017_bis ~ IMP_tariff_r + RET_tariff_r + RET_NTB_IV_r |
                year, data = sub_dta, weights = ~emp_2012)

reg2 <- feols(change_EPOP_2017_bis ~ IMP_tariff_r + RET_tariff_r + RET_NTB_IV_r +
                SUB + as.factor(year):share_crop + as.factor(year):share_livestock + as.factor(year):share_nonag +
                as.factor(year):share_forestry   |
                year, data = sub_dta, weights = ~emp_2012)

reg3 <- feols(change_EPOP_2017_bis ~ IMP_tariff_r + RET_tariff_r + RET_NTB_IV_r +
                SUB + as.factor(year):share_crop + as.factor(year):share_livestock + as.factor(year):share_nonag +
                as.factor(year):share_forestry +
                as.factor(division):as.factor(year) |
                year, data = sub_dta, weights = ~emp_2012)

reg4 <- feols(change_EPOP_2017_bis ~ IMP_tariff_r + RET_tariff_r + RET_NTB_IV_r +
                SUB + as.factor(year):share_crop + as.factor(year):share_livestock + as.factor(year):share_nonag +
                as.factor(year):share_forestry +
                as.factor(division):as.factor(year) + year:change_EPOP_2017_2016 |
                year, data = sub_dta, weights = ~emp_2012)

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
write_csv(as.data.frame(tbl), paste0(exp, "reg_main_NTMIV_controls.csv"))


#################################################################################
# other
#################################################################################

sub_dta <- dta %>% filter(year %in% c(2018:2019))

reg <- feols(change_EPOP_2017 ~ IMP_tariff_r + RET_tariff_r + RET_NTB_r | year,
             data = sub_dta)
summary(reg)
reg <- feols(change_EPOP_2017 ~ IMP_tariff_r + RET_tariff_r + RET_NTB_r + SUB | year,
             data = sub_dta, weights = sub_dta$working_age_pop)

summary(reg)


sub_dta <- dta %>% filter(year %in% c(2017:2019))
reg <- feols(EPOP ~ IMP_tariff_r + RET_tariff_r + RET_NTB_r + SUB | year + czone_2012,
             data = sub_dta,
             vcov = ~czone_2012)

summary(reg)


names(sub_dta)
sub_dta <- dta %>% filter(year %in% c(2015:2019))
reg <- lm(EPOP ~ IMP_tariff_r + RET_tariff_r + RET_NTB_r + SUB + as.factor(year),
          data = sub_dta)

summary(reg)


sub_dta <- dta %>% filter(year %in% c(2015:2019))
reg <- lm(employment ~ IMP_tariff_r + RET_tariff_r + RET_NTB_r + SUB + as.factor(year),
          data = sub_dta)
reg <- feols(employment ~ IMP_tariff_r + RET_tariff_r + RET_NTB_r + SUB | year + czone_2012,
             data = sub_dta,   vcov = ~czone_2012)
summary(reg)

