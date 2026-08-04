
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
library(dplyr)
library(modelsummary)

################################################################################
# LOad data 
dta <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/Pop_reg_dta/merge.csv")
dta_sector <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/Pop_reg_dta/merge_elast_county_subsector.csv")
dta_sector <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/Pop_reg_dta/merge_elast_subsector_county_sector_specific.csv")


exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/step_2/summary/"
################################################################################

# 1) get summary stats

summary(dta)
dta_2018 <- dta %>% filter(year >2017)

# slect  vars of interest
vars <- dta_2018 %>%
  select(EPOP,change_EPOP_2017, IMP_tariff_r, RET_tariff_r, RET_NTB_r,RET_NTB_IV_r, SUB)


# Step 1: build the summary table
sum_table <- datasummary(
  All(vars) ~ Mean + SD + Min + P25 + Median + P75 + Max + N,
  data = vars,
  output = "dataframe"
)

# Step 2: export
library(writexl)
write_xlsx(sum_table, paste0(exp, "summary_stats.xlsx"))

################################################################################

# 2) get correlation table 

library(ggcorrplot)

cor_matrix <- dta_2018 %>%
  select(IMP_tariff_r, RET_tariff_r, RET_NTB_r,RET_NTB_IV_r, SUB) %>% 
  rename(`IMP tariff` = IMP_tariff_r, `RET tariff` = RET_tariff_r, 
         `RET NTM` = RET_NTB_r,`RET NTM (IV)`= RET_NTB_IV_r) %>%  cor(use = "pairwise.complete.obs")

plot <- ggcorrplot(cor_matrix,
                   method   = "square",
                   type     = "lower",
                   lab      = TRUE,
                   lab_size = 5,
                   colors   = c("#6D2B09", "white", "#2B4B6D"))
plot
ggsave(paste0(exp,"correlation_plot_Xvars_IV.png"), plot = plot, width = 12, height = 10, dpi = 300)


library(corrplot)

cor_matrix <- dta_2018 %>%
  select(IMP_tariff_r, RET_tariff_r, RET_NTB_r, SUB,RET_NTB_IV_r) %>%
  rename(`IMP tariff` = IMP_tariff_r, `RET tariff` = RET_tariff_r,
         `RET NTM` = RET_NTB_r, `RET NTM (IV)`= RET_NTB_IV_r) %>%
  cor(use = "pairwise.complete.obs")

png( paste0(exp,"correlation_plot_Xvars_IV.png"), width = 6, height = 5, units = "in", res = 300)
corrplot(cor_matrix,
         method  = "color",
         type    = "lower",
         addCoef.col = "black",
         tl.col  = "black",
         tl.srt  = 45,
         diag    = FALSE,
         col     = COL2("RdBu", 10))
dev.off()


################################################################################

# by sector 
library(dplyr)
library(ggcorrplot)
library(ggplot2)

make_sector_corplot <- function(data, filter_year = NULL, drop_IV = FALSE,
                                exp_path = exp, save = TRUE) {
  
  # 1) filter by year if requested; NULL means keep all years
  df <- data
  if (!is.null(filter_year)) {
    df <- df %>% filter(year == filter_year)
  }
  
  # 2) variable list with labels — build once, reuse always
  var_map <- c(
    SUB                              = "SUB",
    RET_tariff_tot_r_crop            = "RET tariff Crop",
    RET_tariff_tot_r_livestock       = "RET tariff Livestock",
    RET_tariff_tot_r_nonag           = "RET tariff NonAg",
    RET_NTB_tot_r_crop               = "RET NTM Crop",
    RET_NTB_tot_r_livestock          = "RET NTM Livestock",
    RET_NTB_tot_r_nonag              = "RET NTM NonAg",
    RET_NTB_tot_IV_r_crop            = "RET NTM IV Crop",
    RET_NTB_tot_IV_r_livestock       = "RET NTM IV Livestock",
    RET_NTB_tot_IV_r_nonag           = "RET NTM IV NonAg",
    IMP_tariff_tot_ir_crop           = "IMP tariff Crop",
    IMP_tariff_tot_ir_livestock      = "IMP tariff Livestock",
    IMP_tariff_tot_ir_nonag          = "IMP tariff NonAg"
  )
  
  # 3) drop any variable whose LABEL contains "IV", if requested
  if (drop_IV) {
    var_map <- var_map[!grepl("IV", var_map)]
  }
  
  # 4) select + rename in one step using the (possibly reduced) map
  cor_matrix <- df %>%
    select(all_of(names(var_map))) %>%
    rename(!!!setNames(names(var_map), var_map)) %>%
    cor(use = "pairwise.complete.obs")
  
  # 5) plot
  plot <- ggcorrplot(cor_matrix,
                     method   = "square",
                     type     = "lower",
                     lab      = TRUE,
                     lab_size = 5,
                     colors   = c("#6D2B09", "white", "#2B4B6D")) +
    theme(
      axis.text.x  = element_text(size = 15),
      axis.text.y  = element_text(size = 15),
      legend.text  = element_text(size = 15),
      legend.title = element_text(size = 15),
      legend.key.height = unit(1.5, "cm"),
      legend.key.width  = unit(0.5, "cm")
    )
  
  # 6) save with a filename that encodes year + IV status
  if (save) {
    year_tag <- if (is.null(filter_year)) "" else paste0("_", filter_year)
    iv_tag   <- if (drop_IV) "_noIV" else "_withIV"
    fname <- paste0(exp_path, "correlation_plot_Xvars_industry", iv_tag, year_tag, ".png")
    ggsave(fname, plot = plot, width = 12, height = 10, dpi = 300)
  }
  
  plot
}

dta_sector_2018 <- dta_sector %>% filter(year > 2017)

p_all   <- make_sector_corplot(dta_sector_2018)                       # all years, with IV
p_2018  <- make_sector_corplot(dta_sector_2018, filter_year = 2018)
p_2019  <- make_sector_corplot(dta_sector_2018, filter_year = 2019)

# without the IV variables:
p_2018_noIV <- make_sector_corplot(dta_sector_2018, filter_year = 2018, drop_IV = TRUE)
p_2019_noIV <- make_sector_corplot(dta_sector_2018, filter_year = 2019, drop_IV = TRUE)
p_all_noIV <- make_sector_corplot(dta_sector_2018,  drop_IV = TRUE)


ggsave(paste0(exp, "correlation_plot_Xvars_industry_IV_all.png"),       plot = p_all,       width = 12, height = 10, dpi = 300)
ggsave(paste0(exp, "correlation_plot_Xvars_industry_IV_2018.png"),      plot = p_2018,      width = 12, height = 10, dpi = 300)
ggsave(paste0(exp, "correlation_plot_Xvars_industry_IV_2019.png"),      plot = p_2019,      width = 12, height = 10, dpi = 300)
ggsave(paste0(exp, "correlation_plot_Xvars_industry_2018.png"),    plot = p_2018_noIV, width = 12, height = 10, dpi = 300)
ggsave(paste0(exp, "correlation_plot_Xvars_industry_2019.png"),    plot = p_2019_noIV, width = 12, height = 10, dpi = 300)
ggsave(paste0(exp, "correlation_plot_Xvars_industry_all.png"),     plot = p_all_noIV,  width = 12, height = 10, dpi = 300)
