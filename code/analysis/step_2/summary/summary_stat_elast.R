
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
library(modelsummary)
library(dplyr)

################################################################################
# LOad data 
dta <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/Pop_reg_dta/merge_elast.csv")
dta_sector <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/Pop_reg_dta/merge_elast_subsector.csv")


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
write_xlsx(sum_table, paste0(exp, "summary_stats_elast.xlsx"))

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
ggsave(paste0(exp,"correlation_plot_Xvars_elast_IV.png"), plot = plot, width = 12, height = 10, dpi = 300)


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
dta_sector_2018 <- dta_sector %>% filter(year >2017)

names(dta_sector_2018)
library(ggcorrplot)
cor_matrix <- dta_sector_2018 %>% 
  select( SUB,
          RET_tariff_tot_r_crop, RET_tariff_tot_r_livestock, RET_tariff_tot_r_nonag,
          RET_NTB_tot_r_crop, RET_NTB_tot_r_livestock, RET_NTB_tot_r_nonag,
          RET_NTB_tot_IV_r_crop, RET_NTB_tot_IV_r_livestock, RET_NTB_tot_IV_r_nonag,
          IMP_tariff_tot_ir_crop, IMP_tariff_tot_ir_livestock, IMP_tariff_tot_ir_nonag  ) %>%
  rename(
    # RET tariff by sector
    `RET tariff Crop`     = RET_tariff_tot_r_crop,
    `RET tariff Livestock`= RET_tariff_tot_r_livestock,
    `RET tariff NonAg`    = RET_tariff_tot_r_nonag,
    # RET NTM IV by sector
    `RET NTM IV Crop`     = RET_NTB_tot_IV_r_crop,
    `RET NTM IV Livestock`= RET_NTB_tot_IV_r_livestock,
    `RET NTM IV NonAg`    = RET_NTB_tot_IV_r_nonag,
    # RET NTM IV by sector
    `RET NTM Crop`     = RET_NTB_tot_r_crop,
    `RET NTM Livestock`= RET_NTB_tot_r_livestock,
    `RET NTM NonAg`    = RET_NTB_tot_r_nonag,
    # IMP tariff by sector
    `IMP tariff Crop`     = IMP_tariff_tot_ir_crop,
    `IMP tariff Livestock`= IMP_tariff_tot_ir_livestock,
    `IMP tariff NonAg`    = IMP_tariff_tot_ir_nonag  ) %>%
  cor(use = "pairwise.complete.obs")


plot <- ggcorrplot(cor_matrix,
                   method   = "square",
                   type     = "lower",
                   lab      = TRUE,
                   lab_size = 5,
                   colors   = c("#6D2B09", "white", "#2B4B6D"))+
  theme(
    axis.text.x  = element_text(size = 15),
    axis.text.y  = element_text(size = 15),
    legend.text  = element_text(size = 15),
    legend.title = element_text(size = 15),
    legend.key.height = unit(1.5, "cm"),  # taller legend bar
    legend.key.width  = unit(0.5, "cm")  )
plot
ggsave(paste0(exp,"correlation_plot_Xvars_industry_elast_IV.png"), plot = plot, width = 12, height = 10, dpi = 300)




names(dta_sector_2018)
library(ggcorrplot)
cor_matrix <- dta_sector_2018 %>% filter(year == 2018) %>%
  select( SUB,
          RET_tariff_tot_r_crop, RET_tariff_tot_r_livestock, RET_tariff_tot_r_nonag,
          RET_NTB_tot_r_crop, RET_NTB_tot_r_livestock, RET_NTB_tot_r_nonag,
          RET_NTB_tot_IV_r_crop, RET_NTB_tot_IV_r_livestock, RET_NTB_tot_IV_r_nonag,
          IMP_tariff_tot_ir_crop, IMP_tariff_tot_ir_livestock, IMP_tariff_tot_ir_nonag  ) %>%
  rename(
    # RET tariff by sector
    `RET tariff Crop`     = RET_tariff_tot_r_crop,
    `RET tariff Livestock`= RET_tariff_tot_r_livestock,
    `RET tariff NonAg`    = RET_tariff_tot_r_nonag,
    # RET NTM IV by sector
    `RET NTM IV Crop`     = RET_NTB_tot_IV_r_crop,
    `RET NTM IV Livestock`= RET_NTB_tot_IV_r_livestock,
    `RET NTM IV NonAg`    = RET_NTB_tot_IV_r_nonag,
    # RET NTM IV by sector
    `RET NTM Crop`     = RET_NTB_tot_r_crop,
    `RET NTM Livestock`= RET_NTB_tot_r_livestock,
    `RET NTM NonAg`    = RET_NTB_tot_r_nonag,
    # IMP tariff by sector
    `IMP tariff Crop`     = IMP_tariff_tot_ir_crop,
    `IMP tariff Livestock`= IMP_tariff_tot_ir_livestock,
    `IMP tariff NonAg`    = IMP_tariff_tot_ir_nonag  ) %>%
  cor(use = "pairwise.complete.obs")


plot <- ggcorrplot(cor_matrix,
                   method   = "square",
                   type     = "lower",
                   lab      = TRUE,
                   lab_size = 5,
                   colors   = c("#6D2B09", "white", "#2B4B6D"))+
  theme(
    axis.text.x  = element_text(size = 15),
    axis.text.y  = element_text(size = 15),
    legend.text  = element_text(size = 15),
    legend.title = element_text(size = 15),
    legend.key.height = unit(1.5, "cm"),  # taller legend bar
    legend.key.width  = unit(0.5, "cm")  )
plot
ggsave(paste0(exp,"correlation_plot_Xvars_industry_elast_IV_2018.png"), plot = plot, width = 12, height = 10, dpi = 300)


# for 2019

cor_matrix <- dta_sector_2018 %>% filter(year == 2019) %>%
  select( SUB,
          RET_tariff_tot_r_crop, RET_tariff_tot_r_livestock, RET_tariff_tot_r_nonag,
          RET_NTB_tot_r_crop, RET_NTB_tot_r_livestock, RET_NTB_tot_r_nonag,
          RET_NTB_tot_IV_r_crop, RET_NTB_tot_IV_r_livestock, RET_NTB_tot_IV_r_nonag,
          IMP_tariff_tot_ir_crop, IMP_tariff_tot_ir_livestock, IMP_tariff_tot_ir_nonag  ) %>%
  rename(
    # RET tariff by sector
    `RET tariff Crop`     = RET_tariff_tot_r_crop,
    `RET tariff Livestock`= RET_tariff_tot_r_livestock,
    `RET tariff NonAg`    = RET_tariff_tot_r_nonag,
    # RET NTM IV by sector
    `RET NTM IV Crop`     = RET_NTB_tot_IV_r_crop,
    `RET NTM IV Livestock`= RET_NTB_tot_IV_r_livestock,
    `RET NTM IV NonAg`    = RET_NTB_tot_IV_r_nonag,
    # RET NTM IV by sector
    `RET NTM Crop`     = RET_NTB_tot_r_crop,
    `RET NTM Livestock`= RET_NTB_tot_r_livestock,
    `RET NTM NonAg`    = RET_NTB_tot_r_nonag,
    # IMP tariff by sector
    `IMP tariff Crop`     = IMP_tariff_tot_ir_crop,
    `IMP tariff Livestock`= IMP_tariff_tot_ir_livestock,
    `IMP tariff NonAg`    = IMP_tariff_tot_ir_nonag  ) %>%
  cor(use = "pairwise.complete.obs")

plot <- ggcorrplot(cor_matrix,
                   method   = "square",
                   type     = "lower",
                   lab      = TRUE,
                   lab_size = 5,
                   colors   = c("#6D2B09", "white", "#2B4B6D"))+
  theme(
    axis.text.x  = element_text(size = 15),
    axis.text.y  = element_text(size = 15),
    legend.text  = element_text(size = 15),
    legend.title = element_text(size = 15),
    legend.key.height = unit(1.5, "cm"),  # taller legend bar
    legend.key.width  = unit(0.5, "cm")  )
plot
ggsave(paste0(exp,"correlation_plot_Xvars_industry_elast_IV_2019.png"), plot = plot, width = 12, height = 10, dpi = 300)



