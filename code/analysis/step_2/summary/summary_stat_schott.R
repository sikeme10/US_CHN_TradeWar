
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
dta <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/Pop_reg_dta/merge_schott.csv")

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
write_xlsx(sum_table, paste0(exp, "summary_stats_schott.xlsx"))

################################################################################

# 2) get correlation table 

library(ggcorrplot)

cor_matrix <- dta_2018 %>%
  select(IMP_tariff_r, RET_tariff_r, RET_NTB_r,RET_NTB_IV_r, SUB) %>% 
  rename(`IMP tariff` = IMP_tariff_r, `RET tariff` = RET_tariff_r, 
         `RET NTM` = RET_NTB_r,`RET NTM (IV)`= RET_NTB_IV_r) %>%  cor(use = "pairwise.complete.obs")

ggcorrplot(cor_matrix,
           method   = "square",
           type     = "lower",
           lab      = TRUE,
           lab_size = 4,
           colors   = c("#6D2B09", "white", "#2B4B6D"),
           title    = "Correlation Matrix")

library(corrplot)

cor_matrix <- dta_2018 %>%
  select(IMP_tariff_r, RET_tariff_r, RET_NTB_r, SUB,RET_NTB_IV_r) %>%
  rename(`IMP tariff` = IMP_tariff_r, `RET tariff` = RET_tariff_r,
         `RET NTM` = RET_NTB_r, `RET NTM (IV)`= RET_NTB_IV_r) %>%
  cor(use = "pairwise.complete.obs")

png( paste0(exp,"correlation_plot_Xvars_IV_schott.png"), width = 6, height = 5, units = "in", res = 300)
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




