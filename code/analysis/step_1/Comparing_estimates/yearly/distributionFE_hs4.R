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
# SECTOR    <- "Ag"   # e.g. "Ag", "Manuf", etc.
SECTOR    <- "Manu" 


# nice label used in titles/filenames
sector_label_map <- c("Ag" = "Agriculture", "Manuf" = "Manufacturing")
SECTOR_LABEL <- unname(sector_label_map[SECTOR])
if (is.na(SECTOR_LABEL)) SECTOR_LABEL <- SECTOR  # fallback

# directories
ROOT <- "/data/sikeme/TRADE/US_CHN_TradeWar_git"
setwd(ROOT)

OUT_PLOT_DIR <- file.path(ROOT, "output/Compare_values/yearly/plot", as.character(BASE_YEAR))
dir.create(OUT_PLOT_DIR, recursive = TRUE, showWarnings = FALSE)

################################################################################
# 1) Load data
################################################################################
US <- read_csv(file.path(ROOT,paste0("output/Compare_values/yearly/robust/US_ln_NTMs_base_", BASE_YEAR, "_FE_boot_hs4.csv") ))


################################################################################
# 2) Drop extreme values (1st/99th percentile)
################################################################################
quant <- 0.05

FE_q   <- quantile(US$diff_ln_AVE_FE,        quant,      na.rm = TRUE)
FE_qH  <- quantile(US$diff_ln_AVE_FE,        1 - quant,  na.rm = TRUE)
FE_b_q <- quantile(US$diff_ln_AVE_FE_bench,  quant,      na.rm = TRUE)
FE_b_qH<- quantile(US$diff_ln_AVE_FE_bench,  1 - quant,  na.rm = TRUE)
FE_w_q <- quantile(US$diff_ln_AVE_FE_wmean,  quant,      na.rm = TRUE)
FE_w_qH<- quantile(US$diff_ln_AVE_FE_wmean,  1 - quant,  na.rm = TRUE)

cat("FE [1%,99%]:", FE_q, FE_qH,"| Bench [1%,99%]:", FE_b_q, FE_b_qH, "| Wmean [1%,99%]:", FE_w_q, FE_w_qH, "\n")

US1 <- US %>%
  mutate(diff_ln_AVE_FE        = ifelse(diff_ln_AVE_FE        < FE_q   | diff_ln_AVE_FE        > FE_qH,  NA, diff_ln_AVE_FE),
         diff_ln_AVE_FE_bench  = ifelse(diff_ln_AVE_FE_bench  < FE_b_q | diff_ln_AVE_FE_bench  > FE_b_qH,NA, diff_ln_AVE_FE_bench),
         diff_ln_AVE_FE_wmean  = ifelse(diff_ln_AVE_FE_wmean  < FE_w_q | diff_ln_AVE_FE_wmean  > FE_w_qH,NA, diff_ln_AVE_FE_wmean)  )





################################################################################
# Theme
################################################################################
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
    legend.title = element_text(size = 10)
  )

### PLOT THEME


# ---- consistent legend keys across ALL plots ----
legend_breaks <- c("FE", "FE_demeaned", "FE_bench", "tariff", "Chen_et_al")

legend_labels <- c(  "FE"          = "FE",
                     "FE_demeaned" = "FE (demeaned)",
                     "FE_bench"    = "FE (benchmark)",
                     "tariff"      = "Tariff",
                     "Chen_et_al"  = "Chen et al.")

legend_colors <- c(  "FE"          = "blue",
                     "FE_demeaned" = "orange",
                     "FE_bench"    = "purple",
                     "tariff"      = "darkgreen",
                     "Chen_et_al"  = "red")



################################################################################
# A) Sector level plot
################################################################################
names(US1)

US2 <- US1 %>%  group_by(year,sector, hs2, hs4) %>%
  summarise(  diff_ln_AVE_FE        = mean(diff_ln_AVE_FE,              na.rm = TRUE),
              diff_ln_AVE_FE_bench  = mean(diff_ln_AVE_FE_bench,        na.rm = TRUE),
              diff_ln_AVE_FE_wmean  = mean(diff_ln_AVE_FE_wmean,        na.rm = TRUE),
              
              diff_ln_AVE_FE_log        = mean(diff_ln_AVE_FE_log,      na.rm = TRUE),
              diff_ln_AVE_FE_log_bench  = mean(diff_ln_AVE_FE_log_bench,na.rm = TRUE),
              diff_ln_AVE_FE_log_wmean  = mean(diff_ln_AVE_FE_log_wmean,na.rm = TRUE) )



US2 <- US2 %>% filter(year %in% c(2018:2019) )


# If you only want Chen for 2018-2019 as before:
plot_dist <- ggplot(US2) +
  geom_density(aes(x = diff_ln_AVE_FE,       color = "FE"),          linewidth = 1) +
  geom_density(aes(x = diff_ln_AVE_FE_bench, color = "FE_bench"),    linewidth = 1) +
  geom_density(aes(x = diff_ln_AVE_FE_wmean, color = "FE_demeaned"), linewidth = 1) +
  scale_color_manual(
    values = legend_colors[c("FE", "FE_bench", "FE_demeaned")],
    labels = legend_labels[c("FE", "FE_bench", "FE_demeaned")],
    name   = "AVE Measure"
  ) +
  #coord_cartesian(xlim = c(-5, 25)) +
  theme_trade +
  labs(x = "Δ ln(1 + AVE)", y = "Density")

plot_dist



ggsave(  filename = file.path(OUT_PLOT_DIR, paste0("distribution_AVE_", quant, " .png")),
         plot = plot_dist, width = 9, height = 6, dpi = 300)



