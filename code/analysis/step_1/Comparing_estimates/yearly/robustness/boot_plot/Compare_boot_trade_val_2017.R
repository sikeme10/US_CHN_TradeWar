
rm(list = ls())

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
# 0) Paths
################################################################################

setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git")
exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/Compare_values/yearly/robust/"

################################################################################
# 1) Load data
################################################################################

US <- read_csv(paste0(exp, "US_ln_NTMs_base_2015_FE_boot_hs4.csv"))

# trade and tariff data
tariffs <- read_csv(paste0(exp, "estimates_log_tariff_FE_boot.csv"))

# --- diagnostics ---
names(US)
colSums(is.na(US))
names(tariffs)
colSums(is.na(tariffs))

tariffs <- tariffs %>% filter(ExporterISO3 == "USA" & (year %in% c(2017:2020))) %>%
  select( year, hs2 , hs4 , hs6_H5,ExporterISO3, Trade_value_USD,Applied_tariff)


US <- US %>% filter(year %in% c(2017:2020)) %>% 
  select(year, hs2, hs4, sector, hs_section, draw, Trade_value_USD,
         diff_ln_AVE_FE, diff_ln_AVE_FE_bench , diff_ln_AVE_FE_wmean, 
         diff_ln_AVE_FE_log, diff_ln_AVE_FE_log_bench, diff_ln_AVE_FE_log_wmean, 
         diff_log_tariff_2017)


################################################################################
# 2) Get HS6 level trade to see which one goes to 0

################################################################################

hs6_trade_2017 <- tariffs %>%  filter(year == 2017) %>%   group_by(ExporterISO3, hs4, hs6_H5) %>%
  summarise(Trade_value_USD_2017 = sum(Trade_value_USD, na.rm = TRUE), .groups = "drop")

# --- diagnostics ---
colSums(is.na(hs6_trade_2017))

# Merge weights back onto main dataset
tariffs <- left_join(tariffs, hs6_trade_2017)

# --- diagnostics ---
colSums(is.na(tariffs))


# Compute changes (exclude base year 2017 from panel)
US_hs6_trade <- tariffs %>%
  filter(year != 2017) %>%
  mutate( change_trade_USD = Trade_value_USD - Trade_value_USD_2017,
          perc_change_trade_USD = case_when(
            Trade_value_USD_2017 == 0 & change_trade_USD == 0 ~ 0,
            change_trade_USD != 0 & Trade_value_USD_2017 != 0 ~
              change_trade_USD * 100 / Trade_value_USD_2017,
            change_trade_USD != 0 & Trade_value_USD_2017 == 0 ~ NA_real_          ),
          # Cap extreme positive growth (new trade creation inflates % change)
          perc_change_trade_USD_bis = if_else(perc_change_trade_USD > 100,
                                              NA, perc_change_trade_USD),
          trade_destruction = if_else(perc_change_trade_USD == -100, 1, 0),
          trade_creation    = if_else(Trade_value_USD != 0 & Trade_value_USD_2017 == 0, 1, 0) )
names(US_hs6_trade)

US_hs4_hs6_trade <- US_hs6_trade %>%
  group_by(year, hs2, hs4, ExporterISO3) %>%
  summarise(n_hs6        = n_distinct(hs6_H5),
            n_hs6_destruction = sum(trade_destruction == 1, na.rm = TRUE),
            n_hs6_creation    = sum(trade_creation    == 1, na.rm = TRUE),
            share_hs6_destruct = n_hs6_destruction / n_hs6,
            .groups = "drop"  )
summary(US_hs4_hs6_trade)

################################################################################
# 4) Compute trade changes relative to 2017 baseline at HS4 level
#
#    - change_trade_USD:       absolute change (USD)
#    - perc_change_trade_USD:  % change 
#    - trade_destruction:      flag for HS4 lines with 100 % export collapse
#    - trade_creation:         flag for HS4 lines with zero 2017 trade that gained exports
################################################################################

# 2017 baseline trade at HS4 level (single draw)
trade_hs4_2017 <- US %>%
  filter(draw == 1 & year == 2017) %>%
  group_by(sector, hs_section, hs2, hs4) %>%
  summarise(Trade_value_USD_2017 = sum(Trade_value_USD, na.rm = TRUE),
            .groups = "drop")

US_hs4_trade <- left_join(US, trade_hs4_2017)

# Compute changes (exclude base year 2017 from panel)
US_hs4_trade <- US_hs4_trade %>%
  filter(year != 2017 & draw == 1) %>%
  select(-c(diff_ln_AVE_FE, diff_ln_AVE_FE_bench, diff_ln_AVE_FE_wmean,
            diff_ln_AVE_FE_log, diff_ln_AVE_FE_log_bench, diff_ln_AVE_FE_log_wmean,
            diff_log_tariff_2017)) %>%
  mutate(
    change_trade_USD = Trade_value_USD - Trade_value_USD_2017,
    perc_change_trade_USD = case_when(
      Trade_value_USD_2017 == 0 & change_trade_USD == 0 ~ 0,
      change_trade_USD != 0 & Trade_value_USD_2017 != 0 ~
        change_trade_USD * 100 / Trade_value_USD_2017,
      change_trade_USD != 0 & Trade_value_USD_2017 == 0 ~ NA_real_
    ),
    perc_change_trade_USD_bis = if_else(perc_change_trade_USD > 100,
                                        NA_real_, perc_change_trade_USD),
    trade_destruction = if_else(perc_change_trade_USD == -100, 1, 0),
    trade_creation    = if_else(Trade_value_USD != 0 & Trade_value_USD_2017 == 0, 1, 0)
  )


US_hs4_trade <- full_join(US_hs4_trade,US_hs4_hs6_trade)


###############################################################################
# 5) Aggregate AVE changes to HS4 level
#
#    For each (year × HS4) cell:
#      (a) Winsorise raw HS6-level AVE changes at the 1st / 99th percentile
#          to limit the influence of outlier HS6 estimates before averaging
#      (b) Compute mean, SD, SE and 95 % CI across the (winsorised) HS6 draws
#      (c) Flag cells whose 95 % CI excludes zero (statistically significant)
################################################################################
c <- 0.01

# Helper: winsorise a vector at arbitrary quantile bounds
winsor <- function(x, p = c(c, 1-c)) {
  qs <- quantile(x, probs = p, na.rm = TRUE)
  pmin(pmax(x, qs[1]), qs[2])
}
US <- US %>%  mutate( across(c(diff_ln_AVE_FE, diff_ln_AVE_FE_bench, diff_ln_AVE_FE_wmean), ~ winsor(.x), .names = "{.col}_w")  )
summary(US)

# get change in AVEs
US_hs4_AVEs <- US %>%  filter(year != 2017) %>%
  # (a) Winsorise at the HS6 level BEFORE aggregating
  mutate(across(c(diff_ln_AVE_FE, diff_ln_AVE_FE_bench, diff_ln_AVE_FE_wmean),
                ~ winsor(.x),
                .names = "{.col}_w")) %>%
  group_by(year, hs4) %>%
  # (b) Summary statistics (mean, SD, SE, 95 % CI) for each AVE specification
  summarise(
    n = sum(!is.na(diff_ln_AVE_FE_w)),
    
    # -- FE baseline --
    mean_FE      = mean(diff_ln_AVE_FE_w, na.rm = TRUE),
    sd_FE        = sd(diff_ln_AVE_FE_w,   na.rm = TRUE),
    se_FE        = sd_FE / sqrt(n),
    ci_low_FE    = mean_FE - 1.96 * se_FE,
    ci_high_FE   = mean_FE + 1.96 * se_FE,
    
    # -- FE benchmark --
    mean_FE_bench    = mean(diff_ln_AVE_FE_bench_w, na.rm = TRUE),
    sd_FE_bench      = sd(diff_ln_AVE_FE_bench_w,   na.rm = TRUE),
    se_FE_bench      = sd_FE_bench / sqrt(n),
    ci_low_FE_bench  = mean_FE_bench - 1.96 * se_FE_bench,
    ci_high_FE_bench = mean_FE_bench + 1.96 * se_FE_bench,
    
    # -- FE weighted mean --
    mean_FE_wmean    = mean(diff_ln_AVE_FE_wmean_w, na.rm = TRUE),
    sd_FE_wmean      = sd(diff_ln_AVE_FE_wmean_w,   na.rm = TRUE),
    se_FE_wmean      = sd_FE_wmean / sqrt(n),
    ci_low_FE_wmean  = mean_FE_wmean - 1.96 * se_FE_wmean,
    ci_high_FE_wmean = mean_FE_wmean + 1.96 * se_FE_wmean,
    
    mean_diff_log_tariff_2017 = mean(diff_log_tariff_2017, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  # (c) Significance flag: CI excludes zero
  mutate(
    sig_FE       = ci_low_FE    > 0 | ci_high_FE    < 0,
    sig_FE_bench = ci_low_FE_bench > 0 | ci_high_FE_bench < 0,
    sig_FE_wmean = ci_low_FE_wmean > 0 | ci_high_FE_wmean < 0
  )

# --- diagnostics ---
summary(US_hs4_AVEs)




US_hs4 <- full_join(US_hs4_trade, US_hs4_AVEs)
names(US_hs4)
colSums(is.na(US_hs4))
summary(US_hs4)


names(US_hs4)


###############################################################################
common_theme  <- theme_minimal(base_size = 14, base_family = "Times New Roman") +
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
################################################################################

################################################################################

# plot to look at where big AVEs are 


library(ggplot2)
library(patchwork)

df <- subset(US_hs4, year %in% c(2018, 2019))
yl <- range(df$change_trade_USD, na.rm = TRUE)


# percentage

df <- subset(US_hs4, year %in% c(2018, 2019))
summary(df)

# Common y-limits for comparability
yl_perc <- range(df$perc_change_trade_USD_bis, na.rm = TRUE)

common_theme <- theme_classic() +
  theme(legend.position = "bottom",  plot.title = element_text(size = 11),
    plot.background  = element_rect(fill = "white", color = NA)  )

# 1) Tariff (LEFT)
p_tariff_perc <- ggplot(
  df, aes(x = mean_diff_log_tariff_2017, y = perc_change_trade_USD_bis)) +
  geom_point(size = 2, alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 1) +
  coord_cartesian(ylim = yl_perc) +
  labs(x = "Change in tariff rate",  y = "Percentage Change in Trade Value (USD)",  title = "Tariff"  ) +
  common_theme

# 2) AVE FE (RIGHT – top)
p_fe_perc <- ggplot(  df,
  aes(x = mean_FE, y = perc_change_trade_USD_bis, color = sig_FE)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(aes(group = 1),  method = "lm",  se = FALSE,color = "black",    linewidth = 1  ) +
  scale_color_manual(  values = c("FALSE" = "grey70", "TRUE" = "firebrick"),
    labels = c("Not significant", "Significant"),    name = "AVE change"  ) +
  coord_cartesian(ylim = yl_perc) +
  labs( x = "Change in AVE (FE)",  y = NULL,  title = "AVE (FE)") +
  common_theme

# 3) AVE benchmark (RIGHT – bottom)
p_bench_perc <- ggplot( df,
  aes(x = mean_FE_bench, y = perc_change_trade_USD_bis, color = sig_FE)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(aes(group = 1), method = "lm",se = FALSE, color = "black",linewidth = 1  ) +
  scale_color_manual( values = c("FALSE" = "grey70", "TRUE" = "firebrick"),
    labels = c("Not significant", "Significant"),  name = "AVE change"  ) +
  coord_cartesian(ylim = yl_perc) +
  labs( x = "Change in AVE (benchmark)",  y = NULL,  title = "AVE (benchmark)") +
  common_theme

# 3) AVE weighted mean (NEW – middle right)
p_wmean_perc <- ggplot(df,  aes(x = mean_FE_wmean, y = perc_change_trade_USD_bis, color = sig_FE)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(aes(group = 1),   method = "lm",  se = FALSE,  color = "black", linewidth = 1) +
  scale_color_manual(  values = c("FALSE" = "grey70", "TRUE" = "firebrick"),
    labels = c("Not significant", "Significant"),   name = "AVE change"  ) +
  coord_cartesian(ylim = yl_perc) +
  labs(  x = "Change in AVE (weighted mean)",   y = NULL,   title = "AVE (weighted mean)"  ) +
  common_theme

# Combine layout
final_plot_perc <- ((p_tariff_perc /p_fe_perc) | (p_wmean_perc / p_bench_perc)) + plot_layout(widths = c(1.1, 1))
final_plot_perc

ggsave(filename = paste0(exp , "plot/tariff_AVE_perc_trade_changes_2018_2019_base_2017.png"),
       plot = final_plot_perc, width = 12,height = 6, units = "in", dpi = 300,bg = "white")


###################################################################################
# Looking at distribution by percentage change in trade 
###################################################################################
names(df)


hs4_destroyed <- df %>% filter(trade_destruction == 1)

ggplot(df,  aes(x = mean_FE_wmean, y = share_hs6_destruct, color = hs2))+
  geom_point()

ggplot(hs2_destroyed, ) +
    geom_point(aes(x = mean_FE_wmean, y = change_trade_USD, color = as.factor(hs2)),size = 2.5, alpha = 0.8)


###################################################################################




###################################################################################
names(US_hs4)
summary(US_hs4$perc_change_trade_USD_bis)
US_hs4 <- US_hs4 %>%  
  mutate( above_50 = if_else(perc_change_trade_USD_bis < -50, 1, 0),
    above_50 = factor(above_50, levels = c(0, 1),   labels = c(">= -50%", "< -50%"))  ,
    change_cat = case_when(
      perc_change_trade_USD_bis > 0  ~ ">0",
      perc_change_trade_USD_bis <= 0  & perc_change_trade_USD_bis > -25  ~ "0 to -25",
      perc_change_trade_USD_bis <= -25 & perc_change_trade_USD_bis > -50  ~ "-25 to -50",
      perc_change_trade_USD_bis <= -50 & perc_change_trade_USD_bis > -75  ~ "-50 to -75",
      perc_change_trade_USD_bis <= -75 & perc_change_trade_USD_bis > -100 ~ "-75 to -99",
      perc_change_trade_USD_bis == -100 ~ "-100",
      
      TRUE ~ NA_character_  ) ,
    change_cat = factor(change_cat, levels = c( ">0", "0 to -25", "-25 to -50",
        "-50 to -75","-75 to -99", "-100"),ordered = TRUE  ))
table(US_hs4$above_50)
table(US_hs4$change_cat)
summary(US_hs4)

length(unique(US_hs4$hs4))

# Extract last 5 colors from YlOrRd (6 categories, so last 5 of a palette pulled with 6 colors)
custom_colors <- c(
  ">0"        = "blue",   # blue
  "0 to -25"  = "blueviolet",   # light blue
  "-25 to -50"= "cyan3",   # light gray
  "-50 to -75"= "darkgreen",   # light orange
  "-75 to -99"= "#d6604d",   # orange-red
  "-100"      = "#b2182b"    # dark red
)

###############################################################################

# multiple categories
plot_w <- ggplot(subset(US_hs4, !is.na(above_50)), aes(x = mean_FE_wmean, color = change_cat)) +
  geom_density(linewidth = 1) +
  coord_cartesian(xlim= c(-5,10))+
  common_theme +
  labs(  title = "Distribution of Changes in AVEs Imposed by CHN on U.S. at HS4",, x = " Δ ln(1+T_US,CHN) (FE demean)",
         y = "Density",color = "% Change U.S. Exports to \n CHN (relative to 2017)" ,
         )
plot_w
ggsave(filename = paste0(exp , "plot/distribution/distribution_AVEs_perc_change_trade_2017_wmean.png"),
       plot = plot_w, width = 12,height = 6, units = "in", dpi = 300,bg = "white")



# Rebuild each plot WITHOUT its own legend, using the shared color scale
plot <- ggplot(subset(US_hs4, !is.na(above_50)), aes(x = mean_FE, color = change_cat)) +
  geom_density(linewidth = 1) +
  scale_color_manual(values = custom_colors, name = "% Change U.S. Exports to\nCHN (relative to 2017)") +
  coord_cartesian(xlim = c(-5, 10)) +
  common_theme +
  labs(x = "Δ ln(1+AVE) (FE)", y = "Density") +
  theme(legend.position = "none")

plot_w <- ggplot(subset(US_hs4, !is.na(above_50)), aes(x = mean_FE_wmean, color = change_cat)) +
  geom_density(linewidth = 1) +
  scale_color_manual(values = custom_colors, name = "% Change U.S. Exports to\nCHN (relative to 2017)") +
  coord_cartesian(xlim = c(-5, 10)) +
  common_theme +
  labs(x = "Δ ln(1+AVE) (FE demean)", y = "Density") +
  theme(legend.position = "none")

plot_b <- ggplot(subset(US_hs4, !is.na(above_50)), aes(x = mean_FE_bench, color = change_cat)) +
  geom_density(linewidth = 1) +
  scale_color_manual(values = custom_colors, name = "% Change U.S. Exports to\nCHN (relative to 2017)") +
  coord_cartesian(xlim = c(-5, 10)) +
  common_theme +
  labs(x = "Δ ln(1+AVE)(FE benchmark)", y = "Density") +
  theme(legend.position = "none")

plot_tar <- ggplot(subset(US_hs4, !is.na(above_50)), aes(x = mean_diff_log_tariff_2017, color = change_cat)) +
  geom_density(linewidth = 1) +
  scale_color_manual(values = custom_colors, name = "% Change U.S. Exports to\nCHN (relative to 2017)") +
  coord_cartesian(xlim = c(-0.2, 0.75)) +
  common_theme +
  labs(x = "Δ ln(1+ tariff)", y = "Density") +
  theme(legend.position = "none")

# Extract legend from one plot (re-enable legend temporarily just for extraction)
legend_plot <- ggplot(subset(US_hs4, !is.na(above_50)), aes(x = mean_FE, color = change_cat)) +
  geom_density(linewidth = 1) +
  scale_color_manual(values = custom_colors, name = "% Change U.S. Exports to\nCHN (relative to 2017)") +
  common_theme +
  theme(legend.position = "right")

shared_legend <- cowplot::get_legend(legend_plot)

# Combine with patchwork + shared legend via cowplot
plots_combined <- plot / plot_w / plot_b / plot_tar
final_plot <- cowplot::plot_grid(
  plots_combined, shared_legend,ncol = 2,  rel_widths = c(1, 0.2))

final_plot
ggsave(filename = paste0(exp , "plot/distribution/distribution_AVEs_perc_change_trade_2017.png"),
       plot = final_plot, width = 12,height = 6, units = "in", dpi = 300,bg = "white")






# histogram version 
plot <- ggplot(subset(US_hs4, !is.na(change_cat)), aes(x = mean_FE_wmean, fill = change_cat)) +
  geom_histogram(position = "identity", alpha = 0.4, bins = 60) +
  common_theme +
  labs( x = "Weighted mean AVE change",   y = "Count",  fill = "Trade drop" )
plot



###################################################################################
# regressions
###################################################################################

reg <- lm(  change_trade_USD ~ mean_FE + mean_diff_log_tariff_2017 ,
  data = subset(US_hs4, year %in% c(2018, 2019)))
summary(reg)

reg <- lm(change_trade_USD ~ mean_FE_bench + mean_diff_log_tariff_2017 ,
            data = subset(US_hs4, year %in% c(2018, 2019)))
summary(reg)

reg <- lm(perc_change_trade_USD_bis ~ mean_FE + mean_diff_log_tariff_2017 ,
          data = subset(US_hs4, year %in% c(2018, 2019)))
summary(reg)

reg <- lm(perc_change_trade_USD_bis ~ mean_FE_bench + mean_diff_log_tariff_2017 ,
          data = subset(US_hs4, year %in% c(2018, 2019)))
summary(reg)





table(US_hs4$trade_creation)
names(US_hs4)
vars <- US_hs4 %>% filter(year %in% c(2018:2019)) %>%
  ungroup() %>%
  select(
    change_trade_USD,
    perc_change_trade_USD_bis,
    trade_destruction,
    mean_FE,
    mean_FE_bench,
    mean_FE_wmean,
    mean_diff_log_tariff_2017 )
corr_mat <- cor(vars, use = "complete.obs")
library(corrplot)
corrplot(corr_mat, method = "color", type = "upper", addCoef.col = "black",  tl.col = "black",  tl.srt = 45)







