


################################################################################
#                    Gravity regression analysis: FE approach

# check change in trade Vs chnage in AVEs?

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
library(countrycode)
library(Hmisc)
library(haven)
library(sfaR)
library(frontier)

################################################################################
# directory: 
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git")
exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/Compare_values/yearly/robust/"

################################################################################
# 1) Load data 
################################################################################

US <- read_csv(paste0(exp, "US_ln_NTMs_base_2015_FE_boot.csv"))
colSums(is.na(US))
test <- US  %>% filter(is.na(diff_log_tariff_2015))
table(test$year)

################################################################################

# For US ag: 

# create simple average change and weighted average change in 
names(US)
unique(US$year)
unique(US$hs2)
unique(US$hs_section)
length(unique(US$hs6_H5))
################################################################################
# create weights at HS4 level

# Aggregation hs level: create weight in trade (hs6/total US export)
# weights for hs6  to hs4 to aggregate to sector level
hs6_to_hs4_wts <- US %>%
  filter(year == 2015 & draw == 1) %>%
  group_by(hs4, hs6_H5) %>%
  summarise(trade_2015 = sum(Trade_value_USD, na.rm = TRUE), .groups = "drop") %>%
  group_by(hs4) %>%
  mutate(hs4_trade_2015 = sum(trade_2015, na.rm = TRUE),
    w_2015_hs6_hs4   = if_else(!is.na(trade_2015) | trade_2015 !=0 | (trade_2015 & hs4_trade_2015) !=0
                                 , trade_2015 / hs4_trade_2015, 0) ) %>%
  ungroup()
hs6_to_hs4_wts <- hs6_to_hs4_wts %>% mutate(w_2015_hs6_hs4 = if_else(is.na(w_2015_hs6_hs4), 0, w_2015_hs6_hs4))

colSums(is.na(hs6_to_hs4_wts))
US <-left_join(US,hs6_to_hs4_wts)
colSums(is.na(US))
test <- US %>% filter(is.na(diff_log_tariff_2015))
unique(test$year)


################################################################################

# aggregate at HS4 level
names(US)

# from trade and tariff data (because of draw just take one observation )

US_hs4_trade <- US %>% filter(draw == 1)  %>% group_by(year, sector, hs_section, hs2, hs4) %>% summarise(
  # for tariffs: weighted mean using 2015 weights
  weighted_diff_log_tariff_2015  = weighted.mean(diff_log_tariff_2015, w = w_2015_hs6_hs4, na.rm = TRUE),
  mean_diff_log_tariff_2015  = mean(diff_log_tariff_2015, na.rm = TRUE),
  # for trade aggregate by summing
  Trade_value_USD = sum(Trade_value_USD, na.rm = TRUE))
colSums(is.na(US_hs4_trade))
test <- US_hs4_trade %>% filter(is.na(weighted_diff_log_tariff_2015))
unique(test$year)

# get HS4 2015 trade values 
trade_2015 <- US %>% filter(draw == 1 & year == 2015)  %>% group_by(sector, hs_section, hs2,  hs4) %>% 
  summarise(Trade_value_USD_2015 = sum(Trade_value_USD, na.rm = TRUE))


US_hs4_trade <- left_join(US_hs4_trade, trade_2015)
colSums(is.na(US_hs4_trade))

# get change in trade values and percentage change in trade values 
US_hs4_trade <- US_hs4_trade %>% filter(year != 2015) %>%  mutate(
  change_trade_USD = Trade_value_USD - Trade_value_USD_2015,
  perc_change_trade_USD = case_when( Trade_value_USD_2015 == 0 & change_trade_USD == 0 ~ 0,
                                     change_trade_USD != 0 & Trade_value_USD_2015 != 0 ~ round(change_trade_USD * 100 / Trade_value_USD_2015),
                                     change_trade_USD != 0 & Trade_value_USD_2015 == 0 ~ NA_real_ ),
  perc_change_trade_USD_bis = if_else(perc_change_trade_USD > 100 , NA,perc_change_trade_USD ),
  trade_destruction = if_else(perc_change_trade_USD == -100, 1,0),
  trade_creation = if_else(Trade_value_USD != 0 & Trade_value_USD_2015 == 0, 1,0))
summary(US_hs4_trade)
table(US_hs4_trade$trade_destruction)

# get change in AVEs
names(US)
US_hs4_AVEs <- US %>% filter(year != 2015) %>%  group_by(year, hs4) %>% summarise(
  # for tariffs: weighted mean using 2015 weights
  diff_ln_AVE_FE  = mean(diff_ln_AVE_FE, na.rm = TRUE),
  diff_ln_AVE_FE_bench  = mean(diff_ln_AVE_FE_bench, na.rm = TRUE),
  diff_ln_AVE_FE_wmean  = mean(diff_ln_AVE_FE_wmean, na.rm = TRUE),
  diff_ln_AVE_FE_log  = mean(diff_ln_AVE_FE_log, na.rm = TRUE),
  diff_ln_AVE_FE_log_bench  = mean(diff_ln_AVE_FE_log_bench, na.rm = TRUE),
  diff_ln_AVE_FE_log_wmean  = mean(diff_ln_AVE_FE_log_wmean, na.rm = TRUE))
summary(US_hs4_AVEs)


US_hs4_AVEs <- US %>%
  filter(year != 2015) %>%
  group_by(year, hs4) %>%
  summarise(
    n = sum(!is.na(diff_ln_AVE_FE)),
    mean_FE = mean(diff_ln_AVE_FE, na.rm = TRUE),
    sd_FE   = sd(diff_ln_AVE_FE, na.rm = TRUE),
    se_FE   = sd_FE / sqrt(n),
    ci_low_FE  = mean_FE - 1.96 * se_FE,
    ci_high_FE = mean_FE + 1.96 * se_FE,
    
    mean_FE_bench = mean(diff_ln_AVE_FE_bench, na.rm = TRUE),
    sd_FE_bench   = sd(diff_ln_AVE_FE_bench, na.rm = TRUE),
    se_FE_bench   = sd_FE_bench / sqrt(n),
    ci_low_FE_bench  = mean_FE_bench - 1.96 * se_FE_bench,
    ci_high_FE_bench = mean_FE_bench + 1.96 * se_FE_bench,
    
    mean_FE_wmean = mean(diff_ln_AVE_FE_wmean, na.rm = TRUE),
    sd_FE_wmean   = sd(diff_ln_AVE_FE_wmean, na.rm = TRUE),
    se_FE_wmean   = sd_FE_wmean / sqrt(n),
    ci_low_FE_wmean  = mean_FE_wmean - 1.96 * se_FE_wmean,
    ci_high_FE_wmean = mean_FE_wmean + 1.96 * se_FE_wmean,
    
    .groups = "drop"  )
US_hs4_AVEs <- US_hs4_AVEs %>%
  mutate(sig_FE = ifelse(ci_low_FE > 0 | ci_high_FE < 0, TRUE, FALSE),
         sig_FE_bench = ifelse(ci_low_FE_bench > 0 | ci_high_FE_bench < 0, TRUE, FALSE) ,
         sig_FE_wmean = ifelse(ci_low_FE_wmean > 0 | ci_high_FE_wmean < 0, TRUE, FALSE) ,
             )

# windsorise cut first and 99th percentile mean values
winsor <- function(x, p = c(0.01, 0.99)) {
  qs <- quantile(x, probs = p, na.rm = TRUE)
  pmin(pmax(x, qs[1]), qs[2])
}

US_hs4_AVEs <- US_hs4_AVEs %>%
  mutate( across(c(mean_FE, mean_FE_bench, mean_FE_wmean), ~ winsor(.x), .names = "{.col}_w")  )
summary(US_hs4_AVEs)


US_hs4 <- full_join(US_hs4_trade, US_hs4_AVEs)
names(US_hs4)
colSums(is.na(US_hs4))
summary(US_hs4)


###############################################################################
# plot to look at where big AVEs are 


library(ggplot2)
library(patchwork)

df <- subset(US_hs4, year %in% c(2018, 2019))
yl <- range(df$change_trade_USD, na.rm = TRUE)

common_theme <- theme_minimal() +
  theme(legend.position = "bottom",  plot.title = element_text(size = 11),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)  )
# 1) Tariff (LEFT)
p_tariff <- ggplot(df, aes(x = mean_diff_log_tariff_2015, y = change_trade_USD)) +
  geom_point(size = 2, alpha = 0.6) +
  # geom_smooth(  method = "lm",  se = FALSE,  color = "black",   linewidth = 1  ) +
  coord_cartesian(ylim = yl) +
  labs(  x = "Change in tariff rate",   y = "Change in Trade Value (USD)",  title = "Tariff"  ) +
  common_theme

# 2) AVE FE (RIGHT – top)
p_fe <- ggplot(df, aes(x = mean_FE_w, y = change_trade_USD, color = sig_FE)) +
  geom_point(size = 2, alpha = 0.7) +
  # geom_smooth(aes(group = 1), method = "lm",se = FALSE,  color = "black",    linewidth = 1  ) +
  scale_color_manual(  values = c("FALSE" = "grey70", "TRUE" = "firebrick"),  labels = c("Not significant", "Significant"), name = "AVE change"  ) +
  coord_cartesian(ylim = yl) +
  labs(  x = "Change in AVE (FE)",y = NULL,   title = "AVE (FE)"  ) +
  common_theme

# 3) AVE benchmark (RIGHT – bottom)
p_bench <- ggplot(df, aes(x = mean_FE_bench_w, y = change_trade_USD, color = sig_FE_bench)) +
  geom_point(size = 2, alpha = 0.7) +
  # geom_smooth(aes(group = 1),  method = "lm",  se = FALSE,color = "black",  linewidth = 1  ) +
  scale_color_manual( values = c("FALSE" = "grey70", "TRUE" = "firebrick"),  labels = c("Not significant", "Significant"),name = "AVE change"  ) +
  coord_cartesian(ylim = yl) +
  labs(x = "Change in AVE (benchmark)", y = NULL, title = "AVE (benchmark)" ) +
  common_theme

# 3) AVE weighted mean (NEW – middle right)
p_wmean <- ggplot(df, aes(x = mean_FE_wmean, y = change_trade_USD,   color = sig_FE)) +
  geom_point(size = 2, alpha = 0.7) +
  scale_color_manual( values = c("FALSE" = "grey70", "TRUE" = "firebrick"),  labels = c("Not significant", "Significant"),
    name = "AVE change" ) +
  coord_cartesian(ylim = yl) +
  labs(  x = "Change in AVE (weighted demean)",  y = NULL,title = "AVE (weighted demean)"  ) +
  common_theme

# Combine
library(patchwork)
final_plot <- ((p_tariff/p_fe) | (p_wmean / p_bench)) +   plot_layout(widths = c(1.1, 1))
final_plot
ggsave(filename = paste0(exp , "plot/tariff_AVE_trade_changes_2018_2019.png"),
  plot = final_plot, width = 12,height = 6, units = "in", dpi = 300,bg = "white")




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
  df, aes(x = mean_diff_log_tariff_2015, y = perc_change_trade_USD_bis)) +
  geom_point(size = 2, alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "black", linewidth = 1) +
  coord_cartesian(ylim = yl_perc) +
  labs(x = "Change in tariff rate",  y = "Percentage Change in Trade Value (USD)",  title = "Tariff"  ) +
  common_theme

# 2) AVE FE (RIGHT – top)
p_fe_perc <- ggplot(  df,
  aes(x = mean_FE_w, y = perc_change_trade_USD_bis, color = sig_FE)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(aes(group = 1),  method = "lm",  se = FALSE,color = "black",    linewidth = 1  ) +
  scale_color_manual(  values = c("FALSE" = "grey70", "TRUE" = "firebrick"),
    labels = c("Not significant", "Significant"),    name = "AVE change"  ) +
  coord_cartesian(ylim = yl_perc) +
  labs( x = "Change in AVE (FE)",  y = NULL,  title = "AVE (FE)") +
  common_theme

# 3) AVE benchmark (RIGHT – bottom)
p_bench_perc <- ggplot( df,
  aes(x = mean_FE_bench_w, y = perc_change_trade_USD_bis, color = sig_FE)) +
  geom_point(size = 2, alpha = 0.7) +
  geom_smooth(aes(group = 1), method = "lm",se = FALSE, color = "black",linewidth = 1  ) +
  scale_color_manual( values = c("FALSE" = "grey70", "TRUE" = "firebrick"),
    labels = c("Not significant", "Significant"),  name = "AVE change"  ) +
  coord_cartesian(ylim = yl_perc) +
  labs( x = "Change in AVE (benchmark)",  y = NULL,  title = "AVE (benchmark)") +
  common_theme

# 3) AVE weighted mean (NEW – middle right)
p_wmean_perc <- ggplot(df,  aes(x = mean_FE_wmean_w, y = perc_change_trade_USD_bis, color = sig_FE)) +
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
ggsave(filename = paste0(exp , "plot/tariff_AVE_perc_trade_changes_2018_2019.png"),
       plot = final_plot_perc, width = 12,height = 6, units = "in", dpi = 300,bg = "white")


###################################################################################

# Looking at distribution by percentage change in trade 

names(US_hs4)

US_hs4 <- US_hs4 %>%  
  mutate( above_50 = if_else(perc_change_trade_USD_bis < -50, 1, 0),
    above_50 = factor(above_50, levels = c(0, 1),   labels = c(">= -50%", "< -50%"))  ,
    change_cat = case_when(
      perc_change_trade_USD_bis > 0                      ~ "Positive",
      perc_change_trade_USD_bis <= 0  & perc_change_trade_USD_bis > -25  ~ "0 to -25",
      perc_change_trade_USD_bis <= -25 & perc_change_trade_USD_bis > -50  ~ "-25 to -50",
      perc_change_trade_USD_bis <= -50 & perc_change_trade_USD_bis > -75  ~ "-50 to -75",
      perc_change_trade_USD_bis <= -75 & perc_change_trade_USD_bis >= -100 ~ "-75 to -100",
      TRUE ~ NA_character_  ) ,
    change_cat = factor(change_cat, levels = c( "Positive", "0 to -25", "-25 to -50",
        "-50 to -75","-75 to -100"),ordered = TRUE  ))
table(US_hs4$above_50)


# binary for 50
plot <- ggplot(subset(US_hs4, !is.na(above_50)), aes(x = mean_FE_wmean_w, color = above_50)) +
  geom_density(linewidth = 1) +
  common_theme +
  labs(  x = "Weighted mean AVE change",y = "Density",color = "Trade drop"  )
plot

plot <- ggplot(subset(US_hs4, !is.na(above_50)), aes(x = mean_FE_wmean_w, fill = above_50)) +
  geom_histogram(position = "identity", alpha = 0.4, bins = 60) +
  common_theme +
  labs( x = "Weighted mean AVE change",   y = "Count",  fill = "Trade drop" )
plot

################

# multiple categories
plot_w <- ggplot(subset(US_hs4, !is.na(above_50)), aes(x = mean_FE_wmean_w, color = change_cat)) +
  geom_density(linewidth = 1) +
  coord_cartesian(xlim= c(-5,25))+
  common_theme +
  labs(  x = "AVE change (FE demean)",y = "Density",color = "Trade change relative to 2015"  )
plot_w

plot <- ggplot(subset(US_hs4, !is.na(above_50)), aes(x = mean_FE_w, color = change_cat)) +
  geom_density(linewidth = 1) +
  coord_cartesian(xlim= c(-5,25))+
  common_theme +
  labs(  x = "AVE change (FE)",y = "Density",color = "Trade change relative to 2015"  )
plot

plot_b <- ggplot(subset(US_hs4, !is.na(above_50)), aes(x = mean_FE_bench_w, color = change_cat)) +
  geom_density(linewidth = 1) +
  coord_cartesian(xlim= c(-5,25))+
  common_theme +
  labs(  x = "AVE change (FE benchmark)",y = "Density",color = "Trade change relative to 2015"  )
plot_b

plot_tar <- ggplot(subset(US_hs4, !is.na(above_50)), aes(x = mean_diff_log_tariff_2015, color = change_cat)) +
  geom_density(linewidth = 1) +
  coord_cartesian(xlim= c(-0.2,0.75))+
  common_theme +
  labs(  x = "Change tariff",y = "Density",color = "Trade change relative to 2015"  )
plot_tar

final_plot <- (plot /plot_w /plot_b /plot_tar) + plot_layout(widths = c(1.1, 1))
final_plot
ggsave(filename = paste0(exp , "plot/distribution_AVEs_perc_change_trade.png"),
       plot = final_plot_perc, width = 12,height = 6, units = "in", dpi = 300,bg = "white")



# histogram version 
plot <- ggplot(subset(US_hs4, !is.na(change_cat)), aes(x = mean_FE_wmean_w, fill = change_cat)) +
  geom_histogram(position = "identity", alpha = 0.4, bins = 60) +
  common_theme +
  labs( x = "Weighted mean AVE change",   y = "Count",  fill = "Trade drop" )
plot

###################################################################################

reg <- lm(  change_trade_USD ~ mean_FE_w + mean_diff_log_tariff_2015 ,
  data = subset(US_hs4, year %in% c(2018, 2019)))
summary(reg)

reg <- lm(change_trade_USD ~ mean_FE_bench_w + mean_diff_log_tariff_2015 ,
            data = subset(US_hs4, year %in% c(2018, 2019)))
summary(reg)

reg <- lm(perc_change_trade_USD_bis ~ mean_FE_w + mean_diff_log_tariff_2015 ,
          data = subset(US_hs4, year %in% c(2018, 2019)))
summary(reg)

reg <- lm(perc_change_trade_USD_bis ~ mean_FE_bench_w + mean_diff_log_tariff_2015 ,
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
    mean_FE_w,
    mean_FE_bench_w,
    mean_FE_wmean_w,
    mean_diff_log_tariff_2015 )
corr_mat <- cor(vars, use = "complete.obs")
library(corrplot)
corrplot(corr_mat, method = "color", type = "upper", addCoef.col = "black",  tl.col = "black",  tl.srt = 45)







