################################################################################
#                    Gravity regression analysis: residual approach
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

NTM <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/CHN_WTO_NTM/CHN_WTO_notif.csv")

################################################################################
US$hs6_H5 <- as.numeric(US$hs6_H5)


# Filter year:
US1 <- US %>% filter(year %in% c(2018:2019))
NTM1 <- NTM %>% filter(year %in% c(2017:2020))

# select affected countries to be world and US 
unique(NTM$AFF)
NTM1 <- NTM1 %>% filter(AFF %in% c("All Members", "United States"))
# NTM1 <- NTM %>% filter(AFF %in% c("United States"))


NTM_tot_exp <- NTM1 %>% group_by(year,AFF) %>%
  summarise(count_notif = n_distinct(ID),    .groups = "drop"  )

NTM_count_type_exp <- NTM1 %>% group_by(year,AFF, NTM) %>%
  summarise(count_notif = n_distinct(ID),    .groups = "drop"  )


################################################################################

# get significant estimates 

names(US1)
library(dplyr)

sig_US1 <- US1 %>%
  group_by(hs6_H5,year) %>%
  summarise(
    # diff_ln_AVE_FE
    mean_FE   = mean(diff_ln_AVE_FE, na.rm = TRUE),
    lo_FE     = quantile(diff_ln_AVE_FE, 0.025, na.rm = TRUE),
    hi_FE     = quantile(diff_ln_AVE_FE, 0.975, na.rm = TRUE),
    sig_FE    = !(lo_FE <= 0 & hi_FE >= 0),
    
    # diff_ln_AVE_FE_bench
    mean_FE_bench = mean(diff_ln_AVE_FE_bench, na.rm = TRUE),
    lo_bench   = quantile(diff_ln_AVE_FE_bench, 0.025, na.rm = TRUE),
    hi_bench   = quantile(diff_ln_AVE_FE_bench, 0.975, na.rm = TRUE),
    sig_bench  = !(lo_bench <= 0 & hi_bench >= 0),
    
    # optional: how many bootstrap draws you actually have (should be ~50)
    n_draws = n_distinct(draw)  ) %>%
  arrange(desc(sig_FE), desc(abs(mean_FE)))


sig_US1 <- sig_US1 %>% mutate(
  diff_ln_AVE_FE = if_else(sig_FE, mean_FE, 0),
  diff_ln_AVE_FE_bench = if_else(sig_bench, mean_FE_bench, 0)
)



################################################################################

# aggregate at yearly level 

# aggregate NTM AVE levels at overall change from 2017 to 2019
NTM_tot <- NTM1 %>% group_by(HScombined) %>%
  summarise(count_notif = n_distinct(ID),    .groups = "drop"  )
NTM_tot <- NTM_tot %>% mutate(NTM_binary = if_else(count_notif > 0 , 1,0))

# agggregate US over years 
names(US)  

sig_US1 <- sig_US1 %>% group_by(hs6_H5) %>% summarise( 
  log_tariff_pre_2015 = mean( log_tariff_pre_2015,na.rm = TRUE),
  diff_ln_AVE_FE = mean(diff_ln_AVE_FE ,na.rm = TRUE),
  diff_ln_AVE_FE_bench = mean(diff_ln_AVE_FE_bench ,na.rm = TRUE))

# merge the two 

merge <- left_join( sig_US1,NTM_tot,  by = c("hs6_H5" = "HScombined"))
summary(merge)
merge <- merge  %>% mutate(NTM_binary = if_else(count_notif > 0 , 1,0))

merge <- merge  %>% mutate(NTM_binary = if_else(is.na(NTM_binary) , 0, NTM_binary),
                           count_notif =  if_else(is.na(count_notif),0, count_notif))




################################################################################
# get 2015 as trade as weights for NTMs

US_trade_2015 <- US %>% 
  filter(year == 2015 & draw == 1) %>%
  group_by(hs6_H5) %>%
  summarise(Trade_value_USD_2015 = sum(Trade_value_USD, na.rm = TRUE),  .groups = "drop"  ) %>%
  mutate(  tot_Trade_value_USD_2015 = sum(Trade_value_USD_2015),
           weight_sector = Trade_value_USD_2015 / tot_Trade_value_USD_2015  )

merge1 <- left_join( merge,US_trade_2015)

################################################################################
# create a coverage index
names(merge1)
merge1 <- merge1 %>% mutate(coverage_NTM = weight_sector * NTM_binary * 100)
summary(merge1$coverage_NTM)

################################################################################
# PLOTS 
################################################################################

# overall plots 



# Count notifications
plot <- ggplot(merge1, aes(x = diff_ln_AVE_FE, y = count_notif)) +
  geom_point(alpha = 0.15, size = 1) +
  geom_abline(intercept = 0, slope = 1, color = "red",        linewidth = 1, linetype = "dotted") +
  #  geom_smooth(intercept = 0, slope = 1, color = "red",        linewidth = 1, linetype = "dotted") +
  theme_minimal() +
  labs( title = "Correlation plot of Δ ln(1 + AVE) from FE vs WTO NTM notifications (2018-2019)",
        x = "Δ ln(1 + AVE) FE",    y = "Notification counts"  ) +
  theme(plot.title      = element_text(size = 11, hjust = 0.5),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA),
        axis.text.x     = element_text(size = 9),
        axis.text.y     = element_text(size = 9),
        axis.title.x    = element_text(size = 10),
        axis.title.y    = element_text(size = 10)  )
plot
ggsave(filename = file.path(exp, "plot/", "Correlation_AVE_FE_WTO_notif_signific.png"),plot = plot, width = 8, height = 5, dpi = 300)

plot <- ggplot(merge1, aes(x = diff_ln_AVE_FE_bench, y = count_notif)) +
  geom_point(alpha = 0.15, size = 1) +
  geom_abline(intercept = 0, slope = 1, color = "red",        linewidth = 1, linetype = "dotted") +
  #  geom_smooth(intercept = 0, slope = 1, color = "red",        linewidth = 1, linetype = "dotted") +
  theme_minimal() +
  labs( title = "Correlation plot: Δ ln(1 + AVE) from FE (bench) vs WTO NTM notifications (2018-2019)",
        x = "Δ ln(1 + AVE) FE",    y = "Notification counts"  ) +
  theme(plot.title      = element_text(size = 11, hjust = 0.5),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA),
        axis.text.x     = element_text(size = 9),
        axis.text.y     = element_text(size = 9),
        axis.title.x    = element_text(size = 10),
        axis.title.y    = element_text(size = 10)  )
plot
ggsave(filename = file.path(exp, "plot/", "Correlation_AVE_FE_bench_WTO_notif_signific.png"),plot = plot, width = 8, height = 5, dpi = 300)




# Ntm product covergae ratio
plot <- ggplot(merge1, aes(x = diff_ln_AVE_FE, y = coverage_NTM)) +
  geom_point(alpha = 0.15, size = 1) +
  geom_abline(intercept = 0, slope = 1, color = "red",        linewidth = 1, linetype = "dotted") +
  #  geom_smooth(intercept = 0, slope = 1, color = "red",        linewidth = 1, linetype = "dotted") +
  theme_minimal() +
  labs( title = "Correlation plot of Δ ln(1 + AVE) from FE vs NTM coverage (2018-2019)",
        x = "Δ ln(1 + AVE) FE",    y = "NTM coverage ratio"  ) +
  theme(plot.title      = element_text(size = 11, hjust = 0.5),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA),
        axis.text.x     = element_text(size = 9),
        axis.text.y     = element_text(size = 9),
        axis.title.x    = element_text(size = 10),
        axis.title.y    = element_text(size = 10)  )
plot
ggsave(filename = file.path(exp, "plot/", "Correlation_AVE_FE_NTM_coverage_signific.png"),plot = plot, width = 8, height = 5, dpi = 300)

plot <- ggplot(merge1, aes(x = diff_ln_AVE_FE_bench, y = coverage_NTM)) +
  geom_point(alpha = 0.15, size = 1) +
  geom_abline(intercept = 0, slope = 1, color = "red",        linewidth = 1, linetype = "dotted") +
  #  geom_smooth(intercept = 0, slope = 1, color = "red",        linewidth = 1, linetype = "dotted") +
  theme_minimal() +
  labs( title = "Correlation plot: Δ ln(1 + AVE) from FE (bench) vs NTM coverage (2018-2019)",
        x = "Δ ln(1 + AVE) FE",    y = "NTM coverage ratio"  ) +
  theme(plot.title      = element_text(size = 11, hjust = 0.5),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA),
        axis.text.x     = element_text(size = 9),
        axis.text.y     = element_text(size = 9),
        axis.title.x    = element_text(size = 10),
        axis.title.y    = element_text(size = 10)  )
plot
ggsave(filename = file.path(exp, "plot/", "Correlation_AVE_FE_bench_NTM_coverage_signific.png"),plot = plot, width = 8, height = 5, dpi = 300)





# corrplot
library(corrplot)

names(merge1)
vars <- merge1[, c("diff_ln_AVE_FE", "diff_ln_AVE_FE_bench", "NTM_binary","count_notif" , "coverage_NTM")]


cor_mat <- cor(vars, use = "complete.obs")

# open PNG device
png(paste0(exp, "plot/corrplot_signific.png"), width = 1200, height = 1000, res = 150)

corrplot(cor_mat, method = "circle", addCoef.col = "black")



cor_mat <- cor(vars, use = "pairwise.complete.obs", method = "pearson")

png(paste0(exp, "plot/corrplot_signific.png"),    width = 1200, height = 1000, res = 150)

corrplot(cor_mat, method = "circle",
         type = "upper",
         tl.col = "black",
         tl.srt = 45,
         addCoef.col = "black",
         number.cex = 0.8,
         diag = FALSE
)

dev.off()


################################################################################

# for Ag 

merge_ag <- merge %>% filter(sector == "Ag")


plot <- ggplot(merge_ag, aes(x = diff_ln_AVE_FE, y = count_notif)) +
  geom_point(alpha = 0.15, size = 1) +
  geom_abline(intercept = 0, slope = 1, color = "red",
              linewidth = 1, linetype = "dotted") +
  theme_minimal() +
  labs( title = "Correlation plot of Δ ln(1 + AVE) from FE vs Tariff-adjusted SF",
        x = "Δ ln(1 + AVE) FE",    y = "Notification counts"  ) +
  theme(plot.title      = element_text(size = 11, hjust = 0.5),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA),
        axis.text.x     = element_text(size = 9),
        axis.text.y     = element_text(size = 9),
        axis.title.x    = element_text(size = 10),
        axis.title.y    = element_text(size = 10)  )
plot
ggsave(filename = file.path(exp, "plot/", "Corrlation_FE_SF.png"),plot = plot, width = 8, height = 5, dpi = 300)



plot <- ggplot(merge_ag, aes(x = diff_ln_AVE_u_tariff, y = count_notif)) +
  geom_point(alpha = 0.15, size = 1) +
  #geom_smooth(color = "red") +
  theme_minimal() +
  labs( title = "Correlation plot of Δ ln(1 + AVE) from FE vs Tariff-adjusted SF",
        x = "Δ ln(1 + AVE) FE",    y = "Notification counts"  ) +
  theme(plot.title      = element_text(size = 11, hjust = 0.5),
        panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA),
        axis.text.x     = element_text(size = 9),
        axis.text.y     = element_text(size = 9),
        axis.title.x    = element_text(size = 10),
        axis.title.y    = element_text(size = 10)  )
plot





