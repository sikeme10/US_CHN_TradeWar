



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
exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/Compare_values/"

################################################################################
# 1) Load data 
################################################################################

US <- read_csv(paste0(exp, "US_ln_NTMs.csv"))

NTM <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/CHN_WTO_NTM/CHN_WTO_notif.csv")

################################################################################

# Filter year:
US <- US %>% filter(year %in% c(2018:2019))
NTM <- NTM %>% filter(year %in% c(2018:2019))


# aggregate at yearly level 

# select affected countries to be world and US 
unique(NTM$AFF)
NTM1 <- NTM %>% filter(AFF %in% c("All Members", "United States"))
# NTM1 <- NTM %>% filter(AFF %in% c("United States"))
names(NTM)
unique(nchar(NTM1$HScombined))
NTM1 <- NTM1 %>%  mutate(  HScombined = as.character(HScombined),
                         HScombined = str_pad(HScombined, width = 6, pad = "0")  )
# get count of NTMs by type 
NTM_count_type <- NTM1 %>% group_by(HScombined, NTM) %>%
  summarise(count_notif = n_distinct(ID),    .groups = "drop"  )

NTM_tot <- NTM1 %>% group_by(HScombined) %>%
  summarise(count_notif = n_distinct(ID),    .groups = "drop"  )

# aggregate NTM AVE levels at overall change from 2017 to 2019

names(US)  

US1 <- US %>% group_by(hs6_H5,sector) %>% summarise( 
  diff_log_tariff_2017 = mean( diff_log_tariff_2017,na.rm = TRUE),
  diff_ln_AVE_FE = mean(diff_ln_AVE_FE ,na.rm = TRUE),
  diff_ln_AVE_u = mean(diff_ln_AVE_u ,na.rm = TRUE),
  diff_ln_AVE_u_tariff = mean(diff_ln_AVE_u_tariff ,na.rm = TRUE),)

# merge the two 
merge <- left_join( US1,NTM_tot,  by = c("hs6_H5" = "HScombined"))
summary(merge)
  
################################################################################


# overall plots 



plot <- ggplot(merge, aes(x = diff_ln_AVE_FE, y = count_notif)) +
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
ggsave(filename = file.path(exp, "plot/", "Correlation_AVE_FE_WTOnotif.png"),plot = plot, width = 8, height = 5, dpi = 300)



plot <- ggplot(merge, aes(x = diff_ln_AVE_u_tariff, y = count_notif)) +
  geom_point(alpha = 0.15, size = 1) +
  geom_abline(intercept = 0, slope = 1, color = "red",        linewidth = 1, linetype = "dotted") +
  # geom_smooth( color = "red",linewidth = 1, linetype = "dotted") +
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
ggsave(filename = file.path(exp, "plot/", "Correlation_AVE_SF_WTOnotif.png"),plot = plot, width = 8, height = 5, dpi = 300)


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



# corrplot
library(corrplot)

names(merge_ag)
vars <- merge_ag[, c("diff_ln_AVE_FE", "diff_ln_AVE_u_tariff", "count_notif")]

vars <- vars %>%  rename(  Chen_et_al = diff_ln_AVE_chen,  FE = diff_ln_AVE_FE,
                           SF = diff_ln_AVE_u,  SF_tariff  = diff_ln_AVE_u_tariff )

cor_mat <- cor(vars, use = "complete.obs")

# open PNG device
png(paste0(exp, "plot/corrplot.png"), width = 1200, height = 1000, res = 150)

corrplot(cor_mat, method = "circle", addCoef.col = "black")



