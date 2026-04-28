################################################################################
#          Compare distribution of obtained AVEs based on baseline year

# check change in trade Vs change in AVEs?

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
exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/Compare_values/yearly/robust/elast/"

################################################################################
# 1) Load data 
################################################################################

US_2015 <- read_csv(paste0(exp, "US_ln_NTMs_2015_FE_hs4_elast.csv"))
US_2017 <- read_csv(paste0(exp, "US_ln_NTMs_2017_FE_hs4_elast.csv"))
names(US_2017)

################################################################################
# 3) 2015 baseline
################################################################################


quant <- 0.01

winsor <- function(x, p = c(quant, 1-quant)) {
  qs <- quantile(x, probs = p, na.rm = TRUE)
  x[x < qs[1] | x > qs[2]] <- NA
  x
}

vars <- c("diff_ln_AVE_FE", "diff_ln_AVE_FE_bench", "diff_ln_AVE_FE_wmean",
          "diff_ln_AVE_FE_log", "diff_ln_AVE_FE_log_bench", "diff_ln_AVE_FE_log_wmean",
          "diff_ln_AVE_FE_Chen", "diff_ln_AVE_FE_bench_Chen", "diff_ln_AVE_FE_wmean_Chen",
          "diff_ln_AVE_FE_log_Chen", "diff_ln_AVE_FE_log_bench_Chen", "diff_ln_AVE_FE_log_wmean_Chen")

names(US_2015)

################################################################################
US_2015 <- US_2015 %>%  mutate(across(all_of(vars), ~ winsor(.x), .names = "{.col}"))
summary(US_2015)


US_2017 <- US_2017 %>%  mutate(across(all_of(vars), ~ winsor(.x), .names = "{.col}"))
summary(US_2017)



################################################################################
# for 2015
US_2015_hs4 <- US_2015 %>% filter(year %in% 2016:2020)
US_2015_hs4 <- US_2015_hs4 %>% mutate(baseline_year = 2015)

# for 2017
US_2017_hs4 <- US_2017 %>% filter(year %in% 2017:2020)
US_2017_hs4 <- US_2017_hs4 %>% mutate(baseline_year = 2017)


################################################################################
# merge the two baseline data 
names(US_2017_hs4)
names(US_2015_hs4)
merge_US_hs4 <- rbind(US_2017_hs4, US_2015_hs4)
unique(merge_US_hs4$baseline_year)
unique(merge_US_hs4$year)
colSums(is.na(merge_US_hs4))
summary(merge_US_hs4)

write_csv(merge_US_hs4, paste0(exp, "US_ln_NTMs_2015_2017_hs4_elast.csv"))

merge_US_hs4 <- merge_US_hs4 %>% filter(year %in% c(2018:2019))

################################################################################


# create a theme for ggplot 
theme_trade <- theme_minimal(base_size = 14) +
  theme(    panel.spacing.x = unit(1.2, "lines"),
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
names(merge_US_hs4)

plot <- ggplot(merge_US_hs4) +
  geom_density(aes(x = diff_ln_AVE_FE, fill = "FE"), alpha = 0.2) +
  geom_density(aes(x = diff_ln_AVE_FE_bench, fill = "Benchmark"), alpha = 0.2) +
  geom_density(aes(x = diff_ln_AVE_FE_wmean, fill = "Weighted mean"), alpha = 0.2) +
  facet_wrap(~ baseline_year) +
  scale_fill_manual(values = c( "FE" = "blue","Benchmark" = "red",  "Weighted mean" = "green" ),name = "Specification") +
  theme_trade +
  labs( x = "Δ ln(AVE)",y = "Density" )
 plot
 
 ggsave(filename = paste0(exp , "plot/distribution/change_FE_AVE_dist_hs4_elast.png"),
        plot = plot, width = 12,height = 6, units = "in", dpi = 300,bg = "white")
 

 plot <- ggplot(merge_US_hs4) +
  geom_density(aes(x = diff_ln_AVE_FE_Chen, fill = "FE"), alpha = 0.2) +
  geom_density(aes(x = diff_ln_AVE_FE_bench_Chen, fill = "Benchmark"), alpha = 0.2) +
  geom_density(aes(x = diff_ln_AVE_FE_wmean_Chen, fill = "Weighted mean"), alpha = 0.2) +
  facet_wrap(~ baseline_year) +
  scale_fill_manual(values = c( "FE" = "blue","Benchmark" = "red",  "Weighted mean" = "green" ),name = "Specification") +
  theme_trade +
  labs( x = "Δ ln(AVE)",y = "Density" )

 plot
ggsave(filename = paste0(exp , "plot/distribution/change_FE_AVE_dist_chen_elast.png"),
       plot = plot, width = 12,height = 6, units = "in", dpi = 300,bg = "white")







