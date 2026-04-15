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
exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/Compare_values/yearly/robust/"

################################################################################
# 1) Load data 
################################################################################

US_2015 <- read_csv(paste0(exp, "US_ln_NTMs_base_2015_FE_boot_hs4.csv"))
US_2017 <- read_csv(paste0(exp, "US_ln_NTMs_base_2017_FE_boot_hs4.csv"))

################################################################################
# 2) Parameters
################################################################################

quant <- 0.05

winsor <- function(x, p = c(quant, 1-quant)) {
  qs <- quantile(x, probs = p, na.rm = TRUE)
  x[x < qs[1] | x > qs[2]] <- NA
  x
}

vars <- c("diff_ln_AVE_FE", "diff_ln_AVE_FE_bench", "diff_ln_AVE_FE_wmean",
          "diff_ln_AVE_FE_log", "diff_ln_AVE_FE_log_bench", "diff_ln_AVE_FE_log_wmean")
vars_w <- paste0(vars, "_w")

################################################################################
# 3) 2015 baseline
################################################################################


# Winsorize at draw level
US_2015_hs4 <- US_2015 %>%  mutate(across(all_of(vars), ~ winsor(.x), .names = "{.col}_w"))

US_2015_hs4 <- US_2015_hs4 %>% filter(year %in% 2016:2020)


# Aggregate
US_2015_hs4 <- US_2015_hs4 %>%  group_by(year, sector, hs_section, hs2, hs4) %>%
  summarise(across(all_of(c(vars, vars_w)),
                   list(n    = ~ sum(!is.na(.x)),
                        mean = ~ mean(.x, na.rm = TRUE),
                        sd   = ~ sd(.x, na.rm = TRUE)), .names = "{.col}_{.fn}"), .groups = "drop")

# SE and CI
for (v in vars_w) {
  n_col    <- paste0(v, "_n")
  mean_col <- paste0(v, "_mean")
  sd_col   <- paste0(v, "_sd")
  se_col      <- paste0(v, "_se")
  ci_low_col  <- paste0(v, "_ci_low")
  ci_high_col <- paste0(v, "_ci_high")
  US_2015_hs4[[se_col]]      <- US_2015_hs4[[sd_col]] / sqrt(US_2015_hs4[[n_col]])
  US_2015_hs4[[ci_low_col]]  <- US_2015_hs4[[mean_col]] - 1.96 * US_2015_hs4[[se_col]]
  US_2015_hs4[[ci_high_col]] <- US_2015_hs4[[mean_col]] + 1.96 * US_2015_hs4[[se_col]]
}

# Significance flags
for (v in vars_w) {
  sig_col     <- paste0(v, "_sig")
  ci_low_col  <- paste0(v, "_ci_low")
  ci_high_col <- paste0(v, "_ci_high")
  US_2015_hs4[[sig_col]] <- (US_2015_hs4[[ci_low_col]] > 0 | US_2015_hs4[[ci_high_col]] < 0)
}

US_2015_hs4 <- US_2015_hs4 %>% mutate(baseline_year = 2015)

################################################################################
# 4) 2017 baseline
################################################################################


# Winsorize at draw level
US_2017_hs4 <- US_2017 %>%  mutate(across(all_of(vars), ~ winsor(.x), .names = "{.col}_w"))

US_2017_hs4 <- US_2017_hs4 %>% filter(year %in% 2017:2020)


# Aggregate
US_2017_hs4 <- US_2017_hs4 %>%  group_by(year, sector, hs_section, hs2, hs4) %>%
  summarise(across(all_of(c(vars, vars_w)),
                   list(n    = ~ sum(!is.na(.x)),
                        mean = ~ mean(.x, na.rm = TRUE),
                        sd   = ~ sd(.x, na.rm = TRUE)), .names = "{.col}_{.fn}"), .groups = "drop")

# SE and CI
for (v in vars_w) {
  n_col    <- paste0(v, "_n")
  mean_col <- paste0(v, "_mean")
  sd_col   <- paste0(v, "_sd")
  se_col      <- paste0(v, "_se")
  ci_low_col  <- paste0(v, "_ci_low")
  ci_high_col <- paste0(v, "_ci_high")
  US_2017_hs4[[se_col]]      <- US_2017_hs4[[sd_col]] / sqrt(US_2017_hs4[[n_col]])
  US_2017_hs4[[ci_low_col]]  <- US_2017_hs4[[mean_col]] - 1.96 * US_2017_hs4[[se_col]]
  US_2017_hs4[[ci_high_col]] <- US_2017_hs4[[mean_col]] + 1.96 * US_2017_hs4[[se_col]]
}

# Significance flags
for (v in vars_w) {
  sig_col     <- paste0(v, "_sig")
  ci_low_col  <- paste0(v, "_ci_low")
  ci_high_col <- paste0(v, "_ci_high")
  US_2017_hs4[[sig_col]] <- (US_2017_hs4[[ci_low_col]] > 0 | US_2017_hs4[[ci_high_col]] < 0)
}

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

write_csv(merge_US_hs4, paste0(exp, "US_ln_NTMs_base_2015_2017_hs4_FE_boot.csv"))

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

ggplot(merge_US_hs4) +
  geom_density(aes(x = diff_ln_AVE_FE_mean, fill = "FE"), alpha = 0.2) +
  geom_density(aes(x = diff_ln_AVE_FE_bench_mean, fill = "Benchmark"), alpha = 0.2) +
  geom_density(aes(x = diff_ln_AVE_FE_wmean_mean, fill = "Weighted mean"), alpha = 0.2) +
  facet_wrap(~ baseline_year) +
  scale_fill_manual(values = c( "FE" = "blue","Benchmark" = "red",  "Weighted mean" = "green" ),name = "Specification") +
  theme_trade +
  labs( x = "Δ ln(AVE)",y = "Density" )
 

names(merge_US_hs4)


# windsorized data 
plot <- ggplot(merge_US_hs4) +
  geom_density(aes(x = diff_ln_AVE_FE_mean_w, fill = "FE"), alpha = 0.2) +
  geom_density(aes(x = diff_ln_AVE_FE_bench_mean_w, fill = "Benchmark"), alpha = 0.2) +
  geom_density(aes(x = diff_ln_AVE_FE_wmean_mean_w, fill = "Weighted mean"), alpha = 0.2) +
  facet_wrap(~ baseline_year) +
  scale_fill_manual(values = c( "FE" = "blue","Benchmark" = "red",  "Weighted mean" = "green" ),name = "Specification") +
  theme_trade +
  labs( x = "Δ ln(AVE)",y = "Density", title = "Change in AVEs based on baseline (windsorized)" )
plot
ggsave(filename = paste0(exp , "plot/distribution/change_FE_AVE_dist_windsor.png"),
       plot = plot, width = 12,height = 6, units = "in", dpi = 300,bg = "white")




# significant data 
names(merge_US_hs4)
plot <- ggplot() +
  geom_density(data = subset(merge_US_hs4, diff_ln_AVE_FE_sig == TRUE), aes(x = diff_ln_AVE_FE_mean_w, fill = "FE"), alpha = 0.2) +
  geom_density(data = subset(merge_US_hs4, diff_ln_AVE_FE_bench_sig == TRUE),aes(x = diff_ln_AVE_FE_bench_mean_w, fill = "Benchmark"), alpha = 0.2) +
  geom_density( data = subset(merge_US_hs4, diff_ln_AVE_FE_wmean_sig == TRUE), aes(x = diff_ln_AVE_FE_wmean_mean_w, fill = "Weighted mean"), alpha = 0.2) +
  facet_wrap(~ baseline_year) +
  scale_fill_manual(values = c( "FE" = "blue","Benchmark" = "red",  "Weighted mean" = "green" ),name = "Specification") +
  theme_trade +
  labs( x = "Δ ln(AVE)",y = "Density", title = "Change in AVEs based on baseline (significant and windsorized)" )
plot
ggsave(filename = paste0(exp , "plot/distribution/change_FE_AVE_dist_sig_windsor.png"),
       plot = plot, width = 12,height = 6, units = "in", dpi = 300,bg = "white")






