

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

# load data 
US <- read_csv(paste0(exp, "US_ln_NTMs_base_2015_FE_boot.csv"))


################################################################################

# select year of interest 

US <- US %>% filter(year%in% c(2018:2019))


# sector and HS section level details
US$hs2 <- as.numeric(US$hs2)
US <- US %>% mutate(
  hs_section = case_when(
    hs2 %in% 1:5 ~ 1,
    hs2 %in% 6:14 ~ 2,
    hs2 %in% 15 ~ 3,
    hs2 %in% 16:24 ~ 4,
    hs2 %in% 25:27 ~ 5,
    hs2 %in% 28:38 ~ 6,
    hs2 %in% 39:40 ~ 7,
    hs2 %in% 41:43 ~ 8,
    hs2 %in% 44:46 ~ 9,
    hs2 %in% 47:49 ~ 10,
    hs2 %in% 50:63 ~ 11,
    hs2 %in% 64:67 ~ 12,
    hs2 %in% 68:70 ~ 13,
    hs2 %in% 71 ~ 14,
    hs2 %in% 72:83 ~ 15,
    hs2 %in% 84:85 ~ 16,
    hs2 %in% 86:89 ~ 17,
    hs2 %in% 90:92 ~ 18,
    hs2 %in% 93 ~ 19,
    hs2 %in% 94:96 ~ 20,
    hs2 %in% 97 ~ 21  ),
  sector = case_when(
    hs_section %in% 1:4   ~ "Ag",
    hs_section %in% 5:20  ~ "Manu",
    TRUE                  ~ "Other"))



################################################################################


summary(US$diff_ln_AVE_FE_bench)
summary(US$diff_ln_AVE_FE)

names(US)
p_val <- 0.10

US_agg <- US %>%  group_by(year, sector, hs_section, hs2, hs4) %>%
  summarise(diff_ln_AVE_FE_mean = mean(diff_ln_AVE_FE, na.rm = TRUE),
            diff_ln_AVE_FE_median = median(diff_ln_AVE_FE, na.rm = TRUE),
            diff_ln_AVE_FE_lo   = quantile(diff_ln_AVE_FE, p_val, na.rm = TRUE),
            diff_ln_AVE_FE_hi   = quantile(diff_ln_AVE_FE, 1-p_val, na.rm = TRUE),
            sig_FE    = !(diff_ln_AVE_FE_lo <= 0 & diff_ln_AVE_FE_hi >= 0),
            
            diff_ln_AVE_FE_bench_mean = mean(diff_ln_AVE_FE_bench, na.rm = TRUE),
            diff_ln_AVE_FE_bench_median = median(diff_ln_AVE_FE_bench, na.rm = TRUE),
            diff_ln_AVE_FE_lo_bench   = quantile(diff_ln_AVE_FE_bench, p_val, na.rm = TRUE),
            diff_ln_AVE_FE_hi_bench   = quantile(diff_ln_AVE_FE_bench, 1-p_val, na.rm = TRUE),  
            sig_bench  = !(diff_ln_AVE_FE_lo_bench <= 0 & diff_ln_AVE_FE_hi_bench >= 0),
            
            .groups = "drop"  )


summary(US_agg$diff_ln_AVE_FE_mean)
summary(US_agg$diff_ln_AVE_FE_median)
summary(US_agg$diff_ln_AVE_FE_bench_mean)
summary(US_agg$diff_ln_AVE_FE_bench_median)

# count unique hs4 that are significant
count <- US_agg %>% summarise( total_hs4 = n_distinct(hs4),
                               n_hs4_sig_FE     = n_distinct(hs4[sig_FE]),
                               n_hs4_sig_bench = n_distinct(hs4[sig_bench])  )
count

# create a theme for ggplot 
theme_trade <- theme_minimal(base_size = 14) +
  theme(  panel.spacing.x = unit(1.2, "lines"),
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
# for windsorised FE and benchmarked estimates
################################################################################

US_agg

# drop first and 99th percentile
quant <- 0.01
# FE
FE_q0.01 <- quantile(US_agg$diff_ln_AVE_FE_mean, quant, na.rm=TRUE)
FE_q0.99 <- quantile(US_agg$diff_ln_AVE_FE_mean, 1-quant, na.rm=TRUE)
FE_q0.01
FE_q0.99
# for benchmark
FE_bench_q0.01 <- quantile(US_agg$diff_ln_AVE_FE_mean, quant, na.rm=TRUE)
FE_bench_q0.99 <- quantile(US_agg$diff_ln_AVE_FE_mean, 1-quant, na.rm=TRUE)
FE_bench_q0.01
FE_bench_q0.99

# For FE
US_agg_FE_wind <- US_agg %>% filter(diff_ln_AVE_FE_mean >= FE_q0.01 & diff_ln_AVE_FE_mean <= FE_q0.99) %>% 
  select(-diff_ln_AVE_FE_bench_mean, -diff_ln_AVE_FE_bench_median, -diff_ln_AVE_FE_lo_bench,
         -diff_ln_AVE_FE_hi_bench, -sig_bench)
# For bench
US_agg_FE_bench_wind <- US_agg %>% filter(diff_ln_AVE_FE_bench_mean >= FE_bench_q0.01 &
                                              diff_ln_AVE_FE_bench_mean <= FE_bench_q0.99 ) %>%
  select(-diff_ln_AVE_FE_mean, -diff_ln_AVE_FE_median, -diff_ln_AVE_FE_lo, -diff_ln_AVE_FE_hi, -sig_FE)

# merge for windsorized data
US_agg_wind <- full_join(US_agg_FE_wind, US_agg_FE_bench_wind)
names(US_agg_wind)


# density of diff_ln_AVE_FE_mean
FE_plot <- ggplot(US_agg_wind) +
  geom_density(aes(x=diff_ln_AVE_FE_mean), fill="blue", alpha=0.15) +
  geom_density(aes(x=diff_ln_AVE_FE_lo), fill="green", alpha=0.15) +
  xlab("Mean windsorized FE estimate difference (2015 base)") +
  ggtitle("Density of Mean FE estimate differences")
FE_plot

FE_bench_plot <- ggplot(US_agg_wind) +
  geom_density(aes(x=diff_ln_AVE_FE_bench_mean), fill="blue", alpha=0.15) +
  geom_density(aes(x=diff_ln_AVE_FE_lo_bench), fill="green", alpha=0.15) +
  xlab("Mean windsorized FE estimate difference (2015 base)") +
  ggtitle("Density of Mean FE estimate differences")
FE_bench_plot

FE_windsor_plot <- ggplot(US_agg_wind) +
  geom_density(aes(x = diff_ln_AVE_FE_mean, fill = "FE"), alpha = 0.15  ) +
  geom_density(aes(x = diff_ln_AVE_FE_bench_mean, fill = "Benchmarked FE"),  alpha = 0.15  ) +
  xlab("Mean windsorized FE estimate difference (2015 base)") +
  ggtitle("Density of Mean FE estimate differences")+
  scale_fill_manual(name = "Estimate type",  values = c("FE" = "blue", "Benchmarked FE" = "green")  ) +
  theme_trade
FE_windsor_plot

ggsave(filename = paste0(exp, "plot/distribution/FE_windsor_plot.png"),
  plot = FE_windsor_plot, width = 8,height = 6,  dpi = 300)


write_csv(US_agg_wind, paste0(exp, "US_ln_NTMs_base_2015_FE_bench_windsorised.csv"))

###############################################################################
# for significant FE model 
###############################################################################

# significant 
US_agg_FE_sign <- US_agg %>% filter(sig_FE == TRUE)
summary(US_agg_FE_sign$diff_ln_AVE_FE_mean)
summary(US_agg_FE_sign$diff_ln_AVE_FE_median)

# density of diff_ln_AVE_FE_mean
ggplot(US_agg_FE_sign) +
  geom_density(aes(x=diff_ln_AVE_FE_mean), fill="blue", alpha=0.15) +
  geom_density(aes(x=diff_ln_AVE_FE_lo), fill="green", alpha=0.15) +
  xlab("Mean FE estimate difference (2015 base)") +
  ggtitle("Density of Mean FE estimate differences")


# windsorize:
# drop first and 99th percentile
q0.01 <- quantile(US_agg_FE_sign$diff_ln_AVE_FE_mean, 0.01, na.rm=TRUE)
q0.99 <- quantile(US_agg_FE_sign$diff_ln_AVE_FE_mean, 0.99, na.rm=TRUE)
q0.01
q0.99


US_agg_FE_sign_windsor <- US_agg_FE_sign %>% filter(diff_ln_AVE_FE_mean >= q0.01 &
                                                      diff_ln_AVE_FE_mean <= q0.99 )


# density of diff_ln_AVE_FE_mean
FE_plot <- ggplot(US_agg_FE_sign_windsor) +
  geom_density(aes(x=diff_ln_AVE_FE_mean), fill="blue", alpha=0.15) +
  geom_density(aes(x=diff_ln_AVE_FE_lo), fill="green", alpha=0.15) +
  xlab("Mean FE estimate difference (2015 base)") +
  ggtitle("Density of Mean FE estimate differences")
FE_plot


FE_plot <- ggplot(US_agg_FE_sign_windsor) +
  geom_density( aes(x = diff_ln_AVE_FE_mean, fill = "FE"), alpha = 0.15  ) +
  geom_density(
    aes(x = diff_ln_AVE_FE_lo, fill = "Lower-bound FE"),
    alpha = 0.15
  ) +
  scale_fill_manual(
    name = "Estimate type",
    values = c(
      "FE" = "blue",
      "Lower-bound FE" = "green"
    )
  ) +
  xlab("Mean FE estimate difference (2015 base)") +
  ggtitle("Density of Mean FE estimate differences")

FE_plot


###############################################################################
# for significant benchmarked model 
###############################################################################

# significant 
US_agg_FE_bench_sign <- US_agg %>% filter(sig_bench == TRUE)
summary(US_agg_FE_bench_sign$diff_ln_AVE_FE_bench_mean)
summary(US_agg_FE_bench_sign$diff_ln_AVE_FE_bench_median)

# density of diff_ln_AVE_FE_mean
ggplot(US_agg) +
  geom_density(aes(x=diff_ln_AVE_FE_mean), fill="blue", alpha=0.15) +
  geom_density(aes(x=diff_ln_AVE_FE_median), fill="green", alpha=0.15) +
  xlab("Mean FE estimate difference (2015 base)") +
  ggtitle("Density of Mean FE estimate differences")



# windsorize:
# drop first and 99th percentile
q0.01 <- quantile(US_agg_FE_bench_sign$diff_ln_AVE_FE_mean, 0.01, na.rm=TRUE)
q0.99 <- quantile(US_agg_FE_bench_sign$diff_ln_AVE_FE_mean, 0.99, na.rm=TRUE)
q0.01
q0.99


US_agg_FE_bench_sign_windsor <- US_agg_FE_bench_sign %>% filter(diff_ln_AVE_FE_mean >= q0.01 &
                                                                  diff_ln_AVE_FE_mean <= q0.99 )


# density of diff_ln_AVE_FE_mean
bench_plot <- ggplot(US_agg_FE_bench_sign_windsor) +
  geom_density(aes(x=diff_ln_AVE_FE_mean), fill="blue", alpha=0.15) +
  # geom_density(aes(x=diff_ln_AVE_FE_median), fill="green", alpha=0.15) +
  xlab("Mean FE estimate difference (2015 base)") +
  ggtitle("Density of Mean FE estimate differences")
bench_plot


library(patchwork)

FE_plot + bench_plot

# save significant 
FE_sig_windsor_plot <- ggplot() +
  geom_density(data = US_agg_FE_sign_windsor, aes(x = diff_ln_AVE_FE_mean, fill = "FE"), alpha = 0.15  ) +
  geom_density(data = US_agg_FE_bench_sign_windsor, aes(x = diff_ln_AVE_FE_mean, fill = "Benchmarked FE"),alpha = 0.15  ) +
  scale_fill_manual(    name = "Estimate type", values = c( "FE" = "blue","Benchmarked FE" = "green"  )  ) +
  xlab("Mean significant windsorized FE estimate difference (2015 base)") +
  ggtitle("Density of Mean FE estimate differences") +
  theme_trade

FE_sig_windsor_plot

ggsave(filename = paste0(exp, "plot/distribution/FE_sig_windsor_plot.png"),
       plot = FE_sig_windsor_plot, width = 8,height = 6,  dpi = 300)






