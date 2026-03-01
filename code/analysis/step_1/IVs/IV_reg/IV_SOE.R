
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
library(ggplot2)

################################################################################
# directory: 
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git")
dir_dta <-  "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/Compare_values/yearly/robust"


################################################################################
# 1) Load data 
################################################################################

# Load FE estimates 
trade_dta <- read_csv(paste0(dir_dta, "/US_ln_NTMs_base_2015.csv"))

dta_bis <- read_csv(paste0(dir_dta, "/US_ln_NTMs_base_2015_FE_bench_windsorised.csv"))
dta <- read_csv(paste0(dir_dta, "/US_ln_NTMs_base__2015_2017_hs4_FE_boot.csv"))

table(dta$year)  
colSums(is.na(dta))
unique(dta$year)
names(dta)

# Load SOE data
SOE <- read_csv("data/SOE_dta/SOE_share_2010.csv")


################################################################################
# filter year 
# dta <- dta %>% filter(year %in% c(2018,2019))


names(SOE)
SOE <- SOE %>% select(-Year)
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
# aggregate tariff data at hs4 level to include in the regression
################################################################################

# choose baseline year for trade weights 

# keep 2015 trade values to  aggregate tariff data at hs4 level 
trade_2015_weights <- trade_dta %>%  filter(year == 2015) %>% 
  select(hs2, hs4, hs6_H5, Trade_value_USD) %>%  group_by(hs4) %>%
  mutate( tot_Trade_value_USD_2015 = sum(Trade_value_USD, na.rm = TRUE),
    weight_hs4 = if_else(tot_Trade_value_USD_2015 == 0, 0, Trade_value_USD / tot_Trade_value_USD_2015)  ) %>%  ungroup()


tariff <- trade_dta %>% select(year,hs2, hs4, hs6_H5,log_tariff, diff_log_tariff_2015) %>% 
  filter(year %in% c(2018, 2019))
# merge back with trade data 
tariff <- left_join(tariff, trade_2015_weights)
colSums(is.na(tariff))
tariff <- tariff %>% mutate(across(c(Trade_value_USD , tot_Trade_value_USD_2015, weight_hs4), ~ coalesce(.x, 0) ))
# calculate weighted tariffs 
tariff <- tariff %>%  group_by(year, hs2, hs4) %>%
  summarise(log_tariff_weighted = weighted.mean(log_tariff, w = weight_hs4, na.rm = TRUE),
            diff_log_tariff_2015_weighted = weighted.mean(diff_log_tariff_2015, w = weight_hs4, na.rm = TRUE),
            .groups = "drop"  ) 



################################################################################

# keep 2018 to 2019 for change in NTMs
# dta <- dta %>% filter(year %in% c(2018, 2019))

# include product codes at HS sectiona nd sector level

dta$hs2 <- as.numeric(dta$hs2)
dta <- dta %>% mutate(
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
names(dta)

# merge with tariff data 
dta <- left_join(dta, tariff)
colSums(is.na(dta))
test <- dta %>% filter(is.na(dta$diff_log_tariff_2015_weighted ))

# put 0s for NA in change in tariff (merging creates NAs)
dta <- dta %>% mutate(diff_log_tariff_2015_weighted  = coalesce(diff_log_tariff_2015_weighted, 0))
  
  
names(dta)
################################################################################
# aggregate at HS 4 level 
################################################################################

# For SOE data

# get product level 
SOE <- SOE %>% mutate(hs4 =  str_sub(hs6_H5, 1,4), hs2 =  str_sub(hs6_H5, 1,2))

# for SOE:
SOE_hs4 <- SOE %>% group_by(hs4) %>%
  summarise(Trade_value_USD_SOE = sum(Trade_value_USD_SOE, na.rm=TRUE),
            tot_Trade_value_USD = sum(tot_Trade_value_USD, na.rm=TRUE)  ) %>%
  ungroup()
SOE_hs4 <- SOE_hs4 %>% mutate(share_value_SOE = Trade_value_USD_SOE / tot_Trade_value_USD ) %>%
  select(hs4, share_value_SOE)

test <- SOE_hs4 %>% filter(share_value_SOE ==0)

plot <- ggplot(SOE_hs4)+
  geom_histogram(aes(x=share_value_SOE), bins = 30, color = "black" ,fill = "blue")+
  labs(title = "Histogram of the import share by SOE across HS4 industries \n for Chinese imports from the US in 2010",
       x = "SOE share",    y = "Count"  ) +
  theme_trade
plot

ggsave(filename = "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/summary/SOE_dist/SOE_share_hs4_distribution.png",
       plot = plot, width = 9,height = 6, units = "in", dpi = 300,bg = "white")



################################################################################

# FOR AVE data 

################################################################################

# merge with SOE data 

class(dta$hs4)
class(SOE_hs4$hs4)

merged_hs4 <- full_join(dta, SOE_hs4)
colSums(is.na(merged_hs4))
# drop NA
# merged_hs4 <- merged_hs4 %>% filter(!is.na(share_value_SOE) & !is.na(diff_ln_AVE_FE_mean) & !is.na(diff_ln_AVE_FE_bench_mean) )

merged_hs4_all <- merged_hs4
merged_hs4 <- merged_hs4 %>% filter(year %in% c(2018:2019))

################################################################################
# PLot 
################################################################################

names(merged_hs4)

# plot 

ggplot(merged_hs4,aes(x = share_value_SOE, y = diff_ln_AVE_FE_wmean_mean_w)) +
  geom_point( alpha = 0.6) +
  facet_wrap(~ baseline_year) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "Change in AVE vs. SOE share",  x = "SOE share in 2010",  y = "Change in AVE (FE demean approach)"  ) +
  theme_minimal()

ggplot(merged_hs4,aes(x = share_value_SOE, y = diff_ln_AVE_FE_mean_w)) +
  geom_point( alpha = 0.6) +
  facet_wrap(~ baseline_year) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "Change in AVE vs. SOE share",  x = "SOE share in 2010",  y = "Change in AVE (FE approach)"  ) +
  theme_minimal()

ggplot(merged_hs4,aes(x = share_value_SOE, y = diff_ln_AVE_FE_bench_mean_w)) +
  geom_point( alpha = 0.6) +
  facet_wrap(~ baseline_year) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "Change in AVE vs. SOE share",  x = "SOE share in 2010",  y = "Change in AVE (FE bench approach)"  ) +
  theme_minimal()




################################################################################

# Model 1 – full sample
library(fixest)
length(unique(merged_hs4$hs4))
names(merged_hs4)
unique(merged_hs4$sector)
test <- merged_hs4 %>% filter(sector == "Other")

# Full sample
FE_2015 <- feols(diff_ln_AVE_FE_mean_w ~ share_value_SOE + diff_log_tariff_2015_weighted,
  data = subset(merged_hs4, baseline_year == 2015))

FE_bench_2015 <- feols(diff_ln_AVE_FE_bench_mean_w ~ share_value_SOE + diff_log_tariff_2015_weighted,
  data = subset(merged_hs4, baseline_year == 2015))

FE_demean_2015 <- feols(diff_ln_AVE_FE_wmean_mean_w ~ share_value_SOE + diff_log_tariff_2015_weighted,
                data = subset(merged_hs4, baseline_year == 2015))

# Table
etable(FE_2015, FE_bench_2015, FE_demean_2015,
       headers = c("2015 FE", "2015 FE bench","2015 FE demean"),
       digits = 4,
       fitstat = ~ n + r2 + ar2 + f + f.p + rmse)


FE_2017 <- feols(diff_ln_AVE_FE_mean_w ~ share_value_SOE + diff_log_tariff_2015_weighted,
                 data = subset(merged_hs4, baseline_year == 2017))

FE_bench_2017 <- feols(diff_ln_AVE_FE_bench_mean_w ~ share_value_SOE + diff_log_tariff_2015_weighted,
                       data = subset(merged_hs4, baseline_year == 2017))

FE_demean_2017 <- feols(diff_ln_AVE_FE_wmean_mean_w ~ share_value_SOE + diff_log_tariff_2015_weighted,
                        data = subset(merged_hs4, baseline_year == 2017))

# Table
etable(FE_2017, FE_bench_2017, FE_demean_2017,
       headers = c("2017 FE", "2017 FE bench","2017 FE demean"),
       digits = 4,
       fitstat = ~ n + r2 + ar2 + f + f.p + rmse)


###############################################################################

# significant sample 

FE_2015_sig <- feols(diff_ln_AVE_FE_mean_w ~ share_value_SOE + diff_log_tariff_2015_weighted,
                 data = subset(merged_hs4, baseline_year == 2015 & diff_ln_AVE_FE_sig == TRUE ))

FE_bench_2015_sig <- feols(diff_ln_AVE_FE_bench_mean_w ~ share_value_SOE + diff_log_tariff_2015_weighted,
                       data = subset(merged_hs4, baseline_year == 2015 & diff_ln_AVE_FE_sig == TRUE))

FE_demean_2015_sig <- feols(diff_ln_AVE_FE_wmean_mean_w ~ share_value_SOE + diff_log_tariff_2015_weighted,
                        data = subset(merged_hs4, baseline_year == 2015 & diff_ln_AVE_FE_sig == TRUE))

# Table
etable(FE_2015_sig, FE_bench_2015_sig, FE_demean_2015_sig,
       headers = c("2015 FE sig", "2015 FE bench sig","2015 FE demean sig"),
       digits = 4,
       fitstat = ~ n + r2 + ar2 + f + f.p + rmse)


FE_2017_sig <- feols(diff_ln_AVE_FE_mean_w ~ share_value_SOE + diff_log_tariff_2015_weighted,
                 data = subset(merged_hs4, baseline_year == 2017 & diff_ln_AVE_FE_sig == TRUE))

FE_bench_2017_sig <- feols(diff_ln_AVE_FE_bench_mean_w ~ share_value_SOE + diff_log_tariff_2015_weighted,
                       data = subset(merged_hs4, baseline_year == 2017 & diff_ln_AVE_FE_sig == TRUE))

FE_demean_2017_sig <- feols(diff_ln_AVE_FE_wmean_mean_w ~ share_value_SOE + diff_log_tariff_2015_weighted,
                        data = subset(merged_hs4, baseline_year == 2017 & diff_ln_AVE_FE_sig == TRUE))

# Table
etable(FE_2017_sig, FE_bench_2017_sig, FE_demean_2017_sig,
       headers = c("2017 FE sig", "2017 FE bench sig","2017 FE demean sig"),
       digits = 4,
       fitstat = ~ n + r2 + ar2 + f + f.p + rmse)


##################################################################################
# with interaction


# Full sample
FE_2015 <- feols(diff_ln_AVE_FE_mean_w ~ sector*share_value_SOE + sector*diff_log_tariff_2015_weighted,
                 data = subset(merged_hs4, baseline_year == 2015))

FE_bench_2015 <- feols(diff_ln_AVE_FE_bench_mean_w ~ sector*share_value_SOE + sector*diff_log_tariff_2015_weighted,
                       data = subset(merged_hs4, baseline_year == 2015))

FE_demean_2015 <- feols(diff_ln_AVE_FE_wmean_mean_w ~ sector*share_value_SOE + sector*diff_log_tariff_2015_weighted,
                        data = subset(merged_hs4, baseline_year == 2015))

# for 2017 
FE_2017 <- feols(diff_ln_AVE_FE_mean_w ~ sector*share_value_SOE + sector*diff_log_tariff_2015_weighted,
                 data = subset(merged_hs4, baseline_year == 2017))

FE_bench_2017 <- feols(diff_ln_AVE_FE_bench_mean_w ~ sector*share_value_SOE + sector*diff_log_tariff_2015_weighted,
                       data = subset(merged_hs4, baseline_year == 2017))

FE_demean_2017 <- feols(diff_ln_AVE_FE_wmean_mean_w ~ sector*share_value_SOE + sector*diff_log_tariff_2015_weighted,
                        data = subset(merged_hs4, baseline_year == 2017))

# Table
# Table
etable(FE_2015, FE_bench_2015, FE_demean_2015,
       headers = c("2015 FE", "2015 FE bench","2015 FE demean"),
       digits = 4,
       fitstat = ~ n + r2 + ar2 + f + f.p + rmse)

etable(FE_2017, FE_bench_2017, FE_demean_2017,
       headers = c("2017 FE", "2017 FE bench","2017 FE demean"),
       digits = 4,
       fitstat = ~ n + r2 + ar2 + f + f.p + rmse)



##################################################################################
# pretrend checks 
##################################################################################
names(merged_hs4_all)
table(merged_hs4_all$year)

# Full sample
FE_2015 <- feols(diff_ln_AVE_FE_mean_w ~ as.factor(year)*share_value_SOE + diff_log_tariff_2015_weighted,
                 data = subset(merged_hs4_all, baseline_year == 2015))


FE_bench_2015 <- feols(diff_ln_AVE_FE_bench_mean_w ~ as.factor(year)*share_value_SOE + diff_log_tariff_2015_weighted,
                       data = subset(merged_hs4_all, baseline_year == 2015))

FE_demean_2015 <- feols(diff_ln_AVE_FE_wmean_mean_w ~ as.factor(year)*share_value_SOE + diff_log_tariff_2015_weighted,
                        data = subset(merged_hs4_all, baseline_year == 2015))

# Table
etable(FE_2015, FE_bench_2015, FE_demean_2015,
       headers = c("2015 FE", "2015 FE bench","2015 FE demean"),
       digits = 4,
       fitstat = ~ n + r2 + ar2 + f + f.p + rmse)

##################################################################################
# Checking relation to tariffs 
##################################################################################
names(merged_hs4)

# for 2017 
FE_2017 <- feols(diff_ln_AVE_FE_mean_w ~  diff_log_tariff_2015_weighted,
                 data = subset(merged_hs4, baseline_year == 2017))

FE_bench_2017 <- feols(diff_ln_AVE_FE_bench_mean_w ~ diff_log_tariff_2015_weighted,
                       data = subset(merged_hs4, baseline_year == 2017))

FE_demean_2017 <- feols(diff_ln_AVE_FE_wmean_mean_w ~ diff_log_tariff_2015_weighted,
                        data = subset(merged_hs4, baseline_year == 2017))

FE_log_2017 <- feols(diff_ln_AVE_FE_log_mean_w ~  diff_log_tariff_2015_weighted,
                 data = subset(merged_hs4, baseline_year == 2017))

FE_bench_log_2017 <- feols(diff_ln_AVE_FE_log_bench_mean_w ~ diff_log_tariff_2015_weighted,
                       data = subset(merged_hs4, baseline_year == 2017))

FE_demean_log_2017 <- feols(diff_ln_AVE_FE_log_wmean_mean_w ~ diff_log_tariff_2015_weighted,
                        data = subset(merged_hs4, baseline_year == 2017))




etable(FE_2017, FE_bench_2017, FE_demean_2017,
       headers = c("2017 FE", "2017 FE bench","2017 FE demean"),
       digits = 4,
       fitstat = ~ n + r2 + ar2 + f + f.p + rmse)

etable(FE_log_2017, FE_bench_log_2017, FE_demean_log_2017,
       headers = c("2017 FE", "2017 FE bench","2017 FE demean"),
       digits = 4,
       fitstat = ~ n + r2 + ar2 + f + f.p + rmse)







