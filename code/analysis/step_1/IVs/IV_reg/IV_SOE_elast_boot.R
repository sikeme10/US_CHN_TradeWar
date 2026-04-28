
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
dir_dta <-  "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/Compare_values/yearly/robust/elast/"

exp <-  "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/IVs/regression/elast/"

################################################################################
# 1) Load data 
################################################################################

# Load FE estimates 
tariff_dta <- read_csv(paste0("/data/sikeme/TRADE/US_CHN_TradeWar_git/output/Compare_values/yearly/robust/US_ln_NTMs_base_2015_FE_boot_hs4.csv"))

# NTM data 
dta <- read_csv(paste0(dir_dta, "/US_ln_NTMs_base_2015_2017_hs4_FE_boot.csv"))

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
################################################################################
# aggregate tariff data at hs4 level to include in the regression
################################################################################


names(tariff_dta)
colSums(is.na(tariff_dta))
test <- tariff_dta %>% filter(is.na(diff_log_tariff_2015))
table(test$year)
tariff_dta1 <- tariff_dta %>%  filter(year %in% c(2016:2020) & draw == 1 ) %>% 
  select(year, hs2, hs4, diff_log_tariff_2015, diff_log_tariff_2017)

test <- tariff_dta1 %>% filter(year == 2020)
summary(test)
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
table(dta$sector)

# merge with tariff data 
tariff_dta1$hs4 <- as.numeric(tariff_dta1$hs4)
dta <- left_join(dta, tariff_dta1)
colSums(is.na(dta))
summary(dta)
test <- dta %>% filter(is.na(dta$diff_log_tariff_2015 ))

# put 0s for NA in change in tariff (merging creates NAs)
dta <- dta %>% mutate(diff_log_tariff_2015  = coalesce(diff_log_tariff_2015, 0))
  
  
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


################################################################################

# FOR AVE data 

################################################################################

# merge with SOE data 

class(dta$hs4)
class(SOE_hs4$hs4)
SOE_hs4$hs4 <- as.numeric(SOE_hs4$hs4)
merged_hs4 <- full_join(dta, SOE_hs4)
colSums(is.na(merged_hs4))
# drop NA
# merged_hs4 <- merged_hs4 %>% filter(!is.na(share_value_SOE) & !is.na(diff_ln_AVE_FE_mean) & !is.na(diff_ln_AVE_FE_bench_mean) )

merged_hs4_all <- merged_hs4
colSums(is.na(merged_hs4_all))
merged_hs4 <- merged_hs4 %>% filter(year %in% c(2018:2019))

################################################################################
# PLot 
################################################################################

names(merged_hs4)
unique(merged_hs4$year)
# plot 

ggplot(merged_hs4,aes(x = share_value_SOE, y = diff_ln_AVE_FE_wmean_w_mean)) +
  geom_point( alpha = 0.6) +
  facet_wrap(~ baseline_year) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "Change in AVE vs. SOE share",  x = "SOE share in 2010",  y = "Change in AVE (FE demean approach)"  ) +
  theme_minimal()

ggplot(merged_hs4,aes(x = share_value_SOE, y = diff_ln_AVE_FE_w_mean)) +
  geom_point( alpha = 0.6) +
  facet_wrap(~ baseline_year) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "Change in AVE vs. SOE share",  x = "SOE share in 2010",  y = "Change in AVE (FE approach)"  ) +
  theme_minimal()

ggplot(merged_hs4,aes(x = share_value_SOE, y = diff_ln_AVE_FE_bench_w_mean)) +
  geom_point( alpha = 0.6) +
  facet_wrap(~ baseline_year) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "Change in AVE vs. SOE share",  x = "SOE share in 2010",  y = "Change in AVE (FE bench approach)"  ) +
  theme_minimal()


ggplot(subset(merged_hs4, sector == "Ag"), 
       aes(x = share_value_SOE, y = diff_ln_AVE_FE_bench_w_mean, color = factor(hs_section))) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ baseline_year) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(    title  = "Change in AVE vs. SOE share",
    x      = "SOE share in 2010",
    y      = "Change in AVE (FE bench approach)",
    color  = "HS2 sector"  ) +
  theme_minimal()


ggplot(subset(merged_hs4, sector == "Manu"), 
       aes(x = share_value_SOE, y = diff_ln_AVE_FE_bench_w_mean, color = factor(hs_section))) +
  geom_point(alpha = 0.6) +
  facet_wrap(~ baseline_year) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(    title  = "Change in AVE vs. SOE share",
           x      = "SOE share in 2010",
           y      = "Change in AVE (FE bench approach)",
           color  = "HS2 sector"  ) +
  theme_minimal()


################################################################################

# Model 1 – full sample
library(fixest)
length(unique(merged_hs4$hs4))
names(merged_hs4)
table(merged_hs4$sector)
test <- merged_hs4 %>% filter(sector == "Other")

# Full sample
FE_2015 <- feols(diff_ln_AVE_FE_w_mean ~ share_value_SOE + diff_log_tariff_2015,
  data = subset(merged_hs4, baseline_year == 2015))

FE_bench_2015 <- feols(diff_ln_AVE_FE_bench_w_mean ~ share_value_SOE + diff_log_tariff_2015,
  data = subset(merged_hs4, baseline_year == 2015))

FE_demean_2015 <- feols(diff_ln_AVE_FE_wmean_w_mean ~ share_value_SOE + diff_log_tariff_2015,
                data = subset(merged_hs4, baseline_year == 2015))

# Table
etable(FE_2015, FE_bench_2015, FE_demean_2015,
       headers = c("2015 FE", "2015 FE bench","2015 FE demean"),
       digits = 4,
       fitstat = ~ n + r2 + ar2 + f + f.p + rmse)


FE_2017 <- feols(diff_ln_AVE_FE_w_mean ~ share_value_SOE + diff_log_tariff_2015,
                 data = subset(merged_hs4, baseline_year == 2017))

FE_bench_2017 <- feols(diff_ln_AVE_FE_bench_w_mean ~ share_value_SOE + diff_log_tariff_2015,
                       data = subset(merged_hs4, baseline_year == 2017))

FE_demean_2017 <- feols(diff_ln_AVE_FE_wmean_w_mean ~ share_value_SOE + diff_log_tariff_2015,
                        data = subset(merged_hs4, baseline_year == 2017))



# Table
tbl <- etable(FE_2017, FE_bench_2017, FE_demean_2017,
       headers = c("2017 FE", "2017 FE bench","2017 FE demean"),
       digits = 4,
       fitstat = ~ n + r2 + ar2 + f + f.p + rmse)
tbl
# Then write to CSV (easy to paste into Excel)
write.csv(tbl, paste0(exp, "SOE_NTM_2017.csv"), row.names = FALSE)


FE_2017 <- feols(diff_ln_AVE_FE_w_mean ~ share_value_SOE,
                 data = subset(merged_hs4, baseline_year == 2017))

FE_bench_2017 <- feols(diff_ln_AVE_FE_bench_w_mean ~ share_value_SOE ,
                       data = subset(merged_hs4, baseline_year == 2017))

FE_demean_2017 <- feols(diff_ln_AVE_FE_wmean_w_mean ~ share_value_SOE ,
                        data = subset(merged_hs4, baseline_year == 2017))



# Table
tbl <- etable(FE_2017, FE_bench_2017, FE_demean_2017,
              headers = c("2017 FE", "2017 FE bench","2017 FE demean"),
              digits = 4,
              fitstat = ~ n + r2 + ar2 + f + f.p + rmse)
tbl
# Then write to CSV (easy to paste into Excel)
write.csv(tbl, paste0(exp, "SOE_NTM_2017.csv"), row.names = FALSE)



##############################################################################

AVE <- feols(diff_ln_AVE_FE_wmean_w_mean ~ share_value_SOE,
                  data = subset(merged_hs4, baseline_year == 2017 & year %in% c(2018:2019)))
AVE_2017 <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year) + share_value_SOE,
                  data = subset(merged_hs4, baseline_year == 2017 & year %in% c(2018:2019)))
AVE_2017_tar <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year) + diff_log_tariff_2017 + share_value_SOE,
                      data = subset(merged_hs4, baseline_year == 2017 & year %in% c(2018:2019)))
AVE_2017_sector <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year) + as.factor(sector) +
                           diff_log_tariff_2017 + share_value_SOE,
                         data = subset(merged_hs4, baseline_year == 2017 & year %in% c(2018:2019)))
AVE_2017_sector_interact <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year) +
                                    diff_log_tariff_2017 + as.factor(sector) * share_value_SOE,
                                  data = subset(merged_hs4, baseline_year == 2017 & year %in% c(2018:2019)))

# Table
tbl <- etable(AVE, AVE_2017, AVE_2017_tar, AVE_2017_sector, AVE_2017_sector_interact,
              headers = c("AVE", "AVE FE", "with tariff", "sector", "interact"),
              digits = 4,
              fitstat = ~ n + r2 + ar2 + f + f.p + rmse)
tbl
write.csv(tbl, paste0(exp, "SOE_NTM_2017.csv"), row.names = FALSE)


# with cluster
AVE <- feols(diff_ln_AVE_FE_wmean_w_mean ~ share_value_SOE,
             data = subset(merged_hs4, baseline_year == 2017 & year %in% c(2018:2019)), cluster = ~hs4)
AVE_2017 <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year) + share_value_SOE,
                  data = subset(merged_hs4, baseline_year == 2017 & year %in% c(2018:2019)), cluster = ~hs4)
AVE_2017_tar <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year) + diff_log_tariff_2017 + share_value_SOE,
                      data = subset(merged_hs4, baseline_year == 2017 & year %in% c(2018:2019)), cluster = ~hs4)
AVE_2017_sector <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year) + as.factor(sector) +
                           diff_log_tariff_2017 + share_value_SOE,
                         data = subset(merged_hs4, baseline_year == 2017 & year %in% c(2018:2019)), cluster = ~hs4)
AVE_2017_sector_interact <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year) +
                                    diff_log_tariff_2017 + as.factor(sector) * share_value_SOE,
                                  data = subset(merged_hs4, baseline_year == 2017 & year %in% c(2018:2019)), cluster = ~hs4)
# Table
tbl <- etable(AVE, AVE_2017, AVE_2017_tar, AVE_2017_sector, AVE_2017_sector_interact,
              headers = c("AVE", "AVE FE", "with tariff", "sector", "interact"),
              digits = 4,
              fitstat = ~ n + r2 + ar2 + f + f.p + rmse)
tbl
write.csv(tbl, paste0(exp, "SOE_NTM_2017_cluster.csv"), row.names = FALSE)


AVE <- feols(diff_ln_AVE_FE_wmean_w_mean ~ share_value_SOE,
             data = subset(merged_hs4, baseline_year == 2017 & year %in% c(2018:2019)), cluster = ~hs4)
AVE_2017 <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year) + share_value_SOE,
                  data = subset(merged_hs4, baseline_year == 2017 & year %in% c(2018:2019)), cluster = ~hs4)
AVE_2017_tar <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year) + diff_log_tariff_2017 + share_value_SOE,
                      data = subset(merged_hs4, baseline_year == 2017 & year %in% c(2018:2019)), cluster = ~hs4)
AVE_2017_sector <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year) + as.factor(sector) +
                           diff_log_tariff_2017 + share_value_SOE,
                         data = subset(merged_hs4, baseline_year == 2017 & year %in% c(2018:2019)), cluster = ~hs4)
AVE_2017_sector_interact <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year) +
                                    diff_log_tariff_2017 + as.factor(sector) * share_value_SOE,
                                  data = subset(merged_hs4, baseline_year == 2017 & year %in% c(2018:2019)), cluster = ~hs4)
# Table
tbl <- etable(AVE, AVE_2017, AVE_2017_tar, AVE_2017_sector, AVE_2017_sector_interact,
              headers = c("AVE", "AVE FE", "with tariff", "sector", "interact"),
              digits = 4,
              fitstat = ~ n + r2 + ar2 + f + f.p + rmse)
tbl




# with windsorized data
AVE <- feols(diff_ln_AVE_FE_wmean_mean ~ share_value_SOE,
             data = subset(merged_hs4, baseline_year == 2017 & year %in% c(2018:2019)))
AVE_2017 <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year) + share_value_SOE,
                  data = subset(merged_hs4, baseline_year == 2017 & year %in% c(2018:2019)))
AVE_2017_tar <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year) + diff_log_tariff_2017 + share_value_SOE,
                      data = subset(merged_hs4, baseline_year == 2017 & year %in% c(2018:2019)))
AVE_2017_sector <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year) + as.factor(sector) +
                           diff_log_tariff_2017 + share_value_SOE,
                         data = subset(merged_hs4, baseline_year == 2017 & year %in% c(2018:2019)))
AVE_2017_sector_interact <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year) +
                                    diff_log_tariff_2017 + as.factor(sector) * share_value_SOE,
                                  data = subset(merged_hs4, baseline_year == 2017 & year %in% c(2018:2019)))

# Table
tbl_w <- etable(AVE, AVE_2017, AVE_2017_tar, AVE_2017_sector, AVE_2017_sector_interact,
              headers = c("AVE",  "AVE FE", "with tariff", "sector", "interact"),
              digits = 4,
              fitstat = ~ n + r2 + ar2 + f + f.p + rmse)
tbl_w
# Then write to CSV (easy to paste into Excel)
write.csv(tbl_w, paste0(exp, "SOE_NTM_2017_w.csv"), row.names = FALSE)




################################################################################
# get predicted vale to construct IVS:
################################################################################
{
# Fit the model
AVE_2017_2017_sector_interact <- feols(diff_ln_AVE_FE_wmean_w_mean ~ diff_log_tariff_2015 + 
                                         as.factor(year) + as.factor(sector) +  share_value_SOE,
                                       data = subset(merged_hs4, baseline_year == 2017 & year %in% c(2018:2019)))

# Create the subset used in estimation
df_model <- subset(merged_hs4, baseline_year == 2017 & year %in% c(2018:2019))

# Add a row index before dealing with NAs
df_model$row_id <- seq_len(nrow(df_model))

# Identify non-NA rows across all variables used in the model
used_rows <- which(complete.cases(df_model[, c("diff_ln_AVE_FE_wmean_w_mean", 
                                               "diff_log_tariff_2015", 
                                               "year", 
                                               "sector", 
                                               "share_value_SOE")]))

# Assign predictions to matching rows, NA elsewhere
df_model$predicted_diff_ln_AVE_FE_wmean <- NA
df_model$predicted_diff_ln_AVE_FE_wmean[used_rows] <- predict(AVE_2017_2017_sector_interact)

# Diagnostics
summary(df_model$predicted_diff_ln_AVE_FE_wmean)
summary(df_model$diff_ln_AVE_FE_wmean_w_mean)

colSums(is.na(df_model))

plot <- ggplot(df_model) +
  geom_density(aes(x = diff_ln_AVE_FE_wmean_w_mean, fill = "Actual"), alpha = 0.3) +
  geom_density(aes(x = predicted_diff_ln_AVE_FE_wmean, fill = "Predicted"), alpha = 0.3) +
  scale_fill_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  scale_x_continuous(limits = c(-5, 10)) +
  labs(title = "Distribution of Actual vs. Predicted NTM AVE Changes",
       x = expression(Delta ~ ln(1 + AVE)), y = "Density", fill = NULL) +
  theme_trade
plot
ggsave(filename = "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/IVs/summary/SOE_IV_AVE_predicted_distribution_w.png",
       plot = plot, width = 9,height = 6, units = "in", dpi = 300,bg = "white")


df_model <- df_model %>%  select(year, sector, hs_section, hs2, hs4,
                                 diff_ln_AVE_FE_wmean_w_mean, predicted_diff_ln_AVE_FE_wmean, diff_log_tariff_2017) %>% 
  rename(diff_ln_AVE_FE_wmean = diff_ln_AVE_FE_wmean_w_mean)
colSums(is.na(df_model))

write_csv(df_model, "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/IVs/predicted/SOE_IV_predicted_AVE_w.csv")

########################## windsorized

# Fit the model
AVE_2017_2017_sector_interact <- feols(diff_ln_AVE_FE_wmean_mean ~  diff_log_tariff_2015 + 
                                         as.factor(year) + as.factor(sector) +share_value_SOE,
                                       data = subset(merged_hs4, baseline_year == 2017 & year %in% c(2018:2019)))

# Create the subset used in estimation
df_model <- subset(merged_hs4, baseline_year == 2017 & year %in% c(2018:2019))

# Add a row index before dealing with NAs
df_model$row_id <- seq_len(nrow(df_model))

# Identify non-NA rows across all variables used in the model
used_rows <- which(complete.cases(df_model[, c("diff_ln_AVE_FE_wmean_mean", 
                                               "diff_log_tariff_2015", 
                                               "year", 
                                               "sector", 
                                               "share_value_SOE")]))

# Assign predictions to matching rows, NA elsewhere
df_model$predicted_diff_ln_AVE_FE_wmean <- NA
df_model$predicted_diff_ln_AVE_FE_wmean[used_rows] <- predict(AVE_2017_2017_sector_interact)

# Diagnostics
summary(df_model$predicted_diff_ln_AVE_FE_wmean)
summary(df_model$diff_ln_AVE_FE_wmean_mean)

colSums(is.na(df_model))

plot <- ggplot(df_model) +
  geom_density(aes(x = diff_ln_AVE_FE_wmean_w_mean, fill = "Actual"), alpha = 0.3) +
  geom_density(aes(x = predicted_diff_ln_AVE_FE_wmean, fill = "Predicted"), alpha = 0.3) +
  scale_fill_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  scale_x_continuous(limits = c(-5, 10)) +
  labs(title = "Distribution of Actual vs. Predicted NTM AVE Changes",
       x = expression(Delta ~ ln(1 + AVE)), y = "Density", fill = NULL) +
  theme_trade
plot
ggsave(filename = "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/IVs/summary/SOE_IV_AVE_predicted_distribution.png",
       plot = plot, width = 9,height = 6, units = "in", dpi = 300,bg = "white")



df_model <- df_model %>%  select(year, sector, hs_section, hs2, hs4,
                                 diff_ln_AVE_FE_wmean_mean, predicted_diff_ln_AVE_FE_wmean, diff_log_tariff_2017) %>% 
  rename(diff_ln_AVE_FE_wmean = diff_ln_AVE_FE_wmean_mean)
colSums(is.na(df_model))

write_csv(df_model, "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/IVs/predicted/SOE_IV_predicted_AVE.csv")


#################### without tariffs

# Fit the model
AVE_2017_2017_sector_interact <- feols(diff_ln_AVE_FE_wmean_mean ~  
                                         as.factor(year) + as.factor(sector) + share_value_SOE,
                                       data = subset(merged_hs4, baseline_year == 2017 & year %in% c(2018:2019)))

# Create the subset used in estimation
df_model <- subset(merged_hs4, baseline_year == 2017 & year %in% c(2018:2019))

# Add a row index before dealing with NAs
df_model$row_id <- seq_len(nrow(df_model))

# Identify non-NA rows across all variables used in the model
used_rows <- which(complete.cases(df_model[, c("diff_ln_AVE_FE_wmean_mean", 
                                               "diff_log_tariff_2015", 
                                               "year", 
                                               "sector", 
                                               "share_value_SOE")]))

# Assign predictions to matching rows, NA elsewhere
df_model$predicted_diff_ln_AVE_FE_wmean <- NA
df_model$predicted_diff_ln_AVE_FE_wmean[used_rows] <- predict(AVE_2017_2017_sector_interact)

# Diagnostics
summary(df_model$predicted_diff_ln_AVE_FE_wmean)
summary(df_model$diff_ln_AVE_FE_wmean_mean)

colSums(is.na(df_model))

plot <- ggplot(df_model) +
  geom_density(aes(x = diff_ln_AVE_FE_wmean_w_mean, fill = "Actual"), alpha = 0.3) +
  geom_density(aes(x = predicted_diff_ln_AVE_FE_wmean, fill = "Predicted"), alpha = 0.3) +
  scale_fill_manual(values = c("Actual" = "blue", "Predicted" = "red")) +
  scale_x_continuous(limits = c(-5, 10)) +
  labs(title = "Distribution of Actual vs. Predicted NTM AVE Changes",
       x = expression(Delta ~ ln(1 + AVE)), y = "Density", fill = NULL) +
  theme_trade
plot
ggsave(filename = "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/IVs/summary/SOE_IV_AVE_predicted_distribution_bis.png",
       plot = plot, width = 9,height = 6, units = "in", dpi = 300,bg = "white")


df_model <- df_model %>%  select(year, sector, hs_section, hs2, hs4,
                                 diff_ln_AVE_FE_wmean_mean, predicted_diff_ln_AVE_FE_wmean, diff_log_tariff_2017) %>% 
  rename(diff_ln_AVE_FE_wmean = diff_ln_AVE_FE_wmean_mean)
colSums(is.na(df_model))


write_csv(df_model, "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/IVs/predicted/SOE_IV_predicted_AVE_bis.csv")

}

##################################################################################
# pretrend checks SOE shares 
##################################################################################
names(merged_hs4_all)
table(merged_hs4_all$year)
merged_hs4_all <- merged_hs4_all %>% filter(year %in% c(2016:2019))

# Full sample
FE_2015_SOE_notar_demean <- feols(diff_ln_AVE_FE_wmean_mean ~ as.factor(year)*share_value_SOE,
                                  data = subset(merged_hs4_all, baseline_year == 2015))
FE_2015_SOE_tar_demean   <- feols(diff_ln_AVE_FE_wmean_mean ~ as.factor(year)*share_value_SOE + diff_log_tariff_2015,
                                  data = subset(merged_hs4_all, baseline_year == 2015))
FE_2015_SOE_tarX_demean  <- feols(diff_ln_AVE_FE_wmean_mean ~ as.factor(year)*share_value_SOE + diff_log_tariff_2015,
                                  data = subset(merged_hs4_all, baseline_year == 2015))

# Table
tbl <- etable(FE_2015_SOE_notar_demean, FE_2015_SOE_tar_demean, FE_2015_SOE_tarX_demean,
              headers = c("No Tar",   "Tar", "Tar×Year Demean"),
              digits = 4,
              fitstat = ~ n + r2 + ar2 + f + f.p + rmse)
tbl



# windsorized sample
FE_2015_SOE_notar        <- feols(diff_ln_AVE_FE_w_mean    ~ as.factor(year)*share_value_SOE,
                                  data = subset(merged_hs4_all, baseline_year == 2015))
FE_2015_SOE_notar_demean <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year)*share_value_SOE,
                                  data = subset(merged_hs4_all, baseline_year == 2015))
FE_2015_SOE_tar          <- feols(diff_ln_AVE_FE_w_mean    ~ as.factor(year)*share_value_SOE + diff_log_tariff_2015,
                                  data = subset(merged_hs4_all, baseline_year == 2015))
FE_2015_SOE_tar_demean   <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year)*share_value_SOE + diff_log_tariff_2015,
                                  data = subset(merged_hs4_all, baseline_year == 2015))
FE_2015_SOE_tarX         <- feols(diff_ln_AVE_FE_w_mean    ~ as.factor(year)*share_value_SOE + as.factor(year)*diff_log_tariff_2015,
                                  data = subset(merged_hs4_all, baseline_year == 2015))
FE_2015_SOE_tarX_demean  <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year)*share_value_SOE + as.factor(year)*diff_log_tariff_2015,
                                  data = subset(merged_hs4_all, baseline_year == 2015))

# Table
tbl_w <- etable(FE_2015_SOE_notar, FE_2015_SOE_notar_demean,
              FE_2015_SOE_tar,   FE_2015_SOE_tar_demean,
              FE_2015_SOE_tarX,  FE_2015_SOE_tarX_demean,
              headers = c("No Tar", "No Tar Demean",
                          "Tar",    "Tar Demean",
                          "Tar×Year", "Tar×Year Demean"),
              digits = 4,
              fitstat = ~ n + r2 + ar2 + f + f.p + rmse)
tbl_w
write_csv(as.data.frame(tbl_w), paste0(exp, "NTBs_SOE_pretrend.csv"))



# Full sample
FE_2015_SOE_notar_demean <- feols(diff_ln_AVE_FE_wmean_mean ~ as.factor(year)*share_value_SOE,
                                  data = subset(merged_hs4_all, baseline_year == 2015),  cluster = ~hs4)
FE_2015_SOE_tar_demean   <- feols(diff_ln_AVE_FE_wmean_mean ~ as.factor(year)*share_value_SOE + diff_log_tariff_2015,
                                  data = subset(merged_hs4_all, baseline_year == 2015),  cluster = ~hs4)

# Winsorized sample
FE_2015_SOE_notar        <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year)*share_value_SOE,
                                  data = subset(merged_hs4_all, baseline_year == 2015),  cluster = ~hs4)
FE_2015_SOE_tar_w      <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year)*share_value_SOE  + diff_log_tariff_2015,
                                  data = subset(merged_hs4_all, baseline_year == 2015),  cluster = ~hs4)


# Table
tbl <- etable(FE_2015_SOE_notar_demean, FE_2015_SOE_tar_demean,
              FE_2015_SOE_notar, FE_2015_SOE_tar_w,
              headers = c("No Tar", "Tar",  "W: No Tar", "W: Tar"), digits = 4,
              fitstat = ~ n + r2 + ar2 + f + f.p + rmse)
tbl
write_csv(as.data.frame(tbl), paste0(exp, "NTBs_SOE_pretrend.csv"))




# event study analysis 
names(merged_hs4_all)
unique(merged_hs4_all$year)
merged_hs4_all_2015 <- merged_hs4_all %>% filter(baseline_year == 2015 )
merged_hs4_all_2015 <- merged_hs4_all_2015 %>%  mutate(rel_year = year - 2017)

# Plot 1
iplot(es_model1, xlab     = "Years relative to 2016",
      ylab     = "Coefficient (effect of SOE import share)",
      main     = "Without tariff control",  col      = "steelblue",
      pt.join  = TRUE,    ci_level = 0.95)
abline(h = 0, lty = 2, col = "gray40")
p1 <- recordPlot()

# Plot 2
iplot(es_model2,
      xlab     = "Years relative to 2016",  ylab     = "Coefficient (effect of SOE import share)",
      main     = "With tariff control",   col      = "steelblue",    pt.join  = TRUE,
      ci_level = 0.95)
abline(h = 0, lty = 2, col = "gray40")
p2 <- recordPlot()
png("/data/sikeme/TRADE/US_CHN_TradeWar_git/output/IVs/summary/event_study_combined.png", width = 2400, height = 900, res = 200)
par(mfrow = c(1, 2))
replayPlot(p1)
replayPlot(p2)
dev.off()

###############################################################################
# looking at NTB-SOE-tariffs 
###############################################################################


SOE_2017 <- feols(diff_ln_AVE_FE_wmean_mean ~ as.factor(year) + share_value_SOE ,
                  data = subset(merged_hs4, baseline_year == 2017))
SOE_bis_2017 <- feols(diff_ln_AVE_FE_wmean_mean ~ as.factor(year) + share_value_SOE +diff_log_tariff_2017 ,
                  data = subset(merged_hs4, baseline_year == 2017))
tariff_2017 <- feols(diff_ln_AVE_FE_wmean_mean ~ as.factor(year) + diff_log_tariff_2017 ,
                     data = subset(merged_hs4, baseline_year == 2017))
SOE_tar_2017 <- feols(diff_log_tariff_2017 ~  as.factor(year) + share_value_SOE ,
                      data = subset(merged_hs4, baseline_year == 2017))
# Table
tbl <- etable(SOE_2017, SOE_bis_2017 , tariff_2017, SOE_tar_2017,
              headers = c("NTB-SOE", "NTB-SOE-tariff","NTB-tariff","tariff-SOE"),
              digits = 4,
              fitstat = ~ n + r2 + ar2 + f + f.p + rmse)
tbl
write_csv(as.data.frame(tbl), paste0(exp,"NTBs_SOE_tariff.csv"))


SOE_2017 <- feols(diff_ln_AVE_FE_wmean_mean ~ as.factor(year) + as.factor(year)*share_value_SOE ,
                  data = subset(merged_hs4, baseline_year == 2017))
SOE_bis_2017 <- feols(diff_ln_AVE_FE_wmean_mean ~ as.factor(year) + as.factor(year)*share_value_SOE + as.factor(year)*diff_log_tariff_2017 ,
                      data = subset(merged_hs4, baseline_year == 2017))
tariff_2017 <- feols(diff_ln_AVE_FE_wmean_mean ~ as.factor(year) + as.factor(year)*diff_log_tariff_2017 ,
                     data = subset(merged_hs4, baseline_year == 2017))
SOE_tar_2017 <- feols(diff_log_tariff_2017 ~  as.factor(year) + as.factor(year)*share_value_SOE ,
                      data = subset(merged_hs4, baseline_year == 2017))
# Table
tbl <- etable(SOE_2017, SOE_bis_2017 , tariff_2017, SOE_tar_2017,
              headers = c("NTB-SOE", "NTB-SOE-tariff","NTB-tariff","tariff-SOE"),
              digits = 4,
              fitstat = ~ n + r2 + ar2 + f + f.p + rmse)
tbl
write_csv(as.data.frame(tbl), paste0(exp,"NTBs_SOE_tariff.csv"))


SOE_2017 <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year) + share_value_SOE ,
                  data = subset(merged_hs4, baseline_year == 2017))
SOE_bis_2017 <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year) + share_value_SOE +diff_log_tariff_2017 ,
                      data = subset(merged_hs4, baseline_year == 2017))
tariff_2017 <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year) + diff_log_tariff_2017 ,
                     data = subset(merged_hs4, baseline_year == 2017))
SOE_tar_2017 <- feols(diff_log_tariff_2017 ~  as.factor(year) + share_value_SOE ,
                      data = subset(merged_hs4, baseline_year == 2017))
# Table
tbl <- etable(SOE_2017, SOE_bis_2017 , tariff_2017, SOE_tar_2017,
              headers = c("NTB-SOE", "NTB-SOE-tariff","NTB-tariff","tariff-SOE"),
              digits = 4,
              fitstat = ~ n + r2 + ar2 + f + f.p + rmse)
tbl
write_csv(as.data.frame(tbl), paste0(exp,"NTBs_SOE_tariff_w.csv"))















##################################################################################
# Checking relation to tariffs 
##################################################################################
names(merged_hs4)


# for 2015 
FE_2015 <- feols(diff_ln_AVE_FE_w_mean ~  as.factor(year)*diff_log_tariff_2015,
                 data = subset(merged_hs4_all, baseline_year == 2015))

FE_bench_2015 <- feols(diff_ln_AVE_FE_bench_w_mean ~ as.factor(year)*diff_log_tariff_2015,
                       data = subset(merged_hs4_all, baseline_year == 2015))

FE_demean_2015 <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year)*diff_log_tariff_2015,
                        data = subset(merged_hs4_all, baseline_year == 2015))

FE_log_2015 <- feols(diff_ln_AVE_FE_log_w_mean ~  as.factor(year)*diff_log_tariff_2015,
                 data = subset(merged_hs4_all, baseline_year == 2015))

FE_bench_log_2015 <- feols(diff_ln_AVE_FE_log_bench_w_mean ~ as.factor(year)*diff_log_tariff_2015,
                       data = subset(merged_hs4_all, baseline_year == 2015))

FE_demean_log_2015 <- feols(diff_ln_AVE_FE_log_wmean_w_mean ~ as.factor(year)*diff_log_tariff_2015,
                        data = subset(merged_hs4_all, baseline_year == 2015))


etable(FE_2015, FE_bench_2015, FE_demean_2015,
       headers = c("2015 FE", "2015 FE bench","2015 FE demean"),
       digits = 4,
       fitstat = ~ n + r2 + ar2 + f + f.p + rmse)

etable(FE_log_2015, FE_bench_log_2015, FE_demean_log_2015,
       headers = c("2015 FE", "2015 FE bench","2015 FE demean"),
       digits = 4,
       fitstat = ~ n + r2 + ar2 + f + f.p + rmse)



# for 2017 
FE_2017 <- feols(diff_ln_AVE_FE_w_mean ~ diff_log_tariff_2017,
                 data = subset(merged_hs4, baseline_year == 2017))

FE_bench_2017 <- feols(diff_ln_AVE_FE_bench_w_mean ~ diff_log_tariff_2017,
                       data = subset(merged_hs4, baseline_year == 2017))

FE_demean_2017 <- feols(diff_ln_AVE_FE_wmean_w_mean ~ diff_log_tariff_2017,
                        data = subset(merged_hs4, baseline_year == 2017))

FE_log_2017 <- feols(diff_ln_AVE_FE_log_w_mean ~  diff_log_tariff_2017,
                     data = subset(merged_hs4, baseline_year == 2017))

FE_bench_log_2017 <- feols(diff_ln_AVE_FE_log_bench_w_mean ~ diff_log_tariff_2017,
                           data = subset(merged_hs4, baseline_year == 2017))

FE_demean_log_2017 <- feols(diff_ln_AVE_FE_log_wmean_w_mean ~ diff_log_tariff_2017,
                            data = subset(merged_hs4, baseline_year == 2017))


tbl <- etable(FE_2017, FE_bench_2017, FE_demean_2017,
       FE_log_2017, FE_bench_log_2017, FE_demean_log_2017,
       headers = c("2017 FE", "2017 FE bench", "2017 FE demean",
                   "2017 FE log", "2017 FE bench log", "2017 FE demean log"),
       digits = 4,
       fitstat = ~ n + r2 + ar2 + f + f.p + rmse)
write_csv(as.data.frame(tbl), paste0(exp,"NTBs_tariff_specific.csv"))



################################################################################
# relationship: tariff Vs NTBs
################################################################################
FE_2015 <- feols(diff_ln_AVE_FE_wmean_w_mean ~ diff_log_tariff_2015,
                        data = subset(merged_hs4_all, baseline_year == 2015))

FE_2017 <- feols(diff_ln_AVE_FE_wmean_w_mean ~ diff_log_tariff_2017,
                        data = subset(merged_hs4, baseline_year == 2017))


FE_sector_2015 <- feols(diff_ln_AVE_FE_wmean_w_mean ~ sector*diff_log_tariff_2015,
                        data = subset(merged_hs4_all, baseline_year == 2015))

FE_sector_2017 <- feols(diff_ln_AVE_FE_wmean_w_mean ~ sector*diff_log_tariff_2017,
                        data = subset(merged_hs4, baseline_year == 2017))


FE_year_2015 <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year)*diff_log_tariff_2015,
                        data = subset(merged_hs4_all, baseline_year == 2015))

FE_year_2017 <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year)*diff_log_tariff_2017,
                        data = subset(merged_hs4, baseline_year == 2017))



tbl <- etable(FE_2015, FE_2017, FE_sector_2015, FE_sector_2017, FE_year_2015, FE_year_2017,
       headers = c("2015 Base", "2017 Base", 
                   "2015 Sector", "2017 Sector", 
                   "2015 Year", "2017 Year"),
       digits = 4,
       fitstat = ~ n + r2 + ar2 + f + f.p + rmse)
write_csv(as.data.frame(tbl), paste0(exp,"NTBs_tariff.csv"))



##############################################################################

# for 2015 
FE_2015 <- feols(diff_ln_AVE_FE_w_mean ~  as.factor(year)*diff_log_tariff_2015,
                 data = subset(merged_hs4_all, baseline_year == 2015))

FE_bench_2015 <- feols(diff_ln_AVE_FE_bench_w_mean ~ as.factor(year)*diff_log_tariff_2015,
                       data = subset(merged_hs4_all, baseline_year == 2015))

FE_demean_2015 <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year)*diff_log_tariff_2015,
                        data = subset(merged_hs4_all, baseline_year == 2015))

FE_log_2015 <- feols(diff_ln_AVE_FE_log_w_mean ~  as.factor(year)*diff_log_tariff_2015,
                     data = subset(merged_hs4_all, baseline_year == 2015))

FE_bench_log_2015 <- feols(diff_ln_AVE_FE_log_bench_w_mean ~ as.factor(year)*diff_log_tariff_2015,
                           data = subset(merged_hs4_all, baseline_year == 2015))

FE_demean_log_2015 <- feols(diff_ln_AVE_FE_log_wmean_w_mean ~ as.factor(year)*diff_log_tariff_2015,
                            data = subset(merged_hs4_all, baseline_year == 2015))


etable(FE_2015, FE_bench_2015, FE_demean_2015,
       headers = c("2015 FE", "2015 FE bench","2015 FE demean"),
       digits = 4,
       fitstat = ~ n + r2 + ar2 + f + f.p + rmse)

etable(FE_log_2015, FE_bench_log_2015, FE_demean_log_2015,
       headers = c("2015 FE", "2015 FE bench","2015 FE demean"),
       digits = 4,
       fitstat = ~ n + r2 + ar2 + f + f.p + rmse)



# for 2017 
FE_2017 <- feols(diff_ln_AVE_FE_w_mean ~  as.factor(year)*diff_log_tariff_2017,
                 data = subset(merged_hs4, baseline_year == 2017))

FE_bench_2017 <- feols(diff_ln_AVE_FE_bench_w_mean ~ as.factor(year)*diff_log_tariff_2017,
                       data = subset(merged_hs4, baseline_year == 2017))

FE_demean_2017 <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year)*diff_log_tariff_2017,
                        data = subset(merged_hs4, baseline_year == 2017))

FE_log_2017 <- feols(diff_ln_AVE_FE_log_w_mean ~  as.factor(year)*diff_log_tariff_2017,
                     data = subset(merged_hs4, baseline_year == 2017))

FE_bench_log_2017 <- feols(diff_ln_AVE_FE_log_bench_w_mean ~ as.factor(year)*diff_log_tariff_2017,
                           data = subset(merged_hs4, baseline_year == 2017))

FE_demean_log_2017 <- feols(diff_ln_AVE_FE_log_wmean_w_mean ~ as.factor(year)*diff_log_tariff_2017,
                            data = subset(merged_hs4, baseline_year == 2017))


etable(FE_2017, FE_bench_2017, FE_demean_2017,
       headers = c("2017 FE", "2017 FE bench","2017 FE demean"),
       digits = 4,
       fitstat = ~ n + r2 + ar2 + f + f.p + rmse)

etable(FE_log_2017, FE_bench_log_2017, FE_demean_log_2017,
       headers = c("2017 FE", "2017 FE bench","2017 FE demean"),
       digits = 4,
       fitstat = ~ n + r2 + ar2 + f + f.p + rmse)


