
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
# library(Hmisc)
library(haven)
library(sfaR)
library(frontier)
library(ggplot2)

################################################################################
# directory: 
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git")
dir_dta <-  "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/Compare_values/yearly/robust/elast/"

exp <-  "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/IVs/regression/elast/"
sectors_hs4 <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/crosswalk/clean_HS4_sub_sector_edit.csv")

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
    panel.spacing.x  = unit(1.2, "lines"),
    plot.title       = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.6),
    strip.background = element_rect(fill = "grey92", color = "black", linewidth = 0.6),
    strip.text       = element_text(size = 12, face = "bold", color = "black"),
    axis.text.x  = element_text(size = 11),
    axis.text.y  = element_text(size = 11),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "top",
    legend.text  = element_text(size = 12),
    legend.title = element_blank()
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

# merge with sector shares:
names(dta)
table(dta$year)
names(sectors_hs4)
sectors_hs4$hs4 <- as.numeric(sectors_hs4$hs4)


dta <- left_join(dta, sectors_hs4)


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

p <- ggplot(SOE_hs4)+
  geom_histogram(aes(x=share_value_SOE), bins = 30, color = "black" ,fill = "blue")+
  labs(title = "Histogram of the import share by SOE across HS4 industries \n for Chinese imports from the US in 2010",
       x = "Import share by SOE",    y = "Count"  ) +
  theme_trade
p
ggsave(paste0(exp, "SOE_share.png"), plot = p, width = 8, height = 6, dpi = 300)

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

merged_hs4_filtered <- merged_hs4 %>%  filter(subsector %in% c("crop", "livestock", "nonag") & baseline_year == 2017)

p <- ggplot(merged_hs4_filtered, aes(x = share_value_SOE, y = diff_ln_AVE_FE_wmean_w_mean, color = subsector)) +
  geom_point(alpha = 0.4, size = 0.8) +
  # geom_smooth(method = "lm", se = FALSE) +
  geom_smooth(method = "loess", se = FALSE) +
  scale_color_manual(
    values = c("crop" = "#1b9e77", "livestock" = "#d95f02", "nonag" = "#7570b3"),
    labels = c("crop" = "Crop", "livestock" = "Livestock", "nonag" = "Non-ag")  ) +
  labs(
    title = "Change in AVE vs. SOE share, by subsector",
    x = "SOE share in 2010",
    y = "\u0394 ln(1+AVE) (demean approach)",
    color = "Subsector"  ) +
  theme_trade
p
ggsave(paste0(exp, "SOE_share.png"), plot = p, width = 8, height = 6, dpi = 300)


################################################################################

# Model 1 – full sample
library(fixest)
length(unique(merged_hs4$hs4))
names(merged_hs4)
table(merged_hs4$year)
test <- merged_hs4 %>% filter(sector == "Other")

# Full sample: By distribution
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
write.csv(tbl, paste0(exp, "SOE_NTM_2017_FE_models_with_tar.csv"), row.names = FALSE)

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
write.csv(tbl, paste0(exp, "SOE_NTM_2017_FE_models.csv"), row.names = FALSE)


# By subsector 





##############################################################################
# demean model
##############################################################################
merged_hs4_filtered <- merged_hs4 %>%  filter(subsector %in% c("crop", "livestock", "nonag"))

AVE <- feols(diff_ln_AVE_FE_wmean_w_mean ~ share_value_SOE,
                  data = subset(merged_hs4_filtered, baseline_year == 2017 & year %in% c(2018:2019)))
AVE_2017 <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year) + share_value_SOE,
                  data = subset(merged_hs4_filtered, baseline_year == 2017 & year %in% c(2018:2019)))
AVE_2017_tar <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year) + diff_log_tariff_2017 + share_value_SOE,
                      data = subset(merged_hs4_filtered, baseline_year == 2017 & year %in% c(2018:2019)))
AVE_2017_sector <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year) + as.factor(subsector) +
                           diff_log_tariff_2017 + share_value_SOE,
                         data = subset(merged_hs4_filtered, baseline_year == 2017 & year %in% c(2018:2019)))
AVE_2017_sector_interact <- feols(diff_ln_AVE_FE_wmean_w_mean ~  as.factor(year) +
                                    diff_log_tariff_2017 + as.factor(subsector) * share_value_SOE,,
                                  data = subset(merged_hs4_filtered, baseline_year == 2017 & year %in% c(2018:2019)))

# Table
tbl <- etable(AVE, AVE_2017, AVE_2017_tar, AVE_2017_sector, AVE_2017_sector_interact,
              headers = c("AVE", "AVE FE", "with tariff", "sector", "interact"),
              digits = 4,
              fitstat = ~ n + r2 + ar2 + f + f.p + rmse)
tbl
write.csv(tbl, paste0(exp, "SOE_NTM_w_2017.csv"), row.names = FALSE)


# with cluster
AVE <- feols(diff_ln_AVE_FE_wmean_w_mean ~ share_value_SOE,
             data = subset(merged_hs4_filtered, baseline_year == 2017 & year %in% c(2018:2019)), cluster = ~hs4)
AVE_2017 <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year) + share_value_SOE,
                  data = subset(merged_hs4_filtered, baseline_year == 2017 & year %in% c(2018:2019)), cluster = ~hs4)
AVE_2017_tar <- feols(diff_ln_AVE_FE_wmean_w_mean ~ diff_log_tariff_2017 + share_value_SOE,
                      data = subset(merged_hs4_filtered, baseline_year == 2017 & year %in% c(2018:2019)), cluster = ~hs4)
AVE_2017_sector <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(subsector) + diff_log_tariff_2017 + share_value_SOE,
                         data = subset(merged_hs4_filtered, baseline_year == 2017 & year %in% c(2018:2019)), cluster = ~hs4)
AVE_2017_sector_interact <- feols(diff_ln_AVE_FE_wmean_w_mean ~ diff_log_tariff_2017 + as.factor(subsector) * share_value_SOE,
                                  data = subset(merged_hs4_filtered, baseline_year == 2017 & year %in% c(2018:2019)), cluster = ~hs4)
# Table
tbl <- etable(AVE, AVE_2017, AVE_2017_tar, AVE_2017_sector, AVE_2017_sector_interact,
              headers = c("AVE", "AVE FE", "with tariff", "subsector", "interact"),
              digits = 4,
              fitstat = ~ n + r2 + ar2 + f + f.p + rmse)
tbl
write.csv(tbl, paste0(exp, "SOE_NTM_w_2017_cluster.csv"), row.names = FALSE)

###############################################################################
# At subsector level
###############################################################################
subsectors <- c("crop", "livestock", "nonag")

# Without cluster
models_nocluster <- lapply(subsectors, function(s) {
  feols(diff_ln_AVE_FE_wmean_w_mean ~ diff_log_tariff_2017 + share_value_SOE,
        data = subset(merged_hs4_filtered, baseline_year == 2017 & year %in% c(2018:2019) & subsector == s))
})
names(models_nocluster) <- c("Crop (no cl.)", "Livestock (no cl.)", "Non-ag (no cl.)")

# With cluster
models_cluster <- lapply(subsectors, function(s) {
  feols(diff_ln_AVE_FE_wmean_w_mean ~ diff_log_tariff_2017 + share_value_SOE,
        data = subset(merged_hs4_filtered, baseline_year == 2017 & year %in% c(2018:2019) & subsector == s),
        cluster = ~hs4)
})
names(models_cluster) <- c("Crop (cl. hs4)", "Livestock (cl. hs4)", "Non-ag (cl. hs4)")

# Combine into one table, alternating or grouped
tbl_subsector <- etable(models_nocluster, models_cluster,
                        digits = 4,
                        fitstat = ~ n + r2 + ar2 + f + f.p + rmse)
tbl_subsector

write.csv(tbl_subsector, paste0(exp, "SOE_NTM_w_2017_subsector.csv"), row.names = FALSE)



################################################################################
# get predicted vale to construct IVS:
################################################################################
{
# Fit the model
AVE_2017_2017_sector_interact <- feols(diff_ln_AVE_FE_wmean_w_mean ~ diff_log_tariff_2017 + 
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

write_csv(df_model, "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/IVs/predicted/SOE_IV_predicted_AVE_elast_w.csv")


#################### without tariffs

# Fit the model
AVE_2017_2017_sector_interact <- feols(diff_ln_AVE_FE_wmean_w_mean ~  
                                         as.factor(year) + as.factor(sector) + share_value_SOE,
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
ggsave(filename = "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/IVs/summary/SOE_IV_AVE_predicted_distribution_bis.png",
       plot = plot, width = 9,height = 6, units = "in", dpi = 300,bg = "white")


df_model <- df_model %>%  select(year, sector, hs_section, hs2, hs4,
                                 diff_ln_AVE_FE_wmean_w_mean, predicted_diff_ln_AVE_FE_wmean, diff_log_tariff_2017) %>% 
  rename(diff_ln_AVE_FE_wmean = diff_ln_AVE_FE_wmean_w_mean)
colSums(is.na(df_model))


write_csv(df_model, "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/IVs/predicted/SOE_IV_predicted_AVE_elast_bis.csv")

}



##################################################################################
# pretrend checks SOE shares 
##################################################################################
names(merged_hs4_all)
table(merged_hs4_all$year)

# pretrend check on windsorzed
d <- subset(merged_hs4_all, baseline_year == 2015)

# (1) what you wrote: no year FE, no SOE main effect
m1 <- feols(diff_ln_AVE_FE_wmean_w_mean ~ i(year, share_value_SOE, ref = 2016),
            data = d, cluster = ~hs4)

# (2) year FE added, still no main effect: gradient in each year vs 2015 base
m2 <- feols(diff_ln_AVE_FE_wmean_w_mean ~ i(year, share_value_SOE) | year,
            data = d, cluster = ~hs4)

# (3) your Table 2 structure: year dummies + SOE main effect + interactions
m3 <- feols(diff_ln_AVE_FE_wmean_w_mean ~ share_value_SOE +
              i(year, share_value_SOE, ref = 2016) | year,
            data = d, cluster = ~hs4)

etable(m1, m2, m3, digits = 4, fitstat = ~ n + r2)



# pretrend check on windsorzed
d <- subset(merged_hs4_all, baseline_year == 2015)

# (1) what you wrote: no year FE, no SOE main effect
m1 <- feols(diff_ln_AVE_FE_wmean_mean ~ i(year, share_value_SOE, ref = 2016),
            data = d, cluster = ~hs4)

# (2) year FE added, still no main effect: gradient in each year vs 2015 base
m2 <- feols(diff_ln_AVE_FE_wmean_mean ~ i(year, share_value_SOE) + as.factor(year),
            data = d, cluster = ~hs4)

# (3) your Table 2 structure: year dummies + SOE main effect + interactions
m3 <- feols(diff_ln_AVE_FE_wmean_mean ~ share_value_SOE +
              i(year, share_value_SOE, ref = 2016) + as.factor(year),
            data = d, cluster = ~hs4)

tbl_w <- etable(m1, m2, m3, digits = 4, fitstat = ~ n + r2)
tbl_w
write_csv(as.data.frame(tbl_w), paste0(exp, "NTBs_SOE_pretrend.csv"))



# windsorized sample
FE_2015_SOE_notar_demean <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year)*share_value_SOE,
                                  data = subset(merged_hs4_all, baseline_year == 2015))
FE_2015_SOE_tar_demean   <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year)*share_value_SOE + diff_log_tariff_2015,
                                  data = subset(merged_hs4_all, baseline_year == 2015))
FE_2015_SOE_tarX_demean  <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year)*share_value_SOE + as.factor(year)*diff_log_tariff_2015,
                                  data = subset(merged_hs4_all, baseline_year == 2015))
# with cluster
FE_2015_SOE_notar_demean_clust <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year)*share_value_SOE,
                                        data = subset(merged_hs4_all, baseline_year == 2015), cluster = ~hs4)
FE_2015_SOE_tar_demean_clust   <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year)*share_value_SOE + diff_log_tariff_2015,
                                        data = subset(merged_hs4_all, baseline_year == 2015), cluster = ~hs4)
FE_2015_SOE_tarX_demean_clust  <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year)*share_value_SOE + as.factor(year)*diff_log_tariff_2015,
                                        data = subset(merged_hs4_all, baseline_year == 2015), cluster = ~hs4)

# Table
tbl_w <- etable(FE_2015_SOE_notar_demean, FE_2015_SOE_tar_demean, FE_2015_SOE_tarX_demean,
                FE_2015_SOE_notar_demean_clust, FE_2015_SOE_tar_demean_clust, FE_2015_SOE_tarX_demean_clust,
                headers = c("No Tariff", "Tariff", "Tariff x Year",
                            "No Tariff", "Tariff", "Tariff x Year"),
                digits = 4,
                fitstat = ~ n + r2 + ar2 + f + f.p + rmse)
tbl_w
write_csv(as.data.frame(tbl_w), paste0(exp, "NTBs_SOE_pretrend.csv"))

#####################################

# event study

library(broom)
library(broom)
library(dplyr)
library(ggplot2)

merged_hs4_all_2015 <- merged_hs4_all %>% # filter(subsector == "crop") %>%
  filter(baseline_year == 2015) %>%    mutate(rel_year = year - 2017)

# --- Choose your outcome variable here ---
outcome_var <- "diff_ln_AVE_FE_wmean_w_mean"


# --- Eight models total ---
es_model_noFE <- feols(as.formula(paste(outcome_var, "~ i(rel_year, share_value_SOE, ref = -1)")),
                       data = merged_hs4_all_2015)

es_model_yearFE <- feols(as.formula(paste(outcome_var, "~ i(rel_year, share_value_SOE, ref = -1) | year")),
                         data = merged_hs4_all_2015)

es_model_hs4FE <- feols(as.formula(paste(outcome_var, "~ i(rel_year, share_value_SOE, ref = -1) | hs4")),
                        data = merged_hs4_all_2015)

es_model_yearhs4FE <- feols(as.formula(paste(outcome_var, "~ i(rel_year, share_value_SOE, ref = -1) | year + hs4")),
                            data = merged_hs4_all_2015)

es_model_tar <- feols(as.formula(paste(outcome_var, "~ i(rel_year, share_value_SOE, ref = -1) + diff_log_tariff_2015")),
                      data = merged_hs4_all_2015)

es_model_tar_yearFE <- feols(as.formula(paste(outcome_var, "~ i(rel_year, share_value_SOE, ref = -1) + diff_log_tariff_2015 | year")),
                             data = merged_hs4_all_2015)

es_model_tar_hs4FE <- feols(as.formula(paste(outcome_var, "~ i(rel_year, share_value_SOE, ref = -1) + diff_log_tariff_2015 | hs4")),
                            data = merged_hs4_all_2015)

es_model_tar_yearhs4FE <- feols(as.formula(paste(outcome_var, "~ i(rel_year, share_value_SOE, ref = -1) + diff_log_tariff_2015 | year + hs4")),
                                data = merged_hs4_all_2015)

# --- Helper: tag both the FE spec and the tariff facet ---
tidy_es <- function(model, fe_label, tariff_label) {
  out <- tidy(model, conf.int = TRUE) %>%
    filter(grepl("rel_year", term)) %>%
    mutate(rel_year = as.numeric(sub(".*rel_year::(-?[0-9]+):.*", "\\1", term)))
  
  out <- bind_rows(  out,
    data.frame(rel_year = -1, estimate = 0, conf.low = 0, conf.high = 0)  )
  out$fe_spec <- fe_label
  out$tariff_facet <- tariff_label
  out
}

es_all <- bind_rows(
  tidy_es(es_model_noFE, "No FE", "Without tariff control"),
  tidy_es(es_model_yearFE, "Year FE", "Without tariff control"),
  tidy_es(es_model_hs4FE, "HS4 FE", "Without tariff control"),
  tidy_es(es_model_yearhs4FE, "Year + HS4 FE", "Without tariff control"),
  tidy_es(es_model_tar, "No FE", "With tariff control"),
  tidy_es(es_model_tar_yearFE, "Year FE", "With tariff control"),
  tidy_es(es_model_tar_hs4FE, "HS4 FE", "With tariff control"),
  tidy_es(es_model_tar_yearhs4FE, "Year + HS4 FE", "With tariff control"))

es_all$fe_spec <- factor(es_all$fe_spec, levels = c("No FE", "Year FE", "HS4 FE", "Year + HS4 FE"))
es_all$tariff_facet <- factor(es_all$tariff_facet, levels = c("Without tariff control", "With tariff control"))

# --- Plot: facet by tariff control, color by FE spec ---
p <- ggplot(es_all, aes(x = rel_year, y = estimate, color = fe_spec)) +
  geom_point(position = position_dodge(width = 0.3)) +
  geom_line(position = position_dodge(width = 0.3)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.15,
                position = position_dodge(width = 0.3)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  geom_vline(xintercept = -1, linetype = "dotted", color = "gray60") +
  facet_wrap(~ tariff_facet) +
  labs(title = paste("Event study:", outcome_var),
       x = "Years relative to 2017", y = "Coefficient", color = "Specification") +
  theme_trade
p
ggsave(paste0(exp, "event_study_SOE_pretrend.png"), plot = p, width = 8, height = 6, dpi = 300)







###############################################################################
# looking at NTB-SOE-tariffs 
###############################################################################

merged_hs4_2017 <- merged_hs4 %>% filter( baseline_year == 2017 & subsector %in% c("crop", "livestock", "nonag"))

SOE_2017 <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year) + share_value_SOE ,
                  data = subset(merged_hs4_2017, baseline_year == 2017))
SOE_bis_2017 <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year) + share_value_SOE +diff_log_tariff_2017 ,
                  data = subset(merged_hs4_2017, baseline_year == 2017))
tariff_2017 <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year) + diff_log_tariff_2017 ,
                     data = subset(merged_hs4_2017, baseline_year == 2017))
SOE_tar_2017 <- feols(diff_log_tariff_2017 ~  as.factor(year) + share_value_SOE ,
                      data = subset(merged_hs4_2017, baseline_year == 2017))

SOE_2017_clust <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year) + share_value_SOE ,
                  data = subset(merged_hs4_2017, baseline_year == 2017), cluster = ~hs4)
SOE_bis_2017_clust <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year) + share_value_SOE +diff_log_tariff_2017 ,
                      data = subset(merged_hs4_2017, baseline_year == 2017), cluster = ~hs4)
tariff_2017_clust <- feols(diff_ln_AVE_FE_wmean_w_mean ~ as.factor(year) + diff_log_tariff_2017 ,
                     data = subset(merged_hs4_2017, baseline_year == 2017), cluster = ~hs4)
SOE_tar_2017_clust <- feols(diff_log_tariff_2017 ~  as.factor(year) + share_value_SOE ,
                      data = subset(merged_hs4_2017, baseline_year == 2017), cluster = ~hs4)

# Table
tbl <- etable(SOE_2017, SOE_bis_2017 , tariff_2017, SOE_tar_2017,
              SOE_2017_clust, SOE_bis_2017_clust , tariff_2017_clust, SOE_tar_2017_clust,
              headers = c("NTB-SOE", "NTB-SOE-tariff","NTB-tariff","tariff-SOE",
                          "NTB-SOE_clust", "NTB-SOE-tariff_clust","NTB-tariff_clust","tariff-SOE_clust"),
              digits = 4,
              fitstat = ~ n + r2 + ar2 + f + f.p + rmse)
tbl
write_csv(as.data.frame(tbl), paste0(exp,"NTBs_SOE_tariff.csv"))




# correlation plote between tariff and NTMs
merged_hs4_2017 <- merged_hs4 %>% filter( baseline_year == 2017 & subsector %in% c("crop", "livestock", "nonag") )
names(merged_hs4_2017)

cor_by_subsector_year <- merged_hs4_2017 %>%
  group_by(subsector, year) %>%
  summarise(
    correlation = cor(diff_ln_AVE_FE_wmean_w_mean, diff_log_tariff_2017, use = "complete.obs"),
    n = sum(!is.na(diff_ln_AVE_FE_wmean_w_mean) & !is.na(diff_log_tariff_2017)),
    .groups = "drop"
  )

cor_by_subsector_year
write_csv(cor_by_subsector_year, paste0(exp,"correlation_tariff_NTM_subsector.csv"))



