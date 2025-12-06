

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

trade <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/output/summary/prelim_reg/gravity_pois_FE.csv")
names(trade)
US_NTMs <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/output/Compare_values/US_ln_NTMs.csv")
names(US_NTMs)

################################################################################
# get share of export from US

trade <- trade %>% select(year,month,hs2, hs4, hs6_H5, ExporterISO3, ImporterISO3,Applied_tariff,Trade_value_USD)

# create total exportes to create shares 

# create a total export to ROW
total_exp <- trade %>% group_by(year,month,hs2, hs4, hs6_H5, ImporterISO3,) %>% 
  summarise( tot_Trade_value_USD = sum(Trade_value_USD, na.rm = TRUE))
# total US export to China 
total_US_exp  <- trade %>% filter(ExporterISO3 == "USA") %>% group_by(year) %>% 
  summarise( tot_US_Trade_value_USD = sum(Trade_value_USD, na.rm = TRUE))


# Get US trade

US_trade <- trade %>% filter(ExporterISO3 == "USA") 
US_trade <- left_join(US_trade, total_exp)
names(US_trade)
colSums(is.na(US_trade))

# share of export <- 
US_trade <- US_trade %>% mutate(share_US_CHN_export = Trade_value_USD *100 / tot_Trade_value_USD )

# clean ntm data
US_NTMs <- US_NTMs %>% select(year,month,hs2, hs4, hs6_H5, ExporterISO3, ImporterISO3,
                              u, teJLMS , FE_benchmark_exporter , u_benchmark_exporter,
                              elastcities , ln_AVE_FE , ln_AVE_u)

# join NTm and trade data 
US <- left_join(US_trade, US_NTMs)
colSums(is.na(US))


# create a log of tariff variable 
summary(US$Applied_tariff)
US <- US %>% mutate(ln_tariff  = log(1+ Applied_tariff/100))
summary(US$ln_tariff)


################################################################################
# aggregate at the yearly level 
################################################################################

# average of tariffs and NTMs AVE 

US_yearly <- US %>% group_by(year,hs2, hs4, hs6_H5, ExporterISO3, ImporterISO3) %>% 
  summarise(Trade_value_USD =  sum(Trade_value_USD, na.rm = TRUE),
            tot_Trade_value_USD =  sum(tot_Trade_value_USD, na.rm = TRUE),
            ln_tariff =  mean(ln_tariff, na.rm = TRUE),
            ln_AVE_FE =  mean(ln_AVE_FE, na.rm = TRUE),
            ln_AVE_u =  mean(ln_AVE_u, na.rm = TRUE)    ) %>%
  mutate(across(everything(), ~ ifelse(is.nan(.), NA, .)))

colSums(is.na(US_yearly))

US_yearly_hs2 <- US_yearly %>% group_by(year,hs2, ExporterISO3, ImporterISO3) %>% 
  summarise(tot_Trade_value_USD_HS2 =  sum(Trade_value_USD, na.rm = TRUE))
  
US_yearly <- left_join(US_yearly, US_yearly_hs2)
names(US_yearly)

# create a weight to aggregate a HS2 level
# US tarde value / US tot trade value at HS2
US_yearly <- US_yearly %>% mutate(
  weights_HS2_US_export = Trade_value_USD / tot_Trade_value_USD_HS2)
names(US_yearly)

# Aggregate at the HS2 level;
US_yearly_HS2 <- US_yearly %>% group_by(year,hs2, ExporterISO3, ImporterISO3) %>% 
  summarise(Trade_value_USD = sum(Trade_value_USD, na.rm = TRUE),
            tot_Trade_value_USD = sum(tot_Trade_value_USD, na.rm = TRUE),
            ln_tariff = mean(weights_HS2_US_export*ln_tariff, na.rm = TRUE),
            ln_AVE_FE = mean(weights_HS2_US_export*ln_AVE_FE, na.rm = TRUE),
            ln_AVE_u = mean(weights_HS2_US_export*ln_AVE_u, na.rm = TRUE)  )%>%
  mutate(across(everything(), ~ ifelse(is.nan(.), NA, .)))

# create share export of US for Total Chinese export 
US_yearly_HS2 <- US_yearly_HS2 %>% mutate(
  share_US_CHN_export = if_else(!is.na(tot_Trade_value_USD), Trade_value_USD/tot_Trade_value_USD, 0) )
names(US_yearly_HS2)    
table(US_yearly_HS2$year)

# add share of total US export 
US_yearly_HS2 <- left_join(US_yearly_HS2, total_US_exp)
# create share of US export by HS 2 
US_yearly_HS2 <- US_yearly_HS2 %>% mutate(
  share_US_export = if_else(!is.na(tot_US_Trade_value_USD), Trade_value_USD/tot_US_Trade_value_USD, 0) )


################################################################################
# Create lag variable to get change year by year 

# get lag variable  and duoble lag variables 

US_yearly_HS2 <- US_yearly_HS2 %>% 
  arrange(ExporterISO3, ImporterISO3, hs2, year) %>%   # make sure it's ordered
  group_by(ExporterISO3, ImporterISO3, hs2) %>% 
  mutate(
    # 1-year lags
    ln_tariff_lag        = lag(ln_tariff, 1),
    ln_AVE_FE_lag        = lag(ln_AVE_FE, 1),
    ln_AVE_u_lag         = lag(ln_AVE_u, 1),
    share_US_CHN_export_lag  = lag(share_US_CHN_export, 1),
    year_lag             = lag(year, 1),
    
    # 2-year lags (lag of lag)
    ln_tariff_lag2       = lag(ln_tariff, 2),
    ln_AVE_FE_lag2       = lag(ln_AVE_FE, 2),
    ln_AVE_u_lag2        = lag(ln_AVE_u, 2),
    share_US_CHN_export_lag2 = lag(share_US_CHN_export, 2),
    year_lag2            = lag(year, 2),
    
    # 1-year changes (only for consecutive years)
    d_ln_tariff = if_else(year - year_lag == 1, ln_tariff - ln_tariff_lag, NA_real_),
    d_ln_AVE_FE = if_else(year - year_lag == 1, ln_AVE_FE - ln_AVE_FE_lag, NA_real_),
    d_ln_AVE_u  = if_else(year - year_lag == 1, ln_AVE_u  - ln_AVE_u_lag, NA_real_),
    d_share_US_CHN_export = if_else(
      year - year_lag == 1,
      share_US_CHN_export - share_US_CHN_export_lag,
      NA_real_
    ),
    
    # 2-year changes (optional; delete if you don't need them)
    d2_ln_tariff = if_else(year - year_lag2 == 2, ln_tariff - ln_tariff_lag2, NA_real_),
    d2_ln_AVE_FE = if_else(year - year_lag2 == 2, ln_AVE_FE - ln_AVE_FE_lag2, NA_real_),
    d2_ln_AVE_u  = if_else(year - year_lag2 == 2, ln_AVE_u  - ln_AVE_u_lag2, NA_real_),
    d2_share_US_CHN_export = if_else(
      year - year_lag2 == 2,
      share_US_CHN_export - share_US_CHN_export_lag2,
      NA_real_
    )
  ) %>% 
  ungroup()

names(US_yearly_HS2)

# get import share for 2017 US export tp China
Import_share_2017 <- US_yearly_HS2 %>% select(hs2, ExporterISO3, ImporterISO3, year, share_US_CHN_export, share_US_export) %>%
  filter(year == 2017) %>% rename(share_US_CHN_export_2017 = share_US_CHN_export,
                                  share_US_export_2017 = share_US_export) %>% select(-year)

Change_tariffs_2018 <- US_yearly_HS2 %>% 
  select(    hs2,    ExporterISO3,    ImporterISO3,    year,
    d_ln_tariff,    d_ln_AVE_FE,    d_ln_AVE_u  ) %>%   filter(year == 2018) %>% 
  rename(  d_ln_tariff_2018_2017   = d_ln_tariff,  d_ln_AVE_FE_2018_2017   = d_ln_AVE_FE,
           d_ln_AVE_u_2018_2017    = d_ln_AVE_u) %>% select(-year)

Change_tariffs_2019 <- US_yearly_HS2 %>% 
  select(    hs2,    ExporterISO3,    ImporterISO3,    year,
             d2_ln_tariff,    d2_ln_AVE_FE,    d2_ln_AVE_u  ) %>%   filter(year == 2019) %>% 
  rename(d_ln_tariff_2019_2017   = d2_ln_tariff, d_ln_AVE_FE_2019_2017   = d2_ln_AVE_FE,
         d_ln_AVE_u_2019_2017    = d2_ln_AVE_u ) %>% select(-year)

Final_HS2 <- Import_share_2017 %>% 
  left_join(Change_tariffs_2018, by = c("hs2", "ExporterISO3", "ImporterISO3")) %>% 
  left_join(Change_tariffs_2019, by = c("hs2", "ExporterISO3", "ImporterISO3"))
names(Final_HS2)
Final_HS2$hs2 <- as.numeric(Final_HS2$hs2)


############### 
# merge with Chen data 
Chen <- read_csv("data/chen_NTB_tariff/hs2_agriculture_manufacturing_clean.csv")
names(Chen)
Chen <- Chen %>% select(-Country, - ISO3_Code) %>% rename(hs2 = HS2 , Chen_US_import_share =US_import_share,
                                                          Chen_tau_tariff_CHN = tau_tariff_CHN, 
                                                          Chen_tau_NTB = tau_NTB)
Final_HS3 <- left_join(Final_HS2,Chen)
  
write_csv(Final_HS3, paste0(exp, "Compare_NTM_chen.csv"))



##################################################################################

# PLot tariffs and 

names(US)
summary(trade$Applied_tariff)

# estimate AVEs 
US <- US %>% mutate(AVE_NTM_FE =( exp(ln_AVE_FE) -1)*100,
                    AVE_NTM_u = (exp(ln_AVE_u) -1)*100)
summary(US$AVE_NTM_FE)
summary(US$AVE_NTM_u)

# plot to see 


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
  sector = case_when( hs_section %in% 1:4   ~ "Ag", hs_section %in% 5:20  ~ "Manu",    TRUE                  ~ "Other"))

names(US)

# Aggregate at the sector level by weighting with trade 
US <- US %>% group_by(year, sector) %>% mutate(
  tot_US_Trade_value_USD = sum(Trade_value_USD, na.rm= TRUE)) %>% ungroup()


US <- US %>% mutate(
  weights = if_else(tot_US_Trade_value_USD != 0 , Trade_value_USD /tot_US_Trade_value_USD,0))
summary(US$Applied_tariff)


US_sector <- US %>% group_by(year, month, sector) %>% summarise(
  Applied_tariff_weighted = mean(weights*Applied_tariff, na.rm=TRUE),
  Applied_tariff_simple = mean(Applied_tariff, na.rm=TRUE),
  AVE_NTM_u_weighted = mean(weights*AVE_NTM_u, na.rm=TRUE),
  AVE_NTM_FE_weighted = mean(weights*AVE_NTM_FE, na.rm=TRUE),
  AVE_NTM_u_simple = mean(AVE_NTM_u, na.rm=TRUE),
  AVE_NTM_FE_simple = mean(AVE_NTM_FE, na.rm=TRUE))
summary(US_sector$Applied_tariff_simple)
summary(US_sector$Applied_tariff_weighted)
summary(US_sector$AVE_NTM_u_simple)
summary(US_sector$AVE_NTM_FE_simple)
summary(US_sector$AVE_NTM_FE_weighted)

US_sector <- US_sector %>%  mutate(date = as.Date(paste(year, month, "01", sep = "-"))  ) 


# agriculture: 
summary(US$FE)
plot <- ggplot(subset(US_sector, sector == "Ag")) +
  geom_smooth(aes(x = date, y = Applied_tariff_weighted),   se = TRUE,
    color = "blue"  ) +
  geom_smooth(aes(x = date, y = AVE_NTM_u_weighted),  se = TRUE,
    color = "red"  ) +
  labs(  title = "Weighted average: Ad valorem tariff and NTMs over time fro Agricultural sector",   x = "Date",   y = "Ad Valorem rate"  ) +
  theme_minimal(base_size = 14) +
  theme(    plot.title = element_text(size = 12),   
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)  )
plot
ggsave( paste0(exp, "plot/Ag_AVEs_plot_weighted_smooth.png"), plot, width = 10, height = 6, dpi = 300)


plot <- ggplot(subset(US_sector, sector == "Ag")) +
  geom_line(aes(x = date, y = Applied_tariff_weighted),    color = "blue"  ) +
  geom_line(aes(x = date, y = AVE_NTM_u_weighted),    color = "red"  ) +
  labs(  title = "Weighted average: Ad valorem tariff and NTMs over time fro Agricultural sector",   x = "Date",   y = "Ad Valorem rate"  ) +
  theme_minimal(base_size = 14) +
  theme(    plot.title = element_text(size = 12),   
    panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA)  )
plot
ggsave( paste0(exp, "plot/Ag_AVEs_plot_weighted.png"), plot, width = 10, height = 6, dpi = 300)

        
plot <- ggplot(subset(US_sector, sector == "Ag")) +
  geom_line(aes(x = date, y = Applied_tariff_simple), color = "blue"  ) +
  geom_line(aes(x = date, y = AVE_NTM_u_simple),  color = "red"  ) +
  labs(  title = "Simple average: Ad valorem tariff and NTMs over time fro Agricultural sector",   x = "Date",   y = "Ad Valorem rate"  ) +
  theme_minimal(base_size = 14) +
  theme(    plot.title = element_text(size = 12),   
    panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA)  )
plot
ggsave( paste0(exp, "plot/Ag_AVEs_plot_simple_smooth.png"),  plot, width = 10, height = 6, dpi = 300)



# Manufacturing 

plot <- ggplot(subset(US_sector, sector == "Manu")) +
  geom_smooth(aes(x = date, y = Applied_tariff_weighted),   se = TRUE,
              color = "blue"  ) +
  geom_smooth(aes(x = date, y = AVE_NTM_u_weighted),  se = TRUE,
              color = "red"  ) +
  labs(  title = "Weighted average: Ad valorem tariff and NTMs over time for Manufacturing sector",   x = "Date",   y = "Ad Valorem rate"  ) +
  theme_minimal(base_size = 14) +
  theme(    plot.title = element_text(size = 12),   
    panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA)  )
plot
ggsave( paste0(exp, "plot/Manu_AVEs_plot_weighted_smooth.png"), plot, width = 10, height = 6, dpi = 300)

plot <- ggplot(subset(US_sector, sector == "Manu")) +
  geom_line(aes(x = date, y = Applied_tariff_weighted),    color = "blue"  ) +
  geom_line(aes(x = date, y = AVE_NTM_u_weighted),    color = "red"  ) +
  labs(  title = "Weighted average: Ad valorem tariff and NTMs over time for Manufacturing sector",   x = "Date",   y = "Ad Valorem rate"  ) +
  theme_minimal(base_size = 14) +
  theme(   
    plot.title = element_text(size = 12),   plot.title = element_text(size = 12),   
    panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA)  )
plot
ggsave( paste0(exp, "plot/Manu_AVEs_plot_weighted.png"), plot, width = 10, height = 6, dpi = 300)

plot <- ggplot(subset(US_sector, sector == "Ag")) +
  geom_line(aes(x = date, y = Applied_tariff_simple), color = "blue"  ) +
  geom_line(aes(x = date, y = AVE_NTM_u_simple),  color = "red"  ) +
  labs(  title = "Simple average: Ad valorem tariff and NTMs over time for Manufacturing sector",   x = "Date",   y = "Ad Valorem rate"  ) +
  theme_minimal(base_size = 14) +
  theme(    plot.title = element_text(size = 12),   
    panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA)  )
plot
ggsave( paste0(exp, "plot/Manu_AVEs_plot_simple.png"), plot, width = 10, height = 6, dpi = 300)
