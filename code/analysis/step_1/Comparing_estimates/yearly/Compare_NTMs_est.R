



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
exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/Compare_values/yearly/"

################################################################################
# 1) Load data 
################################################################################

# Load FE estimates 
fe <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/output/FE/yearly/gravity_pois_FE.csv")
sf <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/output/stochastic/yearly/sfaR_efficiency_average_merged.csv")

################################################################################

# merge the data 


# select data we want 
names(fe)
colSums(is.na(fe))
names(res)
names(sf)


fe <- fe %>% select(year,hs2, hs4, hs6_H5, ExporterISO3, ImporterISO3,Trade_value_USD, Applied_tariff, fe_id, FE)
sf <- sf %>% select(year,hs2, hs4, hs6_H5, ExporterISO3, ImporterISO3,Applied_tariff, u,teJLMS,
                    u_tariff , teJLMS_tariff)

# merge all data 


# Full join all three datasets
dta <- fe %>%    full_join(sf,  by = c("year", "hs2", "hs4", "hs6_H5",
                        "ExporterISO3", "ImporterISO3", "Applied_tariff"))
colSums(is.na(dta))
names(dta)

# create a log 
dta <- dta %>% mutate(log_tariff = log(1+Applied_tariff/100) )
summary(dta$log_tariff)
summary(dta$Applied_tariff)

# drop Nas
dta <- dta %>%  filter(!if_all(c(FE, teJLMS), is.na))
write_csv(dta , paste0(exp, "estimates_reduced_form.csv"))


dta <- read_csv( paste0(exp, "estimates_reduced_form.csv"))
names(dta)

dta <- dta %>% mutate(log_tariff = log(1+Applied_tariff/100) )


################################################################################
# # add some product level variables 
################################################################################

# check values:
summary(dta$FE)
summary(dta$u)

# add HS section for each HS6 product code 
class(dta$hs6_H5)
unique(nchar(dta$hs6_H5))
unique(dta$hs2)
table(dta$hs2)

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
table(dta$sector)
table(dta$hs_section)
table(dta$hs2)
table(dta$hs2, dta$hs_section)
names(dta)


################################################################################
# If we want to create a benchmark for each values 
# we take the max value of the FE and u among all exporter for a specific product, month, year

# dta <- dta %>% group_by(year, month, hs6_H5) %>%
#   mutate( FE_benchmark_exporter = {
#     m <- max(FE, na.rm = TRUE)
#     ifelse(is.infinite(m), NA_real_, m)   
#     },
#     u_benchmark_exporter = {
#       m <- min(u, na.rm = TRUE)
#       ifelse(is.infinite(m), NA_real_, m)   
#       }  ) %>%
#   ungroup()
# summary(dta$FE_benchmark_exporter)
# summary(dta$u_benchmark_exporter)




# if we want to use pre 2017 as a benchmark 
# dta <- dta %>% group_by(month, ExporterISO3, hs6_H5) %>%
#   mutate( FE_pre_2017_m = mean(FE[year %in% c(2015, 2017)], na.rm = TRUE),
#     u_pre_2017_m = mean(u[year %in% c(2015, 2017)], na.rm = TRUE),
#     u_tariff_pre_2017_m = mean(u_tariff[year %in% c(2015, 2017)], na.rm = TRUE) ) %>%  ungroup()

dta <- dta %>%  group_by(ExporterISO3, hs6_H5) %>%
  mutate( FE_pre_2017 = mean(FE[year %in% c(2017)], na.rm = TRUE),
    u_pre_2017 = mean(u[year %in% c(2017)], na.rm = TRUE),
    u_tariff_pre_2017 = mean(u_tariff[year %in% c(2017)], na.rm = TRUE),
    log_tariff_pre_2017 = mean(log_tariff[year %in% c(2017)], na.rm = TRUE)) %>%  ungroup()

summary(dta$log_tariff)

################################################################################
# Add elasticities
################################################################################

# add elasticities from chen et al.

dta <- dta %>% mutate(elasticities = case_when(sector == "Ag" ~  3 ,
                          sector == "Manu" ~ 1.97,
                          sector == "Other" ~ 5 ))

dta <- dta %>% arrange(year, hs6_H5)

################################################################################

# create adjusted values of efficiency and FE estimates
# do the difference with 2017 baseline 
dta <- dta %>%
  mutate(
    # Difference in FE relative to 2017 baseline
    diff_FE_2017 = if_else( year > 2017 & !is.na(FE) & !is.na(FE_pre_2017),
      FE - FE_pre_2017,      NA_real_    ),
    
    # Difference in u (inefficiency) relative to 2017 baseline
    diff_u_2017 = if_else(year > 2017 & !is.na(u) & !is.na(u_pre_2017),
       u_pre_2017 - u,      NA_real_    ),
    
    # Difference in tariff-adjusted u relative to 2017 baseline
    diff_u_tariff_2017 = if_else( year > 2017 & !is.na(u_tariff) & !is.na(u_tariff_pre_2017),
      u_tariff_pre_2017 - u_tariff,     NA_real_    ),
    
    # Difference in log tariffs relative to 2017 baseline
    diff_log_tariff_2017 = if_else(  year > 2017 & !is.na(log_tariff) & !is.na(log_tariff_pre_2017),
      log_tariff - log_tariff_pre_2017,      NA_real_    )  )

test  <- dta %>% filter(year >2017)
colSums(is.na(test))
summary(dta$diff_FE_2017)
summary(dta$diff_u_2017)
summary(dta$diff_u_tariff_2017)
summary(dta$diff_log_tariff_2017)
################################################################################

# estimate ln(1+T) and changes in ln(1+T)

dta <- dta %>%
  mutate(
    ln_AVE_FE = (1/(1-elasticities))*FE,
    ln_AVE_u = (1/(1-elasticities))*-u,
    ln_AVE_u_tariff = (1/(1-elasticities))*-u_tariff,
    diff_ln_AVE_FE =if_else(year %in% c(2018:2020), (1/(1-elasticities))*diff_FE_2017, NA) ,
    diff_ln_AVE_u =if_else(year %in% c(2018:2020), (1/(1-elasticities))*diff_u_2017, NA) ,
    diff_ln_AVE_u_tariff =if_else(year %in% c(2018:2020), (1/(1-elasticities))*diff_u_tariff_2017, NA) )
summary(dta$ln_AVE_FE)
summary(dta$ln_AVE_u)
summary(dta$ln_AVE_u_tariff)
summary(dta$diff_ln_AVE_FE)
summary(dta$diff_ln_AVE_u)
summary(dta$diff_ln_AVE_u_tariff)
colSums(is.na(dta))


write_csv(dta , paste0(exp, "estimates_reduced_form1.csv"))

dta <- read_csv( paste0(exp, "estimates_reduced_form1.csv"))
summary(dta$log)
################################################################################

# Plotting ln(1+AVE)

################################################################################
US <- dta  %>% filter(ExporterISO3 == "USA")
US <- US %>%  mutate(date = as.Date(paste(year, month, "01", sep = "-"))  ) 
summary(US)
names(US)
unique(US$hs2)
unique(US$hs_section)
US1 <- US %>% filter(year %in% c(2018,2019))
write_csv(US, paste0(exp, "US_ln_NTMs.csv"))
US <- read_csv(paste0(exp, "US_ln_NTMs.csv"))

################################################################################

# For US ag: 

# create simple average change and weighted average change in 
US_ag <- US %>% filter(sector == "Ag")
names(US_ag)
unique(US_ag$year)
unique(US_ag$hs2)
unique(US_ag$hs_section)
length(unique(US_ag$hs6_H5))

# Ag sector level: create weight in trade (hs6/total US export)
# weights for hs6 to aggregate to sector level
US_ag_2017_hs6 <- US_ag %>%
  filter(year == 2017) %>%
  group_by(hs6_H5) %>%
  summarise(Trade_value_USD_2017 = sum(Trade_value_USD, na.rm = TRUE),  .groups = "drop"  ) %>%
  mutate(  tot_Trade_value_USD_2017 = sum(Trade_value_USD_2017),
           weight_sector = Trade_value_USD_2017 / tot_Trade_value_USD_2017  )
US_ag <-left_join(US_ag,US_ag_2017_hs6 )


# HS section level : weights for hs6 to aggregate to HS-section
US_ag_2017_hs_sect <- US_ag %>%  filter(year == 2017) %>%
  group_by(hs_section, hs6_H5) %>%
  summarise( Trade_value_USD_2017 = sum(Trade_value_USD, na.rm = TRUE),  .groups = "drop_last"    ) %>%
  group_by(hs_section) %>%
  mutate(  hs_sect_tot_Trade_value_USD_2017 = sum(Trade_value_USD_2017),
    weight_hs_sect = if_else( hs_sect_tot_Trade_value_USD_2017 > 0,   
                              Trade_value_USD_2017 / hs_sect_tot_Trade_value_USD_2017,    NA_real_ )) %>%
  ungroup()
US_ag <-left_join(US_ag,US_ag_2017_hs_sect )


############################################################################

# add Chen et al. estimations 
Chen <- read_csv("data/chen_NTB_tariff/hs2_agriculture_manufacturing_clean.csv")
names(Chen)


Chen <- Chen %>% select(-Country, - ISO3_Code) %>% rename(hs2 = HS2 , Chen_US_import_share = US_import_share,
                                                          diff_log_tariff_Chen = tau_tariff_CHN, 
                                                          diff_ln_AVE_chen = tau_NTB)
US_ag <- left_join(US_ag,Chen)
names(US_ag)

# create weights for Chen et al at sector level and HS section

# Ag sector level: create weight in trade (hs6/total US export)
# weights for hs6 to aggregate to sector level
US_ag_2017_hs2_chen <- US_ag %>%
  filter(year == 2017) %>%  group_by(hs2) %>%
  summarise(Trade_value_USD_2017 = sum(Trade_value_USD, na.rm = TRUE),.groups = "drop"  ) %>%
  mutate(  chen_tot_Trade_value_USD_2017 = sum(Trade_value_USD_2017, na.rm = TRUE),
    weight_sector_chen = if_else( chen_tot_Trade_value_USD_2017 > 0,Trade_value_USD_2017 / chen_tot_Trade_value_USD_2017,
      NA_real_    )  ) %>% select(-Trade_value_USD_2017 )
US_ag <-left_join(US_ag,US_ag_2017_hs2_chen )


# HS section level : weights for hs2 to aggregate to HS-section
US_ag_2017_hs_sect_chen <- US_ag %>%  
  filter(year == 2017) %>%
  group_by(hs_section, hs2) %>%
  summarise(Trade_value_USD_2017 = sum(Trade_value_USD, na.rm = TRUE),
    .groups = "drop_last"  ) %>%
  group_by(hs_section) %>%
  mutate( Chen_hs_sect_tot_Trade_value_USD_2017 = sum(Trade_value_USD_2017, na.rm = TRUE),
    weight_hs_sect_Chen = if_else(Chen_hs_sect_tot_Trade_value_USD_2017 > 0,Trade_value_USD_2017 / Chen_hs_sect_tot_Trade_value_USD_2017,
      NA_real_    )  ) %>%  ungroup() %>% select(-Trade_value_USD_2017 )
US_ag <-left_join(US_ag, US_ag_2017_hs_sect_chen )

################################################################################

US_ag <- US_ag %>% filter(year>2017)


################################################################################
# plot change in ln(1+AVE)
################################################################################

# sector level 

# trade weighted and simple average trade costs :
names(US_ag)

US_ag_w <- US_ag %>%  filter(sector == "Ag", year > 2017) %>%  group_by(year) %>%
  summarise( 
    w_FE        = weighted.mean(diff_ln_AVE_FE,        w = weight_sector, na.rm = TRUE),
    w_sf        = weighted.mean(diff_ln_AVE_u,         w = weight_sector, na.rm = TRUE),
    w_sf_tariff = weighted.mean(diff_ln_AVE_u_tariff,  w = weight_sector, na.rm = TRUE),
    w_chen      = weighted.mean(diff_ln_AVE_chen,      w = weight_sector_chen, na.rm = TRUE),
    w_tariff    = weighted.mean(diff_log_tariff_2017,  w = weight_sector, na.rm = TRUE),
    s_FE        = mean(diff_ln_AVE_FE,         na.rm = TRUE),
    s_sf        = mean(diff_ln_AVE_u,          na.rm = TRUE),
    s_sf_tariff = mean(diff_ln_AVE_u_tariff,   na.rm = TRUE),
    s_chen      = mean(diff_ln_AVE_chen,       na.rm = TRUE),
    s_tariff    = mean(diff_log_tariff_2017,   na.rm = TRUE)  ) %>% ungroup() %>%
  mutate( w_chen = mean(w_chen, na.rm=TRUE),    s_chen = mean(s_chen, na.rm = TRUE))


plot <- ggplot(subset(US_ag_w, year %in% c(2018,2019, 2020))) +
  geom_point(aes(x = year, y = w_FE,        color = "FE")) +
  geom_point(aes(x = year, y = w_sf,        color = "sf")) +
  geom_point(aes(x = year, y = w_sf_tariff, color = "sf_tariff")) +
  geom_point(aes(x = year, y = w_chen,      color = "Chen_et_al")) +
  geom_point(aes(x = year, y = w_tariff,    color = "tariff")) +
  scale_color_manual( values = c(
      "FE"         = "steelblue",   "sf"         = "darkorange",
      "sf_tariff"  = "firebrick",       "tariff"     = "darkgreen"    ),
      labels = c(  "FE"         = "FE",
                   "sf"         = "SF",
                   "sf_tariff"  = "Tariff-adjusted SF",  "tariff"     = "Tariff"    ),    name = "Variables"  ) +
  labs(title = "Weighted average Δ ln(1+AVE) for agricultural sector (relative to 2017)",
    x = "Date",   y = "Weighted Δ ln(1+AVE)"  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.spacing.x = unit(1.2, "lines"),   # ← MORE HORIZONTAL SPACE
    plot.title = element_text(size = 11, hjust = 0.5),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA) ,
    axis.text.x = element_text(size = 9), # Axis text (tick labels)
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 11), # Axis titles
    axis.title.y = element_text(size = 11),
    legend.text  = element_text(size = 10), # Legend text and title
    legend.title = element_text(size = 10)  )
plot
ggsave(filename = file.path(exp, "plot/", "Compare_ln_AVE_Ag_weight.png"),plot = plot, width = 8, height = 5, dpi = 300)


# simple average 
plot <- ggplot(subset(US_ag_w, year %in% c(2018,2019))) +
  geom_line(aes(x = date, y = s_FE,        color = "FE")) +
  geom_line(aes(x = date, y = s_sf,        color = "sf")) +
  geom_line(aes(x = date, y = s_sf_tariff, color = "sf_tariff")) +
  #geom_line(aes(x = date, y = s_chen,      color = "Chen_et_al")) +
  geom_line(aes(x = date, y = s_tariff,    color = "tariff")) +
  scale_color_manual(  values = c(  "FE"         = "steelblue",
      "sf"         = "darkorange","sf_tariff"  = "firebrick",    "tariff"     = "darkgreen"    ),
      labels = c(  "FE"         = "FE",
                   "sf"         = "SF",
                   "sf_tariff"  = "Tariff-adjusted SF", "tariff"     = "Tariff"    ),    name = "Variables"  ) +
  labs(  title = "Simple average Δ ln(1+AVE) for agricultural sector (relative to 2017)",
    x = "Date",    y = "Simple average Δ ln(1+AVE)"  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.spacing.x = unit(1.2, "lines"),   # ← MORE HORIZONTAL SPACE
    plot.title = element_text(size = 11, hjust = 0.5),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA) ,
    axis.text.x = element_text(size = 9), # Axis text (tick labels)
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 10), # Axis titles
    axis.title.y = element_text(size = 10),
    legend.text  = element_text(size = 9), # Legend text and title
    legend.title = element_text(size = 9)  )
plot
ggsave(filename = file.path(exp, "plot/", "Compare_ln_AVE_Ag_simple.png"),plot = plot, width = 8, height = 5, dpi = 300)



#############################################################
# HS section level 

names(US_ag)
US_ag_w_sect <- US_ag %>% filter(sector == "Ag", year > 2017) %>%
  group_by(year, hs_section) %>% summarise( 
    w_FE        = weighted.mean(diff_ln_AVE_FE,        w = weight_hs_sect,       na.rm = TRUE),
    w_sf        = weighted.mean(diff_ln_AVE_u,         w = weight_hs_sect,       na.rm = TRUE),
    w_sf_tariff = weighted.mean(diff_ln_AVE_u_tariff,  w = weight_hs_sect,       na.rm = TRUE),
    w_chen      = weighted.mean(diff_ln_AVE_chen,      w = weight_hs_sect_Chen,  na.rm = TRUE),
    w_tariff    = weighted.mean(diff_log_tariff_2017,  w = weight_hs_sect,       na.rm = TRUE),
    s_FE        = mean(diff_ln_AVE_FE,         na.rm = TRUE),
    s_sf        = mean(diff_ln_AVE_u,          na.rm = TRUE),
    s_sf_tariff = mean(diff_ln_AVE_u_tariff,   na.rm = TRUE),
    s_chen      = mean(diff_ln_AVE_chen,       na.rm = TRUE),
    s_tariff    = mean(diff_log_tariff_2017,   na.rm = TRUE),
    .groups = "drop"  ) %>%  group_by(hs_section) %>%
  mutate(  w_chen = mean(w_chen, na.rm = TRUE),   s_chen = mean(s_chen, na.rm = TRUE)  )



plot <- ggplot(subset(US_ag_w_sect, year %in% c(2018,2019))) +
  geom_point(aes(x = year, y = w_FE,        color = "FE")) +
  geom_point(aes(x = year, y = w_sf,        color = "sf")) +
  geom_point(aes(x = year, y = w_sf_tariff, color = "sf_tariff")) +
  # geom_line(aes(x = date, y = w_chen,      color = "Chen_et_al")) +
  geom_point(aes(x = year, y = w_tariff,    color = "tariff")) +
  scale_color_manual(  values = c(  "FE"         = "steelblue",
                                    "sf"         = "darkorange","sf_tariff"  = "firebrick", "tariff"     = "darkgreen"    ),
                       labels = c(  "FE"         = "FE",
                                    "sf"         = "SF",
                                    "sf_tariff"  = "Tariff-adjusted SF",  "tariff"     = "Tariff"    ),    name = "Variables"  ) +
  facet_wrap(~hs_section)+
  labs(  title = "Weighted average Δ ln(1+AVE) for agricultural sector by HS section(relative to 2017) ",
         x = "Date",
         y = "Weighted Δ ln(1+AVE)"  ) +
  theme_minimal(base_size = 14)+
  theme(
    panel.spacing.x = unit(2, "lines"),   # ← MORE HORIZONTAL SPACE
    plot.title = element_text(size = 11, hjust = 0.5),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA) ,
    axis.text.x = element_text(size = 9), # Axis text (tick labels)
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 10), # Axis titles
    axis.title.y = element_text(size = 10),
    legend.text  = element_text(size = 9), # Legend text and title
    legend.title = element_text(size = 9)  )
plot
ggsave(filename = file.path(exp, "plot/", "Compare_ln_AVE_Ag_hs_sect_weight.png"),plot = plot, width = 9, height = 5, dpi = 300)


plot <- ggplot(subset(US_ag_w_sect, year %in% c(2018,2019))) +
  geom_line(aes(x = date, y = s_FE,        color = "FE")) +
  geom_line(aes(x = date, y = s_sf,        color = "sf")) +
  geom_line(aes(x = date, y = s_sf_tariff, color = "sf_tariff")) +
  # geom_line(aes(x = date, y = s_chen,      color = "Chen_et_al")) +
  geom_line(aes(x = date, y = s_tariff,    color = "tariff")) +
  scale_color_manual(  values = c(  "FE"         = "steelblue",
                                    "sf"         = "darkorange","sf_tariff"  = "firebrick", "tariff"     = "darkgreen"    ),
                       labels = c(  "FE"         = "FE",
                                    "sf"         = "SF",
                                    "sf_tariff"  = "Tariff-adjusted SF",  "tariff"     = "Tariff"    ),    name = "Variables"  ) +
  facet_wrap(~hs_section)+
  labs(  title = "Simple average Δ ln(1+AVE) for agricultural sector by HS section (relative to 2017) ",
         x = "Date",
         y = "Simple average Δ ln(1+AVE)"  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.spacing.x = unit(1.2, "lines"),   # ← MORE HORIZONTAL SPACE
    plot.title = element_text(size = 11, hjust = 0.5),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA) ,
    axis.text.x = element_text(size = 9), # Axis text (tick labels)
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 10), # Axis titles
    axis.title.y = element_text(size = 10),
    legend.text  = element_text(size = 9), # Legend text and title
    legend.title = element_text(size = 9)  )
plot
ggsave(filename = file.path(exp, "plot/", "Compare_ln_AVE_Ag_hs_sect_simple.png"),plot = plot, width = 8, height = 5, dpi = 300)

###################################################
# at HS 2 level

names(US_ag)
US_ag_w_hs2 <- US_ag %>% filter(sector == "Ag", year > 2017) %>%
  group_by(date, hs2) %>% summarise( 
    w_FE        = weighted.mean(diff_ln_AVE_FE,        w = weight_hs_sect,       na.rm = TRUE),
    w_sf        = weighted.mean(diff_ln_AVE_u,         w = weight_hs_sect,       na.rm = TRUE),
    w_sf_tariff = weighted.mean(diff_ln_AVE_u_tariff,  w = weight_hs_sect,       na.rm = TRUE),
    w_chen      = weighted.mean(diff_ln_AVE_chen,      w = weight_hs_sect_Chen,  na.rm = TRUE),
    w_tariff    = weighted.mean(diff_log_tariff_2017,  w = weight_hs_sect,       na.rm = TRUE),
    s_FE        = mean(diff_ln_AVE_FE,         na.rm = TRUE),
    s_sf        = mean(diff_ln_AVE_u,          na.rm = TRUE),
    s_sf_tariff = mean(diff_ln_AVE_u_tariff,   na.rm = TRUE),
    s_chen      = mean(diff_ln_AVE_chen,       na.rm = TRUE),
    s_tariff    = mean(diff_log_tariff_2017,   na.rm = TRUE),
    .groups = "drop"  ) %>%  group_by(hs2) %>%
  mutate(  w_chen = mean(w_chen, na.rm = TRUE),   s_chen = mean(s_chen, na.rm = TRUE)  )

unique(US_ag_w_hs2$hs2)
HS <- c( 2, 8,10, 12,15, 16, 22,23)
HS <- c(1:9)
HS <- c(11:23)

hs2_names <- c(
  "2"  = "Meat & edible meat offal (HS 02)",
  "8"  = "Edible fruit & nuts (HS 08)",
  "10" = "Cereals (HS 10)",
  "12" = "Oil seeds & oleaginous fruits (HS 12)",
  "15" = "Animal & veg fats/oils (HS 15)",
  "16" = "Prep. of meat, fish etc. (HS 16)",
  "22" = "Beverages, spirits & vinegar (HS 22)",
  "23" = "Residues & animal feed (HS 23)")
plot <- ggplot(subset(US_ag_w_hs2, hs2 %in% HS)) +
  geom_line(aes(x = date, y = w_FE,        color = "FE")) +
  geom_line(aes(x = date, y = w_sf,        color = "sf")) +
  geom_line(aes(x = date, y = w_sf_tariff, color = "sf_tariff")) +
  geom_line(aes(x = date, y = w_tariff,    color = "tariff")) +
  scale_color_manual( values = c(    "FE"        = "steelblue",
      "sf"        = "darkorange",      "sf_tariff" = "firebrick",      "tariff"    = "darkgreen"    ),    
      labels = c(     "FE"        = "FE",      "sf"        = "SF",      "sf_tariff" = "Tariff-adjusted SF",
      "tariff"    = "Tariff"    ),    name = "Variables"  ) +
  facet_wrap(  ~ hs2,  scales   = "free_y",  labeller = as_labeller(hs2_names)  ) +
  labs( title = "Weighted average Δ ln(1+AVE) for agricultural sector by HS section (relative to 2017)",
    x = "Date",    y = "Weighted Δ ln(1+AVE)"  ) +
  theme_minimal(base_size = 14) +
  theme(panel.spacing.x = unit(2, "lines"),
    plot.title      = element_text(size = 11, hjust = 0.5),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    axis.text.x     = element_text(size = 9),
    axis.text.y     = element_text(size = 9),
    axis.title.x    = element_text(size = 10),
    axis.title.y    = element_text(size = 10),
    legend.text     = element_text(size = 9),
    legend.title    = element_text(size = 9)  )

plot
ggsave(filename = file.path(exp, "plot/", "Compare_ln_AVE_Ag_hs2_weighted.png"),plot = plot, width = 8, height = 5, dpi = 300)

names(US_ag_w_hs2)
summary <- US_ag_w_hs2 %>% group_by(hs2) %>% summarise(
  w_FE = mean(w_FE, na.rm = TRUE), 
  w_sf = mean(w_sf, na.rm = TRUE), 
  w_sf_tariff = mean(w_sf_tariff, na.rm = TRUE), 
  w_tariff = mean(w_tariff, na.rm = TRUE))

hs2_desc <- data.frame(
  hs2 = sprintf("%02d", 1:24),
  description = c(
    "Live animals",
    "Meat and edible meat offal",
    "Fish and seafood",
    "Dairy, eggs, honey",
    "Other animal products",
    "Live trees and plants",
    "Vegetables",
    "Fruits and nuts",
    "Coffee, tea, spices",
    "Cereals",
    "Milling products; malt; starch",
    "Oil seeds and oleaginous fruits",
    "Gums, resins, vegetable saps",
    "Other vegetable materials",
    "Animal & vegetable fats and oils",
    "Prepared meat, fish",
    "Sugar and sugar confectionery",
    "Cocoa and cocoa preparations",
    "Preparations of cereals & bakery",
    "Processed vegetables, fruit, nuts",
    "Misc. edible preparations",
    "Beverages",
    "Animal feed, residues",
    "Tobacco and substitutes"  ),  stringsAsFactors = FALSE)
hs2_desc$hs2 <- as.numeric(hs2_desc$hs2)
summary <- summary %>%  left_join(hs2_desc, by = "hs2")
write_csv(summary, file.path(exp,  "hs2_summary.csv"))

################################################################################
# with Chen et al comparison
################################################################################
  

plot <- ggplot(subset(US_ag_w, year %in% c(2018,2019))) +
  geom_line(aes(x = date, y = w_FE,        color = "FE")) +
  geom_line(aes(x = date, y = w_sf,        color = "sf")) +
  geom_line(aes(x = date, y = w_sf_tariff, color = "sf_tariff")) +
  geom_line(aes(x = date, y = w_chen,      color = "Chen_et_al")) +
  geom_line(aes(x = date, y = w_tariff,    color = "tariff")) +
  scale_color_manual( values = c(
    "FE"         = "steelblue",   "sf"         = "darkorange",
    "sf_tariff"  = "firebrick",    "Chen_et_al" = "purple",      # ← Added color
    "tariff"     = "darkgreen"    ),
    labels = c(  "FE"         = "FE",
                 "sf"         = "SF",
                 "sf_tariff"  = "Tariff-adjusted SF",
                 "Chen_et_al" = "Chen et al.",   "tariff"     = "Tariff"    ),    name = "Variables"  ) +
  labs(title = "Weighted average Δ ln(1+AVE) for agricultural sector (relative to 2017)",
       x = "Date",   y = "Weighted Δ ln(1+AVE)"  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.spacing.x = unit(1.2, "lines"),   # ← MORE HORIZONTAL SPACE
    plot.title = element_text(size = 11, hjust = 0.5),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA) ,
    axis.text.x = element_text(size = 9), # Axis text (tick labels)
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 11), # Axis titles
    axis.title.y = element_text(size = 11),
    legend.text  = element_text(size = 10), # Legend text and title
    legend.title = element_text(size = 10)  )
plot
ggsave(filename = file.path(exp, "plot/", "Compare_ln_AVE_Ag_weight_chen.png"),plot = plot, width = 8, height = 5, dpi = 300)


# simple average 
plot <- ggplot(US_ag_w) +
  geom_line(aes(x = date, y = s_FE,        color = "FE")) +
  geom_line(aes(x = date, y = s_sf,        color = "sf")) +
  geom_line(aes(x = date, y = s_sf_tariff, color = "sf_tariff")) +
  geom_line(aes(x = date, y = s_chen,      color = "Chen_et_al")) +
  geom_line(aes(x = date, y = s_tariff,    color = "tariff")) +
  scale_color_manual(  values = c(  "FE"         = "steelblue",
                                    "sf"         = "darkorange","sf_tariff"  = "firebrick",
                                    "Chen_et_al" = "purple",      "tariff"     = "darkgreen"    ),
                       labels = c(  "FE"         = "FE",
                                    "sf"         = "SF",
                                    "sf_tariff"  = "Tariff-adjusted SF",
                                    "Chen_et_al" = "Chen et al.",   "tariff"     = "Tariff"    ),    name = "Variables"  ) +
  labs(  title = "Simple average Δ ln(1+AVE) for agricultural sector (relative to 2017)",
         x = "Date",    y = "Simple average Δ ln(1+AVE)"  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.spacing.x = unit(1.2, "lines"),   # ← MORE HORIZONTAL SPACE
    plot.title = element_text(size = 11, hjust = 0.5),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA) ,
    axis.text.x = element_text(size = 9), # Axis text (tick labels)
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 10), # Axis titles
    axis.title.y = element_text(size = 10),
    legend.text  = element_text(size = 9), # Legend text and title
    legend.title = element_text(size = 9)  )
plot
ggsave(filename = file.path(exp, "plot/", "Compare_ln_AVE_Ag_simple_chen.png"),plot = plot, width = 8, height = 5, dpi = 300)



#############################################################
# with Chen et al comparison

# HS section level 


plot <- ggplot(US_ag_w_sect) +
  geom_line(aes(x = date, y = w_FE,        color = "FE")) +
  geom_line(aes(x = date, y = w_sf,        color = "sf")) +
  geom_line(aes(x = date, y = w_sf_tariff, color = "sf_tariff")) +
  geom_line(aes(x = date, y = w_chen,      color = "Chen_et_al")) +
  geom_line(aes(x = date, y = w_tariff,    color = "tariff")) +
  scale_color_manual(  values = c(  "FE"         = "steelblue",
                                    "sf"         = "darkorange","sf_tariff"  = "firebrick",
                                    "Chen_et_al" = "purple",      "tariff"     = "darkgreen"    ),
                       labels = c(  "FE"         = "FE",
                                    "sf"         = "SF",
                                    "sf_tariff"  = "Tariff-adjusted SF",
                                    "Chen_et_al" = "Chen et al.",   "tariff"     = "Tariff"    ),    name = "Variables"  ) +
  facet_wrap(~hs_section)+
  labs(  title = "Weighted average Δ ln(1+AVE) for agricultural sector by HS section(relative to 2017) ",
         x = "Date",
         y = "Weighted Δ ln(1+AVE)"  ) +
  theme_minimal(base_size = 14)+
  theme(
    panel.spacing.x = unit(2, "lines"),   # ← MORE HORIZONTAL SPACE
    plot.title = element_text(size = 11, hjust = 0.5),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA) ,
    axis.text.x = element_text(size = 9), # Axis text (tick labels)
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 10), # Axis titles
    axis.title.y = element_text(size = 10),
    legend.text  = element_text(size = 9), # Legend text and title
    legend.title = element_text(size = 9)  )
plot
ggsave(filename = file.path(exp, "plot/", "Compare_ln_AVE_Ag_hs_sect_weight_chen.png"),plot = plot, width = 9, height = 5, dpi = 300)


plot <- ggplot(US_ag_w_sect) +
  geom_line(aes(x = date, y = s_FE,        color = "FE")) +
  geom_line(aes(x = date, y = s_sf,        color = "sf")) +
  geom_line(aes(x = date, y = s_sf_tariff, color = "sf_tariff")) +
  geom_line(aes(x = date, y = s_chen,      color = "Chen_et_al")) +
  geom_line(aes(x = date, y = s_tariff,    color = "tariff")) +
  scale_color_manual(  values = c(  "FE"         = "steelblue",
                                    "sf"         = "darkorange","sf_tariff"  = "firebrick",
                                    "Chen_et_al" = "purple",      "tariff"     = "darkgreen"    ),
                       labels = c(  "FE"         = "FE",
                                    "sf"         = "SF",
                                    "sf_tariff"  = "Tariff-adjusted SF",
                                    "Chen_et_al" = "Chen et al.",   "tariff"     = "Tariff"    ),    name = "Variables"  ) +
  facet_wrap(~hs_section)+
  labs(  title = "Simple average Δ ln(1+AVE) for agricultural sector by HS section (relative to 2017) ",
         x = "Date",
         y = "Simple average Δ ln(1+AVE)"  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.spacing.x = unit(1.2, "lines"),   # ← MORE HORIZONTAL SPACE
    plot.title = element_text(size = 11, hjust = 0.5),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA) ,
    axis.text.x = element_text(size = 9), # Axis text (tick labels)
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 10), # Axis titles
    axis.title.y = element_text(size = 10),
    legend.text  = element_text(size = 9), # Legend text and title
    legend.title = element_text(size = 9)  )
plot
ggsave(filename = file.path(exp, "plot/", "Compare_ln_AVE_Ag_hs_sect_simple_chen.png"),plot = plot, width = 8, height = 5, dpi = 300)




################################################################################

# correlations of change in ln(1+AVE)

################################################################################
names(US_ag)

# If we look at correlation between FE and efficiency 
names(US)
US_changes_2017 <- US %>% filter(year %in% c(2018:2020))




plot <- ggplot(US, aes(x = diff_ln_AVE_FE, y = diff_ln_AVE_u_tariff)) +
  geom_point(alpha = 0.15, size = 1) +
  geom_abline(intercept = 0, slope = 1, color = "red",
              linewidth = 1, linetype = "dotted") +
  theme_minimal() +
  labs( title = "Correlation plot of Δ ln(1 + AVE) from FE vs Tariff-adjusted SF",
    x = "Δ ln(1 + AVE) FE",    y = "Δ ln(1 + AVE) Tariff-adjusted SF"  ) +
  theme(plot.title      = element_text(size = 11, hjust = 0.5),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    axis.text.x     = element_text(size = 9),
    axis.text.y     = element_text(size = 9),
    axis.title.x    = element_text(size = 10),
    axis.title.y    = element_text(size = 10)  )
plot
ggsave(filename = file.path(exp, "plot/", "Corrlation_FE_SF.png"),plot = plot, width = 8, height = 5, dpi = 300)


plot <- ggplot(US_ag, aes(x = diff_ln_AVE_u, y = diff_ln_AVE_u_tariff)) +
  geom_point(alpha = 0.20, size = 1) +
  geom_abline(intercept = 0, slope = 1, color = "red", linewidth = 1,linetype = "dotted", ) +
  theme_minimal() +
  labs(
    title = "Plot of US Δ ln(1+AVE) obtained from SF and Tariff-adjusted SF",
    x = "SF",
    y = "Tariff-adjusted SF"  ) +
  theme(
    plot.title = element_text(size = 12, hjust = 0.5),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA) ,
    axis.text.x = element_text(size = 10), # Axis text (tick labels)
    axis.text.y = element_text(size = 10),
    axis.title.x = element_text(size = 12), # Axis titles
    axis.title.y = element_text(size = 12) )
plot
ggsave(filename = file.path(exp, "plot/", "Plot_stochastic_corr.png"),plot = plot, width = 8, height = 5, dpi = 300)

################################################################################

# Get correlation plot

US_ag1 <- US_ag %>% filter(year %in% c(2018,2019))

vars <- US_ag1[, c("diff_ln_AVE_chen", "diff_ln_AVE_FE", "diff_ln_AVE_u", "diff_ln_AVE_u_tariff")]

vars <- vars %>%  rename(  Chen_et_al = diff_ln_AVE_chen,  FE = diff_ln_AVE_FE,
    SF = diff_ln_AVE_u,  SF_tariff  = diff_ln_AVE_u_tariff )

cor_mat <- cor(vars, use = "complete.obs")

# open PNG device
png(paste0(exp, "plot/corrplot.png"), width = 1200, height = 1000, res = 150)

corrplot(cor_mat, method = "circle", addCoef.col = "black")

dev.off()
 