



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

# Load FE estimates 
fe <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/output/summary/prelim_reg/gravity_pois_FE.csv")
res <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/output/summary/prelim_reg/gravity_pois_residual.csv")
sf <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/output/stochastic/sfaR_efficiency_average_merged.csv")

################################################################################

# merge the data 


# select data we want 
names(fe)
names(res)
names(sf)


fe <- fe %>% select(year,month,hs2, hs4, hs6_H5, ExporterISO3, ImporterISO3,Applied_tariff, fe_id, FE)
res <- res %>% select(year,month,hs2, hs4, hs6_H5, ExporterISO3, ImporterISO3,Applied_tariff, residual)
sf <- sf %>% select(year,month,hs2, hs4, hs6_H5, ExporterISO3, ImporterISO3,Applied_tariff, u,teJLMS,
                    u_tariff , teJLMS_tariff)

# merge all data 
library(dplyr)

# Full join all three datasets
dta <- fe %>%  full_join(res, by = c("year", "month", "hs2", "hs4", "hs6_H5",
                        "ExporterISO3", "ImporterISO3", "Applied_tariff")) %>%
  full_join(sf,  by = c("year", "month", "hs2", "hs4", "hs6_H5",
                        "ExporterISO3", "ImporterISO3", "Applied_tariff"))
colSums(is.na(dta))

# drop Nas
dta <- dta %>%  filter(!if_all(c(FE, residual, teJLMS), is.na))
write_csv(dta , paste0(exp, "estimates_reduced_form.csv"))


dta <- read_csv( paste0(exp, "estimates_reduced_form.csv"))
################################################################################
# # add some product level variables 
################################################################################

# check values:
summary(dta$residual)
summary(dta$FE)
summary(dta$u)

# add HS section for each HS6 product code 
class(dta$hs6_H5)
unique(nchar(dta$hs6_H5))
unique(dta$hs2)


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


################################################################################
# If we want to create a benchmark for each values 
# we take the max value of the FE and u among all exporter for a specific product, month, year

dta <- dta %>% group_by(year, month, hs6_H5) %>%
  mutate( FE_benchmark_exporter = {
    m <- max(FE, na.rm = TRUE)
    ifelse(is.infinite(m), NA_real_, m)   
    },
    u_benchmark_exporter = {
      m <- min(u, na.rm = TRUE)
      ifelse(is.infinite(m), NA_real_, m)   
      }  ) %>%
  ungroup()
summary(dta$FE_benchmark_exporter)
summary(dta$u_benchmark_exporter)

# if we want to use pre 2017 asa bechnmark 
dta <- dta %>% group_by(year, month, exporterISO3,  hs6_H5) %>% mutate(
  FE_pre_2017 = mean(FE where year %in%c(2015, 2017), na.rm = TRUE)
)


################################################################################

# add elasticities from chen et al.

dta <- dta %>% mutate(elastcities = case_when(sector == "Ag" ~  3 ,
                          sector == "Manu" ~ 1.97,
                          sector == "Other" ~ 5 ))

dta <- dta %>% arrange(year, month, hs6_H5)

################################################################################

# create adjusted values of efficencies and FE estimates
dta <- dta %>%
  mutate(
    diff_FE_benchmark = if_else(
      !is.na(FE) & !is.na(FE_benchmark_exporter) & FE == FE_benchmark_exporter,
      NA_real_,  FE - FE_benchmark_exporter    ),
    
    diff_u_benchmark = if_else(
      !is.na(u) & !is.na(u_benchmark_exporter) & u == u_benchmark_exporter,
      NA_real_,      -u + u_benchmark_exporter    )  )
summary(dta$diff_FE_benchmark)
summary(dta$diff_u_benchmark)

################################################################################

# estimate ln(1+T)

dta <- dta %>%
  mutate(
    ln_AVE_FE = (1/(1-elastcities))*diff_FE_benchmark,
    ln_AVE_u = (1/(1-elastcities))*diff_u_benchmark  )
summary(dta$ln_AVE_FE)
summary(dta$ln_AVE_u)

################################################################################
US <- dta  %>% filter(ExporterISO3 == "USA")
US <- US %>%  mutate(date = as.Date(paste(year, month, "01", sep = "-"))  ) 
summary(US$)

write_csv(US, paste0(exp, "US_ln_NTMs.csv"))
US <- read_csv(paste0(exp, "US_ln_NTMs.csv"))

plot <- ggplot(subset(US, sector == "Ag")) +
  geom_smooth(aes(x = date, y = ln_AVE_FE, color = "FE"), se = TRUE) +
  geom_smooth(aes(x = date, y = ln_AVE_u,  color = "u"),  se = TRUE) +
  scale_color_manual(
    name = "Series",
    values = c("FE" = "steelblue", "u" = "darkorange")  ) +
  labs(    title = "Average estimated ln(1+AVE) using FE and stochastic frontier: agricultural sector",   x = "Date",    y = "ln(1+AVE)"  ) +
  theme_minimal(base_size = 14) +
  theme(   plot.title = element_text(size = 12),   
           panel.background = element_rect(fill = "white", color = NA),
            plot.background  = element_rect(fill = "white", color = NA)  )

plot
ggsave(filename = file.path(exp, "plot/", "Compare_AVE_Ag.png"),plot = plot, width = 8, height = 5, dpi = 300)

plot <- ggplot(subset(US, sector == "Ag")) +
  geom_smooth(aes(x = date, y = ln_AVE_u, color = "u"), se = TRUE) +
  facet_wrap(~hs_section) +
  scale_color_manual(  name = "Series",
    values = c("u" = "darkorange")  ) +
  labs(    title = "Average estimated ln(1+AVE) using stochastic frontier: agricultural sector",   x = "Date",    y = "ln(1+AVE)"  )  +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 12),   
    panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA)  )

plot
ggsave(filename = file.path(exp, "plot/", "stochastic_AVE_Ag.png"),plot = plot, width = 8, height = 5, dpi = 300)

plot <- ggplot(subset(US, sector == "Ag")) +
  geom_smooth(aes(x = date, y = ln_AVE_FE, color = "FE"), se = TRUE) +
  facet_wrap(~hs_section) +
  scale_color_manual(  name = "Series",
                       values = c("FE" = "steelblue")  ) +
  labs(    title = "Average estimated ln(1+AVE) using stochastic frontier: agricultural sector",   x = "Date",    y = "ln(1+AVE)"  )  +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(size = 12),   
    panel.background = element_rect(fill = "white", color = NA),
        plot.background  = element_rect(fill = "white", color = NA)  )

plot
ggsave(filename = file.path(exp, "plot/", "FE_AVE_Ag.png"),plot = plot, width = 8, height = 5, dpi = 300)




plot <- ggplot(subset(US, sector == "Manu")) +
  geom_smooth(aes(x = date, y = ln_AVE_FE, color = "FE"), se = TRUE) +
  geom_smooth(aes(x = date, y = ln_AVE_u,  color = "u"),  se = TRUE) +
  scale_color_manual(  name = "Series",
    values = c("FE" = "steelblue", "u" = "darkorange")  ) +
  labs(    title = "Average estimated ln(1+AVE) using stochastic frontier: manufacturing sector",   x = "Date",    y = "ln(1+AVE)"  ) +
  theme_minimal(base_size = 14) +
  theme(   plot.title = element_text(size = 12),   
    panel.background = element_rect(fill = "white", color = NA),
            plot.background  = element_rect(fill = "white", color = NA)  )

plot
ggsave(filename = file.path(exp, "plot/", "Compare_AVE_Manu.png"),plot = plot, width = 8, height = 5, dpi = 300)




plot <- ggplot(subset(US, sector == "Manu")) +
 #  geom_smooth(aes(x = date, y = ln_AVE_FE, color = "FE"), se = TRUE) +
  geom_smooth(aes(x = date, y = ln_AVE_u,  color = "u"),  se = TRUE) +
  scale_color_manual(  name = "Series",
                       values = c("FE" = "steelblue", "u" = "darkorange")  ) +
  labs(    title = "Gravity model US FE value",   x = "Date",    y = "FE value"  ) +
  theme_minimal(base_size = 14) +
  theme(    plot.title = element_text(size = 12),   
    panel.background = element_rect(fill = "white", color = NA),
            plot.background  = element_rect(fill = "white", color = NA)  )

plot

plot <- ggplot(subset(US, sector == "Manu")) +
  geom_smooth(aes(x = date, y = ln_AVE_u, color = "u"), se = TRUE) +
  facet_wrap(~hs_section) +
  scale_color_manual(
    name = "Series",
    values = c("u" = "darkorange")
  ) +
  labs(
    title = "Gravity model US FE value",
    x = "Date",
    y = "FE value"
  ) +
  theme_minimal(base_size = 14) +
  theme( plot.title = element_text(size = 12),   
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA)
  )

plot



 