
################################################################################
#                     Stochastic frontier regression anlaysis


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

exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/summary/prelim_reg"

################################################################################
# 1) Load data 
################################################################################


dta <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_gravity/dta_CHN_gravity_3.csv")
names(dta)
colSums(is.na(dta))

################################################################################

# get HS2 and HS 4 level 
class(dta$hs6_H5)
unique(nchar(dta$hs6_H5))
dta <- dta %>%  mutate(hs2 = substr( hs6_H5 ,1,2),
         hs4 = substr( hs6_H5 ,1,4))
length(unique((dta$hs2)))
length(unique((dta$hs4)))



# look at diff in data 
# create variable for change in trade values and other variables compared to baseline years
names(dta)
dta_change <- dta %>%
  group_by(month, ExporterISO3, hs6_H5) %>%
  mutate(
    Trade_value_USD_baseline =  mean(Trade_value_USD[year %in% 2016:2017], na.rm = TRUE),
    Change_Trade_value_USD =  Trade_value_USD - Trade_value_USD_baseline,
    Applied_tariff_baseline =  mean(Applied_tariff[year %in% 2016:2017], na.rm = TRUE),
    Change_Applied_tariff =  Applied_tariff - Applied_tariff_baseline,
    
    Exporter_GDP_baseline = mean(Exporter_GDP_current_USD[year %in% 2016:2017], na.rm = TRUE),
    Change_Exporter_GDP =  Exporter_GDP_current_USD - Exporter_GDP_baseline,
    Exporter_GDP_perCap_baseline = mean(Exporter_GDPperCap_current_USD[year %in% 2016:2017], na.rm = TRUE),
    Change_Exporter_GDP_perCap =  Exporter_GDPperCap_current_USD - Exporter_GDP_perCap_baseline,
    Exporter_ExchangeRate_baseline = mean(Exporter_Exchange_rate_LCU_per_USD[year %in% 2016:2017], na.rm = TRUE),
    Change_Exporter_ExchangeRate =  Exporter_Exchange_rate_LCU_per_USD - Exporter_ExchangeRate_baseline,
    Exporter_Ag_land_K2_baseline = mean(Exporter_Ag_land_K2[year %in% 2016:2017], na.rm = TRUE),
    Change_Ag_land_K2 =  Exporter_Ag_land_K2 - Exporter_Ag_land_K2_baseline    ) %>%
  ungroup() %>% filter(year %in% c( 2018,2019, 2020))
names(dta_change)
summary(dta_change$Change_Trade_value_USD)
table(dta_change$year)

write_csv(dta_change,"/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_gravity/dta_CHN_gravity_change_3.csv" )

################################################################################
# try to regress a notmal ppml and get residual over time ? 
################################################################################




# get unique HS2 to pool it
unique_HS2 <- unique(dta$hs2)

HS_val <- unique_HS2[1]

# Subset the data for the current HS value
sub_dta <- subset(dta, hs2 == HS_val)
names(sub_dta)


################################################################################
# reg <- feglm(
#     fml = Trade_value_USD ~ contig + dist + comlang_off + Colonial_ties + 
#       Importer_GDP + Exporter_wto + Exporter_eu + Exporter_GDP_current_USD + 
#       Exporter_Gross_Cap_formation_current_USD + Exporter_Ag_land_K2 + rta + fta_and_eia+
#       Exporter_Exchange_rate_LCU_per_USD + Applied_tariff + factor(month)*factor(ExporterISO3)*factor(hs4),
#     data = sub_dta,
#     family = quasipoisson(link = "log"))
#   reg
  
  
# filter if have 0s in trade values for all product at HS2 and a specific partner
sub_dta1 <- sub_dta %>%
  group_by(ExporterISO3) %>%
  filter(any(Trade_value_USD != 0)) %>%   # keep only groups where at least ONE value is non-zero
  ungroup()


library(fixest)
reg <- fepois(
  Trade_value_USD ~ 
    contig + dist + comlang_off + Colonial_ties + 
    Importer_GDP + Exporter_wto + Exporter_eu + Exporter_GDP_current_USD + 
    Exporter_Gross_Cap_formation_current_USD + Exporter_Ag_land_K2 + 
    Exporter_Exchange_rate_LCU_per_USD + log(1+Applied_tariff)  | year + month^hs6_H5 + ExporterISO3^hs6_H5,
  data   = sub_dta1,  vcov   = ~ ExporterISO3   )
reg

reg <- fepois(
  Trade_value_USD ~ 
    contig + dist + comlang_off + Colonial_ties + 
    Importer_GDP + Exporter_wto + Exporter_eu + Exporter_GDP_current_USD + 
    Exporter_Gross_Cap_formation_current_USD + Exporter_Ag_land_K2 + 
    Exporter_Exchange_rate_LCU_per_USD + log(1+Applied_tariff)  | year + month^ExporterISO3^hs6_H5,
  data   = sub_dta1,  vcov   = ~ ExporterISO3   )
reg
idx <- obs(reg)
resid_vec <- resid(reg)
sub_dta1$resid_change <- NA_real_
sub_dta1$resid_change[idx] <- resid_vec

sub_dta1 <- sub_dta1 %>%mutate(
  country_res = if_else(ExporterISO3 == "USA", "USA", "ROW"))
table(sub_dta1$country_res)


ggplot(subset(sub_dta1, year %in% c(2017:2020)),aes(x = resid_change, fill = factor(year))) +
  geom_density(alpha = 0.35) +
  facet_wrap(~ country_res, scales = "free") +
  # coord_cartesian(xlim = c(-1500000, 1500000)) +   # ⬅️ truncate x-axis here
  labs(  title = "Density of Residual Changes by Country Group",
    x = "Residual Change",
    y = "Density",
    fill = "Year"  ) +
  theme_minimal(base_size = 14)



USA <- sub_dta1 %>% filter(ExporterISO3 == "USA")
USA <- USA %>%  mutate(    date = as.Date(paste(year, month, "01", sep = "-"))  )

plot <-  ggplot(subset(USA, year %in% c(2018:2020)), aes(x = date, y = resid_change)) +
  geom_hline(yintercept = 0, color = "red", linetype = "dotted", size = 1) +
  geom_smooth(color = "blue", size = 1.1) +
  labs(  title = "Change in Residuals for USA Over Time",
         x = "time",
         y = "Residual Change"  ) +
  theme_minimal(base_size = 14)
plot

##############################################################################
# 2) looking at changes in trade: compared to a baseline 
##############################################################################


# get unique HS2 to pool it
unique_HS2 <- unique(dta$hs2)

HS_val <- unique_HS2[1]

# Subset the data for the current HS value
sub_dta_change <- subset(dta_change, hs2 == HS_val)
names(sub_dta_change)


# filter if have 0s in trade values for all product at HS2 and a specific partner
sub_dta_change1 <- sub_dta_change %>%
  group_by(ExporterISO3) %>%
  filter(any(Change_Trade_value_USD != 0)) %>%   # keep only groups where at least ONE value is non-zero
  ungroup()


# 1. Add a unique row id to your data
sub_dta_change1 <- sub_dta_change1 %>%
  mutate(row_id = row_number())

# 2. Define the variables that must be non-missing for the regression
rhs_vars <- c(
  "Change_Trade_value_USD",
  "Change_Applied_tariff",
  "Change_Exporter_GDP",
  "Change_Exporter_GDP_perCap",
  "Change_Exporter_ExchangeRate",
  "Change_Ag_land_K2"
)

# 3. Create the NA-free dataset used in the regression
sub_clean <- sub_dta_change1 %>%
  filter(if_all(all_of(rhs_vars), ~ !is.na(.x)))

# 4. Run the regression on the cleaned data
reg_change <- feols(
  Change_Trade_value_USD ~ Change_Applied_tariff + Change_Exporter_GDP +
    Change_Exporter_GDP_perCap + Change_Exporter_ExchangeRate + Change_Ag_land_K2,
  data = sub_clean, vcov = ~ ExporterISO3)

reg_change <- feols(
  Change_Trade_value_USD ~ Change_Applied_tariff + Change_Exporter_GDP +
    Change_Exporter_GDP_perCap + Change_Exporter_ExchangeRate + Change_Ag_land_K2| year,
  data = sub_clean, vcov = ~ ExporterISO3)


# (optional) inspect results
print(reg_change)

# 5. Get residuals and attach them to the cleaned data
sub_clean <- sub_clean %>%  mutate(resid_change = resid(reg_change))

# 6. Merge residuals back to the original data (rows with NA in RHS get NA residuals)
sub_dta_change1 <- sub_dta_change1 %>%
  left_join(sub_clean[, c("row_id", "resid_change")], by = "row_id")
table(sub_dta_change1$year)
##############################################################################
# compare residual distribution between countries

sub_dta_change1 <- sub_dta_change1 %>%mutate(
  country_res = if_else(ExporterISO3 == "USA", "USA", "ROW"))
table(sub_dta_change1$country_res)


ggplot(sub_dta_change1, aes(x = resid_change, color = country_res, fill = country_res)) +
  geom_density(alpha = 0.35, linewidth = 1.1) +
  labs(
    title = "Density of Residual Changes by Country Group",
    x = "Residual Change",
    y = "Density",
    color = "Country Group",
    fill = "Country Group"  ) +
  theme_minimal(base_size = 14)


ggplot( subset(sub_dta_change1, country_res == "USA"),aes(x = resid_change, color = factor(year), fill = factor(year))) +
  geom_density(alpha = 0.35, linewidth = 1.1) +
  coord_cartesian(xlim = c(-2000000, 2000000)) +   # ⬅️ truncate x-axis here
  labs(    title = "Density of Residual Changes for USA by Year",    x = "Residual Change",
    y = "Density",    color = "Year",    fill = "Year"  ) +
  theme_minimal(base_size = 14)



ggplot( subset(sub_dta_change1, country_res == "USA"),aes(x = resid_change)) +
  geom_density(alpha = 0.35, linewidth = 1.1) +
  facet_wrap(~year, scale = "free_y")+
  coord_cartesian(xlim = c(-1500000, 1500000)) +   # ⬅️ truncate x-axis here
  labs(    title = "Density of Residual Changes for USA by Year",    x = "Residual Change",
           y = "Density",    color = "Year",    fill = "Year"  ) +
  theme_minimal(base_size = 14)

ggplot(sub_dta_change1, aes(x = resid_change, fill = factor(year))) +
  geom_density(alpha = 0.35) +
  facet_wrap(~ country_res, scales = "free") +
  coord_cartesian(xlim = c(-1500000, 1500000)) +   # ⬅️ truncate x-axis here
  labs(
    title = "Density of Residual Changes by Country Group",
    x = "Residual Change",
    y = "Density",
    fill = "Year"
  ) +
  theme_minimal(base_size = 14)

##############################################################################
# plot US residual over timer 

USA <- sub_dta_change1 %>% filter(ExporterISO3 == "USA") 

USA <- USA %>%  mutate(    date = as.Date(paste(year, month, "01", sep = "-"))  )

plot <-  ggplot(USA, aes(x = date, y = resid_change)) +
  geom_hline(yintercept = 0, color = "red", linetype = "dotted", size = 1) +
  geom_smooth(color = "blue", size = 1.1) +
  labs(  title = "Change in Residuals for USA Over Time",
    x = "time",
    y = "Residual Change"  ) +
  theme_minimal(base_size = 14)
plot

ggsave(plot, file = paste0(exp,"/USA_resid_change_time.png"))



# if look at averages
USA1 <- USA %>% group_by(year,month) %>%  summarise(avg_resid_change = mean(resid_change, na.rm = TRUE),
            sd = sd(resid_change, na.rm = TRUE),sum_resid_change = sum(resid_change, na.rm = TRUE))


ggplot(USA1, aes(x = date, y = avg_resid_change)) +
  geom_smooth(color = "blue", size = 1.1) +
  geom_point(color = "darkblue", size = 2) +
  labs(   title = "Average Change in Residuals for USA Over Time",  x = "Year-Month",
    y = "Average Residual Change"  ) +
  theme_minimal(base_size = 14)


ggplot(USA, aes(x = date, y = sum_resid_change)) +
  geom_smooth(color = "blue", size = 1.1) +
  geom_point(color = "darkblue", size = 2) +
  labs(
    title = "Average Change in Residuals for USA Over Time",
    x = "Year-Month",
    y = "Average Residual Change"
  ) +
  theme_minimal(base_size = 14)


##############################################################################
# aggregate at the yearly level
##############################################################################
names(dta)

dta <- dta %>% 
  group_by(year, ExporterISO3, hs6_H5, ImporterISO3, `HS6 Description`, hs2, hs4) %>%
  mutate(
    Trade_value_USD_yearly = sum(Trade_value_USD, na.rm = TRUE),
    Unit_Price = mean(Unit_Price, na.rm = TRUE),
    Colonial_ties = first(Colonial_ties),
    Importer_GDP  = first(Importer_GDP),
    Exporter_GDP  = first(Exporter_GDP),
    Exporter_wto  = first(Exporter_wto),
    Exporter_eu   = first(Exporter_eu),
    Exporter_GDP_current_USD = first(Exporter_GDP_current_USD),
    Exporter_GDPperCap_current_USD = first(Exporter_GDPperCap_current_USD),
    Exporter_Gross_Cap_formation_current_USD = first(Exporter_Gross_Cap_formation_current_USD),
    Exporter_Ag_land_K2 = first(Exporter_Ag_land_K2),
    Exporter_Exchange_rate_LCU_per_USD = first(Exporter_Exchange_rate_LCU_per_USD)
  ) %>%
  ungroup()



