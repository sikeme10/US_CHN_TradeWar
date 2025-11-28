
################################################################################
#                     Structural data creation

# get data ready for structural model
# log differentiation of variable of interest 
# aggregate at the yearly level?

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

exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_structural/"


################################################################################
# 1) Load data 
################################################################################

dta <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_gravity/dta_CHN_gravity_3_quant.csv")

# dta <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_gravity/dta_CHN_gravity_3.csv")

names(dta)
colSums(is.na(dta))


################################################################################
# drop if a lot 0s in observations 
################################################################################
# a lot of observation so decide to keep only obs with values for most years 

# if we keep only observations that contains no missing values throughout the year and the products by exporter 
# drop observation  where all observations of a trade value equal 0.

dta1 <- dta %>% group_by(ExporterISO3, hs6_H5) %>% filter(any(Trade_value_USD != 0))
table(dta1$year)
str(dta1)

################################################################################
# 1) Aggregate at the yearly level
################################################################################

# for price if the price is 0 put NA so that when taking average of prices it s not counted 
#dta1 <- dta1 %>% mutate( Unit_Price = ifelse( Unit_Price == 0 , NA, Unit_Price ) )
class(dta1$Unit_Price)
dta1$Unit_Price <- as.numeric(dta1$Unit_Price)


# dta <- dta %>% mutate( Quantity = ifelse( is.na(Quantity) , 0, Quantity ) )
# head(dta)
# colSums(is.na(dta))


# group _by year exporter, hs6 and  
dta_yearly  <- dta1 %>% group_by( year, ImporterISO3, ExporterISO3, hs6_H5 ) %>%
  summarise( Trade_value_USD = sum( Trade_value_USD , na.rm=TRUE ),
             Quantity = sum( Quantity , na.rm=TRUE ),
             Unit_Price = mean( Unit_Price , na.rm=TRUE ),
             Applied_tariff = mean( Applied_tariff , na.rm=TRUE ) ) %>%
  ungroup()

table(dta_yearly$year)  



dta_yearly$Unit_Price <- ifelse(is.nan(dta_yearly$Unit_Price), NA, dta_yearly$Unit_Price)
################################################################################

# log differentiating the variables of interest 

# outcome variable and independent variable (such as tariff) 
names(dta_yearly)
# log variable of interest first
dta_yearly <- dta_yearly %>% mutate(log_Trade_value_USD = if_else(Trade_value_USD == 0, NA, log(Trade_value_USD)) ,
                                    log_Quantity = if_else(Quantity == 0, NA, log(Quantity)) ,
                                    log_Unit_Price = if_else(is.na(Unit_Price)|Unit_Price ==0, NA, log(Unit_Price)),
                                    log_Applied_tariff = log( Applied_tariff +1) )
summary(dta_yearly$log_Trade_value_USD)
summary(dta_yearly$log_Unit_Price)
summary(dta_yearly$log_Applied_tariff)


# then do the log differentiation by exporter - hs6 - year
dta_yearly <- dta_yearly %>%
  group_by( ExporterISO3, hs6_H5 ) %>%
  arrange( ExporterISO3, hs6_H5, year ) %>%
  mutate( d_log_Trade_value_USD = log_Trade_value_USD - lag(log_Trade_value_USD),
          d_log_Quantity = log_Quantity - lag(log_Quantity),
          d_log_Unit_Price = log_Unit_Price - lag(log_Unit_Price),
          d_log_Applied_tariff = log_Applied_tariff - lag(log_Applied_tariff) ) %>%
  ungroup()

test <- dta_yearly %>% group_by(ExporterISO3, hs6_H5) %>% filter(any(log_Trade_value_USD != 0))

# drop if year == 2015
dta_yearly <- dta_yearly %>% filter( year != 2015 )

# if we keep only observations that contains no missing values throughout the year and the products

dta_yearly1 <- dta_yearly %>% group_by(ExporterISO3, hs6_H5) %>% filter(any(log_Trade_value_USD != 0))

################################################################################
 
# write_csv(dta_yearly1, file.path( exp, "dta_CHN_structural_yearly.csv" ) )

# write_csv(dta_yearly1, file.path( exp, "dta_CHN_structural_quant_yearly.csv" ) )


write_csv(dta_yearly1, file.path( exp, "dta_CHN_structural_quant_yearly_drop0.csv" ) )


