


################################################################################
#                      RTA data


################################################################################


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
rm(list=ls())

################################################################################

setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git")


################################################################################

# download data from:
#vhttps://www.ewf.uni-bayreuth.de/en/research/RTA-data/index.html

# CU 


# load data
rta <- read_csv("data/gravity/rta_20221214.csv")
rta <- subset(rta, year>2014) %>% rename(fta_eia = ftaeia)
names(rta)


rta <- rta %>%
  rename(
    # has_rta = rta,                # 1 if any regional trade agreement exists
    customs_union = cu,           # 1 if Customs Union
    free_trade_agreement = fta,   # 1 if Free Trade Agreement
    partial_scope_agreement = psa,# 1 if Partial Scope Agreement
    econ_integration_agreement = eia, # 1 if Economic Integration Agreement
    cu_and_eia = cueia,           # 1 if CU + EIA
    fta_and_eia = fta_eia,         # 1 if FTA + EIA
    psa_and_eia = psaeia          # 1 if PSA + EIA
  )

rta <- rta[c("exporter", "importer" ,"year","rta", "customs_union", "free_trade_agreement","partial_scope_agreement",
             "econ_integration_agreement", "econ_integration_agreement", "cu_and_eia", "fta_and_eia", "psa_and_eia")]

################################################################################
unique(rta$importer)
names(rta) <- make.unique(names(rta1))

# filter get China as importer 
rta <- rta %>% filter(importer == "CHN")

# rename names of country variables 
rta <- rta %>% rename(PartnerISO3 = exporter,  ReporterISO3= importer )

################################################################################

# export
write_csv(rta, "data/gravity/clean_rta.csv")
















