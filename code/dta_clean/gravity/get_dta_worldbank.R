
################################################################################
#                      Worldbank data gathering


# get worldbank data for gravity models


################################################################################

install.packages("WDI")
library(WDI)
rm(list=ls())

setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git")
################################################################################
# examples 


# Search for GDP-related indicators
WDIsearch("exchange rate")



# we can get : 

# real GDP
WDIsearch("GDP (current $)")
# indicator :NY.GDP.MKTP.CD 

# GDP per capita (current US$), \
WDIsearch("GDP per capita")
# indicator : NY.GDP.PCAP.CD

# Gross capital formation (current US$)  
WDIsearch("Gross capital formatio")
# indicator : NE.GDI.TOTL.CD

# Agricultural land (hectares)
WDIsearch("Agricultural land")
# indicator : AG.LND.AGRI.K2


# Official exchange rate, LCU per USD, period average
WDIsearch("Official exchange rate")
# indicator : PA.NUS.FCRF

################################################################################

# Get data 

GDP <- WDI(country = "all", indicator = "NY.GDP.MKTP.CD",
               start = 2015, end = 2023)
GDP <- GDP %>% rename(GDP_current_USD = NY.GDP.MKTP.CD)
names(GDP_Cap)

GDP_Cap <- WDI(country = "all", indicator = "NY.GDP.MKTP.CD",
            start = 2015, end = 2023)
GDP_Cap <- GDP_Cap %>% rename(GDP_per_capita_current_USD = NY.GDP.MKTP.CD)
names(GDP_Cap)

Cap_formation <- WDI(country = "all", indicator = "NE.GDI.TOTL.CD",
               start = 2015, end = 2023)
Cap_formation <- Cap_formation %>% rename(Gross_Cap_formation_current_USD = NE.GDI.TOTL.CD)
names(Cap_formation)

Ag_land <- WDI(country = "all", indicator = "AG.LND.AGRI.K2",
               start = 2015, end = 2023)
Ag_land <- Ag_land %>% rename(Ag_land_K2 = AG.LND.AGRI.K2)
names(Ag_land)

exchange_rate <- WDI(country = "all", indicator = "PA.NUS.FCRF",
                     start = 2015, end = 2023)
exchange_rate <- exchange_rate %>% rename(Exchange_rate_LCU_per_USD = PA.NUS.FCRF)

names(exchange_rate)

################################################################################


dta <- bind_cols(GDP,GDP_Cap, Cap_formation, Ag_land, exchange_rate)



dta <- reduce(list(GDP, GDP_Cap, Cap_formation, Ag_land, exchange_rate),
  full_join,  by = c("country", "iso2c", "iso3c", "year"))

unique(dta$iso3c)

write_csv(dta, "data/gravity/Worldbank_dta.csv")



