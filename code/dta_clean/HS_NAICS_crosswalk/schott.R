




rm(list=ls())
# Set directory
setwd("/data/sikeme/TRADE/NTM_trade_war/data")
getwd()




library(haven)
dta <- read_dta("crosswalk/schott/hs_sic_naics_exports_89_123_20240801.dta")

################################################################################


# filter year 2012:
dta_2012 <- dta %>% filter(year == 2012)


################################################################################

# get it at HS 6  product code and HS2 

class(dta_2012$commodity)
unique(nchar(dta_2012$commodity))


# Create HS10 variable as character, padded to 10 digits
dta_2012$HS10 <- stringr::str_pad(  stringr::str_replace_all(format(dta_2012$commodity, scientific = FALSE), " ", ""),
  width = 10,  side = "left",  pad = "0")
dta_2012$HS6 <- as.character(substr(dta_2012$HS10, 1, 6))
dta_2012$HS2 <- as.character(substr(dta_2012$HS6, 1, 2))

################################################################################

names(dta_2012)
class(dta_2012$naics)
unique(nchar(dta_2012$naics))


dta_2012$naics3 <- as.character(substr(dta_2012$naics, 1, 3))
dta_2012$naics4 <- as.character(substr(dta_2012$naics, 1, 4))


dta_2012 <- dta_2012 %>% select(year,naics3 ,naics4,naics, HS2, HS6, HS10)


write_csv(dta_2012, "/data/sikeme/TRADE/NTM_trade_war/data/crosswalk/schott/naics_HS_schott_2012.csv")







