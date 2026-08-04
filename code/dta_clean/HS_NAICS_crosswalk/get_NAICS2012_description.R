




# Set directory
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git/data")
getwd()



library(readxl)
naics  <- read_excel("crosswalk/2012_NAICS_Structure.xls", skip = 2)

colSums(is.na(naics))


# rename the varaiables

naics <- naics %>% rename(naics = `2012 NAICS Code`, naics_description = `2012 NAICS Title`) %>% 
  filter(!is.na(naics)) %>% select(- `Change Indicator`) 




# Filter to 6-digit codes only
naics_6digit <- naics %>%  filter(nchar(as.character(naics)) == 6) 

write_csv(naics_6digit, "crosswalk/2012_naics_description.csv")










