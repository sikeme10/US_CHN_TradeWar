




library(tidyr)
library(dplyr)
library(readr)
library(concordance)
library(readr)
library(readxl )
library(haven)   # for reading .dta files
library(dplyr)   # for combining and data manipulation
library(purrr)   # for map functions
library(stringr)
library(labelled)


rm(list=ls())
# Set directory
setwd("/data/sikeme/TRADE/NTM_trade_war/data")
getwd()

################################################################################

# merge all trade data from schott at monthly level 

################################################################################



# Load necessary libraries

# Set the folder path where your .dta files are
path <- "/data/sikeme/TRADE/NTM_trade_war/data/trade/schott/monthly_export"


# Get all .dta files in the folder
dta_files <- list.files(path, pattern = "\\.dta$", full.names = TRUE)

# Combine all .dta files into one dataframe
combined_data <- dta_files %>%
  map_dfr(function(file) {
    message("Reading: ", basename(file))
    df <- read_dta(file)
    
    # Extract year and month from filename, e.g. exp_detl_2017_3n.dta
    parts <- stringr::str_match(basename(file), "exp_detl_(\\d{4})_(\\d{1,2})n\\.dta$")
    df$year <- as.integer(parts[2])
    df$month <- as.integer(parts[3])
    
    return(df)
  })

# Quick check of the combined dataset
glimpse(combined_data)

write_csv(combined_data, "/data/sikeme/TRADE/NTM_trade_war/data/trade/schott/monthly_export/US_combined_export.csv" )


################################################################################
# for exports 
################################################################################

# clean and harmonize var name

rm(list=ls())
library(readr)
US_export <- read_csv("trade/schott/monthly_export/US_combined_export.csv")
head(US_export)
names(US_export)



# merge with countrycodes to get ISO3 codes 
countryCodes <- read_csv("trade/schott/country_codes.csv")
US_export <- left_join(US_export, countryCodes, by = c("cty_code" = "Country_Code") )


# all_val_mo = 	15-digit Total Value (tot export value )

names(US_export)
# select variable of interest 
US_export1 <- US_export %>%
  select(commodity, cty_code, year,month, all_val_mo, sic, naics, Country, ISO_Code, ISO3_Code)




############################################################################

# aggregate by HS6 level and create an export value variable 
US_export1 <-US_export1 %>% group_by(commodity, year,month, cty_code, sic, naics, Country, ISO_Code, ISO3_Code) %>%
  summarise(export_val_USD = sum(all_val_mo, na.rm = TRUE))


# change HS product code in character
unique(nchar(US_export1$commodity))

US_export1$commodity <- format(US_export1$commodity, scientific = FALSE)
US_export1$commodity <- str_pad(as.character(US_export1$commodity), width = 10,
                                side = "left",    pad = "0")
US_export1 <- US_export1 %>% rename(HS10 = commodity) %>% 
  mutate(HS6 = substr(as.character(HS10), 1, 6))
unique(nchar(US_export1$HS6))
class(US_export1$HS6)



# change NAICS code in character
unique(nchar(US_export1$naics))

# more aggregate level naics variables 
US_export1 <- US_export1 %>% rename(naics6 = naics) %>% 
  mutate(naics4 = substr(as.character(naics6), 1, 4),
         naics3 = substr(as.character(naics6), 1, 3))



############################################################################


# export clean data
write_csv(US_export1,  "/data/sikeme/TRADE/NTM_trade_war/data/trade/schott/monthly_export/US_cleaned_export.csv")
