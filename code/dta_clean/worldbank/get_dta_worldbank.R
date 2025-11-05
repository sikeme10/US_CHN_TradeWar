
################################################################################
#                      Worldbank data gathering


# get worldbank data for gravity models


################################################################################

install.packages("WDI")
library(WDI)


setwd("/data")
################################################################################
# examples 


# Search for GDP-related indicators
WDIsearch("exchange rate")



# Example: GDP (current US$) for United States and India from 2000 to 2022
data <- WDI(country = c("US", "IN"),
            indicator = "NY.GDP.MKTP.CD",
            start = 2000,
            end = 2022)


# we can get : 

# GDP per capita (current US$), 
# Gross capital formation (current US$)  
# Agricultural land (hectares)
# Official exchange rate, LCU per USD, period average



################################################################################

