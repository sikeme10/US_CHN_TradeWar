install.packages(c("httr", "jsonlite"))

library(httr)
library(jsonlite)

################################################################################

# https://api.census.gov/data/2022/ecnbasic/variables.html

# RCPTOT:	Sales, value of shipments, or revenue ($1,000)

################################################################################

# Replace "YOUR_API_KEY" with your actual API key
api_key <- "32514956a9d5e79bc7d1bb3437726ff7627c7d22"


# Construct the API URL to retrieve all data, including NAICS code descriptions
url <- paste0("https://api.census.gov/data/2012/ecnbasic?get=NAICS2012,NAICS2012_TTL,RCPTOT&for=US:*&key=", api_key)



# Make the API request
response <- GET(url)

# Check if the request was successful
if (status_code(response) == 200) {
  # Parse the JSON response
  data <- fromJSON(content(response, "text"), flatten = TRUE)
  
  # Convert the data to a data frame
  output_data <- as.data.frame(data, stringsAsFactors = FALSE)
  
  # Set the column names
  colnames(output_data) <- tolower(output_data[1, ])
  
  # Remove the first row (column names)
  output_data <- output_data[-1, ]
  
  # Convert rcptot to numeric
  output_data$rcptot <- as.numeric(output_data$rcptot)
  
  # Create a mapping of NAICS code ranges to sectors
  sector_mapping <- list(
    "21" = "mining",
    "22" = "utilities",
    "23" = "construction",
    "31" = "manufacturing",
    "32" = "manufacturing",
    "33" = "manufacturing",
    "42" = "wholesale trade",
    "44" = "retail trade",
    "45" = "retail trade",
    "48" = "transportation and warehousing",
    "49" = "transportation and warehousing",
    "51" = "information",
    "52" = "finance and insurance",
    "53" = "real estate and rental and leasing",
    "54" = "professional, scientific, and technical services",
    "55" = "management of companies and enterprises",
    "56" = "administrative and support and waste management and remediation services",
    "61" = "educational services",
    "62" = "health care and social assistance",
    "71" = "arts, entertainment, and recreation",
    "72" = "accommodation and food services",
    "81" = "other services (except public administration)",
    "92" = "public administration"
  )
  
  # Function to determine the sector based on NAICS code
  get_sector <- function(naics_code) {
    for (prefix in names(sector_mapping)) {
      if (startsWith(naics_code, prefix)) {
        return(sector_mapping[[prefix]])
      }
    }
    return("unknown")
  }
  
  # Add the sector column to the output data frame
  output_data$sector <- sapply(output_data$naics2012, get_sector)
  
  # Print the resulting data frame
  print(output_data)
} else {
  cat("Error retrieving data from the API.\n")
}


################################################################################

names(output_data)

# rename variable of interest
output_data <- output_data %>% 
  rename(NAICS6_2012 = naics2012, NAICS_description = naics2012_ttl, 
         Tot_output_1000dollars = rcptot) %>%
  mutate(year = 2012)

# select only 6 digit codes 
unique(nchar(output_data$NAICS6_2012))
test <- output_data %>% filter(nchar(output_data$NAICS6_2012) == 8)
output_data <- output_data %>% filter(nchar(output_data$NAICS6_2012) ==6)



output_data <-output_data %>%  mutate(NAICS4_2012 = substr(NAICS6_2012, 1, 4),
                                      NAICS3_2012 = substr(NAICS6_2012, 1, 3))

############################################################################


write_csv(output_data , "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/Census_output/census_output_NAICS6.csv")





