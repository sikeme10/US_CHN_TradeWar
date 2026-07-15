library(httr)
library(readr)
library(dplyr)
library(purrr)


####################################################################################
# get QCEW at industry level for all US from 2015 to 2020
################################################################################



years <- 2015

get_qcew_annual <- function(year) {
  url <- paste0("https://data.bls.gov/cew/data/files/", year,
                "/csv/", year, "_annual_singlefile.zip")
  
  tmp_zip <- tempfile(fileext = ".zip")
  tmp_dir <- tempdir()
  
  GET(url, write_disk(tmp_zip, overwrite = TRUE))
  unzip(tmp_zip, exdir = tmp_dir)
  
  csv_file <- list.files(tmp_dir, pattern = paste0(year, ".annual.singlefile.csv"),
                         full.names = TRUE)
  
  df <- read_csv(csv_file, col_types = cols(.default = "c")) %>%
    mutate(year = year)
  
  return(df)
}

qcew_all <- map_dfr(years, get_qcew_annual)




class(qcew_all$industry_code)
unique(nchar(qcew_all$industry_code))

qcew_naics6 <- qcew_all %>% filter(nchar(industry_code) == 6)
unique(qcew_naics6$industry_code)
names(qcew_naics6)
unique(qcew_naics6$industry_code)
unique(qcew_naics6$area_fips)



test <- qcew_naics6 %>% filter(area_fips == "US000")

qcew_naics6 <- qcew_naics6 %>% filter(area_fips == "US000")
unique(qcew_naics6$year)
table(qcew_naics6$year)


write_csv(qcew_naics6,  "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/QCEW_industry_2015_2019.csv")




qcew_naics6 <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/QCEW_industry_2015_2019.csv")
names(qcew_naics6)
unique(qcew_naics6$qtr)

table(qcew_naics6$disclosure_code, qcew_naics6$year)



library(httr)
library(readr)
library(dplyr)
library(purrr)

years <- 2015:2020

get_qcew_annual_by_industry <- function(year) {
  url <- paste0("https://data.bls.gov/cew/data/files/", year,
                "/csv/", year, "_annual_by_industry.zip")
  
  tmp_zip <- tempfile(fileext = ".zip")
  tmp_dir <- file.path(tempdir(), paste0("qcew_", year))
  dir.create(tmp_dir, showWarnings = FALSE)
  
  GET(url, write_disk(tmp_zip, overwrite = TRUE), progress())
  unzip(tmp_zip, exdir = tmp_dir)
  
  csv_files <- list.files(tmp_dir, pattern = "\\.csv$", full.names = TRUE)
  
  df <- map_dfr(csv_files, ~ read_csv(.x, col_types = cols(.default = "c"))) %>%
    mutate(year = year)
  
  return(df)
}

qcew_all <- map_dfr(years, get_qcew_annual_by_industry)





