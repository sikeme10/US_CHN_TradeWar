library(httr)
library(readr)
library(dplyr)
library(purrr)


####################################################################################
# get QCEW at industry level for all US from 2015 to 2020
################################################################################



year <- 2017

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

qcew <- map_dfr(years, get_qcew_annual)

write_csv(qcew, paste0("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/qcew/", year, ".annual.singlefile.csv"))




