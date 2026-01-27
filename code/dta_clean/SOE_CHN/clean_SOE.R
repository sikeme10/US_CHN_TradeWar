

################################################################################
#                    Gravity regression analysis: residual approach


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
library(forcats)
library(polyglotr)
library(stringr)
library(labelled)
library(janitor)
library(readxl)
################################################################################
# directory: 
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git")
exp <- "data/SOE_dta/2010"

################################################################################
# 1) Load data 
################################################################################

# Load FE estimates 

# to get country codes from excel:
countries <- read_excel("data/SOE_dta/国家地区代码对照表.xlsx")
old_country_names <- names(countries)
new_country_names<- str_extract(old_country_names, "[^_]+$")
names(countries) <- new_country_names
countries <- countries %>% clean_names()
countries <-  countries %>% rename(CntryNm_CHN = cntry_nm , ISO3 = eng, CntryNm = eng_2 )

# ################################################################################
# by files
# ################################################################################
# 
# library(haven)
# dta <- read_dta("data/SOE_dta/2010/201001.dta")
# 
# ################################################################################
# # change names of variables: translation
# 
# 
# old_names <- names(dta)
# 
# # English name = last underscore chunk
# new_names <- str_extract(old_names, "[^_]+$")
# 
# # Chinese label = everything before last underscore
# labels_zh <- str_remove(old_names, "_[^_]+$")
# 
# # translate in english
# labels_en <- google_translate(labels_zh,  source = "zh",  target = "en")
# 
# # Apply names
# names(dta) <- new_names
# 
# # Apply labels
# var_label(dta) <- labels_en
# names(dta)
# var_label(dta)
# 
# 
# ################################################################################
# 
# names(dta)
# 
# 
# # a) change ImpExpTypeNm
# val_CHN <- unique(dta$ImpExpTypeNm)
# val_EN <- google_translate(val_CHN,  source = "zh",  target = "en")
# 
# dta <- dta %>% mutate(ImpExpTypeNm =  case_when(
#   ImpExpTypeNm == "出口"~ "export",
#   ImpExpTypeNm == "进口"~ "import"))
# 
# 
# 
# # b) change HSNm (product name)
# val_CHN <- unique(dta$HSNm)
# val_EN <- google_translate(val_CHN,  source = "zh",  target = "en")
# # translate the names 
# dta <- dta %>% mutate(HSNm = recode(HSNm, !!!setNames(val_EN, val_CHN),
#                                              .default = HSNm    )  )
# unique(dta$HSNm)
# 
# 
# 
# 
# # c) change EntNatNm (SOE)
# library(dplyr)
# 
# val_CHN <- unique(dta$EntNatNm)
# table(dta$EntNatNm)
# val_CHN <- val_CHN[nzchar(trimws(val_CHN))]  # remove "" and "   "
# val_EN <- google_translate(val_CHN, source = "zh", target = "en")
# 
# dta <- dta %>% mutate(EntNatNm = recode(EntNatNm, !!!setNames(val_EN, val_CHN),
#       .default = EntNatNm  )  )
# table(dta$EntNatNm)
# 
# 
# # d) change BegEndCntryNm (country name)
# 
# # get isocodes
# unique(dta$BegEndCntryNm)
# dta1 <- left_join(dta, countries, by=c("BegEndCntryCd" = "cntry_cd"))
# test <- dta1 %>% filter(is.na(ISO3)) %>% select(CntryNm) %>% distinct()
# 
# 
# # e) unit change
# val_CHN <- unique(dta1$QuantityUnitNm)
# val_EN <- google_translate(val_CHN, source = "zh", target = "en")
# dta1 <- dta1 %>% mutate(QuantityUnitNm_EN = recode(QuantityUnitNm, !!!setNames(val_EN, val_CHN),
#                                         .default = QuantityUnitNm  )  )
# 
# 
# 
# ################################################################################
# # select data of interest 
# 
# names(dta1)
# colSums(is.na(dta1))
# 
# dta1 <- dta1 %>%
#   select(Year, EndDt, ImpExpTypeCd, ImpExpTypeNm, HSCd, HSNm, USD, Quantity, Price,
#     EntCd, EntNm, EntNatCd, EntNatNm, BegEndCntryNm, ISO3, CntryNm,  QuantityUnitCd, QuantityUnitNm ,QuantityUnitNm_EN)
# 

################################################################################
# in a loop
################################################################################
# -----------------------------
# 1) Translation cache helpers
# -----------------------------
.translate_cache <- new.env(parent = emptyenv())

translate_cached <- function(x, source = "zh", target = "en") {
  x <- as.character(x)
  out <- character(length(x))
  
  for (i in seq_along(x)) {
    key <- paste(source, target, x[i], sep = "||")
    if (exists(key, envir = .translate_cache, inherits = FALSE)) {
      out[i] <- get(key, envir = .translate_cache, inherits = FALSE)
    } else {
      tr <- google_translate(x[i], source = source, target = target)
      assign(key, tr, envir = .translate_cache)
      out[i] <- tr
    }
  }
  out
}

recode_with_translation <- function(vec, source = "zh", target = "en") {
  vec_chr <- as.character(vec)
  
  vals <- unique(vec_chr)
  vals_nonblank <- vals[nzchar(trimws(vals))]
  
  # nothing to translate
  if (length(vals_nonblank) == 0) return(vec_chr)
  
  vals_tr <- translate_cached(vals_nonblank, source = source, target = target)
  map <- setNames(vals_tr, vals_nonblank)
  
  dplyr::recode(vec_chr, !!!map, .default = vec_chr)
}

# -----------------------------
# 2) Cleaner for one .dta file
# -----------------------------
clean_one_file <- function(path, countries_tbl) {
  dta <- read_dta(path)
  
  # ---- Rename variables and create labels (from "中文_English" format) ----
  old_names <- names(dta)
  new_names <- str_extract(old_names, "[^_]+$")
  labels_zh <- str_remove(old_names, "_[^_]+$")
  
  names(dta) <- new_names
  
  # translate and apply variable labels (cached)
  labels_en <- translate_cached(labels_zh, source = "zh", target = "en")
  var_label(dta) <- labels_en
  
  # ---- a) ImpExpTypeNm (出口/进口) ----
  if ("ImpExpTypeNm" %in% names(dta)) {
    dta <- dta %>%
      mutate(
        ImpExpTypeNm = case_when(
          ImpExpTypeNm == "出口" ~ "export",
          ImpExpTypeNm == "进口" ~ "import",
          TRUE ~ as.character(ImpExpTypeNm)
        )
      )
  }
  
  # ---- b) HSNm (product name) ----
  if ("HSNm" %in% names(dta)) {
    dta <- dta %>%
      mutate(HSNm = recode_with_translation(HSNm, source = "zh", target = "en"))
  }
  
  # ---- c) EntNatNm (enterprise type) ----
  if ("EntNatNm" %in% names(dta)) {
    dta <- dta %>%
      mutate(EntNatNm = recode_with_translation(EntNatNm, source = "zh", target = "en"))
  }
  
  # ---- d) Join country name + ISO3 from excel crosswalk ----
  # Requires BegEndCntryCd in dta and cntry_cd in countries_tbl
  if ("BegEndCntryCd" %in% names(dta) && "cntry_cd" %in% names(countries_tbl)) {
    dta <- dta %>%
      left_join(countries_tbl, by = c("BegEndCntryCd" = "cntry_cd"))
  }
  
  # ---- e) QuantityUnitNm translation ----
  if ("QuantityUnitNm" %in% names(dta)) {
    dta <- dta %>%
      mutate(
        QuantityUnitNm_EN = recode_with_translation(QuantityUnitNm, source = "zh", target = "en")
      )
  }
  
  # ---- Final selection (any_of won't error if a column is missing) ----
  keep <- c(
    "Year", "EndDt", "ImpExpTypeCd", "ImpExpTypeNm",
    "HSCd", "HSNm",
    # your value column might be "USD" or "Sum_USD"
    "USD", "Sum_USD",
    "Quantity", "Price",
    "EntCd", "EntNm", "EntNatCd", "EntNatNm",
    "BegEndCntryNm", "ISO3", "CntryNm",
    "QuantityUnitCd", "QuantityUnitNm", "QuantityUnitNm_EN"
  )
  
  dta %>%
    select(any_of(keep)) %>%
    mutate(source_file = basename(path))
}

# -----------------------------
# 3) Loop all 2010 files (track progress)
# -----------------------------
# -----------------------------
# Loop all 2010 files
# Clean each one and write CSV immediately
# -----------------------------

files_2010 <- list.files( "data/SOE_dta/2010", pattern = "\\.dta$", full.names = TRUE)

for (i in seq_along(files_2010)) {
  # i <- 1
  infile <- files_2010[i]
  
  cat(sprintf("[%d / %d] Processing %s\n",i, length(files_2010), basename(infile) )  )
  
  # Clean one file # or countries_tbl if that's your object
  dta_clean <- clean_one_file(path = infile, countries_tbl = countries  )
  
  # Build output filename: same folder, *_clean.csv
  outfile <- sub( "\\.dta$","_clean.csv",  infile )
  
  # Write immediately
  write_csv(dta_clean, outfile)
  
  cat(sprintf("      → saved %s\n", basename(outfile)))
  
  # Free memory aggressively
  rm(dta_clean)
  gc()
}
