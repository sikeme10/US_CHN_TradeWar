################################################################################
# BUILD A CLEAN HS6 -> NAICS6 CROSSWALK (2012 vintage)
#
# PURPOSE
#   Produce a crosswalk in which each HS6 product code maps to exactly one
#   6-digit NAICS industry, so that trade data (HS level) can be merged onto
#   production/output data (NAICS level) without duplicating value.
#
# INPUTS
#   naics_HS_schott_2012.csv        Schott concordance         (HS6, naics)
#   NAICS_HS_2012.csv               Charlton/"Diane" concordance (HS6, naics6_D,
#                                                                 j, crop)
#   output_NAICS_6.csv              Census output by NAICS6, defines the set of
#                                   NAICS codes we actually need
#   HS_2012_2017_merged.csv         Official HS product codes + descriptions,
#                                   defines the set of HS codes we actually need
#   2012_naics_description.csv      NAICS6 code -> description lookup
#
# OUTPUTS
#   clean_HS6_naics6_2012_test.csv  Pre-manual-fix version
#   clean_HS6_naics6_2012.csv       Final HS6-NAICS6 crosswalk
#   clean_HS6_naics4_2012.csv       Same crosswalk collapsed to NAICS4
#   clean_HS6_naics3_2012.csv       Same crosswalk collapsed to NAICS3
#
# STRATEGY
#   1. Restrict both concordances to the HS and NAICS universes we care about.
#   2. Keep only HS codes that map to a single NAICS code in each source.
#   3. Charlton takes priority; Schott fills the gaps.
#   4. The `concordance` package fills whatever is still missing.
#   5. A manual pass (Excel) fills the remaining holes.
################################################################################


# ---- Packages ----------------------------------------------------------------
library(stringr)
library(haven)
library(concordance)
library(readr)
library(readr)
library(tidyr)
library(dplyr)
library(data.table)
library(stringi)
library(fixest)
library(countrycode)
library(tidyverse)
library(vroom)


# ---- Session setup -----------------------------------------------------------
rm(list=ls())
# Set directory
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git/data")
getwd()


################################################################################
# 1. IMPORT DATA
################################################################################

library(haven)

# Schott concordance: HS6 -> naics
dta_schott <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/crosswalk/clean_HS6_naics6_schott_2012.csv")

# Charlton concordance: HS6 -> naics6_D, plus sector labels (j, crop)
dta_D <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/crosswalk/HS6_NAICS_Diane/NAICS_HS_2012.csv")

# Census output data: defines the NAICS6 universe we need to cover
output_NAICS6 <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/Census_output/output_NAICS_6.csv")

# Official HS codes and descriptions: defines the HS6 universe we need to cover
HS_product <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/HS_codes/HS_2012_2017_merged.csv")

# NAICS6 code -> description lookup
NAICS6 <- read_csv("crosswalk/2012_naics_description.csv")

# Quick look at the column names of each input
names(dta_schott)
names(dta_D)
names(HS_product)
names(output_NAICS6)
length(unique(output_NAICS6$NAICS6_2012))


######################################################################################
# 2. CLEAN THE SCHOTT NAICS FIELD
#    Schott codes some industries with an "X" placeholder digit. Coercing to
#    numeric turns those into NA, which we then drop.
######################################################################################

# drop naics where have "X"
dta_schott$naics <- as.numeric(dta_schott$naics)
colSums(is.na(dta_schott))
dta_schott <- dta_schott %>% filter(!is.na(naics))


######################################################################################
# 3. DIAGNOSTICS: SIZE OF EACH SOURCE BEFORE FILTERING
######################################################################################

# checks
length(unique(dta_schott$naics))
length(unique(dta_schott$HS6))

length(unique(dta_D$naics6_D))
length(unique(dta_D$HS6))

length(unique(output_NAICS6$NAICS6_2012))

length(unique(HS_product$HS_2012_Product_Code))


# ---- 3a. Restrict to valid HS6 codes -----------------------------------------
# How many codes in each concordance are actually valid 2012 HS codes?

# 1) check product codes first:

sum(unique(dta_schott$HS6) %in% unique(HS_product$HS_2012_Product_Code))
sum(unique(dta_D$HS6) %in% unique(HS_product$HS_2012_Product_Code))

# filter product codes that are in HS_product$HS_2012_Product_Code
dta_schott <- dta_schott %>% filter(HS6 %in% unique(HS_product$HS_2012_Product_Code))
length(unique(dta_schott$HS6))
colSums(is.na(dta_schott))
dta_schott

# Same restriction for Charlton, then drop rows with a missing key on either side
# filter product codes that are in HS_product$HS_2012_Product_Code
dta_D <- dta_D %>% filter(HS6 %in% unique(HS_product$HS_2012_Product_Code))
length(unique(dta_D$HS6))
length(unique(dta_D$naics6_D))
colSums(is.na(dta_D))
dta_D <- dta_D %>% filter(!is.na(HS6))
dta_D <- dta_D %>% filter(!is.na(naics6_D))


# ---- 3b. Restrict to NAICS codes that appear in the Census output data --------
# Any NAICS code without output data is useless downstream.

# 2) check NAICS codes:
sum(unique(dta_schott$naics) %in% unique(output_NAICS6$NAICS6_2012))
sum(unique(dta_D$naics6_D) %in% unique(output_NAICS6$NAICS6_2012))

# filter product codes that are in output_NAICS6$NAICS6_2012
dta_schott <- dta_schott %>% filter(naics %in% unique(output_NAICS6$NAICS6_2012))
length(unique(dta_schott$naics))
length(unique(dta_schott$HS6))
colSums(is.na(dta_schott))


# filter product codes that are in output_NAICS6$NAICS6_2012
dta_D <- dta_D %>% filter(naics6_D %in% unique(output_NAICS6$NAICS6_2012))
length(unique(dta_D$HS6))
length(unique(dta_D$naics6_D))
colSums(is.na(dta_D))


################################################################################
# 4. ENFORCE A MANY-TO-ONE MAPPING (HS -> NAICS)
#
# Both raw concordances are many-to-many. We keep only HS codes whose mapping is
# unambiguous, i.e. that point to a single NAICS industry. The reverse direction
# (many HS codes per NAICS) is fine and expected, so it is only inspected below,
# not filtered.
################################################################################

# for each avoid the many to many mapping
# multiple products codes map to multiple NAICS code and vice versa
# we would like to have Many HS codes map to only one NAICS industry
# we drop HS codes that map to multiple NAICS code 

# ---- 4a. Schott: keep unambiguous HS codes ------------------------------------
# How many unique NAICS each HS6 maps to
HS6_to_NAICS_schott <- dta_schott %>%  group_by(HS6) %>%
  summarise(n_naics = n_distinct(naics)) %>%  arrange(desc(n_naics))
HS6_to_NAICS_schott
# we can select only HS codes and naics combination that has HS codes mapping to only one NAICS code
HS6_to_NAICS_schott <- HS6_to_NAICS_schott %>% filter(n_naics ==1)
length(unique(HS6_to_NAICS_schott$HS6))

# filter only one to one mapping:
dta_schott <- dta_schott %>% filter(HS6 %in% HS6_to_NAICS_schott$HS6)
length(unique(dta_schott$HS6))
length(unique(dta_schott$naics))


# ---- 4b. Charlton: same procedure ---------------------------------------------
# How many unique NAICS6_D each HS6 maps to
HS6_to_NAICS_diane <- dta_D %>%  group_by(HS6) %>% summarise(n_naics = n_distinct(naics6_D)) %>%
  arrange(desc(n_naics))
HS6_to_NAICS_diane
# we can select only HS codes and naics combination that has HS codes mapping to only one NAICS code
HS6_to_NAICS_diane <- HS6_to_NAICS_diane %>% filter(n_naics ==1)
length(unique(HS6_to_NAICS_diane$HS6))
# filter only one to one mapping:
dta_D <- dta_D %>% filter(HS6 %in% HS6_to_NAICS_diane$HS6)
length(unique(dta_D$HS6))
length(unique(dta_D$naics6_D))


# ---- 4c. Inspect the reverse direction (diagnostic only) ----------------------
# How many unique HS6 each maps to NAICS maps
# schott
NAICS_to_HS6 <- dta_schott %>%  group_by(naics) %>%
  summarise(n_hs6 = n_distinct(HS6)) %>%  arrange(desc(n_hs6))
NAICS_to_HS6

# How many unique HS6 each NAICS6_D maps to
#diane
NAICS_to_HS6 <- dta_D %>% group_by(naics6_D) %>%  summarise(n_hs6 = n_distinct(HS6)) %>%
  arrange(desc(n_hs6))
NAICS_to_HS6


######################################################################################
# 5. HARMONISE COLUMNS AND ATTACH PRODUCT DESCRIPTIONS
######################################################################################

# check product codes that are in Charlton and not in Schott and vice versa
names(dta_D)
names(dta_schott)
names(HS_product)

# Keep only the columns needed downstream; rename Charlton's sector labels
# (j -> subsector, crop -> ag_subsector)
dta_D <- dta_D %>% select(HS6 , naics6_D, j, crop) %>% rename(subsector = j , ag_subsector = crop )
dta_schott <- dta_schott %>% select(HS6 , naics)


# One row per HS6-NAICS pair in each source
# drop duplicates
sum(duplicated(dta_D[, c("HS6", "naics6_D")]))
sum(duplicated(dta_schott[, c("HS6", "naics")]))
dta_D <- dta_D %>% distinct(HS6, naics6_D, .keep_all = TRUE)
dta_schott <- dta_schott %>% distinct(HS6, naics, .keep_all = TRUE)

# Attach the official HS description to each product code
# merge with HS_product description
HS_product_2012 <- HS_product %>% select(HS_2012_Product_Code, HS_2012_Product_Description)
length(unique(HS_product_2012$HS_2012_Product_Code))
HS_product_2012 <- HS_product_2012 %>% distinct(HS_2012_Product_Description,HS_2012_Product_Code, .keep_all = TRUE)

dta_D <- left_join(dta_D, HS_product_2012, by= c("HS6" = "HS_2012_Product_Code"))
dta_schott <- left_join(dta_schott, HS_product_2012, by= c("HS6" = "HS_2012_Product_Code"))


######################################################################################
# 6. COMBINE THE TWO SOURCES
#    Charlton is the preferred source (it carries the ag/nonag sector labels).
#    Schott is used only for HS codes Charlton does not cover.
######################################################################################

# Merge the two datasets

# we first select only product codes that are in Chalrton and not in Schott
# whatever is not in Charlton, we use Schott

dta_schott2 <- dta_schott %>% filter(!HS6 %in% unique(dta_D$HS6))
length(unique(dta_schott2$naics))
length(unique(dta_schott2$HS6))

# Align the NAICS column name across the two sources before stacking
dta_D <- dta_D %>% rename(naics = naics6_D)
length(unique(dta_D$HS6))
length(unique(dta_D$naics))

merge <- bind_rows(dta_D, dta_schott2)
length(unique(merge$HS6))
length(unique(merge$naics))
colSums((is.na(merge)))


######################################################################################
# 7. RECOVER UNMATCHED HS CODES
#    Step 7a re-inserts every official HS code as a row (NAICS still NA).
#    Step 7b uses the `concordance` package to fill those NAs.
######################################################################################

# whatever product was not matched we can use package to get back

# Sanity checks on coverage and key types before the full join
length(unique(merge$HS6))
class(merge$HS6)
class(HS_product$HS_2012_Product_Code)
length(unique(merge$naics))
length(unique(HS_product$HS_2012_Product_Code))
colSums(is.na(merge))

# are ll product codes in merge in HS 2012 revision product codes?
sum(merge$HS6 %in% HS_product$HS_2012_Product_Code)


# ---- 7a. Add back HS codes that matched neither source ------------------------
Product_codes1 <- HS_product %>% select(HS_2012_Product_Code, HS_2012_Product_Description) %>%
  rename(HS6 = HS_2012_Product_Code)
length(unique(Product_codes1$HS6))
sum(duplicated(Product_codes1[, c("HS6", "HS_2012_Product_Description")]))
Product_codes1 <- Product_codes1 %>% distinct(HS6, HS_2012_Product_Description, .keep_all = TRUE)

# add missing HS 6 code in merge
merge1 <- full_join(merge, Product_codes1, by = c("HS6","HS_2012_Product_Description"))
length(unique(merge1$HS6))
colSums(is.na(merge1))
merge1 <- merge1 %>% filter(!is.na(HS6))


# ---- 7b. Fill remaining NAs with the concordance package ----------------------
# all = FALSE returns the single best (highest weight) NAICS match per HS code.
# use the concordance option to match with weights
# by having weights being False --> output HS-NAICS match with the highest weight

library(concordance)
merge2 <- merge1 %>%
  mutate(naics1 = ifelse(is.na(naics),
                         sapply(HS6, function(x) {
                           result <- concord_hs_naics(x, origin = "HS4", destination = "NAICS2012", dest.digit = 6, all = FALSE)
                           result[[1]]
                         }),
                         naics))
colSums(is.na(merge2))
length(unique(merge2$naics1))
length(unique(merge2$HS6))

# Collapse the filled column back into `naics` and drop anything still unmatched
merge3 <- merge2 %>% select(HS6, naics1,HS_2012_Product_Description, subsector , ag_subsector) %>% rename(naics = naics1)
length(unique(merge3$naics))
length(unique(merge3$HS6))
colSums(is.na(merge3))
merge3 <- merge3 %>% filter(!is.na(naics))


################################################################################
# 8. VALIDATE NAICS CODES AGAINST THE 2012 NAICS LIST
#    The concordance package can return codes from other NAICS vintages, so any
#    code without a 2012 description is set back to NA.
################################################################################

# check if an output exist for each :
unique(nchar(NAICS6$naics))
NAICS6$naics <- as.character(NAICS6$naics)
length(unique(merge3$naics))
test <- merge3 %>% filter((naics %in% unique(NAICS6$naics)))
length(unique(test$naics))


# ---- 8a. Set overlap between our codes and the official 2012 list -------------
# Pull the two vectors
naics_a <- unique(merge3$naics)
naics_b <- unique(NAICS6$naics)

# Basic counts
length(naics_a)
length(naics_b)

# Overlap
length(intersect(naics_a, naics_b))   # codes in both
length(setdiff(naics_a, naics_b))     # in A but not B
length(setdiff(naics_b, naics_a))     # in B but not A

names(NAICS6)


# ---- 8b. Attach descriptions and null out invalid codes -----------------------
merge4 <- left_join(merge3, NAICS6)
colSums(is.na(merge4))
test <- merge4 %>% filter(is.na(naics_description ))
length(unique(test$HS6))
sum(!grepl("^\\d+$", as.character(test$naics[!is.na(test$naics)])))
# drop if not in 2012 NAICS codes
merge4 <- merge4 %>%  mutate(naics = ifelse(is.na(naics_description), NA, naics))
# Non-numeric naics values
sum(grepl("[^0-9]", as.character(merge4$naics[!is.na(merge4$naics)])))
colSums(is.na(merge4))


################################################################################
# 9. RESOLVE DUPLICATE SECTOR LABELS
#    A single HS6-NAICS pair can appear twice with conflicting `subsector`
#    values. Rules applied:
#      - if one of the duplicates is crop/livestock, keep the agricultural label
#      - mining is reclassified as nonag, with "mining" stored in ag_subsector
################################################################################

# drop replicates (due to subsector )

colSums(is.na(merge3))

# recode if ag and nonag just put it in ag
unique(merge4$subsector)
merge4 <- merge4 %>%  group_by(HS6, naics) %>%
  mutate(subsector = case_when( n() > 1 & any(subsector %in% c("crop", "livestock")) ~ subsector[subsector %in% c("crop", "livestock")][1],
                                TRUE ~ subsector  )) %>%  ungroup()

# # put mining in nonag
# merge4 <- merge4 %>% mutate(ag_subsector = if_else(subsector == "mining", "mining", ag_subsector),
#                             subsector = if_else(subsector =="mining", "nonag", subsector ))
unique(merge4$subsector)
unique(merge4$ag_subsector)
colSums(is.na(merge4))

# Numeric version of the NAICS key (diagnostic; not what gets written out)
merge5 <- merge4 %>% mutate(naics = as.numeric(naics))
colSums(is.na(merge5))


################################################################################
# 10. MANUAL FIX ROUND
#     The intermediate file is exported, the remaining gaps in HS-NAICS-subsector
#     are filled by hand, and the corrected workbook is read back in.
#     NOTE: this overwrites merge5 from the previous block.
################################################################################

write_csv(merge4, "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/crosswalk/clean_HS6_naics6_2012_test.csv")

# by hand refill HS naics subscetor codes using Claude:
library(readxl)
merge5  <- read_excel("crosswalk/clean_HS6_naics6_2012_new_version.xlsx")


################################################################################
# 11. FINAL HS6 -> NAICS6 CROSSWALK
################################################################################

merge5 <- merge5 %>% distinct(HS6,naics, .keep_all = TRUE)

# Final diagnostics: code counts, missingness, column types
length(unique(merge5$naics))
length(unique(merge5$HS6))
colSums(is.na(merge5))

sapply(merge5, class)
test <- merge5 %>%  group_by(naics) %>%
  summarise(n_subsector = n_distinct(subsector),
            subsectors = paste(unique(subsector), collapse = ", ")  ) %>%
  filter(n_subsector > 1)


merge5 <- merge5 %>% select(HS6, naics, HS_2012_Product_Description, naics_description, subsector, ag_subsector )
# get at HS6-naics 6 level
write_csv(merge5, "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/crosswalk/clean_HS6_naics6_2012.csv")

# Read back to confirm the file wrote correctly
test <- read_csv( "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/crosswalk/clean_HS6_naics6_2012.csv")


######################################################################################
# 12. COARSER HS AGGREGATIONS: Get HS4 level to crop, naics code and so forth 
#     Collapse the crosswalk to NAICS4 and NAICS3.
#     NOTE: this section starts from `dta1`, which is not created anywhere above.
######################################################################################

merge5 <-read_csv( "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/crosswalk/clean_HS6_naics6_2012.csv")


# get HS4 and make sure it connects to 

# merge with subsector
merge5$HS6 <-  as.character(merge5$HS6)
length(unique(merge5$HS6))
unique(nchar(merge5$HS6))
merge5$HS6 <- ifelse(nchar(merge5$HS6) == 5, paste0("0", merge5$HS6), merge5$HS6)
merge5$hs4 <- as.character(substr(merge5$HS6, 1, 4)) 
length(unique(merge5$hs4))

merge_hs4  <- merge5 %>% select(-naics, -naics_description)
merge_hs4 <- merge_hs4 %>% distinct(hs4, subsector)
length(unique(merge_hs4$hs4))

# some missing HS codes
HS_product <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/HS_codes/HS_2012_2017_merged.csv")
HS6 <- as.data.frame(HS_product$HS_2012_Product_Code)
names(HS6) <- "HS6"
HS6$HS6 <- as.character(HS6$HS6)
length(unique(HS6$HS6))
unique(nchar(HS6$HS6))
# pad any 5-digit codes back to 6 digits (leading zero lost on numeric read)
HS6$HS6 <- ifelse(nchar(HS6$HS6) == 5, paste0("0", HS6$HS6), HS6$HS6)
HS6$hs4 <- substr(HS6$HS6, 1, 4)
unique_hs4 <- unique(HS6$hs4)

missing_hs4 <- setdiff(unique_hs4, merge_hs4$hs4)
missing_hs4 <- missing_hs4[!is.na(missing_hs4)]
length(missing_hs4)

if (length(missing_hs4) > 0) {
  add_rows <- data.frame(
    hs4 = missing_hs4,
    subsector = NA,
    stringsAsFactors = FALSE
  )
  merge_hs4 <- rbind(merge_hs4, add_rows)
}

# sanity checks
nrow(merge_hs4) == length(unique(merge_hs4$hs4))   # should be TRUE if hs4 was unique before
sum(is.na(merge_hs4$subsector))                     # how many now unclassified
length(unique(merge_hs4$hs4))

hs4_fill <- tribble(
  ~hs4,    ~subsector,
  "0105",  "livestock",   # live poultry
  "0501",  "nonag",       # HUMAN hair - not an animal product
  "0507",  "livestock",   # ivory, horns, antlers, coral
  "1701",  "crop",        # cane/beet sugar
  "1703",  "crop",        # molasses
  "2621",  "mining",      # slag and ash
  "2716",  "nonag",       # ELECTRICAL ENERGY - not extraction
  "3704",  "nonag",
  "3705",  "nonag",
  "3706",  "nonag",
  "3915",  "nonag",       # plastics scrap
  "4004",  "nonag",       # rubber scrap
  "4707",  "forestry",    # recovered paper
  "4906",  "nonag",       # plans, drawings, manuscripts
  "5805",  "nonag",
  "6309",  "nonag",       # worn clothing
  "6310",  "nonag",       # rags
  "7112",  "nonag",       # precious metal scrap
  "7204",  "nonag",       # ferrous scrap
  "7404",  "nonag",       # copper scrap
  "7503",  "nonag",       # nickel scrap
  "7602",  "nonag",       # aluminium scrap
  "7802",  "nonag",       # lead scrap
  "7902",  "nonag",       # zinc scrap
  "8002",  "nonag",       # tin scrap
  "8908",  "nonag",       # vessels for breaking up
  "9701",  "nonag",
  "9702",  "nonag",
  "9703",  "nonag",
  "9704",  "nonag",
  "9705",  "nonag",
  "9706",  "nonag"
)

merge_hs4 <- merge_hs4 %>%
  mutate(hs4 = sprintf("%04s", as.character(hs4))) %>%   # keep leading zeros
  # add any of the 32 that are absent entirely
  bind_rows(hs4_fill %>% filter(!hs4 %in% merge_hs4$hs4)) %>%
  # fill the ones present but NA
  left_join(hs4_fill, by = "hs4", suffix = c("", ".f")) %>%
  mutate(
    subsector = coalesce(subsector, subsector.f)
  ) %>%
  select(hs4, subsector) %>%
  distinct() %>%
  arrange(hs4)

# --- checks: both should be TRUE / 0 -----------------------------------------
dup_hs4 <- merge_hs4 %>%  count(hs4) %>%  filter(n > 1) %>%  pull(hs4)
merge_hs4 %>%  filter(hs4 %in% dup_hs4) %>%  arrange(hs4)


merge_hs4 <- merge_hs4 %>% filter(!is.na(hs4))
length(unique(merge_hs4$hs4))
merge_hs4 <- merge_hs4 %>% distinct(hs4, subsector)
merge_hs4 <- merge_hs4 %>%
  group_by(hs4) %>%
  filter(!(subsector == "nonag" & n() > 1)) %>%
  ungroup() %>%
  distinct()

write_csv(merge_hs4, "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/crosswalk/clean_HS4_sub_sector.csv")

# edit the rest by hand
  
merge_hs4 <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/crosswalk/clean_HS4_sub_sector_edit.csv")
# 
# # different NAICS code level
# 
# # ---- 12a. NAICS 4 -------------------------------------------------------------
# # at naics 4
# names(dta1)
# 
# # Removing the 6-digit codes collapses many rows into duplicates
# # drop naics 6 levels to get distinct codes again
# dta2 <- dta1 %>% select(- naics6_D, -naics )
# 
# 
# # drop duplicates
# dta2 <- distinct(dta2)
# length(unique(dta1$HS6))
# 
# # Same source priority as before: Charlton first, Schott as fallback
# dta3 <- dta2 %>% rename(naics3_S = naics3, naics4_S = naics4) %>%
#   mutate(naics3 = if_else(is.na(naics3_D), naics3_S, naics3_D ),
#          naics4 = if_else(is.na(naics4_D), naics4_S, naics4_D ))
# 
# 
# write_csv(dta3, "/data/sikeme/TRADE/NTM_trade_war/data/crosswalk/clean_HS6_naics4_2012.csv")
# 
# 
# ######################################################################################
# # ---- 12b. NAICS 3 -------------------------------------------------------------
# 
# # at naics 3
# names(dta3)
# 
# # drop naics 4 levels to get distinct codes again
# dta4 <- dta3 %>% select(-naics4 , -naics4_D,-naics4_S, -naics3_S )
# 
# 
# # drop duplicates
# dta4 <- distinct(dta4)
# length(unique(dta4$HS6))
# # Any HS6 still appearing more than once is ambiguous at the 3-digit level
# test <- dta4 %>%  group_by(HS6) %>%    filter(n() > 1) %>%  ungroup()
# 
# 
# # Keep the first row per HS6 so shares computed later still sum correctly
# # sometimes one product code allocated to two different NAICS 3 digit code
# # can be a problem when calculating the shares where it might not match anymore....
# dta4_unique <- dta4 %>%  group_by(HS6) %>%
#   slice(1) %>%        # keep the first observation per HS6
#   ungroup()
# 
# write_csv(dta4_unique, "/data/sikeme/TRADE/NTM_trade_war/data/crosswalk/clean_HS6_naics3_2012.csv")