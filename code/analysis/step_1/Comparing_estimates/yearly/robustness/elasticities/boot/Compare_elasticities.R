
rm(list=ls()); gc()

library(data.table)
library(dplyr)
library(readr)
################################################################################
# directory: 
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git")
exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/Compare_values/yearly/robust/elast/"



elast <- fread("data/elast/clean_Elasticities_Soderbery2018.csv")
elast <- as.data.frame(elast)

# Aggregate to HS4 level
elast <- elast %>%
  group_by(ImporterISO3, hs4) %>%
  summarise(elasticities = mean(elasticities, na.rm = TRUE), .groups = "drop")

# HS2 from HS4
elast$hs4 <- sprintf("%04d", elast$hs4)
elast$hs2 <- as.numeric(substr(elast$hs4, 1, 2))

# HS section
elast$hs_section <- case_when(
  elast$hs2 %in% 1:5   ~ 1,
  elast$hs2 %in% 6:14  ~ 2,
  elast$hs2 == 15      ~ 3,
  elast$hs2 %in% 16:24 ~ 4,
  elast$hs2 %in% 25:27 ~ 5,
  elast$hs2 %in% 28:38 ~ 6,
  elast$hs2 %in% 39:40 ~ 7,
  elast$hs2 %in% 41:43 ~ 8,
  elast$hs2 %in% 44:46 ~ 9,
  elast$hs2 %in% 47:49 ~ 10,
  elast$hs2 %in% 50:63 ~ 11,
  elast$hs2 %in% 64:67 ~ 12,
  elast$hs2 %in% 68:70 ~ 13,
  elast$hs2 == 71      ~ 14,
  elast$hs2 %in% 72:83 ~ 15,
  elast$hs2 %in% 84:85 ~ 16,
  elast$hs2 %in% 86:89 ~ 17,
  elast$hs2 %in% 90:92 ~ 18,
  elast$hs2 == 93      ~ 19,
  elast$hs2 %in% 94:96 ~ 20,
  elast$hs2 == 97      ~ 21,
  .default = NA
)
# Sector
elast$sector <- ifelse(elast$hs_section %in% 1:4,  "Ag",
                       ifelse(elast$hs_section %in% 5:20, "Manu", "Other"))

# CHEN elasticities
elast$CHEN_elasticities <- ifelse(elast$sector == "Ag",   3,
                                  ifelse(elast$sector == "Manu", 1.97, 5))


names(elast)

# compare at sector level: 
summary <- elast %>% group_by(sector) %>%
  summarise(elasticities_mean =  mean(elasticities, na.rm = TRUE),
            elasticities_median =  median(elasticities, na.rm = TRUE),
            elasticities_sd =  sd(elasticities, na.rm = TRUE),
            CHEN_elasticities =  mean(CHEN_elasticities, na.rm = TRUE),)
  
summary
  
################################################################################

# at sector level:
sectors <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/crosswalk/clean_HS6_naics6_2012.csv")


# merge with subsector
sectors$HS6 <-  as.character(sectors$HS6)
unique(nchar(sectors$HS6))
sectors$HS6 <- ifelse(nchar(sectors$HS6) == 5, paste0("0", sectors$HS6), sectors$HS6)
sectors$hs4 <- as.character(substr(sectors$HS6, 1, 4)) 
sectors <- sectors %>% select(-naics, -naics_description)

test <- sectors %>%  group_by(hs4) %>%  summarise(n_subsector = n_distinct(subsector)) %>%  filter(n_subsector > 1)

sectors <- sectors %>%
  group_by(hs4) %>%
  mutate(
    non_na_sub = if (all(is.na(subsector))) NA_character_ else first(na.omit(subsector)),
    subsector = case_when(
      any(is.na(subsector)) & n_distinct(subsector, na.rm = TRUE) == 1 ~ non_na_sub,
      n_distinct(subsector) > 1 & any(subsector == "crop")      & any(subsector == "nonag")     ~ "crop",
      n_distinct(subsector) > 1 & any(subsector == "livestock")  & any(subsector == "nonag")     ~ "livestock",
      n_distinct(subsector) > 1 & any(subsector == "mining")     & any(subsector == "nonag")     ~ "nonag",
      n_distinct(subsector) > 1 & any(subsector == "forestry")   & any(subsector == "nonag")     ~ "nonag",
      n_distinct(subsector) > 1 & any(subsector == "crop")       & any(subsector == "livestock") ~ "crop",
      TRUE ~ subsector    )) %>%
  select(-non_na_sub) %>%  ungroup()
# verify no more conflicts
test <- sectors %>%  group_by(hs4) %>%  summarise(n_subsector = n_distinct(subsector)) %>%  filter(n_subsector > 1)

# for the NAs had to change them manually:
sectors <- sectors %>%
  mutate(subsector = case_when(
    # Livestock
    hs4 %in% c("0102", "0105","0207", "0405", "0407","0408", "2307", "2308", "2309",
               "4107", "4112", "4113") ~ "livestock",
    # Crop
    hs4 %in% c("0601", "0602", "0801", "0901", "0902", "0903",
               "0905", "0906", "0907", "0908", "0909", "1002",
               "1106", "1107", "1202", "1203", "1204", "1213",
               "1404", "1508", "1509", "1511", "1513", "1516","1510",
               "1517", "1518", "1520", "1702", "1704", "1801",
               "1802", "1803", "1804", "1805", "1806", "1902",
               "1903", "1905", "2002", "2003", "2006", "2007",
               "2102", "2104", "2106", "2203", "2204", "2205",
               "2206", "2208", "2209", "2305", "3826") ~ "crop",
    
    # Nonag
    hs4 %in% c("0308", "2201", "2503", "2848", "3504", "3823",
               "3824", "3825", "4010", "4807", "5603", "6115",
               "6908", "7217", "7508", "7907", "7920", "8469",
               "8471", "8486", "8528", "8548", "9620", "9700",
               "9800", "9801", "9804", "9805") ~ "nonag",
    # Nonag (anything above HS chapter 24)
    is.na(subsector) & as.numeric(hs4) > 2400 ~ "nonag",
    # Keep existing non-NA values
    TRUE ~ subsector  ))

sectors_hs4 <- sectors %>%  group_by(hs4, subsector) %>%
  summarise(ag_subsector = paste(unique(ag_subsector[!is.na(ag_subsector)]), collapse = ", "),  .groups = "drop"  )
length(unique(sectors_hs4$hs4))


elast1 <- left_join(elast,sectors_hs4 )

  
# compare at sector level: 
summary <- elast1 %>% group_by(subsector) %>%
  summarise(elasticities_mean =  mean(elasticities, na.rm = TRUE),
            elasticities_median =  median(elasticities, na.rm = TRUE),
            elasticities_sd =  sd(elasticities, na.rm = TRUE),
            CHEN_elasticities =  mean(CHEN_elasticities, na.rm = TRUE),)

summary

