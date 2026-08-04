




rm(list=ls())
# Set directory
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git/data")
getwd()

# HS 2012 revision (HS2012)


library(haven)
dta <- read_dta("crosswalk/schott/hs_sic_naics_exports_89_123_20240801.dta")
output_NAICS6 <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/Census_output/output_NAICS_6.csv")

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
dta_2012$naics5 <- as.character(substr(dta_2012$naics, 1, 5))

dta_2012 <- dta_2012 %>% select(year,naics3 ,naics4,naics5,naics, HS2, HS6, HS10)


################################################################################
# aggregate at hs6 level 
names(dta_2012)

dta_2012 <- dta_2012 %>% select(-HS10)

# drop duplicates
sum(duplicated(dta_2012[, c("HS6", "naics")]))
dta_2012 <- dta_2012 %>% distinct()


################################################################################
# check NAICS that have "X" 
# if the NAICS have X we will assign it all NAICS that are in the upper NAICSdigit u

X_vals <- dta_2012 %>%  filter(!grepl("^[0-9]+$", naics)) %>% select(-c("naics3" ,"naics4", "naics5"))

length(unique(dta_2012$naics))


# we use NAICS 6 digit  data from the output data for that to get best concordance 
names(output_NAICS6)
length(unique(output_NAICS6$NAICS6_2012))
naics_codes <- output_NAICS6 %>% select(NAICS6_2012, NAICS_description) %>% rename(naics6 = NAICS6_2012)
sapply(naics_codes, class)
unique(nchar(naics_codes$naics6 ))
naics_codes$naics3 <- as.character(substr(naics_codes$naics6, 1, 3))
naics_codes$naics4 <- as.character(substr(naics_codes$naics6, 1, 4))
naics_codes$naics5 <- as.character(substr(naics_codes$naics6, 1, 5))

X_vals <- X_vals %>%
  mutate(naics_prefix = str_remove_all(naics, "X+$"),
         match_level  = case_when(
           str_count(naics, "X") == 1 ~ "naics5",
           str_count(naics, "X") == 2 ~ "naics4",
           str_count(naics, "X") == 3 ~ "naics3",
           TRUE ~ "naics6"    )  )

# Join for each level then bind
expanded <- bind_rows(
  X_vals %>% filter(match_level == "naics5") %>%
    left_join(naics_codes %>% select(naics6, NAICS_description, naics3, naics4, naics5),
              by = c("naics_prefix" = "naics5")),
  
  X_vals %>% filter(match_level == "naics4") %>%
    left_join(naics_codes %>% select(naics6, NAICS_description, naics3, naics4, naics5),
              by = c("naics_prefix" = "naics4")),
  
  X_vals %>% filter(match_level == "naics3") %>%
    left_join(naics_codes %>% select(naics6, NAICS_description, naics3, naics4, naics5),
              by = c("naics_prefix" = "naics3")))


names(dta_2012)
names(expanded)

# merge back with origingal data 
expanded <- expanded %>% select(year, naics, HS2, HS6, naics6)
dta_2012 <- full_join(dta_2012, expanded)

dta_2012 <- dta_2012 %>%  mutate(naics = if_else(grepl("[^0-9]", naics), as.character(naics6), naics)) %>%
  select(-naics6)
X_vals <- dta_2012 %>%  filter(!grepl("^[0-9]+$", naics)) %>% select(-c("naics3" ,"naics4", "naics5"))


################################################################################
# export data 
write_csv(dta_2012, "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/crosswalk/schott/naics_HS_schott_2012.csv")







