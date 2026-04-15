



################################################################################
# we create two variables here: fraction of US industry i sold domestically

################################################################################


library(readr)
library(tidyr)
library(dplyr)
library(data.table)
library(stringi)
library(fixest)
library(countrycode)
library(tidyverse)
library(vroom)




rm(list=ls())
# Set directory
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/")
getwd()


###############################################################################
# At commuting zones 


dta <- read_dta("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/crosswalk_CZ_county/cw_cty_czone.dta")

###############################################################################

# 12025 → 12086: Miami-Dade FL renamed in 1997, 2012 code is 12086
# 8014 added: Broomfield CO created in 2001, not in crosswalk at all → new row needed with CZ 28900
# 30113 removed: Yellowstone NP territory merged into 30031/30067 in 1997, gone by 2012
# 46131 removed: Washabaugh SD merged into 46071 in 1983, gone by 2012
# 51560 removed: Clifton Forge VA merged into 51005 in 2001, gone by 2012
# 51780 removed: South Boston VA merged into 51083 in 1995, gone by 2012
# Everything else: same FIPS in 1990 and 2012

# data from 1990, need to account for changes in county to 2012

names(dta)


dta <- dta %>% rename(cty_fips_1990 = cty_fips, czone_1990    = czone) %>%
  mutate(cty_fips_2012 = cty_fips_1990,   czone_2012    = czone_1990)


########################################################################
# in 1990s:
# Florida, 1997: Dade county (FIPS 12025) is renamed as Miami-Dade
# county (FIPS 12086).
# Action: replace FIPS code 12086 with the old code 12025
# → in 2012 data the county appears as 12086, so we update cty_fips_2012 to 12086
dta <- dta %>%
  mutate(cty_fips_2012 = if_else(cty_fips_1990 == 12025L, 12086L, cty_fips_2012))


# in 2000s:
# Colorado, 2001: Broomfield county (FIPS 8014) is created out of parts of
# Adams, Boulder, Jefferson, and Weld counties. The Census Bureau
# estimates that the resulting population loss was 21,512 for Boulder,
# 15,870 for Adams, 1,726 for Jefferson, and 69 for Weld county.
# Action: assign FIPS code 8014 to CZ 28900 which comprises the three
# counties Adams, Boulder and Jefferson.
# → Broomfield does not exist in the 1990 crosswalk, so we add a new row
dta <- dta %>% bind_rows(tibble(cty_fips_1990 = NA_integer_,
                   czone_1990    = NA_integer_,
                   cty_fips_2012 = 8014L,
                   czone_2012    = 28900L))



# Final crosswalk: select and arrange
dta <- dta %>%  select(cty_fips_1990, cty_fips_2012, czone_1990, czone_2012) %>%
  arrange(cty_fips_2012)

write_csv(dta, "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/crosswalk_CZ_county/cw_cty_czone_2012.csv")


###############################################################################

dta <- read_csv( "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/crosswalk_CZ_county/cw_cty_czone_2012.csv")
names(dta)
class(dta$cty_fips_2012)

# Extract state FIPS from county FIPS
dta <- dta %>%  mutate(state_fips = as.integer(cty_fips_2012 %/% 1000))

# State to census division crosswalk (source: U.S. Census Bureau)
state_division <- data.frame(
  state_fips = c(
    9,23,25,33,44,50,          # Div 1: New England
    34,36,42,                   # Div 2: Middle Atlantic
    17,18,26,39,55,             # Div 3: East North Central
    19,20,27,29,31,38,46,       # Div 4: West North Central
    10,11,12,13,24,37,45,51,54, # Div 5: South Atlantic
    1,21,28,47,                 # Div 6: East South Central
    5,22,40,48,                 # Div 7: West South Central
    4,8,16,30,32,35,49,56,      # Div 8: Mountain
    2,6,15,41,53                # Div 9: Pacific
  ),
  division = c(
    rep(1,6), rep(2,3), rep(3,5), rep(4,7),
    rep(5,9), rep(6,4), rep(7,4), rep(8,8), rep(9,5)
  )
)

# Merge division onto county data
dta <- dta %>%  left_join(state_division, by = "state_fips")

# Check no missing
sum(is.na(dta$division))

# Assign each CZ to the division where most of its counties are
cz_division <- dta %>%  count(czone_2012, division) %>%
  group_by(czone_2012) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(czone_2012, division)


write_csv(cz_division, "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/crosswalk_CZ_county/census_div_czone_2012.csv")



