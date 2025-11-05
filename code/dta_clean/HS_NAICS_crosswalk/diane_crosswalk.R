


library(stringr)
library(haven)
library(concordance)
dta <- read_dta("crosswalk/HS6_NAICS_Diane/mappinghsnaics6.dta")
names(dta)

#################################################################################

# Product level codes are at HS0 revisions 

dta <- dta %>% rename(  ProductCode_H0 = hs,  naics6 = naics)
class(dta$ProductCode_H0)

# product code as character:

dta$ProductCode_H0 <- str_pad(as.character(dta$ProductCode_H0), width = 6, side = "left", pad = "0")
length(unique(dta$ProductCode_H0))
unique(dta$ProductCode_H0)
unique(nchar(dta$ProductCode_H0))


# concordance:
dta$ProductCode_H4 <-  concord_hs(sourcevar = dta$ProductCode_H0,  origin = "HS4",  destination = "HS0", dest.digit  = 6 )
dta %>%  filter(ProductCode_H0 != ProductCode_H4) %>% summarise(n_distinct_observations = n())
length(unique(dta$ProductCode_H4))
length(unique(dta$ProductCode_H0))
colSums(is.na(dta))


class(dta$ProductCode_H4)

dta <-dta %>% rename(HS6 = ProductCode_H4)
dta$HS2 <- as.character(substr(dta$HS6, 1, 2))


#####################
names(dta)

dta <- dta %>% select(HS2 , HS6 , naics2,naics3,naics4, naics6, j, crop) %>% rename(
  naics6_D = naics6 , naics2_D = naics2, naics3_D = naics3 , naics4_D = naics4)

write_csv(dta, "crosswalk/HS6_NAICS_Diane/NAICS_HS_2012.csv")












