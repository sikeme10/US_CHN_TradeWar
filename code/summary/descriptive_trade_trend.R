
library(tidyr)
library(dplyr)
library(readr)
library(concordance)
library(readr)
library(readxl )


US_CHN_WLD <- read_csv("/data/sikeme/TRADE/NTM_trade_war/code/summary/US_CHN_WLD.csv")
View(US_CHN_WLD)



library(readr)
US_export <- read_csv("/data/sikeme/TRADE/NTM_trade_war/code/summary/US_export.csv")
View(US_export)
head(US_export)
unique(US_export$TradeFlowName)

dta <- US_export %>% filter(TradeFlowName == "Gross Exp." & ReporterISO3 == "USA") %>% rename(
  exp_val = `TradeValue in 1000 USD`)
names(dta)
head(dta)
unique(dta$PartnerISO3)

dta_wide <- dta %>%
  select(PartnerISO3, Year, ProductCode, exp_val) %>% 
  pivot_wider(
    names_from = PartnerISO3,
    values_from = exp_val,
    values_fill = list(exp_val = 0),
    values_fn   = list(exp_val = sum),  # in case duplicates exist
    names_glue  = "exp_val_{PartnerISO3}"  # custom column names
  )


# Get HS code description; 
HSCodeandDescription <- read_excel("crosswalk/HSCodeandDescription.xlsx", sheet = "HS12")
names(HSCodeandDescription)
HS2 <- HSCodeandDescription %>% filter(Level ==2)
names(HS2)


dta_wide <- dta_wide %>%  left_join(HS2, by = c("ProductCode" = "Code"))


names(dta_wide)
class(dta_wide$ProductCode)
unique(dta_wide$ProductCode)
dta_wide$ProductCode <- as.numeric(dta_wide$ProductCode)


dta_wide <- dta_wide %>%
  mutate(
    share_CHN_exp = exp_val_CHN / exp_val_WLD * 100,
    sector = if_else(ProductCode %in% c(1:14), "Ag", "NonAg") # agricultural HS are from 1 to 14 
  )


Ag_export <- dta_wide %>% filter(sector == "Ag")
names(Ag_export)






library(ggplot2)

ggplot(subset(dta_wide, sector == "Ag"), aes(x = Year, y = share_CHN_exp, color = as.factor(Description))) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    x = "Year",
    y = "China Share of Exports (%)",
    color = "Product Code",
    title = "Share of U.S. Exports to China over Time by Product Code"
  ) +
  theme_minimal()




ggplot(subset(dta_wide, sector == "Ag"), aes(x = Year, y = exp_val_CHN, color = as.factor(Description))) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    x = "Year",
    y = "China Share of Exports (%)",
    color = "Product Code",
    title = " U.S. Exports to China over Time by Product Code"
  ) +
  theme_minimal()

ggplot(subset(dta_wide, sector == "Ag"), aes(x = Year, y = exp_val_CHN, color = as.factor(Description))) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    x = "Year",
    y = "China Share of Exports (%)",
    color = "Product Code",
    title = " U.S. Exports to China over Time by Product Code"
  ) +
  theme_minimal()


ggplot(subset(dta_wide, sector == "Ag"), aes(x = Year, y = exp_val_WLD, color = as.factor(Description))) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    x = "Year",
    y = "China Share of Exports (%)",
    color = "Product Code",
    title = " U.S. Exports to WLD over Time by Product Code"
  ) +
  theme_minimal()





