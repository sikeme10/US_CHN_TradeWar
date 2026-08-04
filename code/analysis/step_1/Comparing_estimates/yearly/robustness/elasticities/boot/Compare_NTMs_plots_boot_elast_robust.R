################################################################################
#          Compare distribution of obtained AVEs based on baseline year

# check change in trade Vs change in AVEs?

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
library(readr); library(dplyr); library(fixest); library(ggplot2)

################################################################################
# directory: 
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git")
ROOT <- "/data/sikeme/TRADE/US_CHN_TradeWar_git"

exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/Compare_values/yearly/robust/elast/"


################################################################################
dta  <- read_csv(paste0(exp, "US_ln_NTMs_base_2015_2017_hs4_FE_boot.csv")) # default quant: 0.01
dta  <- read_csv(paste0(exp, "US_ln_NTMs_base_2015_2017_hs4_FE_boot_0.05.csv"))

names(dta)

table(dta$year)


# dta1 <- read_csv("output/Compare_values/yearly/robust/elast/US_ln_NTMs_2015_2017_hs4_elast.csv")
# names(dta1)

################################################################################
sectors_hs4 <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/crosswalk/clean_HS4_sub_sector_edit.csv")



sectors_hs4$hs4 <- as.numeric(sectors_hs4$hs4)
dta$hs4 <- as.numeric(dta$hs4)

# join
dta <- left_join(dta, sectors_hs4) 
colSums(is.na(dta))
test <- dta %>% filter(is.na(subsector))
unique(test$hs4)




################################################################################
# look at pretrend
################################################################################

dta_2015 <- dta %>% filter(baseline_year == 2015)
table(dta_2015$year)


dta_2017 <- dta %>% filter(baseline_year == 2017)
table(dta_2017$year)

# 2015 is zero by construction in this series, so drop it.
# 2016 becomes the reference: the coefficient on 2017 is the pre-trend test.
dta_2015 <- dta_2015 %>%  filter(year != 2015) %>%  mutate(year_f = factor(year))

################################################################################
# 2. Event study, one outcome
################################################################################
mod_year <- feols(diff_ln_AVE_FE_wmean_w_mean ~ i(year_f, ref = "2016") | hs4,
                  data = dta_2015, cluster = ~hs4)
summary(mod_year)
iplot(mod_year, plot_prms = list(las = 1))



# By sector
unique(dta_2015$sector)

dta_2015_sub <- dta_2015 %>% filter(sector == "Ag")

mod_year <- feols(diff_ln_AVE_FE_wmean_w_mean ~ i(year_f, ref = "2016") | hs4,
                  data = dta_2015_sub, cluster = ~hs4)

summary(mod_year)
iplot(mod_year, plot_prms = list(las = 1))




# by subsector
unique(dta_2015$subsector)

dta_2015_sub <- dta_2015 %>% filter(subsector == "crop")

mod_year <- feols(diff_ln_AVE_FE_wmean_w_mean ~ i(year_f, ref = "2016") | hs4,
                  data = dta_2015_sub, cluster = ~hs4)

summary(mod_year)
iplot(mod_year, plot_prms = list(las = 1))





theme_trade <- theme_minimal(base_size = 14, base_family = "Times New Roman") +
  theme(
    panel.spacing.x  = unit(1.2, "lines"),
    plot.title       = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.6),
    strip.background = element_rect(fill = "grey92", color = "black", linewidth = 0.6),
    strip.text       = element_text(size = 12, face = "bold", color = "black"),
    axis.text.x  = element_text(size = 11),
    axis.text.y  = element_text(size = 11),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "top",
    legend.text  = element_text(size = 12),
    legend.title = element_blank()
  )

library(broom)

subsectors <- c("crop", "livestock", "nonag")

coefs_list <- lapply(subsectors, function(s) {
  d <- dta_2015 %>% filter(subsector == s)
  mod <- feols(diff_ln_AVE_FE_wmean_w_mean ~ i(year_f, ref = "2016") | hs4,
               data = d, cluster = ~hs4)
  broom::tidy(mod, conf.int = TRUE) %>%
    mutate(subsector = s)
})

coefs_all <- bind_rows(coefs_list) %>%
  mutate(
    year = as.numeric(gsub("year_f::", "", term)),
    subsector = str_to_title(subsector)
  )

ref_rows <- data.frame(
  year = 2016,
  estimate = 0, conf.low = 0, conf.high = 0,
  subsector = str_to_title(subsectors)
)

coefs_plot <- bind_rows(
  coefs_all %>% select(year, estimate, conf.low, conf.high, subsector),
  ref_rows
) %>% arrange(subsector, year)

p <- ggplot(coefs_plot, aes(x = year, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.3, fatten = 1.5, linewidth = 0.5) +
  facet_wrap(~ subsector) +
  labs(x = "Year", y = "Coefficient (ref. 2016)") +
  theme_trade

p

ggsave(  filename = paste0(exp, "/event_study_pretrend_subsector_2015.png"),
  plot = p,  width = 12, height = 4, dpi = 300, bg = "white")


################################################################################
names(dta_2017)

library(ggplot2)
names(dta_2017)

theme_trade <- theme_minimal(base_size = 14, base_family = "Times New Roman") +
  theme(
    panel.spacing.x  = unit(1.2, "lines"),
    plot.title       = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    panel.border     = element_rect(color = "black", fill = NA, linewidth = 0.6),
    strip.background = element_rect(fill = "grey92", color = "black", linewidth = 0.6),
    strip.text       = element_text(size = 12, face = "bold", color = "black"),
    axis.text.x  = element_text(size = 11),
    axis.text.y  = element_text(size = 11),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.position = "top",
    legend.text  = element_text(size = 12),
    legend.title = element_blank()
  )

p <- dta_2017 %>%
  filter(subsector %in% c("crop", "livestock", "nonag")) %>%
  mutate(subsector = str_to_title(subsector)) %>%
  ggplot(aes(x = diff_ln_AVE_FE_wmean_w_mean, y = diff_ln_AVE_FE_wmean_Chen_w_mean)) +
  geom_point(alpha = 0.5,  size = 0.8) +
  geom_smooth(method = "lm", formula = y ~ x - 1, se = TRUE, color = "red")+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "blue") +
  facet_wrap(~ subsector) +
  labs(
    x = "\u0394 ln(1+AVE) (Soderbery et al.)",
    y = "\u0394 ln(1+AVE)  (Chen et al.)"  ) +
  theme_trade
p

OUT_PLOT_DIR <- file.path(ROOT, "output/Compare_values/yearly/robust/elast/plot", as.character(2017), "boot")
ggsave(filename = paste0(exp, "correlation_soderbery_chen_demean.png"),
  plot = p,  width = 10, height = 4, dpi = 300, bg = "white")


# normale FE model 
p <- dta_2017 %>%
  filter(subsector %in% c("crop", "livestock", "nonag")) %>%
  mutate(subsector = str_to_title(subsector)) %>%
  ggplot(aes(x = diff_ln_AVE_FE_w_mean, y = diff_ln_AVE_FE_Chen_w_mean)) +
  geom_point(alpha = 0.5,  size = 0.8) +
  geom_smooth(method = "lm", formula = y ~ x - 1, se = TRUE, color = "red")+
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "blue") +
  facet_wrap(~ subsector) +
  labs(
    x = "\u0394 ln(1+AVE) (Soderbery et al.)",
    y = "\u0394 ln(1+AVE)  (Chen et al.)"  ) +
  theme_trade
p

OUT_PLOT_DIR <- file.path(ROOT, "output/Compare_values/yearly/robust/elast/plot", as.character(2017), "boot")
ggsave(filename = paste0(exp, "correlation_soderbery_chen_FE.png"),
       plot = p,  width = 10, height = 4, dpi = 300, bg = "white")



