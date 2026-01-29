



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

################################################################################
# directory: 
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git")
exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/Compare_values/yearly/robust/"

################################################################################
# 1) Load data 
################################################################################

US <- read_csv(paste0(exp, "US_ln_NTMs_base_2015_FE_boot.csv"))

################################################################################

# For US manu: 

# create simple average change and weighted average change in 
US_manu <- US %>% filter(sector == "Manu")
names(US_manu)
unique(US_manu$year)
unique(US_manu$hs2)
unique(US_manu$hs_section)
length(unique(US_manu$hs6_H5))

# manu sector level: create weight in trade (hs6/total US export)
# weights for hs6 to aggregate to sector level
US_manu_2015_hs6 <- US_manu %>%
  filter(year == 2015) %>%
  group_by(hs6_H5) %>%
  summarise(Trade_value_USD_2015 = sum(Trade_value_USD, na.rm = TRUE),  .groups = "drop"  ) %>%
  mutate(  tot_Trade_value_USD_2015 = sum(Trade_value_USD_2015),
           weight_sector = Trade_value_USD_2015 / tot_Trade_value_USD_2015  )
US_manu <-left_join(US_manu,US_manu_2015_hs6 )


# HS section level : weights for hs6 to aggregate to HS-section
US_manu_2015_hs_sect <- US_manu %>%  filter(year == 2015) %>%
  group_by(hs_section, hs6_H5) %>%
  summarise( Trade_value_USD_2015 = sum(Trade_value_USD, na.rm = TRUE),  .groups = "drop_last"    ) %>%
  group_by(hs_section) %>%
  mutate(  hs_sect_tot_Trade_value_USD_2015 = sum(Trade_value_USD_2015),
    weight_hs_sect = if_else( hs_sect_tot_Trade_value_USD_2015 > 0,   
                              Trade_value_USD_2015 / hs_sect_tot_Trade_value_USD_2015,    NA_real_ )) %>%
  ungroup()
US_manu <-left_join(US_manu,US_manu_2015_hs_sect )


# HS2 level : weights for hs6 to aggregate to Hs2
US_manu_2015_hs2 <- US_manu %>%  filter(year == 2015) %>%
  group_by(hs2, hs6_H5) %>%
  summarise( Trade_value_USD_2015 = sum(Trade_value_USD, na.rm = TRUE),  .groups = "drop_last"    ) %>%
  group_by(hs2) %>%
  mutate(  hs2_tot_Trade_value_USD_2015 = sum(Trade_value_USD_2015),
           weight_hs2 = if_else( hs2_tot_Trade_value_USD_2015 > 0,   
                                     Trade_value_USD_2015 / hs2_tot_Trade_value_USD_2015,    NA_real_ )) %>%
  ungroup()
US_manu <-left_join(US_manu,US_manu_2015_hs2 )


################################################################################
# 3) Add Chen et al estimates
################################################################################
# add Chen et al. estimations 
Chen <- read_csv("data/chen_NTB_tariff/hs2_agriculture_manufacturing_clean.csv")
names(Chen)


Chen <- Chen %>% select(-Country, - ISO3_Code) %>% rename(hs2 = HS2 , Chen_US_import_share = US_import_share,
                                                          diff_log_tariff_Chen = tau_tariff_CHN, 
                                                          diff_ln_AVE_chen = tau_NTB)
US_manu$hs2 <- as.numeric(US_manu$hs2)
US_manu <- left_join(US_manu,Chen)
names(US_manu)

# create weights for Chen et al at sector level and HS section

# Manu sector level: create weight in trade (hs6/total US export)
# weights for hs6 to aggregate to sector level
US_manu_2015_hs2_chen <- US_manu %>%
  filter(year == 2015) %>%  group_by(hs2) %>%
  summarise(Trade_value_USD_2015 = sum(Trade_value_USD, na.rm = TRUE),.groups = "drop"  ) %>%
  mutate(  chen_tot_Trade_value_USD_2015 = sum(Trade_value_USD_2015, na.rm = TRUE),
    weight_sector_chen = if_else( chen_tot_Trade_value_USD_2015 > 0,Trade_value_USD_2015 / chen_tot_Trade_value_USD_2015,
      NA_real_    )  ) %>% select(-Trade_value_USD_2015 )
US_manu <-left_join(US_manu,US_manu_2015_hs2_chen )


# HS section level : weights for hs2 to aggregate to HS-section
US_manu_2015_hs_sect_chen <- US_manu %>%  
  filter(year == 2015) %>%
  group_by(hs_section, hs2) %>%
  summarise(Trade_value_USD_2015 = sum(Trade_value_USD, na.rm = TRUE),
    .groups = "drop_last"  ) %>%
  group_by(hs_section) %>%
  mutate( Chen_hs_sect_tot_Trade_value_USD_2015 = sum(Trade_value_USD_2015, na.rm = TRUE),
    weight_hs_sect_Chen = if_else(Chen_hs_sect_tot_Trade_value_USD_2015 > 0,Trade_value_USD_2015 / Chen_hs_sect_tot_Trade_value_USD_2015,
      NA_real_    )  ) %>%  ungroup() %>% select(-Trade_value_USD_2015 )
US_manu <-left_join(US_manu, US_manu_2015_hs_sect_chen )




################################################################################

US_manu <- US_manu %>% filter(year>2015)



################################################################################
# 4) Plot change in ln(1+AVE)
################################################################################


  # create a theme for ggplot 
theme_trade <- theme_minimal(base_size = 14) +
  theme(
    panel.spacing.x = unit(1.2, "lines"),
    plot.title = element_text(size = 11, hjust = 0.5),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background  = element_rect(fill = "white", color = NA),
    axis.text.x = element_text(size = 9),
    axis.text.y = element_text(size = 9),
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    legend.text  = element_text(size = 10),
    legend.title = element_text(size = 10)
  )

################################################################################
# a) sector level 
################################################################################

# trade weighted and simple average trade costs :
names(US_manu)

US_manu_w <- US_manu %>%  filter(sector == "Manu") %>%  group_by(year, draw) %>%
  summarise( 
    w_FE        = weighted.mean(diff_ln_AVE_FE,        w = weight_sector, na.rm = TRUE),
    w_FE_bench  = weighted.mean(diff_ln_AVE_FE_bench,  w = weight_sector, na.rm = TRUE), # get quantiles 
    # 95% CI (across draws)
    FE_lo   = quantile(w_FE, 0.025, na.rm = TRUE),
    FE_hi   = quantile(w_FE, 0.975, na.rm = TRUE),
    
    w_chen      = weighted.mean(diff_ln_AVE_chen,      w = weight_sector_chen, na.rm = TRUE),
    w_tariff    = weighted.mean(diff_log_tariff_2015,  w = weight_sector, na.rm = TRUE),
    
    
    s_FE        = mean(diff_ln_AVE_FE,         na.rm = TRUE),
    s_FE_bench  = mean(diff_ln_AVE_FE_bench,   na.rm = TRUE),

    s_chen      = mean(diff_ln_AVE_chen,       na.rm = TRUE),# chen
    s_tariff    = mean(diff_log_tariff_2015,   na.rm = TRUE)  ) %>% ungroup() %>%
  mutate( w_chen = mean(w_chen, na.rm=TRUE),    s_chen = mean(s_chen, na.rm = TRUE))

US_manu_q <- US_manu_w %>%  group_by(year) %>%
  summarise(FE_mean = mean(w_FE, na.rm = TRUE),
            FE_lo   = quantile(w_FE, 0.025, na.rm = TRUE),
            FE_hi   = quantile(w_FE, 0.975, na.rm = TRUE),
            
            FEb_mean = mean(w_FE_bench, na.rm = TRUE),
            chen_mean = mean(w_chen, na.rm = TRUE),
            tariff_mean = mean(w_tariff, na.rm = TRUE),    .groups = "drop"  )




plot <- ggplot(US_manu_q, aes(x = year)) +
  
  ## FE CI
  geom_ribbon(  aes(ymin = FE_lo, ymax = FE_hi), fill = "blue", alpha = 0.2  ) +
  geom_line(aes(y = FE_mean, color = "FE"),    linewidth = 1  ) +
  
  ## Tariff
  geom_line(aes(y = tariff_mean, color = "tariff"),   linewidth = 1  ) +
  
  ## Benchmark
  geom_line(  aes(y = FEb_mean, color = "FE_bench"),  linetype = "dashed",  linewidth = 1  ) +
  
  scale_color_manual(
    values = c( "FE" = "blue","tariff" = "darkgreen", "FE_bench" = "blue"),    name = "Variables"  ) +
  labs(  title = "Weighted average Δ ln(1+AVE), Manufacturing (relative to 2015)",
    x = "Year",    y = "Weighted Δ ln(1+AVE)"  ) +
  theme_trade

plot
ggsave(filename = file.path(exp, "plot/", "Compare_ln_AVE_Manu_weight_boot.png"),plot = plot, width = 8, height = 5, dpi = 300)





################################################################################
# a) HS section level
################################################################################

names(US_manu)
US_manu_w_sect <- US_manu %>% filter(sector == "Manu") %>%
  group_by(year, draw,  hs_section) %>% summarise( 
    w_FE        = weighted.mean(diff_ln_AVE_FE,        w = weight_hs_sect, na.rm = TRUE),
    w_FE_bench  = weighted.mean(diff_ln_AVE_FE_bench,  w = weight_hs_sect, na.rm = TRUE),
   
    w_chen      = weighted.mean(diff_ln_AVE_chen,      w = weight_hs_sect_Chen, na.rm = TRUE),
    w_tariff    = weighted.mean(diff_log_tariff_2015,  w = weight_hs_sect, na.rm = TRUE),
    s_FE        = mean(diff_ln_AVE_FE,         na.rm = TRUE),
    s_FE_bench  = mean(diff_ln_AVE_FE_bench,   na.rm = TRUE),
    
    s_chen      = mean(diff_ln_AVE_chen,       na.rm = TRUE),
    s_tariff    = mean(diff_log_tariff_2015,   na.rm = TRUE) ,
    .groups = "drop"  ) %>%  group_by(hs_section) %>%
  mutate(  w_chen = mean(w_chen, na.rm = TRUE),   s_chen = mean(s_chen, na.rm = TRUE)  )

US_manu_q_sect <- US_manu_w_sect %>%  group_by(year, hs_section) %>%
  summarise(  FE_mean = mean(w_FE, na.rm = TRUE),
    FE_lo   = quantile(w_FE, 0.025, na.rm = TRUE),
    FE_hi   = quantile(w_FE, 0.975, na.rm = TRUE),
    FEb_mean = mean(w_FE_bench, na.rm = TRUE),
    tariff_mean = mean(w_tariff, na.rm = TRUE),
    # optional: Chen CI too (comment out if you don't want it)
    chen_mean = mean(w_chen, na.rm = TRUE),
    chen_lo   = quantile(w_chen, 0.025, na.rm = TRUE),
    chen_hi   = quantile(w_chen, 0.975, na.rm = TRUE),    .groups = "drop"  )


plot <- ggplot(US_manu_q_sect, aes(x = year)) +
  
  # FE CI band
  geom_ribbon(aes(ymin = FE_lo, ymax = FE_hi), fill = "blue", alpha = 0.2) +
  geom_line(aes(y = FE_mean, color = "FE"), linewidth = 1) +
  
  # tariff
  geom_line(aes(y = tariff_mean, color = "tariff"), linewidth = 1) +
  
  # benchmark (dashed)
  geom_line(aes(y = FEb_mean, color = "FE_bench"), linetype = "dashed", linewidth = 1) +
  
  # optional: Chen CI + line
  geom_line(aes(y = chen_mean, color = "Chen_et_al"), linewidth = 1) +
  
  scale_color_manual(
    values = c("FE" = "blue", "tariff" = "darkgreen", "FE_bench" = "blue", "Chen_et_al" = "purple"),
    labels = c("FE" = "FE", "tariff" = "Tariff", "FE_bench" = "FE (benchmark)", "Chen_et_al" = "Chen et al."),
    name = "Variables"  ) +
  facet_wrap(~ hs_section, scales = "free_y") +
  labs(   title = "Weighted average Δ ln(1+AVE), Manufacturing by HS section (relative to 2015)",
    x = "Year",    y = "Weighted Δ ln(1+AVE)"  ) +
  theme_trade
  
plot

ggsave(filename = file.path(exp, "plot/", "Compare_ln_AVE_Manu_hs_sect_weight_boot.png"),plot = plot, width = 9, height = 5, dpi = 300)




################################################################################
# a) HS 2 level level
################################################################################


US_manu_w_hs2 <- US_manu %>%  filter(sector == "Manu") %>%  group_by(year, draw,hs2) %>%
  summarise( 
    w_FE        = weighted.mean(diff_ln_AVE_FE,        w = weight_hs2, na.rm = TRUE),
    w_FE_bench  = weighted.mean(diff_ln_AVE_FE_bench,  w = weight_hs2, na.rm = TRUE), 
    
    w_chen      = mean(diff_ln_AVE_chen, na.rm = TRUE),
    w_tariff    = weighted.mean(diff_log_tariff_2015,  w = weight_hs2, na.rm = TRUE),
    
    
    s_FE        = mean(diff_ln_AVE_FE,         na.rm = TRUE),
    s_FE_bench  = mean(diff_ln_AVE_FE_bench,   na.rm = TRUE),
    
    s_chen      = mean(diff_ln_AVE_chen,       na.rm = TRUE),# chen
    s_tariff    = mean(diff_log_tariff_2015,   na.rm = TRUE)  ) %>% ungroup()




# get a summary table (only interested in mean change between 2018 and 2019

summary <- US_manu_w_hs2 %>%  filter(year %in% c(2018,2019)) %>% group_by(hs2) %>%
  summarise(FE_mean = mean(w_FE, na.rm = TRUE),
            FE_med = median(w_FE, na.rm = TRUE),
            FE_lo   = quantile(w_FE, 0.025, na.rm = TRUE),
            FE_hi   = quantile(w_FE, 0.975, na.rm = TRUE),
            
            FEbench_mean = mean(w_FE_bench, na.rm = TRUE),
            FEbench_med = median(w_FE_bench, na.rm = TRUE),
            FEbench_lo   = quantile(w_FE_bench, 0.025, na.rm = TRUE),
            FEbench_hi   = quantile(w_FE_bench, 0.975, na.rm = TRUE),
            
            chen_mean = mean(w_chen, na.rm = TRUE),
            tariff_mean = mean(w_tariff, na.rm = TRUE),    .groups = "drop"  )
unique(US_manu_w_hs2$hs2)

hs2_desc <- data.frame(
  hs2 = sprintf("%02d", 25:96),
  description = c(
    # 25–27 Mineral products
    "Salt; sulphur; earths and stone; lime and cement",      # 25
    "Ores, slag and ash",                                    # 26
    "Mineral fuels, oils, waxes",                            # 27
    
    # 28–38 Chemicals
    "Inorganic chemicals; precious metal compounds",         # 28
    "Organic chemicals",                                     # 29
    "Pharmaceutical products",                               # 30
    "Fertilizers",                                           # 31
    "Dyes, pigments, paints, inks",                          # 32
    "Essential oils, perfumes, cosmetics",                   # 33
    "Soap, detergents, waxes, cleaning products",            # 34
    "Albuminoidal substances; glues; enzymes",               # 35
    "Explosives; pyrotechnics; matches",                     # 36
    "Photographic and cinematographic goods",                # 37
    "Miscellaneous chemical products",                       # 38
    
    # 39–40 Plastics & rubber
    "Plastics and articles thereof",                         # 39
    "Rubber and articles thereof",                           # 40
    
    # 41–43 Hides, leather, furs
    "Raw hides and skins; leather",                          # 41
    "Articles of leather; saddlery; travel goods",           # 42
    "Furskins and artificial fur",                           # 43
    
    # 44–46 Wood & related
    "Wood and articles of wood; charcoal",                   # 44
    "Cork and articles of cork",                             # 45
    "Straw, esparto, basketware",                            # 46
    
    # 47–49 Pulp, paper, printing
    "Pulp of wood; recovered paper",                         # 47
    "Paper and paperboard; articles",                        # 48
    "Printed books, newspapers, manuscripts",                # 49
    
    # 50–63 Textiles & apparel
    "Silk",                                                  # 50
    "Wool and animal hair",                                  # 51
    "Cotton",                                                # 52
    "Other vegetable textile fibres",                        # 53
    "Man-made filaments",                                    # 54
    "Man-made staple fibres",                                # 55
    "Wadding, felt, nonwovens; ropes",                       # 56
    "Carpets and textile floor coverings",                   # 57
    "Special woven fabrics; lace; tapestries",               # 58
    "Impregnated, coated or laminated textiles",             # 59
    "Knitted or crocheted fabrics",                          # 60
    "Knitted or crocheted apparel",                          # 61
    "Apparel, not knitted or crocheted",                     # 62
    "Other made-up textile articles",                        # 63
    
    # 64–67 Footwear & headgear
    "Footwear and parts thereof",                            # 64
    "Headgear and parts thereof",                            # 65
    "Umbrellas, walking-sticks, whips",                      # 66
    "Prepared feathers; artificial flowers",                 # 67
    
    # 68–70 Stone, ceramics, glass
    "Articles of stone, plaster, cement",                    # 68
    "Ceramic products",                                      # 69
    "Glass and glassware",                                   # 70
    
    # 71 Precious materials
    "Pearls, precious stones, precious metals",              # 71
    
    # 72–83 Metals & metal products
    "Iron and steel",                                        # 72
    "Articles of iron or steel",                             # 73
    "Copper and articles thereof",                           # 74
    "Nickel and articles thereof",                           # 75
    "Aluminium and articles thereof",                        # 76
    "Reserved (unused HS chapter)",                          # 77  <-- add this
    "Lead and articles thereof",                             # 78
    "Zinc and articles thereof",                             # 79
    "Tin and articles thereof",                              # 80
    "Other base metals; cermets",                            # 81
    "Tools, cutlery, base metal articles",                   # 82
    "Miscellaneous articles of base metal",                  # 83
    
    # 84–85 Machinery & electrical
    "Machinery and mechanical appliances",                   # 84
    "Electrical machinery and equipment",                    # 85
    
    # 86–89 Transport equipment
    "Railway or tramway locomotives and parts",              # 86
    "Vehicles and parts thereof",                            # 87
    "Aircraft, spacecraft and parts",                        # 88
    "Ships, boats and floating structures",                  # 89
    
    # 90–92 Precision & instruments
    "Optical, medical, measuring instruments",               # 90
    "Clocks and watches",                                    # 91
    "Musical instruments",                                   # 92
    
    # 93–96 Miscellaneous manufactures
    "Arms and ammunition",                                   # 93
    "Furniture; bedding; lamps",                             # 94
    "Toys, games, sports equipment",                         # 95
    "Miscellaneous manufactured articles"                    # 96
  ),
  stringsAsFactors = FALSE
)

hs2_desc$hs2 <- as.numeric(hs2_desc$hs2)

summary <- summary %>%  left_join(hs2_desc, by = "hs2")

library(writexl)
write_xlsx( list("hs2_summary" = summary),  path = file.path(exp, "hs2_summary_manu_boot.xlsx"))










