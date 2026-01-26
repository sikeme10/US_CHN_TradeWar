
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
exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/FE/monthly/"

################################################################################
# 1) Load data 
################################################################################


dta <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_gravity/dta_CHN_gravity_3.csv")
names(dta)
colSums(is.na(dta))

################################################################################
# 2) Setting up data 
################################################################################

# get HS2 and HS 4 level 
class(dta$hs6_H5)
unique(nchar(dta$hs6_H5))
dta <- dta %>%  mutate(hs2 = substr( hs6_H5 ,1,2),
         hs4 = substr( hs6_H5 ,1,4))
length(unique((dta$hs2)))
length(unique((dta$hs4)))

# get log of tariffs:
dta <- dta %>% mutate(log_tariff = log(1+Applied_tariff/100))



################################################################################
# 3) On one HS2: try to regress and get residual over time 
################################################################################

# 
# # get unique HS2 to pool it
# unique_HS2 <- unique(dta$hs2)
# 
# HS_val <- unique_HS2[1]
# 
# # Subset the data for the current HS value
# sub_dta <- subset(dta, hs2 == HS_val)
# names(sub_dta)
# 
# 
# 
#   
# # filter if have 0s in trade values for all product at HS2 and a specific partner
# sub_dta1 <- sub_dta %>%
#   group_by(ExporterISO3) %>%
#   filter(any(Trade_value_USD != 0)) %>%   # keep only groups where at least ONE value is non-zero
#   ungroup()
# 
# 
# library(fixest)
# reg <- fepois(
#   Trade_value_USD ~ 
#     contig + dist + comlang_off + Colonial_ties + rta + fta_and_eia +
#     Importer_GDP + Exporter_wto + Exporter_eu + Exporter_GDP_current_USD + 
#     Exporter_Gross_Cap_formation_current_USD + Exporter_Ag_land_K2 + 
#     Exporter_Exchange_rate_LCU_per_USD + log(1+Applied_tariff)  | year + month^hs6_H5 + ExporterISO3^hs6_H5,
#   data   = sub_dta1,  vcov   = ~ ExporterISO3   )
# reg
# 
# reg <- fepois(
#   Trade_value_USD ~ 
#     contig + dist + comlang_off + Colonial_ties + 
#     Importer_GDP + Exporter_wto + Exporter_eu + Exporter_GDP_current_USD + 
#     Exporter_Gross_Cap_formation_current_USD + Exporter_Ag_land_K2 + 
#     Exporter_Exchange_rate_LCU_per_USD + log(1+Applied_tariff)  | year + month^ExporterISO3^hs6_H5,
#   data   = sub_dta1,  vcov   = ~ ExporterISO3   )
# reg
# idx <- obs(reg)
# resid_vec <- resid(reg)
# sub_dta1$resid_change <- NA_real_
# sub_dta1$resid_change[idx] <- resid_vec
# 
# sub_dta1 <- sub_dta1 %>%mutate(
#   country_res = if_else(ExporterISO3 == "USA", "USA", "ROW"))
# table(sub_dta1$country_res)
# 
# 
# ggplot(subset(sub_dta1, year %in% c(2017:2020)),aes(x = resid_change, fill = factor(year))) +
#   geom_density(alpha = 0.35) +
#   facet_wrap(~ country_res, scales = "free") +
#   # coord_cartesian(xlim = c(-1500000, 1500000)) +   # ⬅️ truncate x-axis here
#   labs(  title = "Density of Residual Changes by Country Group",
#     x = "Residual Change",
#     y = "Density",
#     fill = "Year"  ) +
#   theme_minimal(base_size = 14)
# 
# 
# 
# USA <- sub_dta1 %>% filter(ExporterISO3 == "USA")
# USA <- USA %>%  mutate(    date = as.Date(paste(year, month, "01", sep = "-"))  )
# 
# plot <-  ggplot(subset(USA, year %in% c(2018:2020)), aes(x = date, y = resid_change)) +
#   geom_hline(yintercept = 0, color = "red", linetype = "dotted", size = 1) +
#   geom_smooth(color = "blue", size = 1.1) +
#   labs(  title = "Change in Residuals for USA Over Time",
#          x = "time",
#          y = "Residual Change"  ) +
#   theme_minimal(base_size = 14)
# plot
# 
# plot <-  ggplot(USA, aes(x = date, y = resid_change)) +
#   geom_hline(yintercept = 0, color = "red", linetype = "dotted", size = 1) +
#   geom_smooth(color = "blue", size = 1.1) +
#   labs(  title = "Change in Residuals for USA Over Time",
#          x = "time",
#          y = "Residual Change"  ) +
#   theme_minimal(base_size = 14)
# plot
# 
# 
# 
# plot <- ggplot(subset(USA, year %in% 2017:2020),
#  aes(x = resid_change, fill = factor(year))) +
#   geom_density(alpha = 0.35) +
#   labs(title = "Distribution of Residual Changes for USA (2015–2020)",
#     x = "Residual Change",
#     y = "Density",
#     color = "Year") +
#   theme_minimal(base_size = 14)
# plot
# 
# 
# plot <- ggplot(subset(USA, year %in% 2016:2020),
#                aes(x = resid_change)) +
#   geom_density(alpha = 0.35) +
#   facet_wrap(~year)+
#   labs(title = "Distribution of Residual Changes for USA (2015–2020)",
#        x = "Residual Change",
#        y = "Density",
#        color = "Year") +
#   theme_minimal(base_size = 14)
# plot

##############################################################################
# 4) Residual approach: In a loop for each HS 2 level: Poisson specification
##############################################################################
# run it for each product HS2 level and save the residual

# library(dplyr)
# library(fixest)
# 
# # All unique HS2 values
# unique_HS2 <- unique(dta$hs2)
# 
# # Create an empty list to store results for each HS2
# results_list <- vector("list", length(unique_HS2))
# names(results_list) <- unique_HS2
# 
# for (i in seq_along(unique_HS2)) {
#  # i <- 1
#   HS_val <- unique_HS2[i]
#   message("Running HS2 = ", HS_val)
#   
#   # 1. subset
#   sub_dta <- dta %>% filter(hs2 == HS_val)
#   
#   # 2. drop exporters with all-zero trade
#   sub_dta1 <- sub_dta %>%
#     group_by(ExporterISO3) %>%
#     filter(any(Trade_value_USD != 0, na.rm = TRUE)) %>%
#     ungroup()
#   sub_dta1 <- sub_dta1 %>%
#     mutate(fe_id = interaction(month, ExporterISO3, hs6_H5, drop = TRUE))
#   
#   # 3. run regression
#   reg <- fepois(
#     Trade_value_USD ~ 
#       contig + dist + comlang_off + Colonial_ties + rta + fta_and_eia +
#       Importer_GDP + Exporter_wto + Exporter_eu + Exporter_GDP_current_USD + 
#       Exporter_Gross_Cap_formation_current_USD + Exporter_Ag_land_K2 + 
#       Exporter_Exchange_rate_LCU_per_USD + log(1 + Applied_tariff)  | 
#       year + fe_id,
#     data = sub_dta1,
#     vcov = ~ ExporterISO3
#   )
#   
#   # 4. extract residuals matched to original data rows
#   idx <- obs(reg)        # row indices inside sub_dta1
#   resid_vec <- resid(reg)
#   
#   # 5. add residuals
#   sub_dta1$residual <- NA_real_
#   sub_dta1$residual[idx] <- resid_vec
#   
#   # 6. store the result
#   results_list[[i]] <- sub_dta1
# }
# 
# # Optional: bind all outputs into one dataset
# final_dta_with_residuals <- dplyr::bind_rows(results_list)
# names(final_dta_with_residuals)
# colSums(is.na(final_dta_with_residuals))
# write_csv(final_dta_with_residuals, paste0(exp, "gravity_pois_residual.csv"))

# ##############################################################################
# # 5) Fixed effect approach : In a loop for each HS 2 level: With fixed effeccts 
# ##############################################################################


library(dplyr)
library(fixest)

# All unique HS2 values
unique_HS2 <- unique(dta$hs2)

# Create an empty list to store results for each HS2
results_list <- vector("list", length(unique_HS2))
names(results_list) <- unique_HS2

coef_list <- vector("list", length(unique_HS2))   # to store coefficients
r2_vec    <- numeric(length(unique_HS2))          # to store R² / pseudo-R²
names(coef_list) <- unique_HS2
names(r2_vec)    <- unique_HS2

for (i in seq_along(unique_HS2)) {
  # i <- 1
  HS_val <- unique_HS2[i]
  message("Running HS2 = ", HS_val)
  
  # 1. subset
  sub_dta <- dta %>% filter(hs2 == HS_val)
  
  # 2. drop exporters with all-zero trade
  sub_dta1 <- sub_dta %>%
    group_by(ExporterISO3) %>%
    filter(any(Trade_value_USD != 0, na.rm = TRUE)) %>%
    ungroup()
  sub_dta1 <- sub_dta1 %>%
    mutate(fe_id = interaction(month, year, ExporterISO3, hs4, drop = TRUE))
  
  # 3. run regression
  reg <- fepois(
    Trade_value_USD ~ 
      contig + dist + comlang_off + Colonial_ties + rta + fta_and_eia +
      Importer_GDP + Exporter_wto + Exporter_eu + Exporter_GDP_current_USD + 
      Exporter_Gross_Cap_formation_current_USD + Exporter_Ag_land_K2 + 
      Exporter_Exchange_rate_LCU_per_USD + log_tariff  | 
      fe_id,
    data = sub_dta1,
    vcov = ~ ExporterISO3)
  
  # 4. extract residuals matched to original data rows
  idx <- obs(reg)        # row indices inside sub_dta1
  resid_vec <- resid(reg)
  
  # 5. add residuals
  sub_dta1$residual <- NA_real_
  sub_dta1$residual[idx] <- resid_vec

  # store FE
  fe_list <- fixef(reg)
  fe_id <- data.frame( fe_id    = names(fe_list$fe_id),
    FE = as.numeric(fe_list$fe_id))
  sub_dta1 <- sub_dta1 %>%left_join(fe_id, by = "fe_id")
    
  # 6. store the result
  results_list[[i]] <- sub_dta1
  
  # 🔹 6. store coefficients (long format)
  ct <- summary(reg)$coeftable
  
  coef_list[[i]] <- data.frame(
    hs2      = HS_val,
    term     = rownames(ct),
    estimate = ct[, "Estimate"],
    std_error = ct[, "Std. Error"],
    p_value   = ct[, "Pr(>|z|)"],
    row.names = NULL
  )
  
  # 🔹 7. store pseudo-R² (or other fit stat)
  # "pr2" = pseudo-R² (McFadden-like) in fixest
  r2_vec[i] <- fitstat(reg, "pr2")
  
  # 8. store full data with residuals/FE
  results_list[[i]] <- sub_dta1
  
}

# All coefficients for all HS2 in one data frame
coef_df <- dplyr::bind_rows(coef_list)

# R² per HS2
r2_df <- data.frame(hs2 = unique_HS2, pr2 = as.numeric(r2_vec))

coef_summary <- coef_df %>%  left_join(r2_df, by = "hs2")
write_csv(coef_summary, file.path(exp, "gravity_pois_FE_coeff.csv"))



# Optional: bind all outputs into one dataset
final_dta_with_residuals <- dplyr::bind_rows(results_list)
names(final_dta_with_residuals)
colSums(is.na(final_dta_with_residuals))
write_csv(final_dta_with_residuals, paste0(exp, "gravity_pois_FE.csv"))



# 
# ##############################################################################
# # 5) In a loop for each HS 2 level: PPML specification
# ##############################################################################
# 
# # All unique HS2 values
# unique_HS2 <- unique(dta$hs2)
# 
# # Create an empty list to store results for each HS2
# results_list <- vector("list", length(unique_HS2))
# names(results_list) <- unique_HS2
# 
# for (i in seq_along(unique_HS2)) {
#   # i <- 1
#   HS_val <- unique_HS2[i]
#   message("Running HS2 = ", HS_val)
#   
#   # 1. subset
#   sub_dta <- dta %>% filter(hs2 == HS_val)
#   
#   # 2. drop exporters with all-zero trade
#   sub_dta1 <- sub_dta %>%
#     group_by(ExporterISO3) %>%
#     filter(any(Trade_value_USD != 0, na.rm = TRUE)) %>%
#     ungroup()
#   
#   # 3. run regression
#   
#   reg <- feglm(
#     Trade_value_USD ~ contig + dist + comlang_off + Colonial_ties +
#       Importer_GDP + Exporter_wto + Exporter_eu + Exporter_GDP_current_USD +
#       Exporter_Gross_Cap_formation_current_USD + Exporter_Ag_land_K2 + 
#       rta + fta_and_eia + Exporter_Exchange_rate_LCU_per_USD + log(1+Applied_tariff)
#     | year + month^ExporterISO3^hs6_H5,          # <- fixed effects here
#     data   = sub_dta1,
#     family = quasipoisson(link = "log"))
#   reg
#   
#   
#   
#   # 4. extract residuals matched to original data rows
#   idx <- obs(reg)        # row indices inside sub_dta1
#   resid_vec <- resid(reg)
#   
#   # 5. add residuals
#   sub_dta1$resid_change <- NA_real_
#   sub_dta1$resid_change[idx] <- resid_vec
#   
#   # 6. store the result
#   results_list[[i]] <- sub_dta1
# }
# 
# # Optional: bind all outputs into one dataset
# final_dta_with_residuals <- dplyr::bind_rows(results_list)
# names(final_dta_with_residuals)
# colSums(is.na(final_dta_with_residuals))
# write_csv(final_dta_with_residuals, paste0(exp, "gravity_ppml_residual.csv"))
# 
