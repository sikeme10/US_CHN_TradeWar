
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
exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/FE/yearly/boot"

################################################################################
# 1) Load data 
################################################################################


dta <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_gravity/dta_CHN_gravity_yearly.csv")
names(dta)
colSums(is.na(dta))
table(dta$year)

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
length(unique((dta$hs6_H5)))

# get log of tariffs:
dta <- dta %>% mutate(log_tariff = log(1+Applied_tariff/100))



# 
# # ##############################################################################
# # # 5) Fixed effect approach : In a loop for each HS 2 level: With fixed effects 
# # ##############################################################################
# 
# # a) with only exporter*HS4*year fixed effects
# 
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
# coef_list <- vector("list", length(unique_HS2))   # to store coefficients
# r2_vec    <- numeric(length(unique_HS2))          # to store R² / pseudo-R²
# names(coef_list) <- unique_HS2
# names(r2_vec)    <- unique_HS2
# 
# for (i in seq_along(unique_HS2)) {
#    #i <- 3
#   HS_val <- unique_HS2[i]
#   message("Running HS2 = ", HS_val)
#   
#   # 1. subset
#   sub_dta <- dta %>% filter(hs2 == HS_val)
#   
#   # 2. drop exporters with all-zero trade
#   sub_dta1 <- sub_dta %>%    group_by(ExporterISO3) %>%
#     filter(any(Trade_value_USD != 0, na.rm = TRUE)) %>%
#     ungroup()
#   # create fixed effect of interest 
#   sub_dta1 <- sub_dta1 %>% mutate(fe_id = interaction(year, ExporterISO3, hs4, drop = TRUE))
#   
#   # 3. run regression
#   reg <- fepois(Trade_value_USD ~ 
#       contig + dist + comlang_off + Colonial_ties + rta + fta_and_eia +
#       Importer_GDP + Exporter_wto + Exporter_eu + Exporter_GDP_current_USD + 
#       Exporter_Gross_Cap_formation_current_USD + Exporter_Ag_land_K2 + 
#       Exporter_Exchange_rate_LCU_per_USD + log_tariff  | 
#       fe_id ,
#     data = sub_dta1,
#     vcov = ~ ExporterISO3)
#   
# 
#   # 4. extract residuals matched to original data rows
#   idx <- obs(reg)        # row indices inside sub_dta1
#   resid_vec <- resid(reg)
#   
#   # 5. add residuals
#   sub_dta1$residual <- NA_real_
#   sub_dta1$residual[idx] <- resid_vec
# 
#   # store FE
#   fe_list <- fixef(reg)
#   fe_id <- data.frame( fe_id    = names(fe_list$fe_id),
#     FE = as.numeric(fe_list$fe_id))
#   sub_dta1 <- sub_dta1 %>%left_join(fe_id, by = "fe_id")
#     
#   # 6. store the result
#   results_list[[i]] <- sub_dta1
#   
#   # 🔹 6. store coefficients (long format)
#   ct <- summary(reg)$coeftable
#   
#   coef_list[[i]] <- data.frame(
#     hs2      = HS_val,
#     term     = rownames(ct),
#     estimate = ct[, "Estimate"],
#     std_error = ct[, "Std. Error"],
#     p_value   = ct[, "Pr(>|z|)"],
#     row.names = NULL  )
#   
#   # 🔹 7. store pseudo-R² (or other fit stat)
#   # "pr2" = pseudo-R² (McFadden-like) in fixest
#   r2_vec[i] <- fitstat(reg, "pr2")
#   
#   # 8. store full data with residuals/FE
#   results_list[[i]] <- sub_dta1
#   
# }
# 
# # All coefficients for all HS2 in one data frame
# coef_df <- dplyr::bind_rows(coef_list)
# 
# # R² per HS2
# r2_df <- data.frame(hs2 = unique_HS2, pr2 = as.numeric(r2_vec))
# 
# coef_summary <- coef_df %>%  left_join(r2_df, by = "hs2")
# write_csv(coef_summary, file.path(exp, "gravity_pois_FE_coeff.csv"))
# 
# 
# 
# # Optional: bind all outputs into one dataset
# final_dta_with_residuals <- dplyr::bind_rows(results_list)
# names(final_dta_with_residuals)
# colSums(is.na(final_dta_with_residuals))
# write_csv(final_dta_with_residuals, paste0(exp, "gravity_pois_FE.csv"))
# 
# 
# 

# ##############################################################################
# # 5) In a loop for each HS 2 level: bootstrap
# ##############################################################################

library(dplyr)
library(fixest)
library(readr)

set.seed(123)
# B <- 50
B <- 100

unique_HS2 <- unique(dta$hs2)

boot_fe_list   <- vector("list", length(unique_HS2))
boot_coef_list <- vector("list", length(unique_HS2))

names(boot_fe_list)   <- unique_HS2
names(boot_coef_list) <- unique_HS2

for(i in seq_along(unique_HS2)){
  # test 
  # i <- 1
  HS_val <- unique_HS2[i]
  message("Running HS2 = ", HS_val)
  
  # 1) subset
  sub_dta <- dta %>% filter(hs2 == HS_val)
  
  # 2) drop exporters with all-zero trade
  sub_dta1 <- sub_dta %>%
    group_by(ExporterISO3) %>%
    filter(any(Trade_value_USD != 0, na.rm = TRUE)) %>%
    ungroup()
  
  n <- nrow(sub_dta1)
  
  fe_draws   <- vector("list", B)
  coef_draws <- vector("list", B)
  
  for(b in seq_len(B)){
    
    # 3) ROW bootstrap
    boot_dta <- sub_dta1[sample.int(n, size = n, replace = TRUE), ] %>%
      mutate(fe_id = interaction(year, ExporterISO3, hs4, drop = TRUE))
    
    reg <- tryCatch(
      fepois(
        Trade_value_USD ~
          contig + dist + comlang_off + Colonial_ties + rta + fta_and_eia +
          Importer_GDP + Exporter_wto + Exporter_eu + Exporter_GDP_current_USD +
          Exporter_Gross_Cap_formation_current_USD + Exporter_Ag_land_K2 +
          Exporter_Exchange_rate_LCU_per_USD + log_tariff |
          fe_id,
        data = boot_dta,
        vcov = ~ ExporterISO3
      ),
      error = function(e) NULL
    )
    
    if(is.null(reg)){
      fe_draws[[b]] <- data.frame(
        hs2 = HS_val, draw = b, fe_id = NA_character_, FE = NA_real_
      )
      coef_draws[[b]] <- data.frame(
        hs2 = HS_val, draw = b, term = NA_character_, estimate = NA_real_
      )
      next
    }
    
    # ---------------- FIXED EFFECTS ----------------
    fe_vec <- fixef(reg)$fe_id
    
    fe_draws[[b]] <- data.frame(
      hs2   = HS_val,
      draw  = b,
      fe_id = names(fe_vec),
      FE    = as.numeric(fe_vec),
      row.names = NULL
    )
    
    # ---------------- NON-FE COEFFICIENTS ----------------
    ct <- summary(reg)$coeftable
    
    coef_draws[[b]] <- data.frame(
      hs2      = HS_val,
      draw     = b,
      term     = rownames(ct),
      estimate = ct[, "Estimate"],
      row.names = NULL
    )
  }
  
  boot_fe_list[[i]]   <- bind_rows(fe_draws)
  boot_coef_list[[i]] <- bind_rows(coef_draws)
}

boot_fe_df   <- bind_rows(boot_fe_list)
boot_coef_df <- bind_rows(boot_coef_list)

# ---------------- SAVE RAW BOOTSTRAP DRAWS ----------------
write_csv(boot_fe_df,   file.path(exp, "gravity_pois_FE_boot_fixef.csv"))
write_csv(boot_coef_df, file.path(exp, "gravity_pois_FE_coeff_boot.csv"))


# 
# 
# ################################################################################
# # summarise coef is needed 
# 
# boot_fe_summary <- boot_fe_df %>%
#   filter(!is.na(fe_id)) %>%
#   group_by(hs2, fe_id) %>%
#   summarize(
#     fe_mean = mean(FE, na.rm = TRUE),
#     fe_sd   = sd(FE, na.rm = TRUE),
#     fe_p2.5 = quantile(FE, 0.025, na.rm = TRUE),
#     fe_p50  = quantile(FE, 0.50,  na.rm = TRUE),
#     fe_p97.5= quantile(FE, 0.975, na.rm = TRUE),
#     n_ok    = sum(!is.na(FE)),
#     .groups = "drop"
#   )
# 
# 



# ##############################################################################
# # 5) In a loop for each HS 2 level: bootstrap drop 0s
# ##############################################################################

library(dplyr)
library(fixest)
library(readr)

set.seed(123)
#B <- 50
B <- 100

unique_HS2 <- unique(dta$hs2)

boot_fe_list   <- vector("list", length(unique_HS2))
boot_coef_list <- vector("list", length(unique_HS2))

names(boot_fe_list)   <- unique_HS2
names(boot_coef_list) <- unique_HS2

for(i in seq_along(unique_HS2)){
  # test 
  # i <- 1
  HS_val <- unique_HS2[i]
  message("Running HS2 = ", HS_val)
  
  # 1) subset
  sub_dta <- dta %>% filter(hs2 == HS_val)
  
  # 2) drop zero trade AND exporters with all-zero trade
  sub_dta1 <- sub_dta %>%
    filter(Trade_value_USD > 0) %>%                 # <<< NEW
    group_by(ExporterISO3) %>%
    filter(any(Trade_value_USD > 0)) %>%
    ungroup() %>%
    mutate(ln_trade = log(Trade_value_USD))    
  
  n <- nrow(sub_dta1)
  
  fe_draws   <- vector("list", B)
  coef_draws <- vector("list", B)
  
  for(b in seq_len(B)){
    
    # 3) ROW bootstrap
    boot_dta <- sub_dta1[sample.int(n, size = n, replace = TRUE), ] %>%
      mutate(fe_id = interaction(year, ExporterISO3, hs4, drop = TRUE))
    
    reg <- tryCatch(
      feols(
        ln_trade ~
          contig + dist + comlang_off + Colonial_ties + rta + fta_and_eia +
          Importer_GDP + Exporter_wto + Exporter_eu + Exporter_GDP_current_USD +
          Exporter_Gross_Cap_formation_current_USD + Exporter_Ag_land_K2 +
          Exporter_Exchange_rate_LCU_per_USD + log_tariff |
          fe_id,
        data = boot_dta,
        vcov = ~ ExporterISO3
      ),
      error = function(e) NULL
    )
    
    if(is.null(reg)){
      fe_draws[[b]] <- data.frame(
        hs2 = HS_val, draw = b, fe_id = NA_character_, FE = NA_real_
      )
      coef_draws[[b]] <- data.frame(
        hs2 = HS_val, draw = b, term = NA_character_, estimate = NA_real_
      )
      next
    }
    
    # ---------------- FIXED EFFECTS ----------------
    fe_vec <- fixef(reg)$fe_id
    
    fe_draws[[b]] <- data.frame(
      hs2   = HS_val,
      draw  = b,
      fe_id = names(fe_vec),
      FE    = as.numeric(fe_vec),
      row.names = NULL
    )
    
    # ---------------- NON-FE COEFFICIENTS ----------------
    ct <- summary(reg)$coeftable
    
    coef_draws[[b]] <- data.frame(
      hs2      = HS_val,
      draw     = b,
      term     = rownames(ct),
      estimate = ct[, "Estimate"],
      row.names = NULL
    )
  }
  
  boot_fe_list[[i]]   <- bind_rows(fe_draws)
  boot_coef_list[[i]] <- bind_rows(coef_draws)
}

boot_fe_df   <- bind_rows(boot_fe_list)
boot_coef_df <- bind_rows(boot_coef_list)

# ---------------- SAVE RAW BOOTSTRAP DRAWS ----------------
write_csv(boot_fe_df, file.path(exp, "gravity_logOLS_FE_boot_fixef_drop0.csv"))
write_csv(boot_coef_df, file.path(exp, "gravity_pois_FE_coeff_boot_drop0.csv"))


