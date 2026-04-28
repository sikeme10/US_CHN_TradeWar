
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




# ##############################################################################
# # 5) In a loop for each HS 2 level: bootstrap with ppml
# ##############################################################################

library(dplyr)
library(fixest)
library(readr)

set.seed(123)
B <- 500

unique_HS2 <- unique(dta$hs2)
boot_fe_list   <- vector("list", length(unique_HS2))
boot_coef_list <- vector("list", length(unique_HS2))
names(boot_fe_list)   <- unique_HS2
names(boot_coef_list) <- unique_HS2

for(i in seq_along(unique_HS2)){
   #  i <- 1
    HS_val <- unique_HS2[i]
  message("Running HS2 = ", HS_val)
  
  # 1) subset
  sub_dta <- dta %>% filter(hs2 == HS_val)
  
  # 2) drop exporters with all-zero trade
  sub_dta1 <- sub_dta %>%    group_by(ExporterISO3) %>%
    filter(any(Trade_value_USD != 0, na.rm = TRUE)) %>%
    ungroup()
  
  # list of unique exporters to resample from
  unique_exporters <- unique(sub_dta1$ExporterISO3)
  n_exporters <- length(unique_exporters)
  
  # pre-split the data by exporter for fast lookup
  # this avoids repeatedly filtering inside the bootstrap loop
  exporter_splits <- split(sub_dta1, sub_dta1$ExporterISO3)
  
  fe_draws   <- vector("list", B)
  coef_draws <- vector("list", B)
  
  for(b in seq_len(B)){
    
    # 3) CLUSTER bootstrap on ExporterISO3
    # sample exporters with replacement, keep all their rows together
    sampled_exporters <- sample(unique_exporters, size = n_exporters, replace = TRUE)
    
    # assemble bootstrap dataset by binding the blocks for each sampled exporter
    # note: if the same exporter is drawn twice, its rows appear twice
    # we rename duplicated exporters so fixest treats them as separate clusters
    boot_blocks <- vector("list", n_exporters)
    for(k in seq_along(sampled_exporters)){
      block <- exporter_splits[[sampled_exporters[k]]]
      # suffix the exporter name with the draw index to make duplicate draws distinct
      # this is the standard recommendation for cluster bootstrap: duplicate clusters
      # should be treated as independent replicates
      block$ExporterISO3_boot <- paste0(sampled_exporters[k], "_", k)
      boot_blocks[[k]] <- block
    }
    boot_dta <- bind_rows(boot_blocks) %>%
      mutate(fe_id = interaction(year, ExporterISO3_boot, hs4, drop = TRUE))
    
    reg <- tryCatch(
      fepois(
        Trade_value_USD ~
          contig + dist + comlang_off + Colonial_ties + rta + fta_and_eia +
          Importer_GDP + Exporter_wto + Exporter_eu + Exporter_GDP_current_USD +
          Exporter_Gross_Cap_formation_current_USD + Exporter_Ag_land_K2 +
          Exporter_Exchange_rate_LCU_per_USD + log_tariff |
          fe_id,
        data = boot_dta,
        vcov = ~ ExporterISO3_boot
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
write_csv(boot_fe_df,   file.path(exp, "gravity_pois_FE_boot_cluster_fixef.csv"))
write_csv(boot_coef_df, file.path(exp, "gravity_pois_FE_coeff_boot_cluster.csv"))


# 


rm(boot_fe_df,boot_coef_df ); gc()


# ##############################################################################
# # 5) In a loop for each HS 2 level: bootstrap drop 0s log specification
# ##############################################################################

library(dplyr)
library(fixest)
library(readr)
set.seed(123)
B <- 500

unique_HS2 <- unique(dta$hs2)
boot_fe_list   <- vector("list", length(unique_HS2))
boot_coef_list <- vector("list", length(unique_HS2))
names(boot_fe_list)   <- unique_HS2
names(boot_coef_list) <- unique_HS2

for(i in seq_along(unique_HS2)){
  HS_val <- unique_HS2[i]
  message("Running HS2 = ", HS_val)
  
  # 1) subset
  sub_dta <- dta %>% filter(hs2 == HS_val)
  
  # 2) drop zero trade AND exporters with all-zero trade
  sub_dta1 <- sub_dta %>%
    filter(Trade_value_USD > 0) %>%
    group_by(ExporterISO3) %>%
    filter(any(Trade_value_USD > 0)) %>%
    ungroup() %>%
    mutate(ln_trade = log(Trade_value_USD))
  
  # list of unique exporters to resample from
  unique_exporters <- unique(sub_dta1$ExporterISO3)
  n_exporters <- length(unique_exporters)
  
  # pre-split the data by exporter for fast lookup inside the bootstrap loop
  exporter_splits <- split(sub_dta1, sub_dta1$ExporterISO3)
  
  fe_draws   <- vector("list", B)
  coef_draws <- vector("list", B)
  
  for(b in seq_len(B)){
    
    # 3) CLUSTER bootstrap on ExporterISO3
    # sample exporters with replacement, keep all their rows together
    sampled_exporters <- sample(unique_exporters, size = n_exporters, replace = TRUE)
    
    # assemble bootstrap dataset by binding the blocks for each sampled exporter
    # rename duplicated draws so fixest treats them as independent replicates
    boot_blocks <- vector("list", n_exporters)
    for(k in seq_along(sampled_exporters)){
      block <- exporter_splits[[sampled_exporters[k]]]
      block$ExporterISO3_boot <- paste0(sampled_exporters[k], "_", k)
      boot_blocks[[k]] <- block
    }
    boot_dta <- bind_rows(boot_blocks) %>%
      mutate(fe_id = interaction(year, ExporterISO3_boot, hs4, drop = TRUE))
    
    reg <- tryCatch(
      feols(
        ln_trade ~
          contig + dist + comlang_off + Colonial_ties + rta + fta_and_eia +
          Importer_GDP + Exporter_wto + Exporter_eu + Exporter_GDP_current_USD +
          Exporter_Gross_Cap_formation_current_USD + Exporter_Ag_land_K2 +
          Exporter_Exchange_rate_LCU_per_USD + log_tariff |
          fe_id,
        data = boot_dta,
        vcov = ~ ExporterISO3_boot
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
write_csv(boot_fe_df, file.path(exp, "gravity_logOLS_FE_boot_cluster_fixef_drop0.csv"))
write_csv(boot_coef_df, file.path(exp, "gravity_pois_FE_coeff_boot_cluster_drop0.csv"))


