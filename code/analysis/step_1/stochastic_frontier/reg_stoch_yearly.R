
################################################################################
#                     Stochastic frontier regression anlaysis


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

exp <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/output/stochastic/yearly/"

################################################################################
# 1) Load data 
################################################################################


dta <- read_csv("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/created_gravity/dta_CHN_gravity_yearly.csv")
names(dta)
colSums(is.na(dta))

################################################################################
# 2) eddit the data for the stochastic frontier
################################################################################

# get HS2 and HS 4 level 
class(dta$hs6_H5)
unique(nchar(dta$hs6_H5))
dta <- dta %>%
  mutate(hs2 = substr( hs6_H5 ,1,2),
         hs4 = substr( hs6_H5 ,1,4))
length(unique((dta$hs2)))
length(unique((dta$hs4)))


# Drop observations where have a lot of zeroes 

# dta1 <- dta %>% group_by(ExporterISO3, hs6_H5) %>% filter(any(Trade_value_USD != 0))
dta1 <- dta %>% group_by(ExporterISO3, hs6_H5) %>% filter((Trade_value_USD != 0))


names(dta1)
dta1 <- dta1 %>% 
  group_by( hs6_H5) %>%
  mutate(
    log_Trade_value_USD = log(Trade_value_USD+1),
    log_Trade_value_USD_mean_15_17 = mean(log_Trade_value_USD[year %in% 2015:2017], na.rm = TRUE),
    log_Trade_value_USD_mean = mean(log_Trade_value_USD, na.rm = TRUE),
    # log_Trade_value_USD_demeaned = log_Trade_value_USD - log_Trade_value_USD_mean_15_17,
    log_Trade_value_USD_demeaned = log_Trade_value_USD - log_Trade_value_USD_mean) %>%
  ungroup()
summary(dta1$log_Trade_value_USD_demeaned)



summary(dta1$Applied_tariff)
dta1 <- dta1 %>% mutate(log_tariff = log(1+Applied_tariff/100))
summary(dta1$log_tariff)




# 
# 
# 
# ################################################################################
# # 1) In a loop for all Hs2: stochastic frontier with tariff correlation
# ################################################################################
# library(dplyr)
# library(sfaR)
# library(texreg)
# 
# ## ------------ Helper: texreg -> data.frame -----------------
# # store coeffs and  LR stat
# 
# texreg_to_df <- function(model, model_name = NULL, hs2_value = NA) {
#   tr <- texreg::extract(model)      # texreg S4 object
#   sm <- summary(model)             # summary.sfacross object
# 
#   # extra parameters: sigma_u^2, sigma_v^2, gamma, etc.
#   extra_par <- coef(model, extraPar = TRUE)
#   gamma_val <- unname(extra_par["gamma"])
# 
#   # LR test for inefficiency vs OLS
#   LR_stat <- sm$chisq
#   LR_df   <- sm$df
#   LR_p    <- pchisq(LR_stat, df = LR_df, lower.tail = FALSE)
# 
#   data.frame(
#     hs2      = hs2_value,
#     model    = model_name,
#     term     = names(tr@coef),
#     estimate = unname(tr@coef),
#     se       = unname(tr@se),
#     pvalue   = unname(tr@pvalues),
# 
#     # model-level stuff (repeated on each row, which is fine)
#     AIC      = sm$AIC,
#     BIC      = sm$BIC,
#     logLik   = sm$mlLoglik,   # ML log-likelihood from sfaR
#     N        = sm$Nobs,
# 
#     gamma    = gamma_val,     # share of variance from inefficiency
#     LR_ineff = LR_stat,       # LR test statistic
#     df_ineff = LR_df,         # df of the test
#     p_ineff  = LR_p,          # p-value for "inefficiency exists?"
# 
#     stringsAsFactors = FALSE
#   )
# }
# 
# ## ------------ 1. All HS2 values ----------------------------
# 
# hs2_vals <- sort(unique(dta1$hs2))
# 
# ## ------------ 2. Containers -------------------------------
# 
# results_list <- list()   # will keep data + models per HS2
# coef_list    <- list()   # will keep coefficient tables per HS2
# k <- 1
# 
# # store failed HS :
# failed_hs2 <- c()
# 
# ## ------------ 3. Variables used in SFA --------------------
# 
# vars_sfa <- c(
#   "log_Trade_value_USD_demeaned",
#   "contig", "dist", "comlang_off", "Colonial_ties",
#   "Importer_GDP", "Exporter_wto", "Exporter_GDP_current_USD",
#   "Exporter_Ag_land_K2", "log_tariff", "rta", "fta_and_eia"
# )
# 
# rhs_vars <- c(
#   "contig", "dist", "comlang_off", "Colonial_ties",
#   "Importer_GDP", "Exporter_wto", "Exporter_GDP_current_USD",
#   "Exporter_Ag_land_K2", "log_tariff", "rta", "fta_and_eia"
# )
# 
# ## ------------ 4. Loop over HS2 ----------------------------
# 
# for (HS2 in hs2_vals) {
#   message("=== HS2 = ", HS2, " ===")
# 
#   # test
#    # HS2 <- hs2_vals[1]
# 
#   # Subset and drop rows with NA in SFA vars
#   dta2 <- dta1 %>% filter(hs2 == HS2) %>%
#     filter(if_all(all_of(vars_sfa), ~ !is.na(.)))
# 
#   if (nrow(dta2) < 30) {
#     message("  -> skipped (too few obs: ", nrow(dta2), ")")
#     failed_hs2 <- c(failed_hs2, paste0(HS2, "_too_few_obs"))
#     next
#     }
# 
#   ## ---- Build HS2-specific RHS set (drop non-varying regressors) -------
#   rhs_vars_use <- rhs_vars[sapply(rhs_vars, function(v) n_distinct(dta2[[v]]) > 1)]
# 
#   if (length(rhs_vars_use) == 0) {
#     message("  -> no varying regressors for HS2 = ", HS2, ", skipping")
#     failed_hs2 <- c(failed_hs2, paste0(HS2, "_no_var"))
#     next
#   }
# 
#   # Main SFA formula for this HS2
#   form_sfa <- as.formula(
#     paste("log_Trade_value_USD_demeaned ~", paste(rhs_vars_use, collapse = " + "))
#   )
# 
#   ## ---- Model 1: half-normal --------------------------------------------
#   mod_hn <- try(
#     sfacross(
#       formula   = form_sfa,
#       data      = dta2,
#       udist     = "hnormal",
#       S         = 1,
#       logDepVar = TRUE,
#       method    = "nlminb"
#     ),
#     silent = TRUE
#   )
# 
#   if (inherits(mod_hn, "try-error")) {
#     message("  -> mod_hn failed, skipping HS2 = ", HS2)
#     failed_hs2 <- c(failed_hs2, paste0(HS2, "_HN"))
#     next
#   }
# 
#   ## ---- Model 2: truncated normal, no muhet -----------------------------
#   mod_tn <- try(
#     sfacross(
#       formula   = form_sfa,
#       data      = dta2,
#       udist     = "tnormal",
#       S         = 1,
#       logDepVar = TRUE,
#       method    = "nlminb"
#     ),
#     silent = TRUE
#   )
# 
#   if (inherits(mod_tn, "try-error")) {
#     message("  -> mod_tn failed, skipping HS2 = ", HS2)
#     failed_hs2 <- c(failed_hs2, paste0(HS2, "_TN"))
#     next
#   }
# 
#   ## ---- Model 3: truncated normal + muhet(log_tariff) -------------------
#   # Only meaningful if log_tariff is in the regression and varies
#   has_log_tariff <- "log_tariff" %in% rhs_vars_use
# 
#   if (has_log_tariff) {
# 
#     # starting values from tnormal model (no muhet)
#     start_tn <- coef(mod_tn)
#     # add ONE extra parameter for Zmu_log_tariff (muhet = ~ log_tariff)
#     start_tn <- c(start_tn, 0)  # last 0 is start value for Zmu_log_tariff
# 
#     # 1) Try WITHOUT starting values
#     mod_tn_tar <- try(
#       sfacross(
#         formula   = form_sfa,
#         data      = dta2,
#         udist     = "tnormal",
#         muhet     = ~ log_tariff,
#         S         = 1,
#         logDepVar = TRUE,
#         method    = "nlminb"
#       ),
#       silent = TRUE
#     )
# 
#     # 2) If it fails, try again WITH start_tn
#     if (inherits(mod_tn_tar, "try-error")) {
#       message("    -> retrying mod_tn_tar WITH starting values for HS2 = ", HS2)
# 
#       mod_tn_tar <- try(
#         sfacross(
#           formula   = form_sfa,
#           data      = dta2,
#           udist     = "tnormal",
#           muhet     = ~ log_tariff,
#           S         = 1,
#           logDepVar = TRUE,
#           start     = start_tn,
#           method    = "nlminb"        ),
#         silent = TRUE      )
#     }
# 
#     # 3) If STILL fails
#     if (inherits(mod_tn_tar, "try-error")) {
#       message("    -> mod_tn_tar failed for HS2 = ", HS2)
#       failed_hs2 <- c(failed_hs2, paste0(HS2, "_TN_mu"))
#     }
#   } else {
#     # no log_tariff variation -> cannot estimate muhet(log_tariff)
#     mod_tn_tar <- structure("no_log_tariff", class = "try-error")
#     message("    -> log_tariff not varying or not in RHS for HS2 = ", HS2,
#             " ; skipping muhet model")
#     failed_hs2 <- c(failed_hs2, paste0(HS2, "_no_logTariff_mu"))
#   }
# 
# 
#   ## ---- Efficiencies for TN and TN_muTariff separately ------------------
# 
#   # 1) Always compute efficiencies for the plain truncated-normal model
#   eff_tn      <- sfaR::efficiencies(mod_tn, newData = dta2)
#   dta2_eff_tn <- cbind(dta2, eff_tn)  # teBC, teJLMS, u, etc.
# 
#   # 2) Compute efficiencies for the mu-heterogeneous TN model if it converged
#   if (!inherits(mod_tn_tar, "try-error")) {
#     eff_tn_tar      <- sfaR::efficiencies(mod_tn_tar, newData = dta2)
#     dta2_eff_tn_tar <- cbind(dta2, eff_tn_tar)
#   } else {
#     dta2_eff_tn_tar <- NULL
#   }
# 
#   ## (Optional) 3) If you want efficiencies for the half-normal model as well:
#   # eff_hn      <- sfaR::efficiencies(mod_hn, newData = dta2)
#   # dta2_eff_hn <- cbind(dta2, eff_hn)
# 
#   ## ---- Build coef table for this HS2 -----------------------------------
#   this_coef_parts <- list(texreg_to_df(mod_hn, "HN", HS2),texreg_to_df(mod_tn, "TN", HS2)  )
# 
#   if (!inherits(mod_tn_tar, "try-error")) {
#     this_coef_parts[[length(this_coef_parts) + 1]] <-  texreg_to_df(mod_tn_tar, "TN_muTariff", HS2)
#   }
#     coef_list[[k]] <- dplyr::bind_rows(this_coef_parts)
#   k <- k + 1
# 
#   ## ---- Store everything in results_list --------------------------------
#   results_list[[as.character(HS2)]] <- list(
#     hs2        = HS2,
#     data_raw   = dta2,             # data used in estimation
#     data_tn    = dta2_eff_tn,      # efficiencies from mod_tn
#     data_tn_mu = dta2_eff_tn_tar,  # efficiencies from mod_tn_tar (or NULL if failed)
#     # data_hn  = dta2_eff_hn,      # uncomment if you compute HN efficiencies
#     mod_hn     = mod_hn,
#     mod_tn     = mod_tn,
#     mod_tn_tar = if (!inherits(mod_tn_tar, "try-error")) mod_tn_tar else NULL
#   )
# }
# 
# ## ------------ After the loop: bind all coef tables ---------------------
# 
# coef_all <- dplyr::bind_rows(coef_list)
# # coef_all now has hs2, model, term, estimate, se, pvalue, AIC, BIC, logLik, N
# 
# ## ------------ Optional: bind all HS2 data with inefficiencies ----------
# 
# 
# dta_all_with_eff <- dplyr::bind_rows(  lapply(results_list, function(x) x$data_tn))
# dta_all_with_eff_mu <- dplyr::bind_rows(  lapply(results_list, function(x) x$data_tn_mu))
# 
# 
# # failed HS2 codes:
# failed_hs2
# 
# 
# # save results:
# 
# write_csv(coef_all, file.path(exp, "sfaR_coef_HS2_average_new.csv"))
# write_csv(dta_all_with_eff, file.path(exp, "sfaR_efficiency_average.csv"))
# write_csv(dta_all_with_eff_mu, file.path(exp, "sfaR_efficiency_average_mu.csv"))
# saveRDS(results_list, file.path(exp, "sfaR_results_efficiency_list_HS2_average_new.rds"))
# 
# 




################################################################################
# 1) In a loop for all Hs2: stochastic frontier with tariff correlation with expoential
################################################################################
library(dplyr)
library(sfaR)
library(texreg)
## ------------ Helper: texreg -> data.frame -----------------
# store coeffs and  LR stat

texreg_to_df <- function(model, model_name = NULL, hs2_value = NA) {
  tr <- texreg::extract(model)      # texreg S4 object
  sm <- summary(model)             # summary.sfacross object

  # extra parameters: sigma_u^2, sigma_v^2, gamma, etc.
  extra_par <- coef(model, extraPar = TRUE)
  gamma_val <- unname(extra_par["gamma"])

  # LR test for inefficiency vs OLS
  LR_stat <- sm$chisq
  LR_df   <- sm$df
  LR_p    <- pchisq(LR_stat, df = LR_df, lower.tail = FALSE)

  data.frame(
    hs2      = hs2_value,
    model    = model_name,
    term     = names(tr@coef),
    estimate = unname(tr@coef),
    se       = unname(tr@se),
    pvalue   = unname(tr@pvalues),

    # model-level stuff (repeated on each row, which is fine)
    AIC      = sm$AIC,
    BIC      = sm$BIC,
    logLik   = sm$mlLoglik,   # ML log-likelihood from sfaR
    N        = sm$Nobs,

    gamma    = gamma_val,     # share of variance from inefficiency
    LR_ineff = LR_stat,       # LR test statistic
    df_ineff = LR_df,         # df of the test
    p_ineff  = LR_p,          # p-value for "inefficiency exists?"

    stringsAsFactors = FALSE
  )
}

## ------------ 1. All HS2 values ----------------------------

hs2_vals <- sort(unique(dta1$hs2))

## ------------ 2. Containers -------------------------------

results_list <- list()   # will keep data + models per HS2
coef_list    <- list()   # will keep coefficient tables per HS2
k <- 1

# store failed HS :
failed_hs2 <- c()

## ------------ 3. Variables used in SFA --------------------

vars_sfa <- c(
  "log_Trade_value_USD_demeaned",
  "contig", "dist", "comlang_off", "Colonial_ties",
  "Importer_GDP", "Exporter_wto", "Exporter_GDP_current_USD",
  "Exporter_Ag_land_K2", "log_tariff", "rta", "fta_and_eia"
)

rhs_vars <- c(
  "contig", "dist", "comlang_off", "Colonial_ties",
  "Importer_GDP", "Exporter_wto", "Exporter_GDP_current_USD",
  "Exporter_Ag_land_K2", "log_tariff", "rta", "fta_and_eia"
)

## ------------ 4. Loop over HS2 ----------------------------

for (HS2 in hs2_vals) {
  message("=== HS2 = ", HS2, " ===")

  # test
   HS2 <- hs2_vals[3]

  # Subset and drop rows with NA in SFA vars
  dta2 <- dta1 %>% filter(hs2 == HS2) %>%
    filter(if_all(all_of(vars_sfa), ~ !is.na(.)))

  if (nrow(dta2) < 30) {
    message("  -> skipped (too few obs: ", nrow(dta2), ")")
    failed_hs2 <- c(failed_hs2, paste0(HS2, "_too_few_obs"))
    next
    }

  ## ---- Build HS2-specific RHS set (drop non-varying regressors) -------
  rhs_vars_use <- rhs_vars[sapply(rhs_vars, function(v) n_distinct(dta2[[v]]) > 1)]

  if (length(rhs_vars_use) == 0) {
    message("  -> no varying regressors for HS2 = ", HS2, ", skipping")
    failed_hs2 <- c(failed_hs2, paste0(HS2, "_no_var"))
    next
  }

  # Main SFA formula for this HS2
  form_sfa <- as.formula(
    paste("log_Trade_value_USD_demeaned ~", paste(rhs_vars_use, collapse = " + "))
  )

  ## ---- Model 1: half-normal --------------------------------------------
  mod_hn <- try(
    sfacross(
      formula   = form_sfa,
      data      = dta2,
      udist     = "hnormal",
      S         = 1,
      logDepVar = TRUE,
      method    = "nlminb"
    ),
    silent = TRUE
  )

  if (inherits(mod_hn, "try-error")) {
    message("  -> mod_hn failed, skipping HS2 = ", HS2)
    failed_hs2 <- c(failed_hs2, paste0(HS2, "_HN"))
    next
  }

  ## ---- Model 2: truncated normal, no muhet -----------------------------
  mod_tn <- try(
    sfacross(
      formula   = form_sfa,
      data      = dta2,
      udist     = "tnormal",
      S         = 1,
      logDepVar = TRUE,
      method    = "nlminb"
    ),
    silent = TRUE
  )

  if (inherits(mod_tn, "try-error")) {
    message("  -> mod_tn failed, skipping HS2 = ", HS2)
    failed_hs2 <- c(failed_hs2, paste0(HS2, "_TN"))
    next
  }

  ## ---- Model 3: truncated normal + muhet(log_tariff) -------------------
  # Only meaningful if log_tariff is in the regression and varies
  has_log_tariff <- "log_tariff" %in% rhs_vars_use

  if (has_log_tariff) {

    # starting values from tnormal model (no muhet)
    start_tn <- coef(mod_tn)
    # add ONE extra parameter for Zmu_log_tariff (muhet = ~ log_tariff)
    start_tn <- c(start_tn, 0)  # last 0 is start value for Zmu_log_tariff

    # 1) Try WITHOUT starting values
    mod_tn_tar <- try(
      sfacross(
        formula   = form_sfa,
        data      = dta2,
        udist     = "tnormal",
        muhet     = ~ log_tariff,
        S         = 1,
        logDepVar = TRUE,
        method    = "nlminb"
      ),
      silent = TRUE
    )

    # 2) If it fails, try again WITH start_tn
    if (inherits(mod_tn_tar, "try-error")) {
      message("    -> retrying mod_tn_tar WITH starting values for HS2 = ", HS2)

      mod_tn_tar <- try(
        sfacross(
          formula   = form_sfa,
          data      = dta2,
          udist     = "tnormal",
          muhet     = ~ log_tariff,
          S         = 1,
          logDepVar = TRUE,
          start     = start_tn,
          method    = "nlminb"        ),
        silent = TRUE      )
    }

    # 3) If STILL fails
    if (inherits(mod_tn_tar, "try-error")) {
      message("    -> mod_tn_tar failed for HS2 = ", HS2)
      failed_hs2 <- c(failed_hs2, paste0(HS2, "_TN_mu"))
    }
  } else {
    # no log_tariff variation -> cannot estimate muhet(log_tariff)
    mod_tn_tar <- structure("no_log_tariff", class = "try-error")
    message("    -> log_tariff not varying or not in RHS for HS2 = ", HS2,
            " ; skipping muhet model")
    failed_hs2 <- c(failed_hs2, paste0(HS2, "_no_logTariff_mu"))
  }

  ## ---- Model 4: exponential, no muhet -----------------------------------
  mod_exp <- try(
    sfacross(
      formula   = form_sfa,
      data      = dta2,
      udist     = "exponential",
      S         = 1,
      logDepVar = TRUE,
      method    = "nlminb"
    ),
    silent = TRUE
  )
  
  if (inherits(mod_exp, "try-error")) {
    message("  -> mod_exp failed for HS2 = ", HS2)
    failed_hs2 <- c(failed_hs2, paste0(HS2, "_EXP"))
    mod_exp_tar <- structure("exp_failed", class = "try-error")
  } else {
    
    ## ---- Model 5: exponential + muhet(log_tariff) -------------------------
    if (has_log_tariff) {
      
      # Try without starts first
      mod_exp_tar <- try(
        sfacross(
          formula   = form_sfa,
          data      = dta2,
          udist     = "exponential",
          muhet     = ~ log_tariff,
          S         = 1,
          logDepVar = TRUE,
          method    = "nlminb"
        ),
        silent = TRUE
      )
      
      # If it fails, retry with starting values from mod_exp (+ one extra 0)
      if (inherits(mod_exp_tar, "try-error")) {
        message("    -> retrying mod_exp_tar WITH starting values for HS2 = ", HS2)
        
        start_exp <- coef(mod_exp)
        start_exp <- c(start_exp, 0)
        
        mod_exp_tar <- try(
          sfacross(
            formula   = form_sfa,
            data      = dta2,
            udist     = "exponential",
            muhet     = ~ log_tariff,
            S         = 1,
            logDepVar = TRUE,
            start     = start_exp,
            method    = "nlminb"
          ),
          silent = TRUE
        )
      }
      
      if (inherits(mod_exp_tar, "try-error")) {
        message("    -> mod_exp_tar failed for HS2 = ", HS2)
        failed_hs2 <- c(failed_hs2, paste0(HS2, "_EXP_mu"))
      }
      
    } else {
      mod_exp_tar <- structure("no_log_tariff", class = "try-error")
      message("    -> log_tariff not varying or not in RHS for HS2 = ", HS2,
              " ; skipping EXP muhet model")
      failed_hs2 <- c(failed_hs2, paste0(HS2, "_no_logTariff_EXPmu"))
    }
  }
  
  
  
  

  ## ---- Efficiencies for TN and TN_muTariff separately ------------------

  # 1) Always compute efficiencies for the plain truncated-normal model
  eff_tn      <- sfaR::efficiencies(mod_tn, newData = dta2)
  dta2_eff_tn <- cbind(dta2, eff_tn)  # teBC, teJLMS, u, etc.

  # 2) Compute efficiencies for the mu-heterogeneous TN model if it converged
  if (!inherits(mod_tn_tar, "try-error")) {
    eff_tn_tar      <- sfaR::efficiencies(mod_tn_tar, newData = dta2)
    dta2_eff_tn_tar <- cbind(dta2, eff_tn_tar)
  } else {
    dta2_eff_tn_tar <- NULL
  }
  ## ---- Efficiencies: EXP and EXP_muTariff --------------------------------
  
  if (!inherits(mod_exp, "try-error")) {
    eff_exp      <- sfaR::efficiencies(mod_exp, newData = dta2)
    dta2_eff_exp <- cbind(dta2, eff_exp)
  } else {
    dta2_eff_exp <- NULL
  }
  
  if (!inherits(mod_exp_tar, "try-error")) {
    eff_exp_tar      <- sfaR::efficiencies(mod_exp_tar, newData = dta2)
    dta2_eff_exp_tar <- cbind(dta2, eff_exp_tar)
  } else {
    dta2_eff_exp_tar <- NULL
  }
  

  ## (Optional) 3) If you want efficiencies for the half-normal model as well:
  # eff_hn      <- sfaR::efficiencies(mod_hn, newData = dta2)
  # dta2_eff_hn <- cbind(dta2, eff_hn)

  ## ---- Build coef table for this HS2 -----------------------------------
  this_coef_parts <- list(texreg_to_df(mod_hn, "HN", HS2),    texreg_to_df(mod_tn, "TN", HS2)  )
  
  if (!inherits(mod_tn_tar, "try-error")) {
    this_coef_parts[[length(this_coef_parts) + 1]] <- texreg_to_df(mod_tn_tar, "TN_muTariff", HS2)
  }
  
  if (!inherits(mod_exp, "try-error")) {
    this_coef_parts[[length(this_coef_parts) + 1]] <- texreg_to_df(mod_exp, "EXP", HS2)
  }
  
  if (!inherits(mod_exp_tar, "try-error")) {
    this_coef_parts[[length(this_coef_parts) + 1]] <- texreg_to_df(mod_exp_tar, "EXP_muTariff", HS2)
  }
  
  coef_list[[k]] <- dplyr::bind_rows(this_coef_parts)
  k <- k + 1
  

  ## ---- Store everything in results_list --------------------------------
  results_list[[as.character(HS2)]] <- list(
    hs2          = HS2,
    data_raw     = dta2,
    data_tn      = dta2_eff_tn,
    data_tn_mu   = dta2_eff_tn_tar,
    data_exp     = dta2_eff_exp,
    data_exp_mu  = dta2_eff_exp_tar,
    
    mod_hn       = mod_hn,
    mod_tn       = mod_tn,
    mod_tn_tar   = if (!inherits(mod_tn_tar,  "try-error")) mod_tn_tar  else NULL,
    mod_exp      = if (!inherits(mod_exp,     "try-error")) mod_exp     else NULL,
    mod_exp_tar  = if (!inherits(mod_exp_tar, "try-error")) mod_exp_tar else NULL
  )
  
}

## ------------ After the loop: bind all coef tables ---------------------

coef_all <- dplyr::bind_rows(coef_list)
# coef_all now has hs2, model, term, estimate, se, pvalue, AIC, BIC, logLik, N

## ------------ Optional: bind all HS2 data with inefficiencies ----------


dta_all_with_eff <- dplyr::bind_rows(  lapply(results_list, function(x) x$data_tn))
dta_all_with_eff_mu <- dplyr::bind_rows(  lapply(results_list, function(x) x$data_tn_mu))

dta_all_with_eff_exp <- dplyr::bind_rows(lapply(results_list, function(x) x$data_exp))
dta_all_with_eff_exp_mu <- dplyr::bind_rows(lapply(results_list, function(x) x$data_exp_mu))


# failed HS2 codes:
failed_hs2


# save results:

write_csv(coef_all, file.path(exp, "sfaR_coef_HS2_average_new_exp.csv"))
write_csv(dta_all_with_eff, file.path(exp, "sfaR_efficiency_average.csv"))
write_csv(dta_all_with_eff_mu, file.path(exp, "sfaR_efficiency_average_mu.csv"))
write_csv(dta_all_with_eff_exp,    file.path(exp, "sfaR_efficiency_average_exp.csv"))
write_csv(dta_all_with_eff_exp_mu, file.path(exp, "sfaR_efficiency_average_exp_mu.csv"))

saveRDS(results_list, file.path(exp, "sfaR_results_efficiency_list_HS2_average_new.rds"))










