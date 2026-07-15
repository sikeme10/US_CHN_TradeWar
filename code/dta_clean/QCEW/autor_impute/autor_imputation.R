# =====================================================================
# qcew_impute_readable.R
#
# Step-by-step version of the QCEW suppression imputation
# (Autor, Beck, Dorn & Hanson 2026, Appendix A3; coauthor STATA .do file).
#
# Read it top to bottom. There is ONE helper function (find_governing_node,
# defined first because both dimensions use it); the rest is a plain script
# inside a year loop, organized as numbered STEPs.
#
# Needs data.table:  install.packages("data.table")
# =====================================================================
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/QCEW/")
library(data.table)

# ------------------------- CONFIG ------------------------------------
YEARS           <- 2015                          # set to one year, e.g. 2018, or a range
INPUT_TEMPLATE  <- "qcew/%d.annual.singlefile.csv"    # %d becomes the year; file OR folder
OUTPUT_TEMPLATE <- "qcew/%d_imputed_naics6.csv"
WEIGHT_BY_ESTABS <- TRUE     # TRUE = seed by establishment share (paper); FALSE = uniform (2012 .do)
MAX_ITER        <- 100L
TOL             <- 1e-4      # stop when the largest change in any imputed cell falls below this
# ---------------------------------------------------------------------


# =====================================================================
# HELPER (used for both the industry tree and the geography tree)
#
# For each row, walk up its list of ancestor codes from nearest to
# farthest and return the FIRST ancestor that is disclosed, together
# with that ancestor's reported value. "Disclosed within the same
# group" means: same county (industry tree) or same industry (geo tree).
# =====================================================================
find_governing_node <- function(group, ancestors, disclosed) {
  # group     : vector held fixed in this dimension (county code, or industry code)
  # ancestors : n-by-K character matrix; column 1 is the nearest parent, NA-padded
  # disclosed : data.table(group, node, value) listing ONLY disclosed nodes
  n         <- length(group)
  gov_node  <- rep(NA_character_, n)   # which ancestor governs this row
  gov_value <- rep(NA_real_,      n)   # that ancestor's reported value
  
  for (k in seq_len(ncol(ancestors))) {           # k = 1 nearest parent, 2 grandparent, ...
    still_looking <- is.na(gov_node) & !is.na(ancestors[, k])
    if (!any(still_looking)) break
    
    # look up the candidate ancestor's disclosed value (NA if it is not disclosed)
    cand <- data.table(group = group[still_looking],
                       node  = ancestors[still_looking, k])
    cand[disclosed, on = .(group, node), val := i.value]
    
    found <- !is.na(cand$val)                      # this ancestor is disclosed -> it governs
    rows  <- which(still_looking)[found]
    gov_node[rows]  <- ancestors[still_looking, k][found]
    gov_value[rows] <- cand$val[found]
  }
  data.table(gov_node = gov_node, gov_value = gov_value)
}


# =====================================================================
# MAIN LOOP: one full pass per year
# =====================================================================
for (year in YEARS) {
  
  input_path  <- sprintf(INPUT_TEMPLATE,  year)
  output_path <- sprintf(OUTPUT_TEMPLATE, year)
  if (!file.exists(input_path) && !dir.exists(input_path)) {
    message("No input for ", year, " (", input_path, "); skipping."); next
  }
  message("\n===== YEAR ", year, " =====")
  
  
  # -------------------------------------------------------------------
  # STEP 1. Read the raw QCEW csv(s) and give the key columns short names.
  # -------------------------------------------------------------------
  files <- if (dir.exists(input_path))
    list.files(input_path, "\\.csv$", full.names = TRUE, recursive = TRUE)
  else input_path
  dt <- rbindlist(lapply(files, fread,
                         colClasses = list(character = c("area_fips", "industry_code"))),
                  use.names = TRUE, fill = TRUE)
  
  # column that holds establishment counts is named annual_avg_estabs_count in most vintages
  estab_col <- grep("^annual_avg_estabs", names(dt), value = TRUE)[1]
  setnames(dt, estab_col, "estabs")
  setnames(dt, "annual_avg_emplvl",  "emp",   skip_absent = TRUE)
  setnames(dt, "total_annual_wages", "wages", skip_absent = TRUE)
  
  
  # -------------------------------------------------------------------
  # STEP 2. Keep private ownership and annual rows only.
  #         (own_code 5 = private; qtr "A" = the annual average row)
  # -------------------------------------------------------------------
  dt <- dt[own_code == 5L]
  if ("qtr" %in% names(dt)) dt <- dt[as.character(qtr) %in% c("A", "a")]
  dt[, area_fips     := trimws(as.character(area_fips))]
  dt[, industry_code := trimws(as.character(industry_code))]
  
  
  # -------------------------------------------------------------------
  # STEP 3. Flag suppressed cells. When disclosure_code == "N", BLS reports
  #         emp and wages as 0 even though the cell exists; blank them so we
  #         know to impute them. Establishment counts stay (always reported).
  # -------------------------------------------------------------------
  dt[, suppressed := trimws(as.character(disclosure_code)) == "N"]
  dt[, estabs := as.numeric(estabs)]
  dt[, emp    := as.numeric(emp)]
  dt[, wages  := as.numeric(wages)]
  dt[suppressed == TRUE, c("emp", "wages") := NA_real_]
  
  dt <- unique(dt, by = c("area_fips", "industry_code"))   # one row per cell
  
  
  # -------------------------------------------------------------------
  # STEP 4. Classify geography from area_fips:
  #         US000 = national, XX000 = state, 5 digits = county.
  #         Drop MSA/CSA ("C..." codes) and Puerto Rico / Virgin Islands.
  # -------------------------------------------------------------------
  dt[, geo_level := fifelse(area_fips == "US000", "national",
                            fifelse(grepl("^[0-9]{2}000$", area_fips), "state",
                                    fifelse(grepl("^[0-9]{5}$",    area_fips), "county", NA_character_)))]
  dt <- dt[!is.na(geo_level)]
  dt <- dt[!(geo_level != "national" & substr(area_fips, 1, 2) %in% c("72", "78"))]
  
  
  # -------------------------------------------------------------------
  # STEP 5. Classify industry level (1 = the "10" county total, 2..6 = NAICS
  #         digits). Raw QCEW writes the manufacturing/retail/transport
  #         sectors with hyphens: 31-33, 44-45, 48-49. Keep those as level 2.
  # -------------------------------------------------------------------
  sectors <- c("31-33", "44-45", "48-49")
  plain   <- grepl("^[0-9]{2,6}$", dt$industry_code)        # pure-digit codes
  dt[, ind_level := NA_integer_]
  dt[industry_code == "10",                                   ind_level := 1L]
  dt[industry_code %in% sectors,                              ind_level := 2L]
  dt[plain & nchar(industry_code) == 2L & industry_code != "10", ind_level := 2L]
  dt[plain & nchar(industry_code) == 3L,                      ind_level := 3L]
  dt[plain & nchar(industry_code) == 4L,                      ind_level := 4L]
  dt[plain & nchar(industry_code) == 5L,                      ind_level := 5L]
  dt[plain & nchar(industry_code) == 6L,                      ind_level := 6L]
  
  # drop non-NAICS aggregates: domains/supersectors (101, 1011..1029),
  # nonclassifiable (99, 999, ...) and undefined (999999)
  dt <- dt[!is.na(ind_level)]
  dt <- dt[!(nchar(industry_code) >= 3L & substr(industry_code, 1, 2) == "10")]
  dt <- dt[!grepl("^9+$", industry_code)]
  
  
  # -------------------------------------------------------------------
  # STEP 6. Build the ANCESTOR CODE columns for each cell, nearest first.
  #
  # Industry ancestors of "311111": 31111, 3111, 311, 31-33, 10
  # (the 2-digit step uses the hyphen sector code).
  # Geography ancestors of county "01001": 01000 (state), US000 (national).
  # We store them as character matrices; NA where the chain is shorter.
  # -------------------------------------------------------------------
  # helper to turn a 2-digit prefix into the hyphenated sector code
  sector_of <- function(p2) fcase(p2 %in% c("31","32","33"), "31-33",
                                  p2 %in% c("44","45"),      "44-45",
                                  p2 %in% c("48","49"),      "48-49",
                                  default = p2)
  code <- dt$industry_code
  p5 <- substr(code,1,5); p4 <- substr(code,1,4); p3 <- substr(code,1,3)
  sec <- sector_of(substr(code,1,2))
  lvl <- dt$ind_level
  
  ind_anc <- cbind(
    fcase(lvl==6L, p5, lvl==5L, p4, lvl==4L, p3, lvl==3L, sec, lvl==2L, "10", default=NA_character_),
    fcase(lvl==6L, p4, lvl==5L, p3, lvl==4L, sec, lvl==3L, "10", default=NA_character_),
    fcase(lvl==6L, p3, lvl==5L, sec, lvl==4L, "10", default=NA_character_),
    fcase(lvl==6L, sec, lvl==5L, "10", default=NA_character_),
    fcase(lvl==6L, "10", default=NA_character_)
  )
  
  geo_anc <- cbind(
    fcase(dt$geo_level=="county", paste0(substr(dt$area_fips,1,2),"000"),
          dt$geo_level=="state",  "US000", default=NA_character_),
    fcase(dt$geo_level=="county", "US000", default=NA_character_)
  )
  
  
  # -------------------------------------------------------------------
  # STEP 7. Impute emp and wages. Same machinery for each, so loop over them.
  #         disclosure is identical for emp and wages, so the cluster
  #         structure is the same; only the target totals differ.
  # -------------------------------------------------------------------
  for (v in c("emp", "wages")) {
    message("  imputing ", v, " ...")
    value <- dt[[v]]
    
    ## 7a. INDUSTRY dimension --------------------------------------------------
    ##     Within a county, attach each suppressed 6-digit cell to the nearest
    ##     disclosed industry total above it.
    disc_ind <- data.table(group = dt$area_fips, node = dt$industry_code, value = value
    )[!dt$suppressed]                       # disclosed nodes only
    ind_gov  <- find_governing_node(dt$area_fips, ind_anc, disc_ind)
    
    ##     distributable = governing total  MINUS  the disclosed cells that
    ##     hang directly under it (so we only spread what is left over).
    ded_ind <- data.table(group = dt$area_fips, gov = ind_gov$gov_node, val = value
    )[!dt$suppressed & !is.na(gov), .(deduct = sum(val, na.rm=TRUE)),
      by = .(group, gov)]
    ind_tab <- unique(data.table(group = dt$area_fips, gov = ind_gov$gov_node,
                                 govval = ind_gov$gov_value)[!is.na(gov)])
    ind_tab <- ded_ind[ind_tab, on = .(group, gov)]
    ind_tab[is.na(deduct), deduct := 0]
    ind_tab[, target := pmax(govval - deduct, 0)]                # clip tiny negatives to 0
    
    ##     give every row the distributable of the cluster it belongs to
    ind_target <- ind_tab[data.table(group = dt$area_fips, gov = ind_gov$gov_node),
                          target, on = .(group, gov)]
    ind_cluster <- paste(dt$area_fips, ind_gov$gov_node)          # cluster id
    
    ## 7b. GEOGRAPHY dimension -------------------------------------------------
    ##     Within a 6-digit industry, attach each suppressed county cell to the
    ##     nearest disclosed geographic total (its state, else the nation).
    disc_geo <- data.table(group = dt$industry_code, node = dt$area_fips, value = value
    )[!dt$suppressed]
    geo_gov  <- find_governing_node(dt$industry_code, geo_anc, disc_geo)
    
    ded_geo <- data.table(group = dt$industry_code, gov = geo_gov$gov_node, val = value
    )[!dt$suppressed & !is.na(gov), .(deduct = sum(val, na.rm=TRUE)),
      by = .(group, gov)]
    geo_tab <- unique(data.table(group = dt$industry_code, gov = geo_gov$gov_node,
                                 govval = geo_gov$gov_value)[!is.na(gov)])
    geo_tab <- ded_geo[geo_tab, on = .(group, gov)]
    geo_tab[is.na(deduct), deduct := 0]
    geo_tab[, target := pmax(govval - deduct, 0)]
    
    geo_target  <- geo_tab[data.table(group = dt$industry_code, gov = geo_gov$gov_node),
                           target, on = .(group, gov)]
    geo_cluster <- paste(dt$industry_code, geo_gov$gov_node)
    
    ## 7c. The cells we actually solve for: suppressed, 6-digit, county level.
    leaf <- dt$suppressed & dt$ind_level == 6L & dt$geo_level == "county"
    li   <- which(leaf)
    
    ##     Diagnostic: how many of these have NO disclosed parent to anchor to?
    ##     (gov_node == NA means the whole chain above them is suppressed too.)
    bad_ind <- is.na(ind_gov$gov_node[li])
    bad_geo <- is.na(geo_gov$gov_node[li])
    if (any(bad_ind) || any(bad_geo))
      message(sprintf("    note: of %d suppressed cells, %d have no disclosed industry parent, %d no disclosed geographic parent (%d have neither)",
                      length(li), sum(bad_ind), sum(bad_geo), sum(bad_ind & bad_geo)))
    
    ind_cl <- factor(ind_cluster[li]);  ind_t <- tapply(ind_target[li], ind_cl, `[`, 1)
    geo_cl <- factor(geo_cluster[li]);  geo_t <- tapply(geo_target[li], geo_cl, `[`, 1)
    ind_id <- as.integer(ind_cl);       geo_id <- as.integer(geo_cl)
    
    ## 7d. Seed: establishment share (paper) or uniform (2012 .do).
    seed <- if (WEIGHT_BY_ESTABS) dt$estabs[li] else rep(1, length(li))
    seed[is.na(seed) | seed <= 0] <- 1
    x <- as.numeric(seed)
    
    ## 7e. Fixed point: rescale so each cluster sums to its target, alternating
    ##     the two dimensions until cells stop moving (= the GC/IC loop in STATA).
    ##     rescale() is NA-proof: if a cluster has no target (NA) or an empty
    ##     sum, its factor becomes 1, i.e. those cells are left unchanged in that
    ##     dimension instead of turning into NA. A target of exactly 0 still pins
    ##     its cells to 0.
    rescale <- function(x, id, target_by_cluster) {
      s <- as.numeric(tapply(x, id, sum))
      f <- target_by_cluster / s
      f[!is.finite(f)] <- 1                 # catches NA target, 0/0, and divide-by-zero
      x * f[id]
    }
    for (it in seq_len(MAX_ITER)) {
      x_prev <- x
      x <- rescale(x, ind_id, ind_t)        # industry dimension
      x <- rescale(x, geo_id, geo_t)        # geographic dimension
      d <- max(abs(x - x_prev))
      if (is.finite(d) && d < TOL) break
    }
    message(sprintf("    %d iterations, last max change = %.4g", it, d))
    
    ## 7f. Cells with no constraint in EITHER dimension can't be imputed; mark NA.
    x[bad_ind & bad_geo] <- NA_real_
    
    ## 7g. Write imputed values back; keep reported values where not suppressed.
    imp <- dt[[v]]; imp[li] <- x
    dt[[paste0(v, "_imp")]] <- imp
  }
  
  
  # -------------------------------------------------------------------
  # STEP 8. Sanity check: imputed 6-digit cells should sum to the
  #         reported county total (NAICS "10") in each county.
  # -------------------------------------------------------------------
  for (v in c("emp", "wages")) {
    leaves <- dt[ind_level == 6L & geo_level == "county"]
    got <- leaves[, .(s = sum(get(paste0(v,"_imp")), na.rm=TRUE)), by = area_fips]
    tot <- dt[industry_code == "10" & geo_level == "county", .(area_fips, t = get(v))]
    chk <- tot[got, on = "area_fips"][!is.na(t)]
    message(sprintf("  [check %s] max |sum(6-digit) - county total| = %.4g",
                    v, max(abs(chk$s - chk$t), na.rm = TRUE)))
  }
  
  
  # -------------------------------------------------------------------
  # STEP 9. Save one file per year.
  # -------------------------------------------------------------------
  out <- dt[ind_level == 6L & geo_level == "county",
            .(year, area_fips, industry_code, estabs,
              emp_reported = emp,   emp_imputed = emp_imp,
              wages_reported = wages, wages_imputed = wages_imp,
              imputed = suppressed)]
  fwrite(out, output_path)
  message("  wrote ", output_path, " (", nrow(out), " cells)")
}