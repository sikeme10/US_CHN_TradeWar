# =====================================================================
# qcew_impute.R
# Fixed-point imputation of suppressed QCEW employment & wages,
# following Autor, Beck, Dorn & Hanson (2026), Appendix A3,
# and the coauthor's STATA routine (QCEW_Fillinmissingdta2004.do).
#
# Imputes annual county x 6-digit NAICS cells whose employment/wages
# are suppressed (disclosure_code == "N") but whose establishment
# count is reported.
#
# INPUT: BLS QCEW "annual averages" CSVs.
#   https://www.bls.gov/cew/downloadable-data-files.htm
#   Use the SINGLE FILE annual csv (contains national + state + county),
#   OR the full "by area" set (all of: US000, every state, every county).
#   A partial by-area download will break the geographic step.
#
# DEPENDENCY: data.table (QCEW is ~3M+ rows; base R is too slow).
#   install.packages("data.table")
# =====================================================================

library(data.table)

# ------------------------- CONFIG ------------------------------------
INPUT_PATH       <- "qcew/2012.annual.singlefile.csv"  # file OR folder of csvs
OUTPUT_PATH      <- "qcew_2012_imputed_naics6.csv"
OWN_CODE_KEEP    <- 5L            # 5 = private. Set to NA to keep all ownerships.
WEIGHT_BY_ESTABS <- TRUE          # TRUE = paper (estab share); FALSE = uniform (your 2012 run)
MAX_ITER         <- 100L
TOL              <- 1e-4          # stop when max change in any cell < TOL (in persons / $1000)
VALUE_COLS       <- c("emp", "wages")  # what to impute
# ---------------------------------------------------------------------


# =====================================================================
# 1. LOAD AND NORMALIZE
# =====================================================================
read_qcew <- function(path) {
  files <- if (dir.exists(path)) {
    list.files(path, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
  } else path
  dt <- rbindlist(lapply(files, fread, colClasses = list(character = "area_fips",
                                                         character = "industry_code")),
                  use.names = TRUE, fill = TRUE)
  
  # Column names differ slightly across vintages; standardize.
  setnames(dt, old = grep("estabs", names(dt), value = TRUE)[1], new = "estabs")
  setnames(dt, old = "annual_avg_emplvl",  new = "emp",   skip_absent = TRUE)
  setnames(dt, old = "total_annual_wages", new = "wages", skip_absent = TRUE)
  
  if (!is.na(OWN_CODE_KEEP)) dt <- dt[own_code == OWN_CODE_KEEP]
  if ("qtr" %in% names(dt))  dt <- dt[as.character(qtr) %in% c("A", "a")]  # annual rows only
  
  dt[, area_fips     := trimws(as.character(area_fips))]
  dt[, industry_code := trimws(as.character(industry_code))]
  dt[, suppressed    := trimws(as.character(disclosure_code)) == "N"]
  dt[, estabs        := as.numeric(estabs)]
  for (v in VALUE_COLS) {
    dt[[v]] <- as.numeric(dt[[v]])
    dt[suppressed == TRUE, (v) := NA_real_]   # suppressed cells reported as 0; blank them
  }
  unique(dt, by = c("area_fips", "industry_code"))
}


# =====================================================================
# 2. CLASSIFY GEOGRAPHY AND INDUSTRY; DROP NON-NAICS AGGREGATES
# =====================================================================
classify <- function(dt) {
  # ---- geography ----
  dt[, geo_level := fifelse(area_fips == "US000", "national",
                            fifelse(grepl("^[0-9]{2}000$", area_fips), "state",
                                    fifelse(grepl("^[0-9]{5}$", area_fips), "county", NA_character_)))]
  dt <- dt[!is.na(geo_level)]                       # drops MSA/CSA ("C....") rows
  dt[, statefips := substr(area_fips, 1, 2)]
  dt <- dt[!(geo_level != "national" & statefips %in% c("72", "78"))]  # drop PR, VI
  
  # ---- industry ----
  # Keep the county-total "10" plus pure 2-6 digit NAICS. Sector codes in raw
  # QCEW carry hyphens (31-33, 44-45, 48-49); keep those as the 2-digit level.
  sector_hyphen <- c("31-33", "44-45", "48-49")
  is_plain_naics <- grepl("^[0-9]{2,6}$", dt$industry_code)
  dt[, ind_level := NA_integer_]
  dt[industry_code == "10", ind_level := 1L]
  dt[industry_code %in% sector_hyphen, ind_level := 2L]
  dt[is_plain_naics & nchar(industry_code) == 2L & industry_code != "10", ind_level := 2L]
  dt[is_plain_naics & nchar(industry_code) == 3L, ind_level := 3L]
  dt[is_plain_naics & nchar(industry_code) == 4L, ind_level := 4L]
  dt[is_plain_naics & nchar(industry_code) == 5L, ind_level := 5L]
  dt[is_plain_naics & nchar(industry_code) == 6L, ind_level := 6L]
  
  # Drop high-level non-NAICS aggregates (domains/supersectors: 101, 1011..1029),
  # nonclassifiable (99, 999...) and undefined (999999).
  dt <- dt[!is.na(ind_level)]
  dt <- dt[!(nchar(industry_code) >= 3L & substr(industry_code, 1, 2) == "10")]
  dt <- dt[!grepl("^9+$", industry_code)]
  dt[]
}


# =====================================================================
# 3. ANCESTOR CHAINS (nearest -> farthest), for both trees
# =====================================================================
sector_of <- function(prefix2) {
  data.table::fcase(prefix2 %in% c("31","32","33"), "31-33",
                    prefix2 %in% c("44","45"),      "44-45",
                    prefix2 %in% c("48","49"),      "48-49",
                    default = prefix2)
}

# returns a 5-col character matrix of industry ancestors (parent first), NA-padded
ind_ancestor_matrix <- function(code, ind_level) {
  n <- length(code)
  m <- matrix(NA_character_, nrow = n, ncol = 5)
  pre <- function(k) substr(code, 1, k)
  # parent of 6->5, 5->4, 4->3, 3->sector, 2->"10", 1->none
  lvl <- ind_level
  # column 1 = immediate parent
  m[, 1] <- fcase(lvl == 6L, pre(5),
                  lvl == 5L, pre(4),
                  lvl == 4L, pre(3),
                  lvl == 3L, sector_of(pre(2)),
                  lvl == 2L, "10",
                  default = NA_character_)
  # subsequent ancestors
  m[, 2] <- fcase(lvl == 6L, pre(4),
                  lvl == 5L, pre(3),
                  lvl == 4L, sector_of(pre(2)),
                  lvl == 3L, "10",
                  default = NA_character_)
  m[, 3] <- fcase(lvl == 6L, pre(3),
                  lvl == 5L, sector_of(pre(2)),
                  lvl == 4L, "10",
                  default = NA_character_)
  m[, 4] <- fcase(lvl == 6L, sector_of(pre(2)),
                  lvl == 5L, "10",
                  default = NA_character_)
  m[, 5] <- fcase(lvl == 6L, "10", default = NA_character_)
  m
}

# returns a 2-col character matrix of geographic ancestors (parent first)
geo_ancestor_matrix <- function(area_fips, geo_level) {
  n <- length(area_fips)
  m <- matrix(NA_character_, nrow = n, ncol = 2)
  m[, 1] <- fcase(geo_level == "county", paste0(substr(area_fips, 1, 2), "000"),
                  geo_level == "state",  "US000",
                  default = NA_character_)
  m[, 2] <- fcase(geo_level == "county", "US000", default = NA_character_)
  m
}


# =====================================================================
# 4. NEAREST DISCLOSED ANCESTOR (NDA) AND DISTRIBUTABLE AMOUNTS
#
# For one dimension: `group` is the holding-fixed key (area for the
# industry tree; industry for the geo tree). `node` is the moving key.
# Returns, per row, the governing node id (its NDA), and a lookup of
# distributable = value(governing) - sum(value of disclosed nodes
# whose NDA == that governing node).
# =====================================================================
nearest_disclosed_ancestor <- function(group, anc_mat, disclosed_key) {
  # disclosed_key: a set (named logical / hashed) of paste(group,node) that are disclosed
  nda <- rep(NA_character_, length(group))
  for (k in seq_len(ncol(anc_mat))) {
    todo <- is.na(nda) & !is.na(anc_mat[, k])
    if (!any(todo)) break
    cand <- anc_mat[todo, k]
    hit  <- paste0(group[todo], "\r", cand) %chin% disclosed_key
    sel  <- which(todo)[hit]
    nda[sel] <- cand[hit]
  }
  nda
}

build_dimension <- function(dt, dim = c("industry", "geo"), valcol) {
  dim <- match.arg(dim)
  if (dim == "industry") {
    group <- dt$area_fips
    node  <- dt$industry_code
    anc   <- ind_ancestor_matrix(dt$industry_code, dt$ind_level)
  } else {
    group <- dt$industry_code
    node  <- dt$area_fips
    anc   <- geo_ancestor_matrix(dt$area_fips, dt$geo_level)
  }
  disclosed <- !dt$suppressed
  disclosed_key <- paste0(group[disclosed], "\r", node[disclosed])
  
  nda <- nearest_disclosed_ancestor(group, anc, disclosed_key)
  
  # value lookup for disclosed governing nodes
  val <- dt[[valcol]]
  gov_val <- data.table(group = group[disclosed], node = node[disclosed], v = val[disclosed])
  setkey(gov_val, group, node)
  
  # deductions: disclosed nodes capped directly under a governing node
  ded <- data.table(group = group[disclosed], gov = nda[disclosed], v = val[disclosed])
  ded <- ded[!is.na(gov), .(deduct = sum(v, na.rm = TRUE)), by = .(group, gov)]
  setkey(ded, group, gov)
  
  # distributable per governing node = its value - deductions
  distrib <- gov_val[, .(group, gov = node, govval = v)]
  distrib <- ded[distrib, on = .(group, gov)]            # bring deduct onto every gov node
  distrib[is.na(deduct), deduct := 0]
  distrib[, distributable := govval - deduct]
  distrib[distributable < 0, distributable := 0]         # clip (matches STATA); see note below
  setkey(distrib, group, gov)
  
  list(nda = nda, group = group, distrib = distrib)
}


# =====================================================================
# 5. FIXED-POINT (IPF) OVER THE TWO CLUSTERS
# =====================================================================
impute_value <- function(dt, valcol, ind_dim, geo_dim) {
  n <- nrow(dt)
  leaf <- dt$suppressed & dt$ind_level == 6L & dt$geo_level == "county"
  
  # cluster keys for each leaf
  ind_clu <- paste0(ind_dim$group, "\r", ind_dim$nda)   # (area, governing industry)
  geo_clu <- paste0(geo_dim$group, "\r", geo_dim$nda)   # (industry, governing geo)
  
  # distributable target for each leaf's two clusters
  ind_tgt <- ind_dim$distrib[.(ind_dim$group, ind_dim$nda),
                             distributable, on = .(group, gov)]
  geo_tgt <- geo_dim$distrib[.(geo_dim$group, geo_dim$nda),
                             distributable, on = .(group, gov)]
  
  li  <- which(leaf)
  ic  <- ind_clu[li]; it <- ind_tgt[li]
  gc  <- geo_clu[li]; gt <- geo_tgt[li]
  est <- dt$estabs[li]
  est[is.na(est) | est <= 0] <- 1                      # guard
  
  # seed
  x <- if (WEIGHT_BY_ESTABS) as.numeric(est) else rep(1, length(li))
  
  # factor indices for fast grouped sums
  icf <- as.integer(factor(ic)); itv <- it[!duplicated(icf)][order(unique(icf))]
  gcf <- as.integer(factor(gc)); gtv <- gt[!duplicated(gcf)][order(unique(gcf))]
  ic_target <- tapply(it, icf, function(z) z[1])
  gc_target <- tapply(gt, gcf, function(z) z[1])
  
  rescale <- function(x, f, target) {
    s <- as.numeric(tapply(x, f, sum))
    fac <- ifelse(s > 0, target / s, 0)
    x * fac[f]
  }
  
  for (iter in seq_len(MAX_ITER)) {
    x_old <- x
    x <- rescale(x, icf, ic_target)     # industry dimension
    x <- rescale(x, gcf, gc_target)     # geographic dimension
    delta <- max(abs(x - x_old))
    if (is.finite(delta) && delta < TOL) break
  }
  message(sprintf("  %s: %d iterations, max final change = %.6g", valcol, iter, delta))
  
  out <- dt[[valcol]]
  out[li] <- x
  out
}


# =====================================================================
# 6. DRIVER
# =====================================================================
main <- function() {
  dt <- classify(read_qcew(INPUT_PATH))
  
  for (v in VALUE_COLS) {
    message("Imputing ", v, " ...")
    ind_dim <- build_dimension(dt, "industry", v)
    geo_dim <- build_dimension(dt, "geo",      v)
    dt[[paste0(v, "_imp")]] <- impute_value(dt, v, ind_dim, geo_dim)
  }
  
  # -------- validation: imputed leaves must sum to disclosed parents --------
  for (v in VALUE_COLS) {
    leaves <- dt[ind_level == 6L & geo_level == "county"]
    csum <- leaves[, .(s = sum(get(paste0(v, "_imp")), na.rm = TRUE)), by = area_fips]
    ctot <- dt[industry_code == "10" & geo_level == "county", .(area_fips, tot = get(v))]
    chk  <- ctot[csum, on = "area_fips"][!is.na(tot)]
    chk[, gap := s - tot]
    message(sprintf("[check %s] county totals: max |sum(6digit) - NAICS10| = %.4g",
                    v, max(abs(chk$gap), na.rm = TRUE)))
  }
  
  out <- dt[ind_level == 6L & geo_level == "county",
            .(year = if ("year" %in% names(dt)) year else NA,
              area_fips, industry_code, estabs,
              emp_reported = emp, emp_imputed = emp_imp,
              wages_reported = wages, wages_imputed = wages_imp,
              imputed = suppressed)]
  fwrite(out, OUTPUT_PATH)
  message("Wrote ", OUTPUT_PATH, " (", nrow(out), " county x 6-digit cells)")
  invisible(out)
}

if (sys.nframe() == 0L) main()

