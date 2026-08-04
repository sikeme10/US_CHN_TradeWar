################################################################################
# 01_build_h2a.R
#
# Reads the files downloaded by 00_download_h2a.R and produces:
#   h2a_list  : named list, one raw dataframe per fiscal year (h2a_list[["2019"]])
#   h2a_slim  : one harmonized dataframe stacked across FY2015-2020
#   h2a_year  : FY-level totals of requested and certified positions
#
# Column names are detected by pattern, not hard-coded, because FY2020 (FLAG)
# uses a different layout from FY2015-2019 (iCERT). The script prints what it
# matched in each year. CHECK THAT PRINTOUT. If it picks the wrong column you
# will get a silently wrong panel.
################################################################################

pkgs <- c("readxl", "dplyr", "purrr", "stringr", "tibble", "tidyr")
new  <- setdiff(pkgs, rownames(installed.packages()))
if (length(new)) install.packages(new)
invisible(lapply(pkgs, library, character.only = TRUE))

proj_dir <- path.expand("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/h2a")            # <<< CHANGE ME
manifest <- read.csv(file.path(proj_dir, "download_manifest.csv"),
                     stringsAsFactors = FALSE)


## ---- 1. Read each year -----------------------------------------------------

std_names <- function(x) {
  x <- trimws(x)
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  toupper(x)
}

read_year <- function(path, fy) {
  message("reading FY", fy, ": ", basename(path))
  df <- readxl::read_excel(path, guess_max = 100000, .name_repair = "minimal")
  names(df) <- std_names(names(df))
  df <- df[, !duplicated(names(df)), drop = FALSE]
  df$FY <- as.integer(fy)
  df
}

h2a_list <- purrr::map2(manifest$path, manifest$fy, read_year)
names(h2a_list) <- as.character(manifest$fy)

# Row counts per year, as a first sanity check.
purrr::imap_dfr(h2a_list, ~ tibble::tibble(fy = .y, rows = nrow(.x),
                                           cols = ncol(.x))) |> print()


## ---- 2. Locate the columns that matter -------------------------------------

# Ordered patterns: first match wins.
find_col <- function(nm, patterns) {
  for (p in patterns) {
    hit <- nm[stringr::str_detect(nm, stringr::regex(p, ignore_case = TRUE))]
    if (length(hit)) return(hit[1])
  }
  NA_character_
}

pat <- list(
  case      = c("^CASE_NO$", "^CASE_NUMBER$", "^CASE_ID$", "CASE_NO"),
  status    = c("^CASE_STATUS$", "STATUS"),
  requested = c("^NBR_WORKERS_REQUESTED$",
                "^TOTAL_WORKERS_H_?2A_REQUESTED$",
                "WORKERS?_REQUESTED", "REQUESTED_WORKERS"),
  certified = c("^NBR_WORKERS_CERTIFIED$",
                "^TOTAL_WORKERS_H_?2A_CERTIFIED$",
                "WORKERS?_CERTIFIED", "CERTIFIED_WORKERS"),
  master    = c("^MASTER$", "^MASTER_APP", "MASTER", "^MULTIPLE_WORKSITES?$",
                "PRIMARY_SUB", "^RECORD_TYPE$"),
  wst_state = c("^WORKSITE_STATE$", "^WORKSITE_LOCATION_STATE$",
                "WORKSITE.*STATE"),
  wst_city  = c("^WORKSITE_CITY$", "^WORKSITE_LOCATION_CITY$", "WORKSITE.*CITY"),
  wst_zip   = c("^WORKSITE_POSTAL_CODE$", "^WORKSITE_LOCATION_POSTAL_CODE$",
                "^WORKSITE_ZIP$", "^WORKSITE.*(POSTAL|ZIP)"),
  wst_county = c("^WORKSITE_COUNTY$", "^WORKSITE_LOCATION_COUNTY$",
                 "WORKSITE.*COUNTY"),
  emp_state = c("^EMPLOYER_STATE$", "^EMPLOYER_PROVINCE$", "EMPLOYER.*STATE"),
  emp_city  = c("^EMPLOYER_CITY$", "EMPLOYER.*CITY"),
  emp_zip   = c("^EMPLOYER_POSTAL_CODE$", "^EMPLOYER_ZIP$",
                "^EMPLOYER(?!.*POC).*(POSTAL|ZIP)"),
  decision  = c("^DECISION_DATE$", "DECISION.*DATE")
)



map_tbl <- purrr::imap_dfr(h2a_list, function(df, fy) {
  nm <- names(df)
  tibble::tibble(fy = fy, field = names(pat),
                 matched = purrr::map_chr(pat, ~ find_col(nm, .x)))
}) |>
  tidyr::pivot_wider(names_from = fy, values_from = matched)

cat("\n--- COLUMN MAPPING (verify this before trusting anything below) ---\n")
print(as.data.frame(map_tbl))


## ---- 3. Harmonize ----------------------------------------------------------

slim_one <- function(df, fy) {
  nm <- names(df)
  g  <- function(k) {
    col <- find_col(nm, pat[[k]])
    if (is.na(col)) return(rep(NA, nrow(df)))
    df[[col]]
  }
  num <- function(x) suppressWarnings(as.numeric(gsub("[^0-9.-]", "", as.character(x))))
  
  tibble::tibble(
    fy             = as.integer(fy),
    case_no        = as.character(g("case")),
    case_status    = toupper(trimws(as.character(g("status")))),
    decision_date  = as.character(g("decision")),
    nbr_requested  = num(g("requested")),
    nbr_certified  = num(g("certified")),
    master_raw     = as.character(g("master")),
    worksite_city  = toupper(trimws(as.character(g("wst_city")))),
    worksite_state = toupper(trimws(as.character(g("wst_state")))),
    employer_city  = toupper(trimws(as.character(g("emp_city")))),
    employer_state = toupper(trimws(as.character(g("emp_state")))),
    worksite_zip    = as.character(g("wst_zip")),
    worksite_county = toupper(trimws(as.character(g("wst_county")))),
    employer_zip    = as.character(g("emp_zip"))
  )
}

h2a_slim <- purrr::imap_dfr(h2a_list, slim_one)

# What values does the master field actually take? Look before you filter.
cat("\n--- master field values by FY ---\n")
h2a_slim |> count(fy, master_raw) |> print(n = 100)

cat("\n--- case_status values by FY ---\n")
h2a_slim |> count(fy, case_status) |> print(n = 200)


## ---- 4. De-duplicate to one row per application ----------------------------
#
# THIS IS THE STEP THAT DECIDES YOUR NUMBERS. Three options, pick deliberately.

dedupe_mode <- "case_no"   # "master" | "case_no" | "none"

h2a_apps <- switch(
  dedupe_mode,
  # (a) Use the master flag, once you have confirmed from the record layout
  #     what it means and that the master row carries the full worker count.
  master = h2a_slim |>
    dplyr::filter(master_raw %in% c("1", "Y", "YES", "TRUE", "MASTER")),
  
  # (b) Fallback: one row per case number per year. Assumes the worker counts
  #     are repeated identically across the duplicate rows, NOT split across
  #     them. Verify with the diagnostic below before relying on this.
  case_no = h2a_slim |>
    dplyr::group_by(fy, case_no) |>
    dplyr::slice(1) |>
    dplyr::ungroup(),
  
  none = h2a_slim
)

# Diagnostic for option (b): do duplicate rows of the same case carry the same
# worker counts? If n_distinct > 1 for many cases, slice(1) is wrong and you
# must resolve `master` properly.
cat("\n--- cases whose duplicate rows disagree on worker counts ---\n")
h2a_slim |>
  dplyr::group_by(fy, case_no) |>
  dplyr::summarise(n_rows = dplyr::n(),
                   n_vals_cert = dplyr::n_distinct(nbr_certified),
                   .groups = "drop") |>
  dplyr::filter(n_rows > 1) |>
  dplyr::count(fy, disagree = n_vals_cert > 1) |>
  print()


## ---- 5. Yearly panel -------------------------------------------------------

is_certified <- function(x) stringr::str_detect(x, "CERTIF")

h2a_year <- h2a_apps |>
  dplyr::group_by(fy) |>
  dplyr::summarise(
    n_applications    = dplyr::n(),
    n_certified_cases = sum(is_certified(case_status), na.rm = TRUE),
    workers_requested = sum(nbr_requested, na.rm = TRUE),
    workers_certified = sum(nbr_certified, na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(cert_rate = workers_certified / workers_requested)

cat("\n--- YEARLY PANEL ---\n")
print(as.data.frame(h2a_year))

# Same thing by worksite state, if you want it later.
h2a_year_state <- h2a_apps |>
  dplyr::group_by(fy, worksite_state) |>
  dplyr::summarise(workers_requested = sum(nbr_requested, na.rm = TRUE),
                   workers_certified = sum(nbr_certified, na.rm = TRUE),
                   .groups = "drop")

saveRDS(h2a_list, file.path(proj_dir, "h2a_list.rds"))
write.csv(h2a_slim,       file.path(proj_dir, "h2a_slim.csv"),       row.names = FALSE)
write.csv(h2a_year,       file.path(proj_dir, "h2a_year.csv"),       row.names = FALSE)
write.csv(h2a_year_state, file.path(proj_dir, "h2a_year_state.csv"), row.names = FALSE)

message("\nWrote h2a_year.csv. Validate against OFLC's published annual ",
        "Selected Statistics before using it.")