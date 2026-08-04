################################################################################
# 00_download_h2a.R
#
# Downloads the DOL / OFLC H-2A public disclosure files for FY2015-FY2020,
# plus the matching record layout PDFs (the "readme" files you were told to
# check for the definition of `master`).
#
# Strategy: scrape the Performance Data page for links rather than hard-coding
# URLs. DOL renames these files between releases (FY17 vs FY2017, _EOY, _Q4,
# .xls vs .xlsx), so a hard-coded list rots. There is a manual override at the
# bottom of section 3 for the case where the scrape misses a year.
#
# Run this once. It caches, so re-running is cheap.
################################################################################

## ---- 0. Packages -----------------------------------------------------------
setwd("/data/sikeme/TRADE/US_CHN_TradeWar_git/")

pkgs <- c("httr", "rvest", "xml2", "readxl", "dplyr", "purrr", "stringr", "tibble")
new  <- setdiff(pkgs, rownames(installed.packages()))
if (length(new)) install.packages(new)
invisible(lapply(pkgs, library, character.only = TRUE))


## ---- 1. Settings -----------------------------------------------------------

proj_dir <- path.expand("/data/sikeme/TRADE/US_CHN_TradeWar_git/data/h2a")            # <<< CHANGE ME
raw_dir  <- file.path(proj_dir, "raw")      # xlsx disclosure files land here
doc_dir  <- file.path(proj_dir, "docs")     # record layout PDFs land here

dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(doc_dir, recursive = TRUE, showWarnings = FALSE)

want_years <- 2015:2020
perf_url   <- "https://www.dol.gov/agencies/eta/foreign-labor/performance"

# DOL sometimes returns 403 to bare R requests. Identify yourself.
UA <- httr::user_agent(
  "Mozilla/5.0 (academic research data download; contact: you@university.edu)"
)


## ---- 2. Helpers ------------------------------------------------------------

harvest_links <- function(url = perf_url) {
  resp <- httr::GET(url, UA)
  httr::stop_for_status(resp)
  pg <- xml2::read_html(httr::content(resp, as = "text", encoding = "UTF-8"))
  
  nodes <- rvest::html_elements(pg, "a")
  tibble::tibble(
    text = stringr::str_squish(rvest::html_text2(nodes)),
    href = rvest::html_attr(nodes, "href")
  ) |>
    dplyr::filter(!is.na(href), nzchar(href)) |>
    dplyr::mutate(href = xml2::url_absolute(href, url)) |>
    dplyr::distinct()
}

# Fiscal year from either a 4-digit (FY2019) or 2-digit (FY17) tag.
extract_fy <- function(x) {
  y4 <- stringr::str_match(x, "(?i)FY[ _-]?(20\\d{2})")[, 2]
  y2 <- stringr::str_match(x, "(?i)FY[ _-]?(\\d{2})(?![0-9])")[, 2]
  out <- dplyr::coalesce(y4, ifelse(is.na(y2), NA_character_, paste0("20", y2)))
  as.integer(out)
}

# Quarter tag, if any. Files with no quarter tag (or _EOY) are the full-year
# cumulative release, which is what you want.
extract_qtr <- function(x) {
  as.integer(stringr::str_match(basename(x), "(?i)_Q([1-4])")[, 2])
}

pick_h2a <- function(links, ext_pat, must_match = "disclosure") {
  blob <- paste(links$text, links$href)
  links |>
    dplyr::filter(
      stringr::str_detect(href, stringr::regex(ext_pat, ignore_case = TRUE)),
      stringr::str_detect(blob, stringr::regex("H[-_ ]?2A", ignore_case = TRUE)),
      stringr::str_detect(blob, stringr::regex(must_match, ignore_case = TRUE))
    ) |>
    dplyr::mutate(fy = extract_fy(paste(text, href)),
                  qtr = extract_qtr(href)) |>
    dplyr::filter(fy %in% want_years)
}

# One file per fiscal year: prefer the cumulative annual release over Q1-Q3.
prefer_annual <- function(df) {
  df |>
    dplyr::mutate(rank = dplyr::case_when(is.na(qtr) ~ 0L, qtr == 4L ~ 1L,
                                          TRUE ~ 5L - qtr)) |>
    dplyr::group_by(fy) |>
    dplyr::slice_min(rank, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::arrange(fy)
}

fetch <- function(url, dest_dir) {
  dest <- file.path(dest_dir, basename(url))
  if (file.exists(dest) && file.size(dest) > 1e4) {
    message("  cached: ", basename(dest))
    return(invisible(dest))
  }
  message("  downloading: ", basename(url))
  resp <- httr::GET(url, UA, httr::write_disk(dest, overwrite = TRUE),
                    httr::progress())
  if (httr::http_error(resp)) {
    unlink(dest)
    warning("FAILED (", httr::status_code(resp), "): ", url)
    return(invisible(NA_character_))
  }
  invisible(dest)
}


## ---- 3. Find and download --------------------------------------------------

links <- harvest_links()
message("Found ", nrow(links), " links on the Performance Data page.")

data_urls <- links |> pick_h2a("\\.xlsx?(\\?.*)?$") |> prefer_annual()
doc_urls  <- links |> pick_h2a("\\.pdf(\\?.*)?$",
                               must_match = "record[ _]?layout") |> prefer_annual()

print(data_urls |> dplyr::select(fy, qtr, text, href), n = 50)

missing <- setdiff(want_years, data_urls$fy)
if (length(missing)) {
  warning("No disclosure file scraped for FY: ", paste(missing, collapse = ", "),
          "\nOpen ", perf_url, ", expand 'Disclosure Data', right-click the ",
          "missing H-2A links, and paste them into `manual_urls` below.")
}

# ---- MANUAL OVERRIDE -------------------------------------------------------
# Fill this in only for years the scrape missed. Format: c("2015" = "https://...")
manual_urls <- c(
  # "2015" = "https://www.dol.gov/sites/dolgov/files/ETA/oflc/pdfs/....xlsx"
)

if (length(manual_urls)) {
  data_urls <- dplyr::bind_rows(
    data_urls,
    tibble::tibble(text = "manual", href = unname(manual_urls),
                   fy = as.integer(names(manual_urls)), qtr = NA_integer_,
                   rank = 0L)
  ) |>
    dplyr::distinct(fy, .keep_all = TRUE) |>
    dplyr::arrange(fy)
}
# ----------------------------------------------------------------------------

message("\nDownloading disclosure files ...")
data_files <- purrr::map_chr(data_urls$href, fetch, dest_dir = raw_dir)

message("\nDownloading record layouts ...")
doc_files <- purrr::map_chr(doc_urls$href, fetch, dest_dir = doc_dir)

manifest <- data_urls |>
  dplyr::mutate(path = data_files,
                bytes = file.size(path),
                downloaded = Sys.time()) |>
  dplyr::select(fy, path, bytes, href, downloaded)

write.csv(manifest, file.path(proj_dir, "download_manifest.csv"), row.names = FALSE)
print(manifest |> dplyr::select(fy, bytes, path))


## ---- 4. Inspect the schema before you write any cleaning code --------------
#
# Do not skip this. The FY2020 file is the first FLAG-system release and its
# column names differ substantially from FY2015-2019 (iCERT). You cannot append
# them without a crosswalk.

peek <- function(path) {
  if (is.na(path) || !file.exists(path)) return(NULL)
  nm <- names(readxl::read_excel(path, n_max = 5, .name_repair = "minimal"))
  tibble::tibble(file = basename(path), n_cols = length(nm), col = nm)
}

schema <- purrr::map_dfr(manifest$path, peek)

# Which columns carry the worker counts?
schema |>
  dplyr::filter(stringr::str_detect(col, stringr::regex("worker", TRUE))) |>
  print(n = 100)

# Which columns could plausibly be `master` (the one-row-per-application flag)?
schema |>
  dplyr::filter(stringr::str_detect(
    col, stringr::regex("master|primary|sub|parent|record|type", TRUE))) |>
  print(n = 100)

# Full schema, by file, written out for reference.
write.csv(schema, file.path(proj_dir, "column_inventory.csv"), row.names = FALSE)

message("\nDone. Now open the record layout PDFs in ", doc_dir,
        " and find the field that corresponds to `master` in each year ",
        "before writing the collapse step.")

