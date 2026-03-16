# ============================================================
# Download FSA Payment Files for 2017, 2018, and 2019
# Run: Rscript download_fsa_2017_2019.R
# ============================================================

options(download.file.method = "libcurl")
options(timeout = 600)

# Force HTTP/1.1 to avoid HTTP/2 stream errors
options(download.file.method = "curl", download.file.extra = "--http1.1 -L")

base_url <- "https://www.fsa.usda.gov/sites/default/files/documents/"

# -- 2019 files -----------------------------------------------
files_2019 <- data.frame(
  year = 2019,
  filename = c(
    "state-al-id.foia.na.pmt19.final.dt20297.xlsx",
    "state-il-in.foia.na.pmt19.final.dt20297.xlsx",
    "state-ia.foia.na.pmt19.final.dt20297.xlsx",
    "state-ks.foia.na.pmt19.final.dt20297.xlsx",
    "state-ky-mn.foia.na.pmt19.final.dt20297.xlsx",
    "state-ms-mt.foia.na.pmt19.final.dt20297.xlsx",
    "state-ne-nc.foia.na.pmt19.final.dt20297.xlsx",
    "state-nd-or.foia.na.pmt19.final.dt20297.xlsx",
    "state-pa-tn.foia.na.pmt19.final.dt20297.xlsx",
    "state-tx-va.foia.na.pmt19.final.dt20297.xlsx",
    "state-wa-wy.foia.na.pmt19.final.dt20297.xlsx"
  ),
  label = c(
    "Alabama - Idaho (32.9 MB)",
    "Illinois - Indiana (42.0 MB)",
    "Iowa (29.3 MB)",
    "Kansas (30.5 MB)",
    "Kentucky - Minnesota (39.5 MB)",
    "Mississippi - Montana (30.3 MB)",
    "Nebraska - North Carolina (28.1 MB)",
    "North Dakota - Oregon (36.5 MB)",
    "Pennsylvania - Tennessee (25.3 MB)",
    "Texas - Virginia (32.0 MB)",
    "Washington - Wyoming (16.2 MB)"
  ),
  stringsAsFactors = FALSE
)

# -- 2018 files -----------------------------------------------
files_2018 <- data.frame(
  year = 2018,
  filename = c(
    "state-al-id.na.pmt18.final.dt20297.xlsx",
    "state-ia.na.pmt18.final.dt20297.xlsx",
    "state-il-in.na.pmt18.final.dt20297.xlsx",
    "state-ks.na.pmt18.final.dt20297.xlsx",
    "state-ky-ms.na.pmt18.final.dt20297.xlsx",
    "state-mo-ny.na.pmt18.final.dt20297.xlsx",
    "state-nc-oh.na.pmt18.final.dt20297.xlsx",
    "state-ok-tn.na.pmt18.final.dt20297.xlsx",
    "state-tx-wy.na.pmt18.final.dt20297.xlsx"
  ),
  label = c(
    "Alabama - Idaho (25.5 MB)",
    "Iowa (22.5 MB)",
    "Illinois - Indiana (39 MB)",
    "Kansas (25.5 MB)",
    "Kentucky - Mississippi (37.5 MB)",
    "Montana - New York (36.5 MB)",
    "North Carolina - Ohio (33.5 MB)",
    "Oklahoma - Tennessee (30.5 MB)",
    "Texas - Wyoming (37.5 MB)"
  ),
  stringsAsFactors = FALSE
)

# -- 2017 files -----------------------------------------------
files_2017 <- data.frame(
  year = 2017,
  filename = c(
    "state-al-id.foia.na.pmt17.final.dt20297.xlsx",
    "state-il.foia.na.pmt17.final.dt20297.xlsx",
    "state-in-ia.foia.na.pmt17.final.dt20297.xlsx",
    "state-ks-mi.foia.na.pmt17.final.dt20297.xlsx",
    "state-mn-mt.foia.na.pmt17.final.dt20297.xlsx",
    "state-nd-pa.foia.na.pmt17.final.dt20297.xlsx",
    "state-ne-nc.foia.na.pmt17.final.dt20297.xlsx",
    "state-pr-ut.foia.na.pmt17.final.dt20297.xlsx",
    "state-vt-wy.foia.na.pmt17.final.dt20297.xlsx"
  ),
  label = c(
    "Alabama - Idaho (24.3 MB)",
    "Illinois (26.9 MB)",
    "Indiana - Iowa (37.4 MB)",
    "Kansas - Michigan (36.3 MB)",
    "Minnesota - Montana (34.4 MB)",
    "North Dakota - Pennsylvania (34.6 MB)",
    "Nebraska - North Carolina (22.3 MB)",
    "Puerto Rico - Utah (32.7 MB)",
    "Vermont - Wyoming (13.4 MB)"
  ),
  stringsAsFactors = FALSE
)

# -- Combine and download -------------------------------------
all_files <- rbind(files_2019, files_2018, files_2017)

output_dir <- "/data/sikeme/TRADE/US_CHN_TradeWar_git/data/MFP/fsa_payments"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# Create year subdirectories
for (yr in unique(all_files$year)) {
  yr_dir <- file.path(output_dir, yr)
  if (!dir.exists(yr_dir)) dir.create(yr_dir)
}

cat("Downloading FSA Payment Files (2017-2019)\n")
cat("==========================================\n")
cat(sprintf("Total files: %d\n\n", nrow(all_files)))

for (i in seq_len(nrow(all_files))) {
  url  <- paste0(base_url, all_files$filename[i])
  dest <- file.path(output_dir, all_files$year[i], all_files$filename[i])
  
  cat(sprintf("[%02d/%02d] %d | %s\n", i, nrow(all_files),
              all_files$year[i], all_files$label[i]))
  
  if (file.exists(dest)) {
    cat("        Already exists, skipping.\n")
    next
  }
  
  tryCatch({
    download.file(url, destfile = dest, mode = "wb", quiet = FALSE)
    size_mb <- file.info(dest)$size / 1024^2
    cat(sprintf("        Done (%.1f MB)\n", size_mb))
  }, error = function(e) {
    cat(sprintf("        FAILED: %s\n", e$message))
  })
}

cat("\n==========================================\n")
cat(sprintf("Files saved to: %s/\n", output_dir))
cat("  2019/ - 11 files\n")
cat("  2018/ -  9 files\n")
cat("  2017/ -  9 files\n")

files <- list.files(output_dir, recursive = TRUE, pattern = "\\.xlsx$", full.names = TRUE)
info <- data.frame(
  file = basename(files),
  size_MB = round(file.info(files)$size / 1024^2, 1)
)
print(info, row.names = FALSE)
cat(sprintf("\nTotal files: %d\n", nrow(info)))

