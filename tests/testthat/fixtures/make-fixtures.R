#!/usr/bin/env Rscript
# Regenerate the tiny .csv.zst fixtures used by the test suite.
# Run from anywhere: Rscript tests/testthat/fixtures/make-fixtures.R
#
# These are deliberately small but contain every documented r2u-logs hazard so
# the cleaning/aggregation logic is tested against reality, not a happy path.

# Resolve this script's own directory so output lands in fixtures/ regardless of cwd.
args <- commandArgs(trailingOnly = FALSE)
this <- sub("^--file=", "", grep("^--file=", args, value = TRUE))
fixtures_dir <- if (length(this) == 1L && nzchar(this)) {
  dirname(normalizePath(this))
} else {
  normalizePath(file.path(getwd(), "tests", "testthat", "fixtures"))
}

hdr <- "date,host,dist,deb,pkg,ver,arch,repo,name,day"

write_zst <- function(rel, lines) {
  out <- file.path(fixtures_dir, rel)
  dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)
  csv <- tempfile(fileext = ".csv")
  writeLines(c(hdr, lines), csv)
  status <- system2("zstd", c("-q", "-f", csv, "-o", out))
  unlink(csv)
  if (status != 0L) stop("zstd failed for ", rel)
  cat("wrote", rel, "\n")
}

# r2u host, 2026-01:
#  - dplyr jammy/amd64 x2 (exact duplicates -> must count as 2, not deduped)
#  - dplyr noble/arm64 x1
#  - a Bioconductor row (r-bioc-)
#  - a malformed trailing-colon row (arch="amd64.deb:" -> dropped by arch filter)
#  - a repo=api probe row (dropped)
#  - the 'name' column is the bogus decoy value 'arrow' throughout (must be ignored)
write_zst("r2u/r2u_r2u-2026-01.csv.zst", c(
  "2026-01-05T01:00:00Z,r2u,jammy,r-cran-dplyr_1.1.4-1.ca2204.1_amd64.deb,r-cran-dplyr,1.1.4-1.ca2204.1,amd64,cran,arrow,2026-01-05",
  "2026-01-05T01:00:00Z,r2u,jammy,r-cran-dplyr_1.1.4-1.ca2204.1_amd64.deb,r-cran-dplyr,1.1.4-1.ca2204.1,amd64,cran,arrow,2026-01-05",
  "2026-01-05T02:00:00Z,r2u,noble,r-cran-dplyr_1.1.4-1.ca2404.1_arm64.deb,r-cran-dplyr,1.1.4-1.ca2404.1,arm64,cran,arrow,2026-01-05",
  "2026-01-06T03:00:00Z,r2u,jammy,r-bioc-biocgenerics_0.56.0-1.ca2204.1_all.deb,r-bioc-biocgenerics,0.56.0-1.ca2204.1,all,bioc,arrow,2026-01-06",
  "2026-01-06T04:00:00Z,r2u,jammy,r-cran-islasso_1.6.2-1.ca2204.1_amd64.deb:,r-cran-islasso,1.6.2-1.ca2204.1,amd64.deb:,cran,arrow,2026-01-06",
  "2026-01-06T05:00:00Z,r2u,jammy,bioc-api-package_0.1.0_all.deb,bioc-api-package,0.1.0,all,api,package,2026-01-06"))

# rob host, 2026-01: one dplyr fetch (host union must add it to the r2u count).
write_zst("rob/r2u_rob-2026-01.csv.zst", c(
  "2026-01-05T06:00:00Z,rob,focal,r-cran-dplyr_1.1.4-1.ca2004.1_amd64.deb,r-cran-dplyr,1.1.4-1.ca2004.1,amd64,cran,arrow,2026-01-05"))

# r2u host, 2025-12: a December row (belongs to 2025) PLUS a 2026-01-01 boundary
# row that physically lives in this file (cut at ~06:00 UTC on the 1st).
write_zst("r2u/r2u_r2u-2025-12.csv.zst", c(
  "2025-12-31T23:00:00Z,r2u,jammy,r-cran-dplyr_1.1.4-1.ca2204.1_amd64.deb,r-cran-dplyr,1.1.4-1.ca2204.1,amd64,cran,arrow,2025-12-31",
  "2026-01-01T05:30:00Z,r2u,jammy,r-cran-dplyr_1.1.4-1.ca2204.1_amd64.deb,r-cran-dplyr,1.1.4-1.ca2204.1,amd64,cran,arrow,2026-01-01"))

# rob host, 2025-12: header only (empty data), exercises the empty-file path.
write_zst("rob/r2u_rob-2025-12.csv.zst", character(0))

cat("fixtures written to", fixtures_dir, "\n")
