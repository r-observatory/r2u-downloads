mk_daily <- function() {
  rows <- data.frame(
    package = c("dplyr", "dplyr", "oldpkg"),
    date    = c("2026-05-30", "2026-04-15", "2026-05-29"),
    repo    = c("cran", "cran", "bioc"),
    dist    = "jammy", arch = "amd64",
    count   = c(100L, 50L, 7L), stringsAsFactors = FALSE)
  con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  DBI::dbExecute(con, "CREATE TABLE r2u_downloads_daily
    (package TEXT, date TEXT, repo TEXT, dist TEXT, arch TEXT, count INTEGER)")
  DBI::dbWriteTable(con, "r2u_downloads_daily", rows, append = TRUE)
  con
}

test_that("config exposes identity-asset settings and the identity_state column", {
  expect_true("identity_state" %in% SUMMARY_COLS)
  expect_identical(SUMMARY_COLS[length(SUMMARY_COLS)], "identity_state")
  expect_equal(CRAN_ARCHIVE_REPO, "r-observatory/cran-archive")
  expect_equal(BIOC_META_REPO, "r-observatory/bioconductor-metadata")
  expect_equal(CRAN_NAMES_FLOOR, 15000L)
  expect_equal(BIOC_NAMES_FLOOR, 1500L)
})

test_that("summary anchors windows to the anchor date and computes trend", {
  con <- mk_daily(); on.exit(DBI::dbDisconnect(con))
  s <- DBI::dbGetQuery(con, summary_sql("2026-05-30"))
  dp <- s[s$package == "dplyr", ]
  expect_equal(dp$total_30d, 100L)   # only 2026-05-30 falls in the last 30 days
  expect_equal(dp$total_90d, 150L)   # both dplyr rows fall in the last 90 days
  expect_equal(dp$total_365d, 150L)
  # 2026-04-15 sits in the prior-30d window (2026-03-31..2026-04-30): trend = (100/50-1)*100
  expect_equal(dp$trend, 100.0)
  expect_equal(dp$rank_30d, 1L)
  expect_equal(dp$avg_daily_30d, round(100 / 30, 2))
})

test_that("trend is NULL when the prior-30d window has no downloads", {
  con <- mk_daily(); on.exit(DBI::dbDisconnect(con))
  s <- DBI::dbGetQuery(con, summary_sql("2026-05-30"))
  op <- s[s$package == "oldpkg", ]   # only a single recent row, nothing 30-60d prior
  expect_equal(op$total_30d, 7L)
  expect_true(is.na(op$trend))
})

test_that("repo collapses to a single value, 'mixed' only when a name spans both repos", {
  con <- mk_daily()
  DBI::dbExecute(con, "INSERT INTO r2u_downloads_daily VALUES ('dplyr','2026-05-30','bioc','jammy','amd64',1)")
  on.exit(DBI::dbDisconnect(con))
  s <- DBI::dbGetQuery(con, summary_sql("2026-05-30"))
  expect_equal(s$repo[s$package == "dplyr"], "mixed")
  expect_equal(s$repo[s$package == "oldpkg"], "bioc")
})

test_that("extract_recent_rows respects the window cutoff, extract_year_rows filters by year", {
  con <- mk_daily(); on.exit(DBI::dbDisconnect(con))
  rec <- extract_recent_rows(con, "2026-05-30", window_days = 20L)  # cutoff 2026-05-10
  expect_setequal(unique(rec$date), c("2026-05-30", "2026-05-29"))  # 2026-04-15 excluded
  yr <- extract_year_rows(con, 2026)
  expect_equal(nrow(yr), 3L)
})

test_that("apply_name_display maps known names and falls back to the lowercased token", {
  s <- data.frame(
    package = c("dplyr", "obscurelowercasepkg"), repo = "cran",
    total_30d = 1L, stringsAsFactors = FALSE)
  nm <- c("dplyr" = "dplyr", "rcpp" = "Rcpp")
  out <- apply_name_display(s, nm)
  expect_equal(out$name_display, c("dplyr", "obscurelowercasepkg"))
  expect_equal(names(out)[1:2], c("package", "name_display"))
})

test_that("apply_name_display uses Bioconductor canonical casing from the map", {
  df <- data.frame(package = c("biocgenerics", "dplyr"), total_30d = c(1L, 2L),
                   stringsAsFactors = FALSE)
  nm <- c(biocgenerics = "BiocGenerics", dplyr = "dplyr")
  out <- apply_name_display(df, nm)
  expect_equal(out$name_display[out$package == "biocgenerics"], "BiocGenerics")
})

test_that("apply_identity_state sets state from the ledger and NA when absent", {
  sm <- data.frame(package = c("mass", "deseq2", "ghostpkg"),
                   name_display = c("MASS", "DESeq2", "ghostpkg"),
                   repo = c("cran", "bioc", "cran"),
                   stringsAsFactors = FALSE)
  state_map <- c(mass = "live", deseq2 = "live")   # ghostpkg absent from the ledger
  out <- apply_identity_state(sm, state_map)
  expect_equal(out$identity_state, c("live", "live", NA_character_))
  expect_identical(names(out)[ncol(out)], "identity_state")
})
