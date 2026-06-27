test_that("export_shard writes the daily schema and round-trips", {
  rows <- data.frame(
    package = "dplyr", date = "2026-01-05", repo = "cran",
    dist = "jammy", arch = "amd64", count = 2L, stringsAsFactors = FALSE)
  p <- withr::local_tempfile(fileext = ".db")
  export_shard(p, rows)

  con <- DBI::dbConnect(RSQLite::SQLite(), p)
  on.exit(DBI::dbDisconnect(con))
  got <- DBI::dbGetQuery(con, "SELECT * FROM r2u_downloads_daily")
  expect_equal(got$count, 2L)
  cols <- DBI::dbGetQuery(con, "PRAGMA table_info(r2u_downloads_daily)")$name
  expect_setequal(cols, c("package", "date", "repo", "dist", "arch", "count"))
})

test_that("export_shard handles zero rows (schema only)", {
  empty <- data.frame(
    package = character(0), date = character(0), repo = character(0),
    dist = character(0), arch = character(0), count = integer(0))
  p <- withr::local_tempfile(fileext = ".db")
  export_shard(p, empty)
  con <- DBI::dbConnect(RSQLite::SQLite(), p)
  on.exit(DBI::dbDisconnect(con))
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM r2u_downloads_daily")$n, 0L)
})

test_that("export_summary_shard writes the summary schema", {
  s <- data.frame(
    package = "dplyr", name_display = "dplyr", repo = "cran",
    total_30d = 10L, total_90d = 20L, total_365d = 30L,
    rank_30d = 1L, rank_90d = 1L, rank_365d = 1L,
    avg_daily_30d = 0.33, trend = NA_real_, stringsAsFactors = FALSE)
  p <- withr::local_tempfile(fileext = ".db")
  export_summary_shard(p, s)
  con <- DBI::dbConnect(RSQLite::SQLite(), p)
  on.exit(DBI::dbDisconnect(con))
  got <- DBI::dbGetQuery(con, "SELECT * FROM r2u_downloads_summary")
  expect_equal(got$name_display, "dplyr")
  expect_true(is.na(got$trend))
})
