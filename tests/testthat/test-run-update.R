test_that("cold-start run_update builds year shards, recent, summary, manifest", {
  skip_if_not_installed("duckdb")
  out <- withr::local_tempdir()

  curr <- list(
    "r2u/r2u_r2u-2026-01.csv.zst" = list(sha = "a", size = 1),
    "rob/r2u_rob-2026-01.csv.zst" = list(sha = "b", size = 1),
    "r2u/r2u_r2u-2025-12.csv.zst" = list(sha = "c", size = 1),
    "rob/r2u_rob-2025-12.csv.zst" = list(sha = "d", size = 1))

  io <- list(
    release_download = function(pattern, dir) 1L,                       # no prior release
    contents         = function() curr,
    head_sha         = function() "deadbeef",
    fetch_sources    = function(paths, dir)
      stats::setNames(vapply(paths, function(p) fixture_path(p), character(1)), paths),
    name_map         = function() c("dplyr" = "dplyr", "biocgenerics" = "BiocGenerics"),
    now              = function() as.POSIXct("2026-06-01 06:00:00", tz = "UTC"))

  res <- run_update(io, out)

  for (f in c("r2u-2026.db", "r2u-2025.db", "r2u-recent.db", "r2u-summary.db", "manifest.json")) {
    expect_true(file.exists(file.path(out, f)), info = f)
  }

  # The 2026-01-01 boundary row physically lives in the 2025-12 file but must
  # land in the 2026 shard, and NOT in the 2025 shard.
  c26 <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "r2u-2026.db"))
  on.exit(DBI::dbDisconnect(c26), add = TRUE)
  expect_equal(
    DBI::dbGetQuery(c26, "SELECT SUM(count) n FROM r2u_downloads_daily WHERE date='2026-01-01'")$n, 1)

  c25 <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "r2u-2025.db"))
  on.exit(DBI::dbDisconnect(c25), add = TRUE)
  expect_equal(
    DBI::dbGetQuery(c25, "SELECT COUNT(*) n FROM r2u_downloads_daily WHERE date='2026-01-01'")$n, 0L)
  expect_equal(
    DBI::dbGetQuery(c25, "SELECT COUNT(*) n FROM r2u_downloads_daily WHERE date='2025-12-31'")$n, 1L)

  # canonical name_display flows into the summary
  cs <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "r2u-summary.db"))
  on.exit(DBI::dbDisconnect(cs), add = TRUE)
  disp <- DBI::dbGetQuery(cs, "SELECT name_display FROM r2u_downloads_summary WHERE package='biocgenerics'")$name_display
  expect_equal(disp, "BiocGenerics")

  man <- jsonlite::fromJSON(file.path(out, "manifest.json"), simplifyVector = FALSE)
  expect_true("r2u-2026.db" %in% unlist(man$changed_shards))
  expect_true("r2u-recent.db" %in% unlist(man$changed_shards))
  expect_equal(man$upstream_head_sha, "deadbeef")
})

test_that("heartbeat run (no source change) writes manifest only", {
  out <- withr::local_tempdir()
  prev <- list(
    source_files = list("r2u/x.csv.zst" = list(sha = "1")),
    shards = list(), last_changed = "2026-05-01T00:00:00Z")
  writeLines(jsonlite::toJSON(prev, auto_unbox = TRUE), file.path(out, "manifest.json"))

  io <- list(
    release_download = function(pattern, dir) 0L,                       # manifest present
    contents         = function() list("r2u/x.csv.zst" = list(sha = "1")),  # unchanged
    head_sha         = function() "same",
    fetch_sources    = function(paths, dir) stop("must not fetch on a heartbeat"),
    name_map         = function() character(0),
    now              = function() as.POSIXct("2026-06-02 06:00:00", tz = "UTC"))

  res <- run_update(io, out)
  expect_length(res$changed_shards, 0)

  man <- jsonlite::fromJSON(file.path(out, "manifest.json"), simplifyVector = FALSE)
  expect_equal(man$last_changed, "2026-05-01T00:00:00Z")   # untouched
  expect_equal(man$last_checked, "2026-06-02T06:00:00Z")   # bumped
  expect_equal(length(man$changed_shards), 0L)
})
