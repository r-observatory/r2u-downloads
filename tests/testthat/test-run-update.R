test_that("cold-start run_update builds year shards, recent, summary, manifest", {
  skip_if_not_installed("duckdb")
  out <- withr::local_tempdir()

  curr <- list(
    "r2u/r2u_r2u-2026-01.csv.zst" = list(sha = "a", size = 1),
    "rob/r2u_rob-2026-01.csv.zst" = list(sha = "b", size = 1),
    "r2u/r2u_r2u-2025-12.csv.zst" = list(sha = "c", size = 1),
    "rob/r2u_rob-2025-12.csv.zst" = list(sha = "d", size = 1))

  io <- list(
    release_exists   = function() FALSE,                                # no prior release
    release_download = function(pattern, dir) 1L,
    contents         = function() curr,
    head_sha         = function() "deadbeef",
    fetch_sources    = function(paths, dir)
      stats::setNames(vapply(paths, function(p) fixture_path(p), character(1)), paths),
    identity_dbs     = function() make_identity_dbs(cran = "dplyr", bioc = "BiocGenerics"),
    now              = function() as.POSIXct("2026-06-01 06:00:00", tz = "UTC"))

  res <- run_update(io, out, live_floor = 1L, bioc_floor = 1L)

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

  # canonical name_display + identity_state flow into the summary
  cs <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "r2u-summary.db"))
  on.exit(DBI::dbDisconnect(cs), add = TRUE)
  bg <- DBI::dbGetQuery(cs, "SELECT name_display, identity_state FROM r2u_downloads_summary WHERE package='biocgenerics'")
  expect_equal(bg$name_display, "BiocGenerics")
  expect_equal(bg$identity_state, "live")

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
    release_exists   = function() TRUE,                                 # release exists
    release_download = function(pattern, dir) 0L,                       # manifest present
    contents         = function() list("r2u/x.csv.zst" = list(sha = "1")),  # unchanged
    head_sha         = function() "same",
    fetch_sources    = function(paths, dir) stop("must not fetch on a heartbeat"),
    identity_dbs     = function() stop("must not resolve identity on a heartbeat"),
    now              = function() as.POSIXct("2026-06-02 06:00:00", tz = "UTC"))

  res <- run_update(io, out)
  expect_length(res$changed_shards, 0)

  man <- jsonlite::fromJSON(file.path(out, "manifest.json"), simplifyVector = FALSE)
  expect_equal(man$last_changed, "2026-05-01T00:00:00Z")   # untouched
  expect_equal(man$last_checked, "2026-06-02T06:00:00Z")   # bumped
  expect_equal(length(man$changed_shards), 0L)
})

test_that("a failed window-year shard download aborts (never publishes truncated recent/summary)", {
  skip_if_not_installed("duckdb")
  out <- withr::local_tempdir()
  prev <- list(
    source_files = list(
      "r2u/r2u_r2u-2026-01.csv.zst" = list(sha = "OLD"),
      "rob/r2u_rob-2026-01.csv.zst" = list(sha = "b"),
      "r2u/r2u_r2u-2025-12.csv.zst" = list(sha = "c"),
      "rob/r2u_rob-2025-12.csv.zst" = list(sha = "d")),
    shards = list(
      "r2u-2025.db" = list(rows = 1, date_min = "2025-12-31", date_max = "2025-12-31"),
      "r2u-2026.db" = list(rows = 5, date_min = "2026-01-01", date_max = "2026-05-31")),
    last_changed = "2026-05-01T00:00:00Z")
  writeLines(jsonlite::toJSON(prev, auto_unbox = TRUE), file.path(out, "manifest.json"))

  curr <- list(
    "r2u/r2u_r2u-2026-01.csv.zst" = list(sha = "NEW"),   # 2026 changed
    "rob/r2u_rob-2026-01.csv.zst" = list(sha = "b"),
    "r2u/r2u_r2u-2025-12.csv.zst" = list(sha = "c"),     # 2025 unchanged
    "rob/r2u_rob-2025-12.csv.zst" = list(sha = "d"))

  io <- list(
    release_exists   = function() TRUE,
    release_download = function(pattern, dir) if (pattern == "manifest.json") 0L else 1L,
    contents         = function() curr,
    head_sha         = function() "x",
    fetch_sources    = function(paths, dir)
      stats::setNames(vapply(paths, function(p) fixture_path(p), character(1)), paths),
    identity_dbs     = function() stop("must not resolve identity when the window-year download aborts first"),
    now              = function() as.POSIXct("2026-06-01 06:00:00", tz = "UTC"))

  # 2025 is a window year that prior state says exists, but its download fails:
  expect_error(run_update(io, out), "window-year shard")
})

test_that("force_full = TRUE forces a full rebuild even when upstream is unchanged", {
  skip_if_not_installed("duckdb")
  out <- withr::local_tempdir()

  curr <- list(
    "r2u/r2u_r2u-2026-01.csv.zst" = list(sha = "a", size = 1),
    "rob/r2u_rob-2026-01.csv.zst" = list(sha = "b", size = 1),
    "r2u/r2u_r2u-2025-12.csv.zst" = list(sha = "c", size = 1),
    "rob/r2u_rob-2025-12.csv.zst" = list(sha = "d", size = 1))

  # Prior manifest carries the SAME SHAs so a normal run would be a heartbeat.
  prev <- list(
    source_files = curr,
    shards       = list(),
    last_changed = "2026-05-01T00:00:00Z")
  writeLines(jsonlite::toJSON(prev, auto_unbox = TRUE), file.path(out, "manifest.json"))

  io <- list(
    release_exists   = function() TRUE,
    release_download = function(pattern, dir) 0L,   # manifest pre-written above
    contents         = function() curr,
    head_sha         = function() "deadbeef",
    fetch_sources    = function(paths, dir)
      stats::setNames(vapply(paths, function(p) fixture_path(p), character(1)), paths),
    identity_dbs     = function() make_identity_dbs(cran = "dplyr"),
    now              = function() as.POSIXct("2026-06-01 06:00:00", tz = "UTC"))

  res <- run_update(io, out, force_full = TRUE, live_floor = 1L, bioc_floor = 0L)

  expect_gt(length(res$changed_shards), 0)
  expect_true("r2u-2026.db" %in% res$changed_shards)
  expect_true("r2u-2025.db" %in% res$changed_shards)
  expect_true("r2u-recent.db" %in% res$changed_shards)
  expect_true("r2u-summary.db" %in% res$changed_shards)
})

test_that("a fully-deleted upstream year publishes an empty shard, leaving recent/summary alone", {
  out <- withr::local_tempdir()
  prev <- list(
    source_files = list(
      "r2u/r2u_r2u-2023.csv.zst"    = list(sha = "z"),   # will be deleted
      "r2u/r2u_r2u-2026-01.csv.zst" = list(sha = "a")),
    shards = list(
      "r2u-2023.db" = list(rows = 1, date_min = "2023-01-01", date_max = "2023-12-31"),
      "r2u-2026.db" = list(rows = 5, date_min = "2026-01-01", date_max = "2026-05-31")),
    last_changed = "2026-05-01T00:00:00Z")
  writeLines(jsonlite::toJSON(prev, auto_unbox = TRUE), file.path(out, "manifest.json"))

  curr <- list("r2u/r2u_r2u-2026-01.csv.zst" = list(sha = "a"))  # 2023 gone, 2026 unchanged

  io <- list(
    release_exists   = function() TRUE,
    release_download = function(pattern, dir) if (pattern == "manifest.json") 0L else 1L,
    contents         = function() curr,
    head_sha         = function() "x",
    fetch_sources    = function(paths, dir) stop("no sources to fetch for a deleted year"),
    identity_dbs     = function() stop("must not resolve identity when no window-year changed"),
    now              = function() as.POSIXct("2026-06-01 06:00:00", tz = "UTC"))

  res <- run_update(io, out)
  expect_true(file.exists(file.path(out, "r2u-2023.db")))
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "r2u-2023.db"))
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  expect_equal(DBI::dbGetQuery(con, "SELECT COUNT(*) n FROM r2u_downloads_daily")$n, 0L)
  expect_equal(res$changed_shards, "r2u-2023.db")          # 2023 out of window -> recent/summary untouched
})

test_that("run_update enriches from the identity ledger without dropping any row", {
  skip_if_not_installed("duckdb")
  out <- withr::local_tempdir()

  curr <- list(
    "r2u/r2u_r2u-2026-01.csv.zst" = list(sha = "a", size = 1),
    "rob/r2u_rob-2026-01.csv.zst" = list(sha = "b", size = 1),
    "r2u/r2u_r2u-2025-12.csv.zst" = list(sha = "c", size = 1),
    "rob/r2u_rob-2025-12.csv.zst" = list(sha = "d", size = 1))

  # The bioc ledger fixture is deliberately left empty, so "biocgenerics" (a
  # real package the source data produces) stands in for a token absent from
  # the ledger, while "dplyr" is present and gets fully resolved.
  io <- list(
    release_exists   = function() FALSE,
    release_download = function(pattern, dir) 1L,
    contents         = function() curr,
    head_sha         = function() "deadbeef",
    fetch_sources    = function(paths, dir)
      stats::setNames(vapply(paths, function(p) fixture_path(p), character(1)), paths),
    identity_dbs     = function() make_identity_dbs(cran = "dplyr"),
    now              = function() as.POSIXct("2026-06-01 06:00:00", tz = "UTC"))

  res <- run_update(io, out, live_floor = 1L, bioc_floor = 0L)

  cs <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "r2u-summary.db"))
  on.exit(DBI::dbDisconnect(cs), add = TRUE)
  summ <- DBI::dbGetQuery(cs,
    "SELECT package, name_display, repo, identity_state FROM r2u_downloads_summary ORDER BY package")

  # Nothing dropped: every package the source data produced is still published.
  expect_setequal(summ$package, c("dplyr", "biocgenerics"))

  ghost <- summ[summ$package == "biocgenerics", ]
  expect_equal(ghost$name_display, "biocgenerics")   # token fallback: absent from the ledger
  expect_true(is.na(ghost$identity_state))           # honest unknown, never fabricated
  expect_equal(ghost$repo, "bioc")                   # repo untouched by the enrich step

  known <- summ[summ$package == "dplyr", ]
  expect_equal(known$name_display, "dplyr")
  expect_equal(known$identity_state, "live")
  expect_equal(known$repo, "cran")
})

test_that("run_update degrades honestly when the identity ledger is unreachable", {
  skip_if_not_installed("duckdb")
  out <- withr::local_tempdir()

  curr <- list(
    "r2u/r2u_r2u-2026-01.csv.zst" = list(sha = "a", size = 1),
    "rob/r2u_rob-2026-01.csv.zst" = list(sha = "b", size = 1),
    "r2u/r2u_r2u-2025-12.csv.zst" = list(sha = "c", size = 1),
    "rob/r2u_rob-2025-12.csv.zst" = list(sha = "d", size = 1))

  io <- list(
    release_exists   = function() FALSE,
    release_download = function(pattern, dir) 1L,
    contents         = function() curr,
    head_sha         = function() "deadbeef",
    fetch_sources    = function(paths, dir)
      stats::setNames(vapply(paths, function(p) fixture_path(p), character(1)), paths),
    identity_dbs     = function() stop("identity assets unreachable"),
    now              = function() as.POSIXct("2026-06-01 06:00:00", tz = "UTC"))

  # The run must still succeed (never abort just because the ledger is down).
  res <- run_update(io, out, live_floor = 1L, bioc_floor = 1L)
  expect_true(file.exists(file.path(out, "r2u-summary.db")))

  cs <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "r2u-summary.db"))
  on.exit(DBI::dbDisconnect(cs), add = TRUE)
  summ <- DBI::dbGetQuery(cs, "SELECT package, name_display, identity_state FROM r2u_downloads_summary")

  expect_setequal(summ$package, c("dplyr", "biocgenerics"))
  expect_equal(summ$name_display, summ$package)   # token fallback for every row
  expect_true(all(is.na(summ$identity_state)))    # honest unknown, never fabricated
})

test_that("run_update degrades honestly when the identity size gate fails", {
  skip_if_not_installed("duckdb")
  out <- withr::local_tempdir()

  curr <- list(
    "r2u/r2u_r2u-2026-01.csv.zst" = list(sha = "a", size = 1),
    "rob/r2u_rob-2026-01.csv.zst" = list(sha = "b", size = 1),
    "r2u/r2u_r2u-2025-12.csv.zst" = list(sha = "c", size = 1),
    "rob/r2u_rob-2025-12.csv.zst" = list(sha = "d", size = 1))

  # Ledger fixtures are present and valid, but live_floor is set absurdly high
  # so check_size() rejects them -> same degrade path as an unreachable ledger.
  io <- list(
    release_exists   = function() FALSE,
    release_download = function(pattern, dir) 1L,
    contents         = function() curr,
    head_sha         = function() "deadbeef",
    fetch_sources    = function(paths, dir)
      stats::setNames(vapply(paths, function(p) fixture_path(p), character(1)), paths),
    identity_dbs     = function() make_identity_dbs(cran = "dplyr", bioc = "BiocGenerics"),
    now              = function() as.POSIXct("2026-06-01 06:00:00", tz = "UTC"))

  res <- run_update(io, out, live_floor = 999999L, bioc_floor = 1L)
  expect_true(file.exists(file.path(out, "r2u-summary.db")))

  cs <- DBI::dbConnect(RSQLite::SQLite(), file.path(out, "r2u-summary.db"))
  on.exit(DBI::dbDisconnect(cs), add = TRUE)
  summ <- DBI::dbGetQuery(cs, "SELECT package, name_display, identity_state FROM r2u_downloads_summary")

  expect_setequal(summ$package, c("dplyr", "biocgenerics"))
  expect_equal(summ$name_display, summ$package)   # token fallback for every row
  expect_true(all(is.na(summ$identity_state)))    # honest unknown, never fabricated
})
