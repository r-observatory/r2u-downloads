test_that("merge_shard_coverage carries prior entries forward and overwrites updated years", {
  prev <- list(
    "r2u-2025.db" = list(rows = 10, date_min = "2025-01-01", date_max = "2025-12-31"),
    "r2u-2026.db" = list(rows = 5,  date_min = "2026-01-01", date_max = "2026-03-31"))
  upd <- list(
    "r2u-2026.db" = list(rows = 8, date_min = "2026-01-01", date_max = "2026-05-31"))
  m <- merge_shard_coverage(prev, upd)
  expect_equal(m[["r2u-2025.db"]]$rows, 10)                 # carried forward unchanged
  expect_equal(m[["r2u-2026.db"]]$date_max, "2026-05-31")   # overwritten with new coverage
})

test_that("merge_shard_coverage tolerates a NULL prior map (cold start)", {
  m <- merge_shard_coverage(NULL, list("r2u-2026.db" = list(rows = 1)))
  expect_equal(m[["r2u-2026.db"]]$rows, 1)
})

test_that("write_manifest emits readable json and preserves nulls", {
  p <- withr::local_tempfile(fileext = ".json")
  write_manifest(p, list(tag = "v1", changed_shards = list(),
                         shards = list(), trend_example = NULL))
  back <- jsonlite::fromJSON(p, simplifyVector = FALSE)
  expect_equal(back$tag, "v1")
  expect_equal(length(back$changed_shards), 0L)
})

# --- integrity / completeness core -----------------------------------------

# Build a tiny, real summary DB on disk (canonical r2u_downloads_summary schema
# via export_summary_shard). *.db is gitignored, so tests build their own.
build_summary_db <- function(n = 3L) {
  tmp <- withr::local_tempfile(fileext = ".db", .local_envir = parent.frame())
  export_summary_shard(path = tmp, summary = data.frame(
    package        = paste0("pkg", seq_len(n)),
    name_display   = paste0("pkg", seq_len(n)),
    repo           = rep("cran", n),
    total_30d      = seq_len(n) * 10L,
    total_90d      = seq_len(n) * 30L,
    total_365d     = seq_len(n) * 100L,
    rank_30d       = seq_len(n),
    rank_90d       = seq_len(n),
    rank_365d      = seq_len(n),
    avg_daily_30d  = seq_len(n) * 1.5,
    trend          = rep(NA_real_, n),
    identity_state = rep("live", n),
    stringsAsFactors = FALSE
  ))
  tmp
}

test_that("summary_integrity_core reports filename, bytes, sha256, tables, complete", {
  db <- build_summary_db(3L)

  core <- summary_integrity_core(db, complete = TRUE)

  expect_equal(core$db_filename, basename(db))
  expect_equal(core$db_bytes, as.integer(file.size(db)))
  # sha256 is lowercase 64-char hex of the exact file bytes
  expect_match(core$db_sha256, "^[0-9a-f]{64}$")
  # tables maps every user table to its row count
  expect_equal(core$tables, list(r2u_downloads_summary = 3L))
  expect_true(core$complete)
})

test_that("summary_integrity_core sha256 matches an independent digest of the bytes", {
  skip_if_not_installed("digest")
  db <- build_summary_db(2L)

  core <- summary_integrity_core(db)
  independent <- tolower(digest::digest(file = db, algo = "sha256"))
  expect_equal(core$db_sha256, independent)
})

test_that("write_manifest merges the integrity core as top-level fields", {
  db <- build_summary_db(4L)
  core <- summary_integrity_core(db, complete = TRUE)

  tmp <- withr::local_tempfile(fileext = ".json")
  write_manifest(
    path = tmp,
    obj  = list(tag = "v20260714-000000", changed_shards = list("r2u-summary.db"),
                summary = list(source_rows_read = 1L)),
    core = core
  )

  parsed <- jsonlite::fromJSON(tmp)
  # existing fields preserved
  expect_equal(parsed$tag, "v20260714-000000")
  expect_equal(parsed$summary$source_rows_read, 1L)
  # new top-level integrity/completeness core
  expect_equal(parsed$db_filename, basename(db))
  expect_equal(parsed$db_bytes, as.integer(file.size(db)))
  expect_match(parsed$db_sha256, "^[0-9a-f]{64}$")
  expect_equal(parsed$tables$r2u_downloads_summary, 4L)
  expect_true(parsed$complete)
})

test_that("write_manifest replaces stale core keys instead of duplicating them", {
  db <- build_summary_db(2L)
  core <- summary_integrity_core(db, complete = TRUE)

  # obj already carries a prior core (as happens when out <- prev); the fresh
  # core must overwrite it, and the JSON must contain each key exactly once.
  tmp <- withr::local_tempfile(fileext = ".json")
  write_manifest(
    path = tmp,
    obj  = list(tag = "v2", db_filename = "stale.db", db_bytes = 1L,
                db_sha256 = "stale", tables = list(x = 0L), complete = FALSE),
    core = core
  )

  raw <- readLines(tmp, warn = FALSE)
  expect_equal(sum(grepl('"db_sha256"\\s*:', raw)), 1L)  # exactly one, no dup key
  parsed <- jsonlite::fromJSON(tmp)
  expect_equal(parsed$db_filename, basename(db))         # fresh core wins
  expect_equal(parsed$db_sha256, core$db_sha256)
  expect_true(parsed$complete)
})

test_that("prev_integrity_core carries the core forward, or returns NULL when absent", {
  prev_with <- list(tag = "v1", db_filename = "r2u-summary.db", db_bytes = 42L,
                    db_sha256 = "abc", tables = list(r2u_downloads_summary = 5L),
                    complete = TRUE)
  carried <- prev_integrity_core(prev_with)
  expect_equal(names(carried),
               c("db_filename", "db_bytes", "db_sha256", "tables", "complete"))
  expect_equal(carried$db_sha256, "abc")

  expect_null(prev_integrity_core(list(tag = "v1")))  # no core present
  expect_null(prev_integrity_core(NULL))
})
