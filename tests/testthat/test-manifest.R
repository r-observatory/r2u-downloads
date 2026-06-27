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
