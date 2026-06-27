sample_manifest <- function(changed = c("r2u-2026.db", "r2u-recent.db")) {
  list(
    last_checked = "2026-06-27T04:31:37Z",
    last_changed = "2026-06-27T04:31:27Z",
    upstream_repo = "eddelbuettel/r2u-logs",
    upstream_head_sha = "67a7a71dcb30aa31827beb3bc50c56ae65c5e6c7",
    changed_shards = as.list(changed),
    shards = list(
      "r2u-2026.db"   = list(rows = 2282733, date_min = "2026-01-01", date_max = "2026-06-01"),
      "r2u-recent.db" = list(rows = 5290111, date_min = "2025-04-27", date_max = "2026-06-01")),
    summary = list(affected_years = list(2026), source_rows_read = 115484344))
}

test_that("release notes render freshness, changed shards, and a coverage table", {
  p <- withr::local_tempfile(fileext = ".md")
  write_release_notes(p, sample_manifest())
  md <- paste(readLines(p), collapse = "\n")

  expect_match(md, "Last checked.*2026-06-27 04:31:37 UTC")
  expect_match(md, "67a7a71", fixed = TRUE)              # short upstream sha
  expect_match(md, "115,484,344", fixed = TRUE)          # formatted source rows
  expect_match(md, "`r2u-recent.db` | 5,290,111", fixed = TRUE)  # coverage row, formatted
  expect_match(md, "2025-04-27", fixed = TRUE)
  expect_match(md, "single rolling release", fixed = TRUE)  # describes the release itself
  expect_false(startsWith(md, "#"))                      # no redundant H1 title
  expect_false(grepl("—", md))                      # no em dashes
})

test_that("a heartbeat (no changed shards) says so", {
  p <- withr::local_tempfile(fileext = ".md")
  write_release_notes(p, sample_manifest(changed = character(0)))
  md <- paste(readLines(p), collapse = "\n")
  expect_match(md, "none \\(no upstream change")
})
