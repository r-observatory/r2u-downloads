prev <- list(
  "r2u/a.csv.zst"   = list(sha = "1"),
  "r2u/b.csv.zst"   = list(sha = "2"),
  "r2u/old.csv.zst" = list(sha = "9"))
curr <- list(
  "r2u/a.csv.zst" = list(sha = "1"),
  "r2u/b.csv.zst" = list(sha = "2new"),
  "r2u/c.csv.zst" = list(sha = "3"))

test_that("diff detects added, modified, and deleted files", {
  expect_equal(
    diff_source_state(prev, curr),
    sort(c("r2u/b.csv.zst", "r2u/c.csv.zst", "r2u/old.csv.zst")))
})

test_that("cold start (empty prev) flags every current file", {
  expect_equal(diff_source_state(list(), curr), sort(names(curr)))
})

test_that("no changes yields nothing", {
  expect_equal(diff_source_state(curr, curr), character(0))
})
