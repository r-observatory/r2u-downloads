all_files <- c(
  "r2u/r2u_r2u-2024.csv.zst",
  "r2u/r2u_r2u-2025-q1.csv.zst", "r2u/r2u_r2u-2025-q2.csv.zst",
  "r2u/r2u_r2u-2025-07.csv.zst", "r2u/r2u_r2u-2025-12.csv.zst",
  "r2u/r2u_r2u-2026-01.csv.zst",
  "rob/r2u_rob-2025-q1.csv.zst", "rob/r2u_rob-2025-12.csv.zst",
  "rob/r2u_rob-2026-01.csv.zst")

test_that("year build pulls that year's files plus predecessor December, both hosts", {
  f <- files_for_year(all_files, 2026)
  expect_true("r2u/r2u_r2u-2026-01.csv.zst" %in% f)
  expect_true("r2u/r2u_r2u-2025-12.csv.zst" %in% f)   # predecessor Dec for the Jan-1 boundary
  expect_true("rob/r2u_rob-2026-01.csv.zst" %in% f)
  expect_true("rob/r2u_rob-2025-12.csv.zst" %in% f)
  expect_false("r2u/r2u_r2u-2025-q1.csv.zst" %in% f)  # unrelated 2025 quarter not pulled for 2026
})

test_that("monthly supersedes quarter supersedes annual for the same months", {
  f <- files_for_year(all_files, 2025)
  expect_true("r2u/r2u_r2u-2025-07.csv.zst" %in% f)   # monthly July wins
  expect_true("r2u/r2u_r2u-2025-q1.csv.zst" %in% f)   # Q1 still needed (no monthly for Jan-Mar)
  expect_true("r2u/r2u_r2u-2024.csv.zst" %in% f)      # predecessor (2024 annual) for boundary
})

test_that("a fully-superseded coarse file is dropped (no double counting)", {
  fs <- c(sprintf("r2u/r2u_r2u-2025-%02d.csv.zst", 1:12), "r2u/r2u_r2u-2025.csv.zst")
  f <- files_for_year(fs, 2025)
  expect_false("r2u/r2u_r2u-2025.csv.zst" %in% f)     # annual fully covered by 12 monthlies
  expect_true("r2u/r2u_r2u-2025-06.csv.zst" %in% f)
})

test_that("a partially-overlapping finer file is a hard error (would silently double-count)", {
  # Q1 covers Jan-Mar; a stray monthly-02 overlaps it -> reading both double-counts Feb.
  fs <- c("r2u/r2u_r2u-2025-q1.csv.zst", "r2u/r2u_r2u-2025-02.csv.zst")
  expect_error(files_for_year(fs, 2025), "overlap")
})
