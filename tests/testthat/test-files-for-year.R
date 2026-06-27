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
