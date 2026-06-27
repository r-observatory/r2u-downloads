test_that("affected_years dedups across hosts and periods", {
  expect_equal(
    affected_years(c(
      "r2u/r2u_r2u-2026-01.csv.zst",
      "rob/r2u_rob-2026-02.csv.zst",
      "r2u/r2u_r2u-2025-q1.csv.zst")),
    c(2025L, 2026L))
  expect_equal(affected_years(character(0)), integer(0))
})

test_that("window_years spans the 400-day window across a year boundary", {
  expect_equal(window_years(as.Date("2026-05-31"), 400L), c(2025L, 2026L))
  expect_equal(window_years(as.Date("2026-12-31"), 400L), c(2025L, 2026L))
  expect_equal(window_years(as.Date("2026-02-01"), 400L), c(2024L, 2025L, 2026L))
})
