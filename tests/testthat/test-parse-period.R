test_that("parse_period handles annual, quarterly, monthly, both hosts", {
  expect_equal(parse_period("r2u/r2u_r2u-2024.csv.zst")$year, 2024L)
  expect_equal(parse_period("r2u/r2u_r2u-2024.csv.zst")$period_type, "year")

  q <- parse_period("r2u/r2u_r2u-2025-q1.csv.zst")
  expect_equal(q$year, 2025L)
  expect_equal(q$period_type, "quarter")
  expect_equal(q$period, "q1")

  m <- parse_period("rob/r2u_rob-2026-05.csv.zst")
  expect_equal(m$host, "rob")
  expect_equal(m$year, 2026L)
  expect_equal(m$period_type, "month")
  expect_equal(m$period, "05")

  expect_error(parse_period("r2u/garbage.txt"))
})
