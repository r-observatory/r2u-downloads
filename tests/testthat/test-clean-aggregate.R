test_that("clean_aggregate drops junk, derives day, re-derives name, unions hosts, counts dups", {
  skip_if_not_installed("duckdb")
  files <- c(
    fixture_path("r2u", "r2u_r2u-2026-01.csv.zst"),
    fixture_path("rob", "r2u_rob-2026-01.csv.zst"))
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  res <- DBI::dbGetQuery(con, clean_aggregate_sql(files))

  # junk gone: islasso row had arch="amd64.deb:"; the api probe had repo="api"
  expect_false("islasso" %in% res$package)
  expect_false("api" %in% res$repo)

  # name re-derived from pkg (lowercased), NOT the bogus 'arrow' name column
  expect_true("biocgenerics" %in% res$package)
  expect_false("arrow" %in% res$package)
  expect_true("dplyr" %in% res$package)

  # dplyr on 2026-01-05: r2u jammy/amd64 (x2 dup) + r2u noble/arm64 (x1) + rob focal/amd64 (x1)
  d <- res[res$package == "dplyr" & res$date == "2026-01-05", ]
  expect_equal(sum(d$count), 4L)                                 # host union + all dims
  expect_equal(d$count[d$dist == "jammy" & d$arch == "amd64"], 2L)  # duplicates counted
  expect_equal(d$count[d$dist == "noble" & d$arch == "arm64"], 1L)
  expect_equal(d$count[d$dist == "focal" & d$arch == "amd64"], 1L)  # from rob

  # bioc row tagged repo='bioc'
  expect_equal(res$repo[res$package == "biocgenerics"], "bioc")
})
