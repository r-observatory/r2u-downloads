test_that(".make_name_map: CRAN spelling wins on case-only collision", {
  # Simulate repo-ordered input: CRAN packages first, then Bioc.
  # "ZOO" comes from CRAN, "zoo" from Bioc. The first occurrence per lowercased
  # token must win, so the map entry for "zoo" must carry the CRAN spelling "ZOO".
  m <- .make_name_map(c("ZOO", "zoo"))
  expect_equal(m[["zoo"]], "ZOO")
})

test_that(".make_name_map: returns named vector with lowercased keys", {
  m <- .make_name_map(c("dplyr", "BiocGenerics"))
  expect_named(m, c("dplyr", "biocgenerics"), ignore.order = FALSE)
  expect_equal(unname(m), c("dplyr", "BiocGenerics"))
})

test_that(".make_name_map: empty input returns empty named character", {
  m <- .make_name_map(character(0))
  expect_length(m, 0)
  expect_type(m, "character")
})
