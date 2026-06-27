# Auto-sourced by testthat before tests run. Sources the pipeline code so tests
# can call the helpers and the orchestrator directly.
#
# During test_dir()/test_check() the working directory is the test directory
# (tests/testthat), so the repo root is two levels up.
.r2u_root <- normalizePath(file.path(getwd(), "..", ".."))

source(file.path(.r2u_root, "scripts", "helpers.R"))

# update.R is added later in the build; source it once it exists.
.r2u_update <- file.path(.r2u_root, "scripts", "update.R")
if (file.exists(.r2u_update)) source(.r2u_update)

# Absolute path to a fixture file, e.g. fixture_path("r2u", "r2u_r2u-2026-01.csv.zst").
fixture_path <- function(...) {
  file.path(.r2u_root, "tests", "testthat", "fixtures", ...)
}
