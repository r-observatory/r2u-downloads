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

# Write a cran_names_all/bioc_names_all fixture ledger table at `path` (schema
# matching the real identity assets: name_lower, canonical_name, identity_state,
# first_seen, last_seen), populated from `names` (a character vector of
# canonical package names). Always creates the table, even for an empty
# vector, since robservatory::load_identity requires both tables to exist.
.write_names_db <- function(path, table, names, identity_state = "live") {
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, sprintf(
    "CREATE TABLE %s (name_lower TEXT PRIMARY KEY, canonical_name TEXT,
       identity_state TEXT, first_seen TEXT, last_seen TEXT)", table))
  if (length(names) > 0L) {
    DBI::dbWriteTable(con, table, data.frame(
      name_lower = tolower(names), canonical_name = names,
      identity_state = identity_state, first_seen = "x", last_seen = "y",
      stringsAsFactors = FALSE), append = TRUE)
  }
}

# Build a fixture cran_names_all/bioc_names_all ledger pair (in a tempdir that
# outlives this call, since it must survive until run_update's identity_dbs()
# call reads it later in the same test) for use as an io$identity_dbs()
# stand-in: identity_dbs = function() make_identity_dbs(cran = ..., bioc = ...).
#
# Deliberately does NOT self-clean via withr::defer(envir = parent.frame()):
# this is normally called from inside a throwaway `identity_dbs = function()
# make_identity_dbs(...)` closure, so parent.frame() there is that closure's
# own call frame, which exits (and would fire the deferred unlink) as soon as
# make_identity_dbs() returns -- i.e. immediately, before run_update ever gets
# to read the files. Leftover tempdirs are reclaimed by the OS/session temp
# cleanup; callers that want in-test cleanup can unlink() the returned paths'
# dirname() themselves.
make_identity_dbs <- function(cran = character(0), bioc = character(0)) {
  dir <- tempfile("identity-dbs-")
  dir.create(dir)
  cran_db <- file.path(dir, "cran-archive.db")
  bioc_db <- file.path(dir, "bioc-meta.db")
  .write_names_db(cran_db, "cran_names_all", cran)
  .write_names_db(bioc_db, "bioc_names_all", bioc)
  list(cran = cran_db, bioc = bioc_db)
}
