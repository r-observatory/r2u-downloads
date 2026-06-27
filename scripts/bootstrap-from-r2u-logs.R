#!/usr/bin/env Rscript
# scripts/bootstrap-from-r2u-logs.R — one-shot full build from the entire
# eddelbuettel/r2u-logs history.
#
# update.R is self-bootstrapping (an empty release => every source file is
# "new" => full build), so this is just an explicit entry point for the initial
# load or a forced full rebuild. It runs run_update() against an out dir that
# has no prior manifest.
#
# Usage:
#   Rscript scripts/bootstrap-from-r2u-logs.R [out_dir]
#
# Optional smoke filter (cheap end-to-end check against the live source):
#   R2U_ONLY_YEARS=2022 Rscript scripts/bootstrap-from-r2u-logs.R tmp_smoke/
# restricts the build to the given comma-separated years.

options(timeout = 600)

.this_file <- function() {
  for (i in rev(seq_len(sys.nframe()))) {
    of <- sys.frame(i)$ofile
    if (!is.null(of) && nzchar(of)) return(normalizePath(of))
  }
  a <- commandArgs(FALSE)
  f <- sub("^--file=", "", grep("^--file=", a, value = TRUE))
  if (length(f) == 1L && nzchar(f)) return(normalizePath(f))
  NA_character_
}
.script_dir <- { tf <- .this_file(); if (!is.na(tf)) dirname(tf) else "scripts" }
source(file.path(.script_dir, "update.R"))

args    <- commandArgs(trailingOnly = TRUE)
out_dir <- if (length(args) >= 1) args[1] else "out"

# Force a cold start: ignore any prior manifest/shards on the release.
unlink(file.path(out_dir, "manifest.json"))
io <- default_io()
io$release_exists <- function() FALSE  # treat as a fresh release -> full rebuild

# Optional year filter for a cheap live smoke.
only <- Sys.getenv("R2U_ONLY_YEARS", "")
if (nzchar(only)) {
  keep <- as.integer(strsplit(only, ",")[[1]])
  base_contents <- io$contents
  io$contents <- function() {
    cc <- base_contents()
    cc[vapply(names(cc), function(p) parse_period(p)$year %in% keep, logical(1))]
  }
  cat("Smoke mode: restricting to years", paste(keep, collapse = ", "), "\n")
}

res <- run_update(io, out_dir)
cat("Bootstrap complete. Changed shards:",
    if (length(res$changed_shards)) paste(res$changed_shards, collapse = ", ") else "(none)",
    "\n")
