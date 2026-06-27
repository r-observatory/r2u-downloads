# scripts/helpers.R — pure functions used by update.R, unit-tested in tests/testthat/.

`%||%` <- function(a, b) if (is.null(a)) b else a

# ---------------------------------------------------------------------------
# Source-file naming
# ---------------------------------------------------------------------------

#' Parse a source filename into its components.
#'
#' Names look like "r2u/r2u_r2u-2025-q1.csv.zst" (dir + file). The period token
#' is one of YYYY (annual), YYYY-qN (quarterly), or YYYY-MM (monthly).
#'
#' @param path source path (dir included or not)
#' @return list(path, host, year (int), period_type "year"|"quarter"|"month", period)
parse_period <- function(path) {
  base <- basename(path)
  m <- regmatches(base, regexec("^r2u_(r2u|rob)-(.+)\\.csv\\.zst$", base))[[1]]
  if (length(m) != 3L) stop("unrecognized source filename: ", path)
  host <- m[2]
  tok  <- m[3]
  if (grepl("^[0-9]{4}$", tok)) {
    list(path = path, host = host, year = as.integer(tok), period_type = "year", period = tok)
  } else if (grepl("^[0-9]{4}-q[1-4]$", tok)) {
    list(path = path, host = host, year = as.integer(substr(tok, 1, 4)),
         period_type = "quarter", period = substr(tok, 6, 7))
  } else if (grepl("^[0-9]{4}-[0-9]{2}$", tok)) {
    list(path = path, host = host, year = as.integer(substr(tok, 1, 4)),
         period_type = "month", period = substr(tok, 6, 7))
  } else {
    stop("unrecognized period token: ", tok)
  }
}

# ---------------------------------------------------------------------------
# Year selection
# ---------------------------------------------------------------------------

#' Which calendar years do a set of changed files touch? (sorted, unique)
affected_years <- function(changed_files) {
  if (length(changed_files) == 0) return(integer(0))
  sort(unique(vapply(changed_files, function(f) parse_period(f)$year, integer(1))))
}

#' Calendar years spanned by the rolling window ending at `anchor_date`.
window_years <- function(anchor_date, window_days = 400L) {
  anchor <- as.Date(anchor_date)
  start  <- anchor - as.integer(window_days)
  seq.int(as.integer(format(start, "%Y")), as.integer(format(anchor, "%Y")))
}
