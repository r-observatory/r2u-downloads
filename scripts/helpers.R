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

#' Source files needed to rebuild a single calendar year.
#'
#' For each host (r2u, rob): pick, per month, the finest-granularity file
#' (monthly > quarterly > annual) so overlapping periods never double-count;
#' then add the trailing predecessor file (December of year-1) so the early
#' hours of Jan-1 — which physically live in the prior month's file (files are
#' cut at ~06:00 UTC on the 1st) — are available. Callers keep only rows whose
#' content-date falls in `year`.
files_for_year <- function(all_files, year) {
  parsed <- lapply(all_files, parse_period)

  pick_for_host <- function(host) {
    fs  <- Filter(function(p) p$host == host, parsed)
    iny <- Filter(function(p) p$year == year, fs)

    month_file <- vector("list", 12L)
    for (p in Filter(function(p) p$period_type == "year", iny)) {
      for (mo in 1:12) month_file[[mo]] <- p$path
    }
    for (p in Filter(function(p) p$period_type == "quarter", iny)) {
      q <- as.integer(substr(p$period, 2, 2))
      for (mo in ((q - 1) * 3 + 1):((q - 1) * 3 + 3)) month_file[[mo]] <- p$path
    }
    for (p in Filter(function(p) p$period_type == "month", iny)) {
      month_file[[as.integer(p$period)]] <- p$path
    }
    sel <- unique(unlist(month_file))

    prev <- Filter(function(p) p$year == year - 1L, fs)
    prev_dec <- NULL
    for (p in Filter(function(p) p$period_type == "year", prev)) prev_dec <- p$path
    for (p in Filter(function(p) p$period_type == "quarter", prev)) {
      if (as.integer(substr(p$period, 2, 2)) == 4L) prev_dec <- p$path
    }
    for (p in Filter(function(p) p$period_type == "month", prev)) {
      if (as.integer(p$period) == 12L) prev_dec <- p$path
    }
    c(sel, prev_dec)
  }

  out <- unique(c(pick_for_host("r2u"), pick_for_host("rob")))
  out[!is.na(out) & nzchar(out)]
}

# ---------------------------------------------------------------------------
# Change detection
# ---------------------------------------------------------------------------

#' Which source files changed since the last run? (added, modified, deleted)
#'
#' Maps are named lists keyed by path; each value is list(sha=, size=). An empty
#' `prev_map` (cold start) reports every current file as changed.
diff_source_state <- function(prev_map, curr_map) {
  added   <- setdiff(names(curr_map), names(prev_map))
  deleted <- setdiff(names(prev_map), names(curr_map))
  common  <- intersect(names(prev_map), names(curr_map))
  modified <- common[vapply(common,
    function(n) !identical(prev_map[[n]]$sha, curr_map[[n]]$sha), logical(1))]
  sort(unique(c(added, modified, deleted)))
}
