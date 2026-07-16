# scripts/helpers.R: pure functions used by update.R, unit-tested in tests/testthat/.

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
#' hours of Jan-1, which physically live in the prior month's file (files are
#' cut at ~06:00 UTC on the 1st), are available. Callers keep only rows whose
#' content-date falls in `year`.
files_for_year <- function(all_files, year) {
  parsed <- lapply(all_files, parse_period)

  physical_months <- function(p) {
    if (p$period_type == "year") 1:12
    else if (p$period_type == "quarter") {
      q <- as.integer(substr(p$period, 2, 2)); ((q - 1) * 3 + 1):((q - 1) * 3 + 3)
    } else as.integer(p$period)
  }

  pick_for_host <- function(host) {
    fs  <- Filter(function(p) p$host == host, parsed)
    iny <- Filter(function(p) p$year == year, fs)

    # Assign each month its finest-granularity file (monthly > quarter > annual).
    month_file <- vector("list", 12L)
    for (p in Filter(function(p) p$period_type == "year", iny)) {
      for (mo in 1:12) month_file[[mo]] <- p$path
    }
    for (p in Filter(function(p) p$period_type == "quarter", iny)) {
      for (mo in physical_months(p)) month_file[[mo]] <- p$path
    }
    for (p in Filter(function(p) p$period_type == "month", iny)) {
      month_file[[physical_months(p)]] <- p$path
    }

    # A file is read only if it owns ALL the months it physically covers. If it
    # owns none, a finer file fully supersedes it -> drop. If it owns some but
    # not all, a finer file PARTIALLY overlaps it -> reading both would silently
    # double-count the shared months, so fail loudly (this requires per-file
    # month filtering that the current disjoint-granularity source never needs).
    sel <- character(0)
    for (p in iny) {
      phys  <- physical_months(p)
      owned <- sum(vapply(phys, function(mo) identical(month_file[[mo]], p$path), logical(1)))
      if (owned == 0L) next
      if (owned < length(phys)) {
        stop("overlapping source granularities for year ", year, ": ", p$path,
             " is partially superseded by a finer file (would double-count)")
      }
      sel <- c(sel, p$path)
    }

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

# ---------------------------------------------------------------------------
# Log cleaning + aggregation (DuckDB)
# ---------------------------------------------------------------------------

#' Build the DuckDB query that turns raw r2u log files into daily counts.
#'
#' Reads each file by header (schema has drifted historically, so never trust
#' column order), then:
#'   - derives the day from the authoritative `date` timestamp (ignores `day`);
#'   - re-derives the package name from `pkg` by stripping the r-cran-/r-bioc-
#'     prefix (ignores the unreliable `name` column);
#'   - derives `repo` (cran or bioc) from the pkg prefix via regexp_extract
#'     (ignores the unreliable `repo` column in the raw log);
#'   - drops junk by requiring the r-cran-/r-bioc- prefix (removes trailing-colon
#'     rows and repo=api probes) and restricting arch to (all,amd64,arm64);
#'   - unions all given files (both hosts) and counts every row, duplicates
#'     included.
#'
#' @param files character vector of local .csv.zst paths
#' @return a single SQL string returning columns package,date,repo,dist,arch,count
clean_aggregate_sql <- function(files) {
  flist <- paste(sprintf("'%s'", files), collapse = ", ")
  sprintf("
    SELECT regexp_replace(pkg, '^r-(cran|bioc)-', '') AS package,
           substr(date, 1, 10)                        AS date,
           regexp_extract(pkg, '^r-(cran|bioc)-', 1)  AS repo,
           dist, arch,
           COUNT(*)                                   AS count
      FROM read_csv([%s], header = true, all_varchar = true, union_by_name = true)
     WHERE arch IN ('all', 'amd64', 'arm64')
       AND regexp_matches(pkg, '^r-(cran|bioc)-')
     GROUP BY 1, 2, 3, 4, 5", flist)
}

# ---------------------------------------------------------------------------
# SQLite shard export
# ---------------------------------------------------------------------------

#' Write daily rows to a fresh SQLite shard (overwrite, canonical schema, VACUUM).
#'
#' @param path  output .db path
#' @param rows  data.frame(package, date, repo, dist, arch, count)
export_shard <- function(path, rows) {
  if (file.exists(path)) unlink(path)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(con, "PRAGMA journal_mode=DELETE")  # no WAL in published shards
  DBI::dbExecute(con, "
    CREATE TABLE r2u_downloads_daily (
      package TEXT    NOT NULL,
      date    TEXT    NOT NULL,
      repo    TEXT    NOT NULL,
      dist    TEXT    NOT NULL,
      arch    TEXT    NOT NULL,
      count   INTEGER NOT NULL,
      PRIMARY KEY (package, date, repo, dist, arch))")
  DBI::dbExecute(con, "CREATE INDEX idx_rdd_date ON r2u_downloads_daily(date)")

  if (nrow(rows) > 0) {
    DBI::dbBegin(con)
    DBI::dbExecute(con, "
      INSERT INTO r2u_downloads_daily (package, date, repo, dist, arch, count)
      VALUES (?, ?, ?, ?, ?, ?)",
      params = list(rows$package, rows$date, rows$repo, rows$dist, rows$arch, rows$count))
    DBI::dbCommit(con)
  }

  DBI::dbExecute(con, "VACUUM")
  invisible(NULL)
}

#' Write a minimal SQLite file containing ONLY the summary table.
#'
#' @param path     output .db path
#' @param summary  data.frame matching the r2u_downloads_summary schema
export_summary_shard <- function(path, summary) {
  if (file.exists(path)) unlink(path)
  con <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  DBI::dbExecute(con, "PRAGMA journal_mode=DELETE")
  DBI::dbExecute(con, "
    CREATE TABLE r2u_downloads_summary (
      package       TEXT PRIMARY KEY,
      name_display  TEXT,
      repo          TEXT,
      total_30d     INTEGER,
      total_90d     INTEGER,
      total_365d    INTEGER,
      rank_30d      INTEGER,
      rank_90d      INTEGER,
      rank_365d     INTEGER,
      avg_daily_30d REAL,
      trend         REAL,
      identity_state TEXT)")

  if (nrow(summary) > 0) {
    DBI::dbWriteTable(con, "r2u_downloads_summary", summary, append = TRUE)
  }

  DBI::dbExecute(con, "VACUUM")
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Windowing + summary
# ---------------------------------------------------------------------------

#' All daily rows for one calendar year (sorted), from a working SQLite store.
extract_year_rows <- function(con, year) {
  DBI::dbGetQuery(con, "
    SELECT package, date, repo, dist, arch, count
      FROM r2u_downloads_daily
     WHERE substr(date, 1, 4) = ?
     ORDER BY package, date, repo, dist, arch",
    params = list(sprintf("%04d", as.integer(year))))
}

#' The rolling N-day window of daily rows, anchored to `anchor_date` (the latest
#' data date), NOT to today, because r2u data lags ~1 month.
extract_recent_rows <- function(con, anchor_date, window_days = 400L) {
  cutoff <- format(as.Date(anchor_date) - as.integer(window_days), "%Y-%m-%d")
  DBI::dbGetQuery(con, "
    SELECT package, date, repo, dist, arch, count
      FROM r2u_downloads_daily
     WHERE date >= ?
     ORDER BY package, date, repo, dist, arch",
    params = list(cutoff))
}

#' SQLite query that builds the per-package summary over the table
#' r2u_downloads_daily, collapsing across repo/dist/arch. All windows end at
#' `anchor_date`. trend is NULL when the prior-30d total is 0.
summary_sql <- function(anchor_date) {
  a <- format(as.Date(anchor_date), "%Y-%m-%d")
  sprintf("
    WITH agg AS (
      SELECT package,
        CASE WHEN COUNT(DISTINCT repo) > 1 THEN 'mixed' ELSE MAX(repo) END AS repo,
        SUM(CASE WHEN date >= date('%s','-30 days')  THEN count ELSE 0 END) AS total_30d,
        SUM(CASE WHEN date >= date('%s','-90 days')  THEN count ELSE 0 END) AS total_90d,
        SUM(CASE WHEN date >= date('%s','-365 days') THEN count ELSE 0 END) AS total_365d,
        SUM(CASE WHEN date >= date('%s','-60 days') AND date < date('%s','-30 days')
                 THEN count ELSE 0 END) AS prev_30d
      FROM r2u_downloads_daily
      WHERE date >= date('%s','-365 days')
      GROUP BY package),
    f AS (
      SELECT package, repo, total_30d, total_90d, total_365d,
             ROUND(total_30d / 30.0, 2) AS avg_daily_30d,
             CASE WHEN prev_30d > 0
                  THEN ROUND((total_30d * 1.0 / prev_30d - 1.0) * 100.0, 2)
                  ELSE NULL END AS trend
      FROM agg)
    SELECT package, repo, total_30d, total_90d, total_365d,
           RANK() OVER (ORDER BY total_30d  DESC) AS rank_30d,
           RANK() OVER (ORDER BY total_90d  DESC) AS rank_90d,
           RANK() OVER (ORDER BY total_365d DESC) AS rank_365d,
           avg_daily_30d, trend
      FROM f", a, a, a, a, a, a)
}

#' Add the best-effort canonical `name_display` column to a summary data.frame.
#'
#' `name_map` is a named character vector (names = lowercased token, values =
#' canonical name). Unmatched packages fall back to their lowercased token.
#' `name_display` is placed second (after `package`) to match the shard schema.
apply_name_display <- function(summary_df, name_map) {
  disp <- unname(name_map[summary_df$package])
  miss <- is.na(disp)
  disp[miss] <- summary_df$package[miss]
  summary_df$name_display <- disp
  summary_df[c("package", "name_display",
               setdiff(names(summary_df), c("package", "name_display")))]
}

#' Add the `identity_state` column (live|archived) to a summary data.frame from
#' a `name_lower -> identity_state` map. A package absent from the ledger gets
#' NA (honest unknown), never a fabricated state; it is NOT dropped, because r2u
#' only builds CRAN/Bioc packages and its `repo` prefix already fixes its scope.
apply_identity_state <- function(summary_df, state_map) {
  summary_df$identity_state <- unname(state_map[summary_df$package])
  summary_df
}

# ---------------------------------------------------------------------------
# Manifest (run report + persistent state)
# ---------------------------------------------------------------------------

#' Carry forward the per-shard coverage map, overwriting entries for shards
#' rebuilt this run. `prev` may be NULL (cold start).
merge_shard_coverage <- function(prev, updates) {
  out <- prev %||% list()
  for (k in names(updates)) out[[k]] <- updates[[k]]
  out
}

#' Compute the lowercase hex SHA-256 of a file's exact on-disk bytes.
#'
#' Uses whatever the runner already provides, in preference order:
#'   1. digest  package        (if installed)
#'   2. openssl package        (if installed)
#'   3. sha256sum (coreutils)  — present on the ubuntu-latest CI runner
#'   4. shasum -a 256 (BSD)    — macOS/local fallback
#' No heavy dependency is declared: on CI (which installs only RSQLite,
#' jsonlite, testthat, DBI) the coreutils `sha256sum` path is used. If a
#' sibling pipeline already declares `digest`, that path wins automatically.
file_sha256 <- function(path) {
  if (requireNamespace("digest", quietly = TRUE)) {
    return(tolower(digest::digest(file = path, algo = "sha256")))
  }
  if (requireNamespace("openssl", quietly = TRUE)) {
    con <- file(path, open = "rb")
    on.exit(close(con), add = TRUE)
    return(tolower(as.character(openssl::sha256(con))))
  }
  sha_tool <- Sys.which("sha256sum")
  if (nzchar(sha_tool)) {
    out <- system2(sha_tool, shQuote(path), stdout = TRUE)
    return(tolower(sub("\\s.*$", "", out[1])))
  }
  shasum_tool <- Sys.which("shasum")
  if (nzchar(shasum_tool)) {
    out <- system2(shasum_tool, c("-a", "256", shQuote(path)), stdout = TRUE)
    return(tolower(sub("\\s.*$", "", out[1])))
  }
  stop("No SHA-256 backend found (need one of: digest, openssl, sha256sum, shasum)")
}

#' Build the integrity / completeness core describing a finalized SQLite file.
#'
#' Returns a named list of TOP-LEVEL manifest fields computed from the exact
#' on-disk bytes of `db_path` (call this only after the file is finalized):
#'   * db_filename — basename of the file
#'   * db_bytes    — integer byte size of the file
#'   * db_sha256   — lowercase hex sha256 of the file's exact bytes
#'   * tables      — named list mapping each user table to its row count
#'   * complete    — passed through by the caller (TRUE for a full rebuild)
#' Lets a downstream merge content-verify the asset it pulls and confirm the
#' expected tables/rows are present.
summary_integrity_core <- function(db_path, complete = TRUE) {
  stopifnot(file.exists(db_path))

  con <- DBI::dbConnect(RSQLite::SQLite(), db_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  tbl_names <- DBI::dbGetQuery(con, "
    SELECT name FROM sqlite_master
     WHERE type = 'table' AND name NOT LIKE 'sqlite_%'
     ORDER BY name")$name

  tables <- stats::setNames(
    lapply(tbl_names, function(t) {
      DBI::dbGetQuery(con, sprintf('SELECT count(*) AS n FROM "%s"', t))$n
    }),
    tbl_names
  )

  list(
    db_filename = basename(db_path),
    db_bytes    = as.integer(file.size(db_path)),
    db_sha256   = file_sha256(db_path),
    tables      = tables,
    complete    = complete
  )
}

#' Extract the integrity-core fields (db_filename, db_bytes, db_sha256, tables,
#' complete) from a PRIOR manifest object, or NULL if it carries none.
#'
#' Used to carry the core forward on a run that does NOT rebuild r2u-summary.db
#' (a changed year that falls outside the rolling window leaves the published
#' summary asset untouched), so the manifest keeps correctly describing the
#' still-current file instead of dropping its hash/size/tables.
prev_integrity_core <- function(prev) {
  keys <- c("db_filename", "db_bytes", "db_sha256", "tables", "complete")
  if (is.null(prev) || !all(keys %in% names(prev))) return(NULL)
  prev[keys]
}

#' Write the manifest object as pretty JSON, preserving nulls and empty arrays.
#'
#' `core` (optional) is a named list of TOP-LEVEL fields to merge into the
#' manifest — used to attach the integrity/completeness core built by
#' summary_integrity_core() (db_filename, db_bytes, db_sha256, tables, complete).
#' Any stale copies of those keys already on `obj` are dropped before the merge,
#' since (unlike the sibling reference) this repo's `obj` may be a prev-derived
#' full manifest that already carries them, and c() would otherwise emit
#' duplicate JSON keys.
write_manifest <- function(path, obj, core = NULL) {
  if (!is.null(core)) {
    obj <- c(obj[setdiff(names(obj), names(core))], core)  # merge as top-level fields, not nested
  }
  writeLines(
    jsonlite::toJSON(obj, auto_unbox = TRUE, pretty = TRUE, null = "null"),
    path)
}

#' Render the GitHub release body (markdown) from a manifest object.
#'
#' The release page should be self-describing (freshness, what changed this run,
#' and per-shard coverage) without making consumers open manifest.json.
write_release_notes <- function(path, manifest) {
  or_na <- function(x) {
    if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) "n/a" else as.character(x)
  }
  big <- function(x) {
    if (is.null(x) || length(x) == 0 || is.na(x)) "0" else formatC(as.numeric(x), format = "d", big.mark = ",")
  }
  ts <- function(s) if (is.null(s) || is.na(s)) "n/a" else sub("Z$", " UTC", sub("T", " ", s))

  cs <- manifest$changed_shards
  changed <- if (length(cs) == 0) "none (no upstream change since last run)" else paste(unlist(cs), collapse = ", ")
  sha <- manifest$upstream_head_sha %||% ""
  sha <- if (nzchar(sha)) substr(sha, 1, 7) else "n/a"

  lines <- c(
    "Aggregated r2u (CRAN as Ubuntu Binaries) `.deb` download counts, sourced from [`eddelbuettel/r2u-logs`](https://github.com/eddelbuettel/r2u-logs). Counts are raw, un-deduplicated apt/CI/Docker request volume, complete only through the end of the previous month. See the [README](https://github.com/r-observatory/r2u-downloads#readme) for the full caveats.",
    "",
    "This is a single rolling release. The assets below are SQLite shards: per-year archives (`r2u-YYYY.db`), a rolling 400-day window (`r2u-recent.db`), and a summary-only file (`r2u-summary.db`), alongside `manifest.json`. Each daily run replaces only the shards that changed and refreshes this page.",
    "",
    "| | |",
    "|---|---|",
    sprintf("| **Last checked** | %s |", ts(manifest$last_checked)),
    sprintf("| **Last data change** | %s |", ts(manifest$last_changed)),
    sprintf("| **Upstream** | `%s` @ `%s` |",
            or_na(manifest$upstream_repo %||% "eddelbuettel/r2u-logs"), sha),
    sprintf("| **Source rows read (last run)** | %s |", big(manifest$summary$source_rows_read)),
    sprintf("| **Changed this run** | %s |", changed),
    "",
    "## Shard coverage",
    "",
    "| Shard | Rows | From | To |",
    "|---|---:|---|---|"
  )
  shards <- manifest$shards %||% list()
  for (nm in sort(names(shards))) {
    s <- shards[[nm]]
    lines <- c(lines, sprintf("| `%s` | %s | %s | %s |",
                              nm, big(s$rows), or_na(s$date_min), or_na(s$date_max)))
  }
  lines <- c(lines, "",
    "_Fetch the rolling 400-day window:_",
    "```bash",
    "gh release download current --repo r-observatory/r2u-downloads --pattern r2u-recent.db",
    "```")

  writeLines(lines, path)
  invisible(NULL)
}
