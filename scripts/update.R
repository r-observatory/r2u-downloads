#!/usr/bin/env Rscript
# scripts/update.R — change-gated r2u-downloads producer.
#
# Every run: pull the prior manifest, diff the source repo's per-file blob SHAs,
# and either heartbeat (no change -> bump last_checked) or rebuild the affected
# year shards from source, reassemble the rolling-window recent + summary shards,
# and rewrite the manifest. This script only writes into out/; the workflow
# uploads. The orchestration core, run_update(io, out_dir), takes an injectable
# `io` so it can be unit-tested offline against fixtures.

options(timeout = 600)

suppressPackageStartupMessages({
  library(DBI)
  library(RSQLite)
  library(jsonlite)
})

# Resolve this file's directory whether invoked via Rscript or source(), then
# load the pure helpers (skipped if already sourced, e.g. by the test harness).
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
.script_dir <- {
  tf <- .this_file()
  if (!is.na(tf)) dirname(tf) else "scripts"
}
if (!exists("clean_aggregate_sql", mode = "function")) {
  source(file.path(.script_dir, "helpers.R"))
}

SOURCE_REPO   <- "eddelbuettel/r2u-logs"
PUBLISH_REPO  <- "r-observatory/r2u-downloads"
RECENT_WINDOW <- 400L
SUMMARY_COLS  <- c("package", "name_display", "repo",
                   "total_30d", "total_90d", "total_365d",
                   "rank_30d", "rank_90d", "rank_365d",
                   "avg_daily_30d", "trend")

iso <- function(t) format(t, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

# ---------------------------------------------------------------------------
# Working-store + aggregation helpers (impure: DuckDB / SQLite IO)
# ---------------------------------------------------------------------------

init_daily <- function(con) {
  DBI::dbExecute(con, "CREATE TABLE IF NOT EXISTS r2u_downloads_daily
    (package TEXT, date TEXT, repo TEXT, dist TEXT, arch TEXT, count INTEGER)")
}

load_rows <- function(con, rows) {
  if (nrow(rows) == 0) return(invisible())
  DBI::dbWriteTable(con, "r2u_downloads_daily",
    rows[c("package", "date", "repo", "dist", "arch", "count")], append = TRUE)
}

load_shard <- function(con, path) {
  sc <- DBI::dbConnect(RSQLite::SQLite(), path)
  on.exit(DBI::dbDisconnect(sc), add = TRUE)
  rows <- DBI::dbGetQuery(sc,
    "SELECT package, date, repo, dist, arch, count FROM r2u_downloads_daily")
  load_rows(con, rows)
}

# Run the cleaning/aggregation query over local .csv.zst files via DuckDB.
# Returns a data.frame(package,date,repo,dist,arch,count) with attr "source_rows".
aggregate_files <- function(local_files) {
  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  df <- DBI::dbGetQuery(con, clean_aggregate_sql(local_files))
  flist <- paste(sprintf("'%s'", local_files), collapse = ", ")
  raw <- DBI::dbGetQuery(con, sprintf(
    "SELECT COUNT(*) AS n FROM read_csv([%s], header=true, all_varchar=true, union_by_name=true)",
    flist))$n
  attr(df, "source_rows") <- as.integer(raw)
  df
}

coverage <- function(rows) {
  if (nrow(rows) == 0) return(list(rows = 0L, date_min = NA, date_max = NA))
  list(rows = nrow(rows), date_min = min(rows$date), date_max = max(rows$date))
}

prev_shard_max_date <- function(prev_shards) {
  dmax <- unlist(lapply(prev_shards, function(s) s$date_max), use.names = FALSE)
  dmax <- dmax[!is.na(dmax) & nzchar(dmax)]
  if (length(dmax) == 0) return(as.Date(NA))
  max(as.Date(dmax))
}

# Embed the summary table inside r2u-recent.db (so it is a self-contained shard).
embed_summary <- function(recent_path, summary_df) {
  con <- DBI::dbConnect(RSQLite::SQLite(), recent_path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  DBI::dbExecute(con, "DROP TABLE IF EXISTS r2u_downloads_summary")
  DBI::dbExecute(con, "CREATE TABLE r2u_downloads_summary (
    package TEXT PRIMARY KEY, name_display TEXT, repo TEXT,
    total_30d INTEGER, total_90d INTEGER, total_365d INTEGER,
    rank_30d INTEGER, rank_90d INTEGER, rank_365d INTEGER,
    avg_daily_30d REAL, trend REAL)")
  if (nrow(summary_df) > 0) {
    DBI::dbWriteTable(con, "r2u_downloads_summary", summary_df, append = TRUE)
  }
}

# ---------------------------------------------------------------------------
# Orchestrator
# ---------------------------------------------------------------------------

#' Run one update cycle.
#'
#' @param io  list of IO functions:
#'   release_exists() -> logical (does the rolling `current` release exist?)
#'   release_download(pattern, dir) -> int status (0 = downloaded, non-zero = absent/failed)
#'   contents() -> named list path -> list(sha, size)  (current source files)
#'   head_sha() -> character(1)
#'   fetch_sources(paths, dir) -> named character (source path -> local file)
#'   name_map() -> named character (lowercased token -> canonical)
#'   now() -> POSIXct
#' @param out_dir directory to write shards + manifest into
#' @return list(changed_shards, manifest)
run_update <- function(io, out_dir) {
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
  manifest_path <- file.path(out_dir, "manifest.json")

  # 1. Prior manifest (persistent state). Only treat a MISSING manifest as a
  #    cold start when the release genuinely does not exist yet; if the release
  #    exists but the download fails, abort rather than silently full-rebuilding.
  rel_exists <- io$release_exists()
  if (rel_exists) {
    st <- io$release_download("manifest.json", out_dir)
    if (!identical(st, 0L) || !file.exists(manifest_path)) {
      stop("release 'current' exists but manifest.json could not be downloaded; ",
           "aborting so the prior state stays authoritative")
    }
  }
  prev <- if (file.exists(manifest_path)) {
    jsonlite::fromJSON(manifest_path, simplifyVector = FALSE)
  } else {
    list()
  }
  prev_sources <- prev$source_files %||% list()
  prev_shards  <- prev$shards %||% list()

  # 2. Current upstream state.
  curr <- io$contents()
  head <- io$head_sha()
  now  <- io$now()

  # 3-4. What changed, and which years it touches.
  changed <- diff_source_state(prev_sources, curr)
  ay <- affected_years(changed)

  # 5. Heartbeat: nothing changed.
  if (length(ay) == 0) {
    out <- prev
    out$last_checked      <- iso(now)
    out$upstream_head_sha <- head
    out$source_files      <- curr
    out$changed_shards    <- list()
    out$summary           <- list(affected_years = list(), source_rows_read = 0L)
    write_manifest(manifest_path, out)
    return(list(changed_shards = character(0), manifest = out))
  }

  curr_paths <- names(curr)
  src_dir <- file.path(out_dir, "_src")
  dir.create(src_dir, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(src_dir, recursive = TRUE), add = TRUE)

  con_work <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
  on.exit(DBI::dbDisconnect(con_work), add = TRUE)
  init_daily(con_work)

  changed_shards   <- character(0)
  shard_updates    <- list()
  source_rows_read <- 0L
  rebuilt_max      <- as.Date(character(0))

  # 6-7. Rebuild each affected year from source (filter rows to that year).
  for (yr in ay) {
    shard <- sprintf("r2u-%04d.db", yr)
    fpaths <- files_for_year(curr_paths, yr)

    # Year fully deleted upstream: publish an empty shard so the release no
    # longer serves stale rows for it.
    if (length(fpaths) == 0) {
      empty <- data.frame(package = character(0), date = character(0),
                          repo = character(0), dist = character(0),
                          arch = character(0), count = integer(0))
      export_shard(file.path(out_dir, shard), empty)
      changed_shards <- c(changed_shards, shard)
      shard_updates[[shard]] <- coverage(empty)
      next
    }

    local <- io$fetch_sources(fpaths, src_dir)
    agg <- aggregate_files(unname(local))
    source_rows_read <- source_rows_read + (attr(agg, "source_rows") %||% 0L)
    yr_rows <- agg[substr(agg$date, 1, 4) == sprintf("%04d", yr), , drop = FALSE]

    export_shard(file.path(out_dir, shard), yr_rows)
    changed_shards <- c(changed_shards, shard)
    shard_updates[[shard]] <- coverage(yr_rows)
    if (nrow(yr_rows) > 0) {
      rebuilt_max <- c(rebuilt_max, as.Date(max(yr_rows$date)))
      load_rows(con_work, yr_rows)
    }
  }

  # 7. Anchor = latest data date (from rebuilt rows or prior shard coverage).
  candidates <- c(rebuilt_max, prev_shard_max_date(prev_shards))
  candidates <- candidates[!is.na(candidates)]
  anchor <- if (length(candidates) > 0) max(candidates) else as.Date(NA)

  # 8-9. Rebuild recent + summary only when a window year actually changed.
  wy <- if (!is.na(anchor)) window_years(anchor, RECENT_WINDOW) else integer(0)
  if (!is.na(anchor) && length(intersect(ay, wy)) > 0) {
    for (w in setdiff(wy, ay)) {
      shard <- sprintf("r2u-%04d.db", w)
      st <- io$release_download(shard, out_dir)
      sp <- file.path(out_dir, shard)
      if (file.exists(sp)) {
        load_shard(con_work, sp)
      } else if (!is.null(prev_shards[[shard]])) {
        # The prior manifest says this shard exists, so a missing file here is a
        # failed download, not a genuinely-absent shard. Abort: otherwise we
        # would publish a recent.db/summary.db missing a whole window-year and
        # clobber the correct assets.
        stop("window-year shard ", shard, " is expected on the release ",
             "(status ", st, ") but could not be downloaded; aborting")
      }
      # else: genuinely absent (e.g. a year with no data on a fresh release) -> skip
    }

    recent_rows <- extract_recent_rows(con_work, anchor, RECENT_WINDOW)
    recent_path <- file.path(out_dir, "r2u-recent.db")
    export_shard(recent_path, recent_rows)

    summary_df <- DBI::dbGetQuery(con_work, summary_sql(format(anchor)))
    summary_df <- apply_name_display(summary_df, io$name_map())
    summary_df <- summary_df[SUMMARY_COLS]
    export_summary_shard(file.path(out_dir, "r2u-summary.db"), summary_df)
    embed_summary(recent_path, summary_df)

    changed_shards <- c(changed_shards, "r2u-recent.db", "r2u-summary.db")
    shard_updates[["r2u-recent.db"]] <- coverage(recent_rows)
  }

  # 10-11. Manifest (run report + persistent state).
  out <- list(
    tag               = sprintf("v%s", format(now, "%Y%m%d-%H%M%S", tz = "UTC")),
    generated_at      = iso(now),
    last_checked      = iso(now),
    last_changed      = iso(now),
    upstream_repo     = SOURCE_REPO,
    upstream_head_sha = head,
    source_files      = curr,
    changed_shards    = as.list(changed_shards),
    shards            = merge_shard_coverage(prev_shards, shard_updates),
    summary           = list(affected_years = as.list(ay),
                             source_rows_read = source_rows_read)
  )
  write_manifest(manifest_path, out)
  list(changed_shards = changed_shards, manifest = out)
}

# ---------------------------------------------------------------------------
# Default (production) IO
# ---------------------------------------------------------------------------

with_retry <- function(expr, tries = 3L, wait = 3) {
  for (i in seq_len(tries)) {
    val <- tryCatch(force(expr), error = function(e) e)
    if (!inherits(val, "error")) return(val)
    if (i < tries) Sys.sleep(wait * i)
  }
  stop(val)
}

# Run gh and capture stdout, RAISING on a non-zero exit (system2(stdout=TRUE)
# otherwise only attaches a "status" attribute, so a bare call would never let
# with_retry see a failure). Use for the read calls that gate the whole run.
gh_capture <- function(args) {
  out <- suppressWarnings(system2("gh", args, stdout = TRUE, stderr = TRUE))
  st  <- attr(out, "status") %||% 0L
  if (!identical(as.integer(st), 0L)) {
    stop("gh ", paste(args[1:2], collapse = " "), " failed (status ", st, "): ",
         paste(utils::head(out, 5), collapse = " "))
  }
  out
}

# Best-effort canonical-case map, CRAN only (lowercased token -> canonical).
# Bioconductor names are not included, so r-bioc- packages fall back to their
# lowercased token in name_display — acceptable since name_display is a non-key,
# display-only field (see the spec's name-casing decision). Lowercase collisions
# (which CRAN forbids) resolve to the lexicographically first canonical name.
build_name_map <- function(cran_repo = "https://cloud.r-project.org") {
  canon <- tryCatch(rownames(available.packages(repos = cran_repo)),
                    error = function(e) character(0))
  canon <- sort(unique(canon))
  canon <- canon[!duplicated(tolower(canon))]
  stats::setNames(canon, tolower(canon))
}

default_io <- function() {
  list(
    release_exists = function() {
      st <- suppressWarnings(system2("gh",
        c("release", "view", "current", "--repo", PUBLISH_REPO),
        stdout = FALSE, stderr = FALSE))
      identical(as.integer(st), 0L)
    },
    release_download = function(pattern, dir) {
      # Retry transient failures; a genuinely-absent asset stays non-zero and the
      # caller decides whether that is acceptable (cold release) or fatal.
      for (i in seq_len(3L)) {
        st <- suppressWarnings(system2("gh",
          c("release", "download", "current", "--repo", PUBLISH_REPO,
            "--pattern", pattern, "--dir", dir, "--clobber"),
          stdout = TRUE, stderr = TRUE))
        code <- as.integer(attr(st, "status") %||% 0L)
        if (identical(code, 0L)) return(0L)
        if (i < 3L) Sys.sleep(3 * i)
      }
      code
    },
    contents = function() {
      fetch_dir <- function(d) {
        txt <- with_retry(gh_capture(
          c("api", sprintf("repos/%s/contents/%s", SOURCE_REPO, d), "--paginate")))
        js <- jsonlite::fromJSON(paste(txt, collapse = "\n"), simplifyVector = FALSE)
        out <- list()
        for (it in js) {
          if (identical(it$type, "file") && grepl("\\.csv\\.zst$", it$name)) {
            out[[paste0(d, "/", it$name)]] <- list(sha = it$sha, size = it$size)
          }
        }
        out
      }
      c(fetch_dir("r2u"), fetch_dir("rob"))
    },
    head_sha = function() {
      trimws(paste(with_retry(gh_capture(
        c("api", sprintf("repos/%s/commits/master", SOURCE_REPO), "--jq", ".sha"))),
        collapse = ""))
    },
    fetch_sources = function(paths, dir) {
      stats::setNames(vapply(paths, function(p) {
        url  <- sprintf("https://raw.githubusercontent.com/%s/master/%s", SOURCE_REPO, p)
        dest <- file.path(dir, gsub("/", "_", p))
        with_retry(utils::download.file(url, dest, mode = "wb", quiet = TRUE))
        dest
      }, character(1)), paths)
    },
    name_map = function() build_name_map(),
    now = function() Sys.time()
  )
}

# ---------------------------------------------------------------------------
# CLI entry point
# ---------------------------------------------------------------------------

if (sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  out_dir <- if (length(args) >= 1) args[1] else "out"
  res <- run_update(default_io(), out_dir)
  cat("Changed shards:", if (length(res$changed_shards))
        paste(res$changed_shards, collapse = ", ") else "(none)", "\n")
}
