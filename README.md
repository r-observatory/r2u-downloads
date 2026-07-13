# r2u Downloads

Daily download counts for [r2u](https://eddelbuettel.github.io/r2u/), Dirk
Eddelbuettel's "CRAN as Ubuntu Binaries" service, which serves CRAN and
Bioconductor packages as Ubuntu `.deb` binaries installed via `apt`/`bspm`.
Counts are aggregated from the raw access logs published at
[`eddelbuettel/r2u-logs`](https://github.com/eddelbuettel/r2u-logs) and broken
down by Ubuntu distribution (`dist`) and architecture (`arch`). Data is
published as a set of SQLite shard files attached to a single rolling GitHub
release tag (`current`).

> [!IMPORTANT]
> **What an r2u "download" means, and what it does not.** Each count is a raw
> HTTP fetch of a `.deb` from the r2u apt repository: a binary install via
> `apt`/`bspm`, a direct `apt install`, or a Docker/CI layer build. These logs
> carry **no IP, user-agent, or status**, so there is **no bot filtering and no
> unique-user de-duplication**, so counts are inflated by CI pipelines and Docker
> image builds that re-pull the same packages. They measure a **different
> population on a different mirror** than the
> [cranlogs](https://cranlogs.r-pkg.org/) numbers in
> [`r-observatory/cran-downloads`](https://github.com/r-observatory/cran-downloads)
> and are **not comparable in magnitude**. Data is **complete only through the
> end of the previous month** (the upstream logs are published ~monthly).

## Data Access

All shards live as assets on the
[`current` release](https://github.com/r-observatory/r2u-downloads/releases/tag/current).
Each run uploads only the shards that changed; the rest remain unchanged.

### Recent data (last 400 days)

For most use cases this is the only file you need. It contains the rolling
400-day window of `r2u_downloads_daily` plus the full `r2u_downloads_summary`
table.

```bash
gh release download current \
  --repo r-observatory/r2u-downloads \
  --pattern "r2u-recent.db"
```

```r
url <- "https://github.com/r-observatory/r2u-downloads/releases/download/current/r2u-recent.db"
download.file(url, "r2u-recent.db", mode = "wb")

library(RSQLite)
con <- dbConnect(SQLite(), "r2u-recent.db")

# Last 30 days of dplyr fetches, summed across dist/arch
dbGetQuery(con, "
  SELECT date, SUM(count) AS count
  FROM r2u_downloads_daily
  WHERE package = 'dplyr'
  GROUP BY date
  ORDER BY date DESC LIMIT 30
")

# Top packages by 30-day downloads
dbGetQuery(con, "
  SELECT package, name_display, total_30d, rank_30d
  FROM r2u_downloads_summary
  ORDER BY rank_30d LIMIT 20
")

dbDisconnect(con)
```

```python
import urllib.request, sqlite3
url = "https://github.com/r-observatory/r2u-downloads/releases/download/current/r2u-recent.db"
urllib.request.urlretrieve(url, "r2u-recent.db")

con = sqlite3.connect("r2u-recent.db")
for row in con.execute("""
    SELECT package, total_30d, rank_30d
    FROM r2u_downloads_summary
    ORDER BY rank_30d LIMIT 10"""):
    print(row)
con.close()
```

### Per-year archives

Each calendar year has its own shard (history begins September 2022):

```bash
gh release download current \
  --repo r-observatory/r2u-downloads \
  --pattern "r2u-2024.db"
```

### Full history (all years)

```bash
gh release download current \
  --repo r-observatory/r2u-downloads \
  --pattern "r2u-*.db"
```

To query across years, ATTACH the shards or UNION them (the `package` key is the
lowercased token, identical across shards, so unions are safe):

```r
library(RSQLite)
con <- dbConnect(SQLite(), ":memory:")
for (yr in 2022:2026) {
  shard <- sprintf("r2u-%04d.db", yr)
  if (file.exists(shard)) dbExecute(con, sprintf("ATTACH '%s' AS y%d", shard, yr))
}
```

### Summary only

For top-package lists, ranks, and trends with the smallest download:

```bash
gh release download current \
  --repo r-observatory/r2u-downloads \
  --pattern "r2u-summary.db"
```

### Manifest

`manifest.json` lists which shards changed in the most recent run, the upstream
commit observed, and freshness timestamps (`last_checked`, `last_changed`). It is
useful for downstream consumers and freshness dashboards.

```bash
gh release download current --pattern manifest.json --repo r-observatory/r2u-downloads
cat manifest.json
```

## Example Queries

### Daily downloads for a package, broken down by dist + arch

```sql
SELECT date, dist, arch, count
  FROM r2u_downloads_daily
 WHERE package = 'rcpp'
 ORDER BY date DESC, dist, arch
 LIMIT 50;
```

### Top packages by monthly downloads

```sql
SELECT package, name_display, total_30d, rank_30d, trend
  FROM r2u_downloads_summary
 ORDER BY rank_30d LIMIT 50;
```

### CRAN vs Bioconductor split

```sql
SELECT repo, SUM(count) AS downloads
  FROM r2u_downloads_daily
 GROUP BY repo;
```

### arm64 adoption over time (noble only; data starts ~March 2025)

```sql
SELECT date, SUM(CASE WHEN arch = 'arm64' THEN count ELSE 0 END) AS arm64,
             SUM(count) AS total
  FROM r2u_downloads_daily
 GROUP BY date
 ORDER BY date;
```

## Schema

### `r2u_downloads_daily`

Daily download counts per package, broken down by repo, distribution, and
architecture. Present in `r2u-recent.db` (last 400 days) and in each
`r2u-YYYY.db` archive. Host (the main `r2u` mirror and the smaller `rob` server)
is summed together.

| Column | Type | Description |
|---|---|---|
| `package` | TEXT | Lowercased package token (PK part 1) |
| `date` | TEXT | Date in `YYYY-MM-DD` (PK part 2) |
| `repo` | TEXT | `cran` or `bioc` (PK part 3) |
| `dist` | TEXT | Ubuntu codename: `focal`, `jammy`, `noble` (PK part 4) |
| `arch` | TEXT | `all`, `amd64`, or `arm64` (PK part 5) |
| `count` | INTEGER | Number of `.deb` fetches that day for that combination |

### `r2u_downloads_summary`

Aggregated statistics per package, collapsed across repo/dist/arch, rebuilt each
run. Present in `r2u-recent.db` and `r2u-summary.db`. **Windows are anchored to
the latest available data date** (not "today"), because the source lags ~1
month.

| Column | Type | Description |
|---|---|---|
| `package` | TEXT | Lowercased token (PK) |
| `name_display` | TEXT | Best-effort canonical CRAN/Bioc case (falls back to the lowercased token) |
| `repo` | TEXT | `cran`, `bioc`, or `mixed` (a name present under both) |
| `total_30d` / `total_90d` / `total_365d` | INTEGER | Downloads in the trailing window |
| `rank_30d` / `rank_90d` / `rank_365d` | INTEGER | Rank by the corresponding total |
| `avg_daily_30d` | REAL | Average daily downloads over 30 days |
| `trend` | REAL | % change: last 30 days vs prior 30 days (`NULL` when the prior window is empty) |

## How it works

A daily GitHub Actions job compares the per-file blob SHAs of
[`eddelbuettel/r2u-logs`](https://github.com/eddelbuettel/r2u-logs) against the
last run (recorded in `manifest.json`). When nothing changed it just refreshes
`last_checked`. When a month's file is added or corrected, it rebuilds only the
affected year shard(s): it fetches the `.csv.zst` logs, aggregates them with
DuckDB, drops malformed/probe rows, re-derives package names, and unions
both hosts, then reassembles the rolling `r2u-recent.db` and summary.

## Attribution

Download logs are sourced from
[`eddelbuettel/r2u-logs`](https://github.com/eddelbuettel/r2u-logs) by Dirk
Eddelbuettel, the author of [r2u](https://github.com/eddelbuettel/r2u). This
repository provides only the aggregation pipeline and published snapshots;
please credit the upstream r2u project when using these numbers.

## License

The pipeline code in this repository is proprietary. Copyright (c) 2026 HJJB, LLC. All rights reserved; see [LICENSE](LICENSE). The underlying download data originates from the r2u
service and its logs; please respect the upstream project's terms when
redistributing.

## Feedback

Found a bug, a wrong number, or a missing package? Report it at [r-observatory/feedback](https://github.com/r-observatory/feedback/issues/new/choose). All feedback about R Observatory, the site, the data, and the pipelines, is tracked in one place.
