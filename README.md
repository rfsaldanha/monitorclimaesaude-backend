# monitorclimaesaude-backend

R-first backend for monitoring media signals in Brazil related to climate,
environment, and health.

The first data source is GDELT. Raw source items are kept for auditability and
normalized into DuckDB tables for API access by a future dashboard.

## Stack

- R
- DuckDB
- `targets`
- `plumber`
- `httr2`

## First Run

Install the R dependencies listed in `DESCRIPTION`, then initialize the database:

```r
source("R/api_helpers.R")
source_project_r()
db_init()
```

Run the GDELT collection pipeline:

```r
targets::tar_make()
```

Or run the collector directly:

```r
source("R/api_helpers.R")
source_project_r()
run_gdelt_collection()
```

Backfill derived spatial fields after schema changes:

```r
backfill_signal_spatial_fields()
```

Clear collected monitoring data while keeping schema and source-provider metadata:

```r
db_clear_collected_data()
```

For a smaller test run:

```r
run_gdelt_collection(query_ids = c(
  "disease_arbovirus_climate_brazil",
  "surge_flood_waterborne_brazil"
))
```

Start the API:

```r
plumber::pr("plumber.R") |>
  plumber::pr_run(port = 8000)
```

Useful endpoints:

- `GET /health`
- `GET /runs`
- `GET /sources/providers`
- `GET /sources/domains`
- `GET /signals`
- `GET /signals/activity`
- `GET /signals/spatial`
- `GET /signals/timeseries`
- `GET /signals/surges`
- `POST /collect/gdelt`

Legacy `/rumours/*` aliases are currently kept for compatibility, but new
dashboard work should use `/signals/*`.

GDELT queries live in `inst/queries/gdelt.yml`. The taxonomy is organized into:

- climate-sensitive disease mentions
- disease outbreak and surge signals
- climate-related event signals
- information disorder, distrust, and claim signals
