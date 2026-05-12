db_path <- function(config = read_app_config()) {
  config$database$path
}

db_connect <- function(path = db_path()) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  DBI::dbConnect(duckdb::duckdb(), dbdir = path, read_only = FALSE)
}

db_disconnect <- function(con) {
  if (DBI::dbIsValid(con)) {
    DBI::dbDisconnect(con, shutdown = TRUE)
  }
}

db_init <- function(con = NULL) {
  owns_connection <- is.null(con)
  if (owns_connection) {
    con <- db_connect()
    on.exit(db_disconnect(con), add = TRUE)
  }

  statements <- c(
    "CREATE TABLE IF NOT EXISTS source_providers (
      provider_id TEXT PRIMARY KEY,
      provider_name TEXT NOT NULL,
      provider_type TEXT NOT NULL,
      homepage_url TEXT,
      api_url TEXT,
      description TEXT,
      active BOOLEAN NOT NULL DEFAULT TRUE,
      created_at TIMESTAMP NOT NULL DEFAULT CURRENT_TIMESTAMP
    )",
    "INSERT INTO source_providers (
      provider_id,
      provider_name,
      provider_type,
      homepage_url,
      api_url,
      description,
      active
    )
    SELECT
      'gdelt',
      'GDELT Project',
      'news_index',
      'https://www.gdeltproject.org/',
      'https://api.gdeltproject.org/api/v2/doc/doc',
      'GDELT DOC 2.0 article search API used for climate, environment, and health signal monitoring.',
      TRUE
    WHERE NOT EXISTS (
      SELECT 1 FROM source_providers WHERE provider_id = 'gdelt'
    )",
    "CREATE TABLE IF NOT EXISTS collection_runs (
      run_id TEXT PRIMARY KEY,
      source TEXT NOT NULL,
      started_at TIMESTAMP NOT NULL,
      finished_at TIMESTAMP,
      status TEXT NOT NULL,
      query_count INTEGER NOT NULL DEFAULT 0,
      raw_item_count INTEGER NOT NULL DEFAULT 0,
      signal_count INTEGER NOT NULL DEFAULT 0,
      message TEXT
    )",
    "CREATE TABLE IF NOT EXISTS raw_items (
      raw_item_id TEXT PRIMARY KEY,
      run_id TEXT NOT NULL,
      source TEXT NOT NULL,
      query_id TEXT NOT NULL,
      source_item_id TEXT,
      source_url TEXT,
      observed_at TIMESTAMP,
      collected_at TIMESTAMP NOT NULL,
      payload_hash TEXT NOT NULL,
      payload_json TEXT NOT NULL
    )",
    "CREATE UNIQUE INDEX IF NOT EXISTS idx_raw_items_payload_hash
      ON raw_items(payload_hash)",
    "CREATE TABLE IF NOT EXISTS signals (
      signal_id TEXT PRIMARY KEY,
      raw_item_id TEXT NOT NULL,
      run_id TEXT NOT NULL,
      source TEXT NOT NULL,
      query_id TEXT NOT NULL,
      query_label TEXT,
      topic TEXT,
      rumour_family TEXT,
      title TEXT,
      text TEXT,
      url TEXT,
      domain TEXT,
      language TEXT,
      country TEXT,
      mentioned_country TEXT,
      mentioned_state_code TEXT,
      mentioned_state_name TEXT,
      mentioned_municipality TEXT,
      spatial_confidence TEXT,
      published_at TIMESTAMP,
      collected_at TIMESTAMP NOT NULL,
      score_volume DOUBLE DEFAULT 1,
      score_novelty DOUBLE,
      score_risk DOUBLE,
      review_status TEXT DEFAULT 'unreviewed'
    )",
    "CREATE UNIQUE INDEX IF NOT EXISTS idx_signals_raw_item_id
      ON signals(raw_item_id)",
    "CREATE TABLE IF NOT EXISTS reviews (
      review_id TEXT PRIMARY KEY,
      signal_id TEXT NOT NULL,
      reviewer TEXT,
      status TEXT NOT NULL,
      notes TEXT,
      reviewed_at TIMESTAMP NOT NULL
    )",
    "ALTER TABLE signals ADD COLUMN IF NOT EXISTS mentioned_country TEXT",
    "ALTER TABLE signals ADD COLUMN IF NOT EXISTS mentioned_state_code TEXT",
    "ALTER TABLE signals ADD COLUMN IF NOT EXISTS mentioned_state_name TEXT",
    "ALTER TABLE signals ADD COLUMN IF NOT EXISTS mentioned_municipality TEXT",
    "ALTER TABLE signals ADD COLUMN IF NOT EXISTS spatial_confidence TEXT",
    "CREATE OR REPLACE VIEW signal_activity AS
     SELECT
       source,
       query_id,
       query_label,
       topic,
       rumour_family,
       MIN(COALESCE(published_at, collected_at)) AS first_seen_at,
       MAX(COALESCE(published_at, collected_at)) AS last_seen_at,
       COUNT(*) AS signal_count,
       COUNT(DISTINCT domain) AS domain_count,
       DATE_DIFF('day',
         CAST(MIN(COALESCE(published_at, collected_at)) AS DATE),
         CAST(MAX(COALESCE(published_at, collected_at)) AS DATE)
       ) + 1 AS days_active
     FROM signals
     GROUP BY source, query_id, query_label, topic, rumour_family",
    "CREATE OR REPLACE VIEW rumour_activity AS SELECT * FROM signal_activity",
    "CREATE OR REPLACE VIEW signal_spatial_coverage AS
     SELECT
       source,
       query_id,
       query_label,
       topic,
       rumour_family,
       mentioned_country,
       mentioned_state_code,
       mentioned_state_name,
       mentioned_municipality,
       spatial_confidence,
       MIN(COALESCE(published_at, collected_at)) AS first_seen_at,
       MAX(COALESCE(published_at, collected_at)) AS last_seen_at,
       COUNT(*) AS signal_count,
       COUNT(DISTINCT domain) AS domain_count
     FROM signals
     GROUP BY
       source,
       query_id,
       query_label,
       topic,
       rumour_family,
       mentioned_country,
       mentioned_state_code,
       mentioned_state_name,
       mentioned_municipality,
       spatial_confidence",
    "CREATE OR REPLACE VIEW rumour_spatial_coverage AS SELECT * FROM signal_spatial_coverage",
    "CREATE OR REPLACE VIEW signal_daily_counts AS
     SELECT
       CAST(COALESCE(published_at, collected_at) AS DATE) AS signal_date,
       source,
       query_id,
       query_label,
       topic,
       rumour_family,
       mentioned_country,
       mentioned_state_code,
       mentioned_state_name,
       COUNT(*) AS signal_count,
       COUNT(DISTINCT domain) AS domain_count,
       MIN(COALESCE(published_at, collected_at)) AS first_seen_at,
       MAX(COALESCE(published_at, collected_at)) AS last_seen_at
     FROM signals
     GROUP BY
       signal_date,
       source,
       query_id,
       query_label,
       topic,
       rumour_family,
       mentioned_country,
       mentioned_state_code,
       mentioned_state_name",
    "CREATE OR REPLACE VIEW rumour_daily_counts AS SELECT * FROM signal_daily_counts",
    "CREATE OR REPLACE VIEW signal_surge_candidates AS
     WITH daily AS (
       SELECT * FROM signal_daily_counts
     )
     SELECT
       d.*,
       (
         SELECT AVG(p.signal_count)
         FROM daily p
         WHERE p.source = d.source
           AND p.query_id = d.query_id
           AND COALESCE(p.mentioned_state_code, '') = COALESCE(d.mentioned_state_code, '')
           AND p.signal_date BETWEEN d.signal_date - INTERVAL 7 DAY AND d.signal_date - INTERVAL 1 DAY
       ) AS prior_7day_avg,
       (
         SELECT MAX(p.signal_count)
         FROM daily p
         WHERE p.source = d.source
           AND p.query_id = d.query_id
           AND COALESCE(p.mentioned_state_code, '') = COALESCE(d.mentioned_state_code, '')
           AND p.signal_date BETWEEN d.signal_date - INTERVAL 7 DAY AND d.signal_date - INTERVAL 1 DAY
       ) AS prior_7day_max,
       (
         SELECT SUM(p.signal_count)
         FROM daily p
         WHERE p.source = d.source
           AND p.query_id = d.query_id
           AND COALESCE(p.mentioned_state_code, '') = COALESCE(d.mentioned_state_code, '')
           AND p.signal_date BETWEEN d.signal_date - INTERVAL 30 DAY AND d.signal_date - INTERVAL 1 DAY
       ) AS prior_30day_count,
       DATE_DIFF(
         'day',
         (
           SELECT MAX(p.signal_date)
           FROM daily p
           WHERE p.source = d.source
             AND p.query_id = d.query_id
             AND COALESCE(p.mentioned_state_code, '') = COALESCE(d.mentioned_state_code, '')
             AND p.signal_date < d.signal_date
         ),
         d.signal_date
       ) AS days_since_previous_signal
     FROM daily d",
    "CREATE OR REPLACE VIEW rumour_surge_candidates AS SELECT * FROM signal_surge_candidates",
    "CREATE OR REPLACE VIEW source_domain_summary AS
     SELECT
       source AS provider_id,
       domain,
       country AS source_country,
       language,
       COUNT(*) AS signal_count,
       MIN(COALESCE(published_at, collected_at)) AS first_seen_at,
       MAX(COALESCE(published_at, collected_at)) AS last_seen_at,
       COUNT(DISTINCT query_id) AS query_count,
       COUNT(DISTINCT topic) AS topic_count
     FROM signals
     WHERE domain IS NOT NULL
     GROUP BY source, domain, country, language",
    "CREATE OR REPLACE VIEW source_provider_summary AS
     SELECT
       p.provider_id,
       p.provider_name,
       p.provider_type,
       p.homepage_url,
       p.api_url,
       p.description,
       p.active,
       COUNT(DISTINCT s.signal_id) AS signal_count,
       COUNT(DISTINCT s.domain) AS domain_count,
       MIN(COALESCE(s.published_at, s.collected_at)) AS first_seen_at,
       MAX(COALESCE(s.published_at, s.collected_at)) AS last_seen_at
     FROM source_providers p
     LEFT JOIN signals s ON s.source = p.provider_id
     GROUP BY
       p.provider_id,
       p.provider_name,
       p.provider_type,
       p.homepage_url,
       p.api_url,
       p.description,
       p.active"
  )

  purrr::walk(statements, ~ DBI::dbExecute(con, .x))
  invisible(TRUE)
}

db_new_id <- function(prefix) {
  stamp <- format(Sys.time(), "%Y%m%d%H%M%OS6")
  rand <- paste(sample(c(0:9, letters), 8, replace = TRUE), collapse = "")
  paste(prefix, stamp, rand, sep = "_")
}

db_insert_new_rows <- function(con, table, rows, key) {
  if (nrow(rows) == 0) {
    return(0L)
  }

  temp_table <- paste0("tmp_", table, "_", digest::digest(Sys.time()))
  DBI::dbWriteTable(con, temp_table, rows, temporary = TRUE)
  on.exit(DBI::dbExecute(con, paste("DROP TABLE IF EXISTS", temp_table)), add = TRUE)

  cols <- DBI::dbListFields(con, temp_table)
  cols_sql <- paste(cols, collapse = ", ")
  select_sql <- paste(paste0("t.", cols), collapse = ", ")

  sql <- sprintf(
    "INSERT INTO %s (%s)
     SELECT %s
     FROM %s t
     WHERE NOT EXISTS (
       SELECT 1 FROM %s existing WHERE existing.%s = t.%s
     )",
    table, cols_sql, select_sql, temp_table, table, key, key
  )

  DBI::dbExecute(con, sql)
}

db_finish_run <- function(con, run_id, status, raw_item_count, signal_count, message = NA_character_) {
  DBI::dbExecute(
    con,
    "UPDATE collection_runs
     SET finished_at = ?, status = ?, raw_item_count = ?, signal_count = ?, message = ?
     WHERE run_id = ?",
    params = list(Sys.time(), status, raw_item_count, signal_count, message, run_id)
  )
}

db_clear_collected_data <- function(con = NULL) {
  owns_connection <- is.null(con)
  if (owns_connection) {
    con <- db_connect()
    on.exit(db_disconnect(con), add = TRUE)
  }
  db_init(con)

  tables <- c("reviews", "signals", "raw_items", "collection_runs")
  purrr::walk(tables, ~ DBI::dbExecute(con, paste("DELETE FROM", .x)))
  invisible(TRUE)
}
