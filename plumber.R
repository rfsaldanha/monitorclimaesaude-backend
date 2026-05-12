source("R/api_helpers.R")

#* Health check
#* @get /health
function() {
  con <- api_connect()
  on.exit(db_disconnect(con), add = TRUE)

  list(
    status = "ok",
    database = db_path(),
    checked_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  )
}

#* List recent GDELT collection runs
#* @param limit Maximum number of runs to return
#* @get /runs
function(limit = "") {
  con <- api_connect()
  on.exit(db_disconnect(con), add = TRUE)

  limit <- api_limit(limit)
  DBI::dbGetQuery(
    con,
    "SELECT *
     FROM collection_runs
     ORDER BY started_at DESC
     LIMIT ?",
    params = list(limit)
  )
}

#* List normalized monitoring signals
#* @param topic Optional topic filter
#* @param review_status Optional review status filter
#* @param limit Maximum number of signals to return
#* @get /signals
function(topic = "", review_status = "", limit = "") {
  con <- api_connect()
  on.exit(db_disconnect(con), add = TRUE)

  limit <- api_limit(limit)
  where <- c()
  params <- list()

  if (!is.null(topic) && nzchar(topic)) {
    where <- c(where, "topic = ?")
    params <- c(params, topic)
  }

  if (!is.null(review_status) && nzchar(review_status)) {
    where <- c(where, "review_status = ?")
    params <- c(params, review_status)
  }

  where_sql <- if (length(where) == 0) "" else paste("WHERE", paste(where, collapse = " AND "))
  sql <- paste(
    "SELECT signal_id, source, query_id, query_label, topic, rumour_family,
            title, url, domain, language, country,
            mentioned_country, mentioned_state_code, mentioned_state_name,
            mentioned_municipality, spatial_confidence,
            published_at, collected_at,
            score_volume, score_novelty, score_risk, review_status
     FROM signals",
    where_sql,
    "ORDER BY COALESCE(published_at, collected_at) DESC
     LIMIT ?"
  )

  DBI::dbGetQuery(con, sql, params = c(params, list(limit)))
}

#* List configured source providers
#* @get /sources/providers
function() {
  con <- api_connect()
  on.exit(db_disconnect(con), add = TRUE)

  DBI::dbGetQuery(
    con,
    "SELECT *
     FROM source_provider_summary
     ORDER BY provider_id"
  )
}

#* List publisher/source domains observed in collected signals
#* @param provider_id Optional provider filter
#* @param limit Maximum number of source domains to return
#* @get /sources/domains
function(provider_id = "", limit = "") {
  con <- api_connect()
  on.exit(db_disconnect(con), add = TRUE)

  limit <- api_limit(limit)
  where_sql <- ""
  params <- list()

  if (!is.null(provider_id) && nzchar(provider_id)) {
    where_sql <- "WHERE provider_id = ?"
    params <- list(provider_id)
  }

  sql <- paste(
    "SELECT *
     FROM source_domain_summary",
    where_sql,
    "ORDER BY signal_count DESC, last_seen_at DESC
     LIMIT ?"
  )

  DBI::dbGetQuery(con, sql, params = c(params, list(limit)))
}

#* List signal activity windows by monitored query family
#* @param inactive_after_days Number of days without signals before a signal family is treated as inactive
#* @param limit Maximum number of activity rows to return
#* @get /signals/activity
#* @get /rumours/activity
function(inactive_after_days = "14", limit = "") {
  con <- api_connect()
  on.exit(db_disconnect(con), add = TRUE)

  limit <- api_limit(limit)
  inactive_after_days <- suppressWarnings(as.integer(inactive_after_days))
  if (is.na(inactive_after_days) || inactive_after_days < 1) {
    inactive_after_days <- 14L
  }

  activity <- DBI::dbGetQuery(
    con,
    "SELECT *
     FROM signal_activity
     ORDER BY last_seen_at DESC
     LIMIT ?",
    params = list(limit)
  )

  if (nrow(activity) == 0) {
    return(activity)
  }

  now <- Sys.time()
  last_seen <- as.POSIXct(activity$last_seen_at, tz = "UTC")
  activity$days_since_last_seen <- as.integer(difftime(now, last_seen, units = "days"))
  activity$activity_status <- ifelse(
    activity$days_since_last_seen > inactive_after_days,
    "inactive",
    "active"
  )
  activity
}

#* List spatial coverage by monitored query family and mentioned place
#* @param limit Maximum number of coverage rows to return
#* @get /signals/spatial
#* @get /rumours/spatial
function(limit = "") {
  con <- api_connect()
  on.exit(db_disconnect(con), add = TRUE)

  limit <- api_limit(limit)
  DBI::dbGetQuery(
    con,
    "SELECT *
     FROM signal_spatial_coverage
     ORDER BY last_seen_at DESC, signal_count DESC
     LIMIT ?",
    params = list(limit)
  )
}

#* List daily signal counts for surge and outbreak monitoring
#* @param topic Optional topic filter
#* @param query_id Optional query filter
#* @param limit Maximum number of daily rows to return
#* @get /signals/timeseries
#* @get /rumours/timeseries
function(topic = "", query_id = "", limit = "") {
  con <- api_connect()
  on.exit(db_disconnect(con), add = TRUE)

  limit <- api_limit(limit)
  where <- c()
  params <- list()

  if (!is.null(topic) && nzchar(topic)) {
    where <- c(where, "topic = ?")
    params <- c(params, topic)
  }

  if (!is.null(query_id) && nzchar(query_id)) {
    where <- c(where, "query_id = ?")
    params <- c(params, query_id)
  }

  where_sql <- if (length(where) == 0) "" else paste("WHERE", paste(where, collapse = " AND "))
  sql <- paste(
    "SELECT *
     FROM signal_daily_counts",
    where_sql,
    "ORDER BY signal_date DESC, signal_count DESC
     LIMIT ?"
  )

  DBI::dbGetQuery(con, sql, params = c(params, list(limit)))
}

#* List potential small surges and outbreak/event starts
#* @param min_count Minimum signals on a day
#* @param multiplier Daily count must exceed this multiple of prior 7-day average
#* @param quiet_days Days since previous signal to flag as a new or re-emerging event
#* @param min_domains Minimum distinct domains on a day
#* @param limit Maximum number of rows to return
#* @get /signals/surges
#* @get /rumours/surges
function(
  min_count = "2",
  multiplier = "2",
  quiet_days = "14",
  min_domains = "2",
  limit = ""
) {
  con <- api_connect()
  on.exit(db_disconnect(con), add = TRUE)

  limit <- api_limit(limit)
  min_count <- suppressWarnings(as.integer(min_count))
  multiplier <- suppressWarnings(as.numeric(multiplier))
  quiet_days <- suppressWarnings(as.integer(quiet_days))
  min_domains <- suppressWarnings(as.integer(min_domains))

  if (is.na(min_count) || min_count < 1) min_count <- 2L
  if (is.na(multiplier) || multiplier <= 0) multiplier <- 2
  if (is.na(quiet_days) || quiet_days < 1) quiet_days <- 14L
  if (is.na(min_domains) || min_domains < 1) min_domains <- 2L

  candidates <- DBI::dbGetQuery(
    con,
    "SELECT *
     FROM signal_surge_candidates
     WHERE signal_count >= ?
     ORDER BY signal_date DESC, signal_count DESC
     LIMIT ?",
    params = list(min_count, limit)
  )

  if (nrow(candidates) == 0) {
    return(candidates)
  }

  prior_avg <- candidates$prior_7day_avg
  days_since_previous <- candidates$days_since_previous_signal
  candidates$is_volume_surge <- !is.na(prior_avg) &
    prior_avg > 0 &
    candidates$signal_count >= multiplier * prior_avg
  candidates$is_new_or_reemerging <- is.na(days_since_previous) |
    days_since_previous >= quiet_days
  candidates$is_domain_diverse <- candidates$domain_count >= min_domains
  candidates$surge_flag <- candidates$is_volume_surge |
    candidates$is_new_or_reemerging |
    candidates$is_domain_diverse
  candidates$surge_reason <- ifelse(
    candidates$is_volume_surge,
    "volume_surge",
    ifelse(
      candidates$is_new_or_reemerging,
      "new_or_reemerging",
      ifelse(candidates$is_domain_diverse, "domain_diverse", NA_character_)
    )
  )

  candidates[candidates$surge_flag, ]
}

#* Run GDELT collection now
#* @post /collect/gdelt
function() {
  run_gdelt_collection()
}
