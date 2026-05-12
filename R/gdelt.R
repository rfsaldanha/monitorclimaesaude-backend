gdelt_build_request <- function(query, config = read_app_config()) {
  gdelt_cfg <- config$gdelt
  timeout_seconds <- cfg_value(gdelt_cfg, "timeout_seconds", 60)
  request_interval <- cfg_value(gdelt_cfg, "request_interval_seconds", 5.5)

  httr2::request(gdelt_cfg$endpoint) |>
    httr2::req_user_agent(gdelt_cfg$user_agent) |>
    httr2::req_url_query(
      query = query,
      mode = gdelt_cfg$mode,
      format = gdelt_cfg$format,
      timespan = gdelt_cfg$timespan,
      maxrecords = gdelt_cfg$maxrecords,
      sort = gdelt_cfg$sort
    ) |>
    httr2::req_throttle(
      capacity = 1,
      fill_time_s = request_interval,
      realm = "gdelt-doc"
    ) |>
    httr2::req_retry(
      max_tries = 4,
      max_seconds = 180,
      retry_on_failure = TRUE,
      backoff = function(tries) max(10, request_interval)
    ) |>
    httr2::req_timeout(timeout_seconds)
}

gdelt_fetch_query <- function(query_row, config = read_app_config()) {
  query_variants <- gdelt_query_variants(query_row$query, config)
  responses <- purrr::map(query_variants, gdelt_fetch_query_variant, query_row = query_row, config = config)
  articles <- purrr::flatten(purrr::map(responses, "articles"))
  articles <- gdelt_deduplicate_articles(articles)

  list(
    query = query_row,
    request_url = paste(purrr::map_chr(responses, "request_url"), collapse = " | "),
    retrieved_at = Sys.time(),
    articles = articles
  )
}

gdelt_fetch_query_variant <- function(query, query_row, config = read_app_config()) {
  req <- gdelt_build_request(query, config)
  resp <- httr2::req_perform(req)
  content_type <- httr2::resp_content_type(resp)

  if (!grepl("json", content_type %||% "", ignore.case = TRUE)) {
    body_text <- httr2::resp_body_string(resp)
    stop(
      "GDELT returned non-JSON response for query '",
      query_row$id,
      "': ",
      body_text,
      call. = FALSE
    )
  }

  body <- httr2::resp_body_json(resp, simplifyVector = FALSE)
  articles <- body$articles %||% list()
  articles <- gdelt_filter_articles(articles, config)

  list(
    request_url = httr2::resp_url(resp),
    articles = articles
  )
}

gdelt_query_variants <- function(query, config = read_app_config()) {
  suffixes <- cfg_value(config$gdelt, "query_suffixes", character())
  suffixes <- unlist(suffixes)
  query_domains <- cfg_value(config$gdelt, "query_domains", character())
  query_domains <- unlist(query_domains)

  variants <- character()

  if (length(suffixes) > 0) {
    variants <- c(variants, paste(query, suffixes))
  } else {
    variants <- c(variants, query)
  }

  if (length(query_domains) > 0) {
    variants <- c(variants, paste(query, paste0("domainis:", query_domains)))
  }

  unique(variants)
}

gdelt_deduplicate_articles <- function(articles) {
  if (length(articles) == 0) {
    return(articles)
  }

  ids <- purrr::map_chr(articles, gdelt_article_id)
  articles[!duplicated(ids)]
}

gdelt_filter_articles <- function(articles, config = read_app_config()) {
  filter_cfg <- config$gdelt$post_filter
  if (is.null(filter_cfg) || !isTRUE(filter_cfg$enabled) || length(articles) == 0) {
    return(articles)
  }

  languages <- cfg_value(filter_cfg, "languages", character())
  source_countries <- cfg_value(filter_cfg, "source_countries", character())
  domains <- cfg_value(filter_cfg, "domains", character())
  domain_suffixes <- cfg_value(filter_cfg, "domain_suffixes", character())
  domain_regexes <- cfg_value(filter_cfg, "domain_regexes", character())
  mode <- cfg_value(filter_cfg, "mode", "any")

  keep <- purrr::map_lgl(articles, function(article) {
    checks <- c(
      gdelt_matches_value(article$language %||% NA_character_, languages),
      gdelt_matches_value(article$sourcecountry %||% NA_character_, source_countries),
      gdelt_matches_value(article$domain %||% NA_character_, domains),
      gdelt_matches_domain_suffix(article$domain %||% NA_character_, domain_suffixes),
      gdelt_matches_domain_regex(article$domain %||% NA_character_, domain_regexes)
    )

    checks <- checks[!is.na(checks)]
    if (length(checks) == 0) {
      return(TRUE)
    }

    if (identical(mode, "all")) {
      all(checks)
    } else {
      any(checks)
    }
  })

  articles[keep]
}

gdelt_matches_value <- function(value, allowed) {
  if (length(allowed) == 0 || is.null(allowed)) {
    return(NA)
  }
  if (is.null(value) || length(value) == 0 || is.na(value)) {
    return(FALSE)
  }

  stringr::str_to_lower(value) %in% stringr::str_to_lower(unlist(allowed))
}

gdelt_matches_domain_suffix <- function(domain, suffixes) {
  if (length(suffixes) == 0 || is.null(suffixes)) {
    return(NA)
  }
  if (is.null(domain) || length(domain) == 0 || is.na(domain)) {
    return(FALSE)
  }

  domain <- stringr::str_to_lower(domain)
  suffixes <- stringr::str_to_lower(unlist(suffixes))
  any(stringr::str_ends(domain, stringr::fixed(suffixes)))
}

gdelt_matches_domain_regex <- function(domain, patterns) {
  if (length(patterns) == 0 || is.null(patterns)) {
    return(NA)
  }
  if (is.null(domain) || length(domain) == 0 || is.na(domain)) {
    return(FALSE)
  }

  domain <- stringr::str_to_lower(domain)
  patterns <- unlist(patterns)
  any(purrr::map_lgl(patterns, ~ stringr::str_detect(domain, .x)))
}

gdelt_article_id <- function(article) {
  article$url %||% article$sharingimage %||% digest::digest(article)
}

gdelt_article_observed_at <- function(article) {
  seendate <- article$seendate %||% NA_character_
  if (is.na(seendate) || !nzchar(seendate)) {
    return(as.POSIXct(NA))
  }

  lubridate::ymd_hms(seendate, tz = "UTC", quiet = TRUE)
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) {
    y
  } else {
    x
  }
}
