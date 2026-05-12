gdelt_raw_rows <- function(fetch_result, run_id) {
  articles <- fetch_result$articles
  if (length(articles) == 0) {
    return(tibble::tibble())
  }

  purrr::map_dfr(articles, function(article) {
    payload_json <- jsonlite::toJSON(article, auto_unbox = TRUE, null = "null")
    payload_hash <- digest::digest(payload_json, algo = "sha256")

    tibble::tibble(
      raw_item_id = paste("raw", payload_hash, sep = "_"),
      run_id = run_id,
      source = "gdelt",
      query_id = fetch_result$query$id,
      source_item_id = gdelt_article_id(article),
      source_url = article$url %||% NA_character_,
      observed_at = gdelt_article_observed_at(article),
      collected_at = fetch_result$retrieved_at,
      payload_hash = payload_hash,
      payload_json = as.character(payload_json)
    )
  })
}

gdelt_signal_rows <- function(raw_rows, query_lookup) {
  if (nrow(raw_rows) == 0) {
    return(tibble::tibble())
  }

  purrr::map_dfr(seq_len(nrow(raw_rows)), function(i) {
    raw <- raw_rows[i, ]
    article <- jsonlite::fromJSON(raw$payload_json, simplifyVector = TRUE)
    query <- query_lookup[query_lookup$id == raw$query_id, ]

    title <- clean_text(article$title %||% NA_character_)
    text <- clean_text(article$snippet %||% title)
    spatial <- extract_brazil_state(paste(title, text, sep = " "))

    tibble::tibble(
      signal_id = paste("sig", raw$payload_hash, sep = "_"),
      raw_item_id = raw$raw_item_id,
      run_id = raw$run_id,
      source = raw$source,
      query_id = raw$query_id,
      query_label = query$label %||% NA_character_,
      topic = query$topic %||% NA_character_,
      rumour_family = query$rumour_family %||% NA_character_,
      title = title,
      text = text,
      url = article$url %||% NA_character_,
      domain = article$domain %||% NA_character_,
      language = article$language %||% NA_character_,
      country = article$sourcecountry %||% NA_character_,
      mentioned_country = ifelse(spatial$spatial_confidence == "none", NA_character_, "Brazil"),
      mentioned_state_code = spatial$mentioned_state_code,
      mentioned_state_name = spatial$mentioned_state_name,
      mentioned_municipality = NA_character_,
      spatial_confidence = spatial$spatial_confidence,
      published_at = gdelt_article_observed_at(article),
      collected_at = raw$collected_at,
      score_volume = 1,
      score_novelty = NA_real_,
      score_risk = NA_real_,
      review_status = "unreviewed"
    )
  })
}

clean_text <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return(NA_character_)
  }

  x |>
    stringr::str_replace_all("\\s+", " ") |>
    stringr::str_trim()
}
