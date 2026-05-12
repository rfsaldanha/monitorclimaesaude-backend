run_gdelt_collection <- function(
  config = read_app_config(),
  query_path = "inst/queries/gdelt.yml",
  query_ids = NULL,
  max_queries = NULL,
  con = db_connect(db_path(config))
) {
  on.exit(db_disconnect(con), add = TRUE)
  db_init(con)

  queries <- read_gdelt_queries(query_path)
  if (!is.null(query_ids)) {
    queries <- dplyr::filter(queries, .data$id %in% query_ids)
  }
  if (!is.null(max_queries)) {
    queries <- utils::head(queries, max_queries)
  }
  if (nrow(queries) == 0) {
    stop("No GDELT queries selected.", call. = FALSE)
  }

  run_id <- db_new_id("gdelt")

  DBI::dbAppendTable(
    con,
    "collection_runs",
    tibble::tibble(
      run_id = run_id,
      source = "gdelt",
      started_at = Sys.time(),
      finished_at = as.POSIXct(NA),
      status = "running",
      query_count = nrow(queries),
      raw_item_count = 0L,
      signal_count = 0L,
      message = NA_character_
    )
  )

  tryCatch(
    {
      request_interval <- cfg_value(config$gdelt, "request_interval_seconds", 5.5)
      fetch_attempts <- vector("list", nrow(queries))

      for (i in seq_len(nrow(queries))) {
        query_row <- queries[i, ]
        if (i > 1 && request_interval > 0) {
          Sys.sleep(request_interval)
        }

        fetch_attempts[[i]] <- tryCatch(
          list(
            ok = TRUE,
            query_id = query_row$id,
            result = gdelt_fetch_query(query_row, config),
            error = NA_character_
          ),
          error = function(e) {
            list(
              ok = FALSE,
              query_id = query_row$id,
              result = NULL,
              error = conditionMessage(e)
            )
          }
        )
      }

      failures <- purrr::keep(fetch_attempts, ~ !.x$ok)
      fetches <- purrr::map(purrr::keep(fetch_attempts, ~ .x$ok), "result")
      raw_rows <- purrr::map_dfr(fetches, gdelt_raw_rows, run_id = run_id)
      signal_rows <- gdelt_signal_rows(raw_rows, queries)

      raw_inserted <- db_insert_new_rows(con, "raw_items", raw_rows, "payload_hash")
      signal_inserted <- db_insert_new_rows(con, "signals", signal_rows, "raw_item_id")
      status <- if (length(failures) == 0) {
        "success"
      } else if (length(fetches) > 0) {
        "partial_success"
      } else {
        "error"
      }
      message <- if (length(failures) == 0) {
        "GDELT collection completed"
      } else {
        paste(
          "Failed queries:",
          paste(
            purrr::map_chr(failures, ~ paste0(.x$query_id, " (", .x$error, ")")),
            collapse = "; "
          )
        )
      }

      db_finish_run(
        con = con,
        run_id = run_id,
        status = status,
        raw_item_count = raw_inserted,
        signal_count = signal_inserted,
        message = message
      )

      list(
        run_id = run_id,
        status = status,
        queries = nrow(queries),
        failed_queries = length(failures),
        raw_items_inserted = raw_inserted,
        signals_inserted = signal_inserted
      )
    },
    error = function(e) {
      db_finish_run(
        con = con,
        run_id = run_id,
        status = "error",
        raw_item_count = 0L,
        signal_count = 0L,
        message = conditionMessage(e)
      )
      stop(e)
    }
  )
}
