backfill_signal_spatial_fields <- function(con = NULL) {
  owns_connection <- is.null(con)
  if (owns_connection) {
    con <- db_connect()
    on.exit(db_disconnect(con), add = TRUE)
  }
  db_init(con)

  rows <- DBI::dbGetQuery(
    con,
    "SELECT signal_id, title, text
     FROM signals
     WHERE spatial_confidence IS NULL"
  )

  if (nrow(rows) == 0) {
    return(0L)
  }

  updates <- purrr::map_dfr(seq_len(nrow(rows)), function(i) {
    spatial <- extract_brazil_state(paste(rows$title[i], rows$text[i], sep = " "))
    tibble::tibble(
      signal_id = rows$signal_id[i],
      mentioned_country = ifelse(spatial$spatial_confidence == "none", NA_character_, "Brazil"),
      mentioned_state_code = spatial$mentioned_state_code,
      mentioned_state_name = spatial$mentioned_state_name,
      mentioned_municipality = NA_character_,
      spatial_confidence = spatial$spatial_confidence
    )
  })

  temp_table <- paste0("tmp_signal_spatial_", digest::digest(Sys.time()))
  DBI::dbWriteTable(con, temp_table, updates, temporary = TRUE)
  on.exit(DBI::dbExecute(con, paste("DROP TABLE IF EXISTS", temp_table)), add = TRUE)

  DBI::dbExecute(
    con,
    sprintf(
      "UPDATE signals
       SET
         mentioned_country = u.mentioned_country,
         mentioned_state_code = u.mentioned_state_code,
         mentioned_state_name = u.mentioned_state_name,
         mentioned_municipality = u.mentioned_municipality,
         spatial_confidence = u.spatial_confidence
       FROM %s u
       WHERE signals.signal_id = u.signal_id",
      temp_table
    )
  )
}
