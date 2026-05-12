test_that("GDELT raw rows and signal rows are stable", {
  fetch_result <- list(
    query = tibble::tibble(
      id = "example",
      label = "Example query",
      topic = "health",
      rumour_family = "example_family",
      query = "Brasil dengue"
    ),
    request_url = "https://example.org",
    retrieved_at = as.POSIXct("2026-05-12 12:00:00", tz = "UTC"),
    articles = list(
      list(
        url = "https://news.example/a",
        title = "Dengue e enchentes",
        seendate = "20260512T120000Z",
        domain = "news.example",
        language = "Portuguese",
        sourcecountry = "Brazil"
      )
    )
  )

  raw <- gdelt_raw_rows(fetch_result, run_id = "run_1")
  signals <- gdelt_signal_rows(raw, fetch_result$query)

  expect_equal(nrow(raw), 1)
  expect_equal(nrow(signals), 1)
  expect_equal(signals$topic, "health")
  expect_equal(signals$url, "https://news.example/a")
  expect_true("mentioned_state_code" %in% names(signals))
})
