test_that("GDELT post-filter keeps Portuguese, Brazil, and .br sources", {
  config <- list(
    gdelt = list(
      post_filter = list(
        enabled = TRUE,
        mode = "any",
        languages = "Portuguese",
        source_countries = "Brazil",
        domains = c("g1.globo.com", "metropoles.com"),
        domain_suffixes = ".br",
        domain_regexes = c("(^|\\.)saude\\.[a-z]{2}\\.gov\\.br$")
      )
    )
  )

  articles <- list(
    list(title = "PT", language = "Portuguese", sourcecountry = "Portugal", domain = "example.pt"),
    list(title = "BR", language = "English", sourcecountry = "Brazil", domain = "example.com"),
    list(title = "G1", language = "English", sourcecountry = "United States", domain = "g1.globo.com"),
    list(title = "METROPOLES", language = "English", sourcecountry = "United States", domain = "metropoles.com"),
    list(title = "SAUDE", language = "English", sourcecountry = "United States", domain = "saude.sp.gov.br"),
    list(title = "DOMAIN", language = "English", sourcecountry = "United States", domain = "example.com.br"),
    list(title = "DROP", language = "English", sourcecountry = "United States", domain = "example.com")
  )

  filtered <- gdelt_filter_articles(articles, config)
  expect_equal(length(filtered), 6)
  expect_equal(purrr::map_chr(filtered, "title"), c("PT", "BR", "G1", "METROPOLES", "SAUDE", "DOMAIN"))
})

test_that("GDELT query variants use suffixes and optional targeted query domains", {
  config <- list(
    gdelt = list(
      query_suffixes = "sourcelang:portuguese",
      query_domains = character(),
      priority_domains = c("g1.globo.com", "metropoles.com")
    )
  )

  expect_equal(gdelt_query_variants("dengue", config), "dengue sourcelang:portuguese")

  config$gdelt$query_domains <- "g1.globo.com"
  variants <- gdelt_query_variants("dengue", config)
  expect_equal(
    variants,
    c(
      "dengue sourcelang:portuguese",
      "dengue domainis:g1.globo.com"
    )
  )
})

test_that("GDELT query variants fall back to base query without suffixes", {
  config <- list(
    gdelt = list(
      query_suffixes = character(),
      query_domains = "g1.globo.com"
    )
  )

  variants <- gdelt_query_variants("dengue", config)
  expect_equal(variants, c("dengue", "dengue domainis:g1.globo.com"))
})
