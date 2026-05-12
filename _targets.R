source("R/api_helpers.R")
source_project_r()

targets::tar_option_set(
  packages = c(
    "DBI",
    "digest",
    "dplyr",
    "duckdb",
    "httr2",
    "jsonlite",
    "lubridate",
    "purrr",
    "stringr",
    "tibble",
    "yaml"
  )
)

list(
  targets::tar_target(config, read_app_config()),
  targets::tar_target(init_db, db_init(db_connect(db_path(config)))),
  targets::tar_target(gdelt_collection, run_gdelt_collection(config))
)
