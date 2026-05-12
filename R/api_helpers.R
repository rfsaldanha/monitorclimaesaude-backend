api_connect <- function() {
  source_project_r()
  config <- read_app_config()
  con <- db_connect(db_path(config))
  db_init(con)
  con
}

source_project_r <- function(path = "R") {
  files <- list.files(path, pattern = "\\.R$", full.names = TRUE)
  invisible(lapply(files, source))
}

api_limit <- function(limit, config = read_app_config()) {
  default <- config$api$default_limit
  max_limit <- config$api$max_limit

  limit <- suppressWarnings(as.integer(limit %||% default))
  if (is.na(limit) || limit < 1) {
    limit <- default
  }

  min(limit, max_limit)
}
