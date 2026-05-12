read_app_config <- function(path = "config.yml", profile = "default") {
  if (!file.exists(path)) {
    stop("Config file not found: ", path, call. = FALSE)
  }

  cfg <- yaml::read_yaml(path)
  if (!profile %in% names(cfg)) {
    stop("Config profile not found: ", profile, call. = FALSE)
  }

  cfg[[profile]]
}

cfg_value <- function(x, name, default = NULL) {
  if (is.null(x[[name]])) {
    default
  } else {
    x[[name]]
  }
}
