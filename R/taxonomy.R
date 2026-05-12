read_gdelt_queries <- function(path = "inst/queries/gdelt.yml") {
  if (!file.exists(path)) {
    stop("GDELT query file not found: ", path, call. = FALSE)
  }

  queries <- yaml::read_yaml(path)$queries
  out <- tibble::as_tibble(do.call(rbind, lapply(queries, as.data.frame)))

  if ("signal_family" %in% names(out) && !"rumour_family" %in% names(out)) {
    out$rumour_family <- out$signal_family
  }
  if (!"signal_family" %in% names(out) && "rumour_family" %in% names(out)) {
    out$signal_family <- out$rumour_family
  }

  out
}
