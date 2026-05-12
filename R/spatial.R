brazil_state_lookup <- function() {
  tibble::tribble(
    ~state_code, ~state_name, ~aliases,
    "AC", "Acre", "acre",
    "AL", "Alagoas", "alagoas",
    "AP", "Amapa", "amapa|amapá",
    "AM", "Amazonas", "amazonas",
    "BA", "Bahia", "bahia",
    "CE", "Ceara", "ceara|ceará",
    "DF", "Distrito Federal", "distrito federal|brasilia|brasília",
    "ES", "Espirito Santo", "espirito santo|espírito santo",
    "GO", "Goias", "goias|goiás",
    "MA", "Maranhao", "maranhao|maranhão",
    "MT", "Mato Grosso", "mato grosso",
    "MS", "Mato Grosso do Sul", "mato grosso do sul",
    "MG", "Minas Gerais", "minas gerais",
    "PA", "Para", "para|pará",
    "PB", "Paraiba", "paraiba|paraíba",
    "PR", "Parana", "parana|paraná",
    "PE", "Pernambuco", "pernambuco",
    "PI", "Piaui", "piaui|piauí",
    "RJ", "Rio de Janeiro", "rio de janeiro",
    "RN", "Rio Grande do Norte", "rio grande do norte",
    "RS", "Rio Grande do Sul", "rio grande do sul",
    "RO", "Rondonia", "rondonia|rondônia",
    "RR", "Roraima", "roraima",
    "SC", "Santa Catarina", "santa catarina",
    "SP", "Sao Paulo", "sao paulo|são paulo",
    "SE", "Sergipe", "sergipe",
    "TO", "Tocantins", "tocantins"
  )
}

extract_brazil_state <- function(text) {
  if (is.null(text) || length(text) == 0 || is.na(text) || !nzchar(text)) {
    return(list(
      mentioned_state_code = NA_character_,
      mentioned_state_name = NA_character_,
      spatial_confidence = "none"
    ))
  }

  normalized <- stringr::str_to_lower(text)
  states <- brazil_state_lookup()
  matches <- purrr::map_lgl(states$aliases, function(alias) {
    stringr::str_detect(normalized, paste0("\\b(", alias, ")\\b"))
  })

  if (!any(matches)) {
    return(list(
      mentioned_state_code = NA_character_,
      mentioned_state_name = NA_character_,
      spatial_confidence = "none"
    ))
  }

  matched <- states[which(matches)[1], ]
  list(
    mentioned_state_code = matched$state_code,
    mentioned_state_name = matched$state_name,
    spatial_confidence = "state_mention"
  )
}
