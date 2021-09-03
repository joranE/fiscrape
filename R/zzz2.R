`%ni%` <- Negate(`%in%`)

safe_html <- purrr::safely(.f = xml2::read_html)

#' @importFrom purrr possibly
safe_retry_read_html <- 
  purrr::possibly(~ xml2::read_html(httr::RETRY("GET", url = .x)), 
                  otherwise = xml2::read_html("<html></html>"))

possibly_date <- purrr::possibly(.f = as.Date,otherwise = NA_character_)