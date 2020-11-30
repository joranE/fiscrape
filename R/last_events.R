#' @export
last_events <- function(.date = as.character(Sys.Date() - 14)){
  ev <- tbl(conl,dbplyr::in_schema(options()$fiscrape.schema,"v_event"))
  ev %>%
    filter(date >= .date) %>%
    arrange(date) %>%
    collect()
}