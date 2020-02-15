#' @export
last_events <- function(.date = as.character(Sys.Date() - 14)){
  ev <- tbl(conl,"v_event")
  ev %>%
    filter(date >= .date) %>%
    arrange(date) %>%
    collect()
}