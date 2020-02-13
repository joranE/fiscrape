#' @export
get_max_eventid <- function(){
  res_local <- RSQLite::dbGetQuery(conn = conl,
                                   statement = "select max(eventid) mx from event")
  res_local$mx
}