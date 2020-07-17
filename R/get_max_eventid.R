#' @export
get_max_eventid <- function(){
  res_local <- RSQLite::dbGetQuery(conn = conl,
                                   statement = "select 'Distance',max(eventid) as mx from dst_event union
                                                select 'Sprint',max(eventid) as mx from spr_event union
                                                select 'Stage',max(eventid) as mx from stg_event")
  max(res_local$mx)
}