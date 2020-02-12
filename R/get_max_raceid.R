#' @export
get_max_raceid <- function(){
  res_local <- RSQLite::dbGetQuery(conn = conl,
                                   statement = "select 'dst' as event_type,max(raceid) mx_raceid from dst_event 
                                                union  
                                                select 'spr' as event_type,max(raceid) mx_raceid from spr_event
                                                union
                                                select 'stg' as event_type,max(raceid) mx_raceid from stg_event")
  max(res_local$mx_raceid)
}