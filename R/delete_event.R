#' @export
delete_event <- function(eventid){
  for (i in seq_along(eventid)){
    event_info <- RSQLite::dbGetQuery(conl,sprintf("select * from v_event where eventid = %s",eventid[i]))
    if (nrow(event_info) == 0){
      message("Event with eventid ",eventid[i]," not found.")
      next
    }
    event_type <- event_info$event_type
    print(event_info)
    m <- readline(prompt = "Delete this event (y/n)?")
    if (m != "y"){
      message("Skipping...")
      next
    }
    if (event_type == "Distance"){
      delete_event_dst(eventid[i])
    }
    if (event_type == "Sprint"){
      delete_event_spr(eventid[i])
    }
    if (event_type == "Stage"){
      delete_event_stg(eventid[i])
    }
  }
}

delete_event_dst <- function(eventid){
  rs <- RSQLite::dbSendStatement(conl,sprintf("delete from event_url where eventid = %s",eventid))
  dbClearResult(rs)
  rs <- RSQLite::dbSendStatement(conl,sprintf("delete from event_penalty where eventid = %s",eventid))
  dbClearResult(rs)
  rs <- RSQLite::dbSendStatement(conl,sprintf("delete from dst_pur_link where eventid = %s",eventid))
  dbClearResult(rs)
  rs <- RSQLite::dbSendStatement(conl,sprintf("delete from dst_pur_comb_times where pur_eventid = %s",eventid))
  dbClearResult(rs)
  rs <- RSQLite::dbSendStatement(conl,sprintf("delete from dst_splits where eventid = %s",eventid))
  dbClearResult(rs)
  rs <- RSQLite::dbSendStatement(conl,sprintf("delete from dst_result where eventid = %s",eventid))
  dbClearResult(rs)
  rs <- RSQLite::dbSendStatement(conl,sprintf("delete from dst_event where eventid = %s",eventid))
  dbClearResult(rs)
  rs <- RSQLite::dbSendStatement(conl,sprintf("delete from all_event where eventid = %s",eventid))
  dbClearResult(rs)
}

delete_event_spr <- function(eventid){
  rs <- RSQLite::dbSendStatement(conl,sprintf("delete from event_url where eventid = %s",eventid))
  dbClearResult(rs)
  rs <- RSQLite::dbSendStatement(conl,sprintf("delete from spr_fin_result where eventid = %s",eventid))
  dbClearResult(rs)
  rs <- RSQLite::dbSendStatement(conl,sprintf("delete from spr_qual_result where eventid = %s",eventid))
  dbClearResult(rs)
  rs <- RSQLite::dbSendStatement(conl,sprintf("delete from spr_fin_heats where eventid = %s",eventid))
  dbClearResult(rs)
  rs <- RSQLite::dbSendStatement(conl,sprintf("delete from spr_event where eventid = %s",eventid))
  dbClearResult(rs)
  rs <- RSQLite::dbSendStatement(conl,sprintf("delete from all_event where eventid = %s",eventid))
  dbClearResult(rs)
}

delete_event_stg <- function(eventid){
  rs <- RSQLite::dbSendStatement(conl,sprintf("delete from event_url where eventid = %s",eventid))
  dbClearResult(rs)
  rs <- RSQLite::dbSendStatement(conl,sprintf("delete from event_penalty where eventid = %s",eventid))
  dbClearResult(rs)
  rs <- RSQLite::dbSendStatement(conl,sprintf("delete from stg_event_link where ov_eventid = %s",eventid))
  dbClearResult(rs)
  rs <- RSQLite::dbSendStatement(conl,sprintf("delete from stg_race_link where ov_eventid = %s",eventid))
  dbClearResult(rs)
  rs <- RSQLite::dbSendStatement(conl,sprintf("delete from stg_result where eventid = %s",eventid))
  dbClearResult(rs)
  rs <- RSQLite::dbSendStatement(conl,sprintf("delete from stg_event where eventid = %s",eventid))
  dbClearResult(rs)
  rs <- RSQLite::dbSendStatement(conl,sprintf("delete from all_event where eventid = %s",eventid))
  dbClearResult(rs)
}