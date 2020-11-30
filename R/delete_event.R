#' @export
delete_event <- function(eventid,schema = "public"){
  options(fiscrape.schema = schema)
  on.exit(expr = options(fiscrape.schema = "public"))
  
  schema <- options()$fiscrape.schema
  v_ev <- dplyr::tbl(src = conl,dbplyr::in_schema(schema,"v_event"))
  
  for (i in seq_along(eventid)){
    event_info <- v_ev %>%
      filter(eventid == local(eventid[i])) %>%
      collect()
    if (nrow(event_info) == 0){
      message("Event with eventid ",eventid[i]," not found.")
      next
    }
    event_type <- event_info$event_type
    print(event_info)
    pr <- sprintf("Delete this event from the schema '%s' (y/n)?",options()$fiscrape.schema)
    m <- readline(prompt = pr)
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
  d1 <- "delete from %s.dst_event where eventid = %s"
  d2 <- "delete from %s.all_event where eventid = %s"
  d3 <- "delete from %s.event_tag where eventid = %s"
  
  rs <- RPostgres::dbSendStatement(conl,sprintf(d1,options()$fiscrape.schema,eventid))
  dbClearResult(rs)
  rs <- RPostgres::dbSendStatement(conl,sprintf(d2,options()$fiscrape.schema,eventid))
  dbClearResult(rs)
  rs <- RPostgres::dbSendStatement(conl,sprintf(d3,options()$fiscrape.schema,eventid))
  dbClearResult(rs)
}

delete_event_spr <- function(eventid){
  d1 <- "delete from %s.spr_event where eventid = %s"
  d2 <- "delete from %s.all_event where eventid = %s"
  d3 <- "delete from %s.event_tag where eventid = %s"
  
  rs <- RPostgres::dbSendStatement(conl,sprintf(d1,options()$fiscrape.schema,eventid))
  dbClearResult(rs)
  rs <- RPostgres::dbSendStatement(conl,sprintf(d2,options()$fiscrape.schema,eventid))
  dbClearResult(rs)
  rs <- RPostgres::dbSendStatement(conl,sprintf(d3,options()$fiscrape.schema,eventid))
  dbClearResult(rs)
}

delete_event_stg <- function(eventid){
  d1 <- "delete from %s.stg_event where eventid = %s"
  d2 <- "delete from %s.all_event where eventid = %s"
  d3 <- "delete from %s.event_tag where eventid = %s"
  
  rs <- RPostgres::dbSendStatement(conl,sprintf(d1,options()$fiscrape.schema,eventid))
  dbClearResult(rs)
  rs <- RPostgres::dbSendStatement(conl,sprintf(d2,options()$fiscrape.schema,eventid))
  dbClearResult(rs)
  rs <- RPostgres::dbSendStatement(conl,sprintf(d3,options()$fiscrape.schema,eventid))
  dbClearResult(rs)
}