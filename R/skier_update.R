update_skier <- function(ref_compid,updates,conn){
  updates_squish <- paste(purrr::imap(.x = updates,.f = squish_update),collapse = ",")
  schema <- options()$fiscrape.schema
  q <- "update %s.skier set %s where compid = %s"
  q <- sprintf(q,schema,updates_squish,ref_compid)
  
  DBI::dbWithTransaction(conn,{
    rs <- RPostgres::dbSendStatement(conn,q)
    RPostgres::dbClearResult(rs)
  })
  
  message("Completed: ",q)
}

squish_update <- function(x,nm){
  if (is.character(x)) paste0(nm,"='",x,"'")
  else paste0(nm,"=",x)
}
