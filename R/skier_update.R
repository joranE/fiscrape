update_skier <- function(ref_compid,updates,conn){
  updates_squish <- paste(purrr::imap(.x = updates,.f = squish_update),collapse = ",")
  q <- " update skier set %s where compid = %s"
  q <- sprintf(q,updates_squish,ref_compid)
  RSQLite::dbBegin(conn)
  rs <- RSQLite::dbSendStatement(conn,q)
  RSQLite::dbClearResult(rs)
  RSQLite::dbCommit(conn)
  message("Completed: ",q)
}

squish_update <- function(x,nm){
  if (is.character(x)) paste0(nm,"='",x,"'")
  else paste0(nm,"=",x)
}
