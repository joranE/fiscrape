getMaxRaceID <- function(){
  con_local <- db_xc_local()
  res <- ss_query(con_local,"select max(raceid) mx from main")
  dbDisconnect(con_local)
  res$mx
}