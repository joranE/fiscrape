getMaxRaceID <- function(){
  con_local <- statskier2::db_xc_local()
  res <- statskier2::ss_query(con_local,"select max(raceid) mx from main")
  dbDisconnect(con_local)
  res$mx
}