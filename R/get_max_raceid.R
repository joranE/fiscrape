getMaxRaceID <- function(){
  con <- db_xc()
  res <- query(con,"select max(raceid) mx from main")
  dbDisconnect(con)
  res$mx
}