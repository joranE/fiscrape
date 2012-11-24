getMaxRaceID <- function(){
  con <- dbConnect("SQLite","~/Dropbox/SkiingResults/fis_new.db")
  res <- my_query(con,"select max(raceid) mx from main")
  dbDisconnect(con)
  res$mx
}