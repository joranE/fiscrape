getMaxRaceID <- function(){
  con <- dbConnect(MySQL(), 
                   dbname = databaseName, 
                   host = options()$mysql$host, 
                   port = options()$mysql$port, 
                   user = options()$mysql$user, 
                   password = options()$mysql$password)
  res <- query(con,"select max(raceid) mx from main")
  dbDisconnect(con)
  res$mx
}