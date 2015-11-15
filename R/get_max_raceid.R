getMaxRaceID <- function(){
  con <- dbConnect(MySQL(), 
                   dbname = options()$mysql$dbName, 
                   host = options()$mysql$host, 
                   port = options()$mysql$port, 
                   user = options()$mysql$user, 
                   password = options()$mysql$password)
  res <- statskier::query(con,"select max(raceid) mx from main")
  dbDisconnect(con)
  res$mx
}