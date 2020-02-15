try_handler <- function(e){
  RSQLite::dbRollback(conn = conl,name = "orig")
  message(e)
  return(FALSE)
}

try_finally <- function(){
  RSQLite::dbCommit(conn = conl,name = "orig")
}