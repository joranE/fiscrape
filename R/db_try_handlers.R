try_handler <- function(e){
  RSQLite::dbRollback(conn = conl,name = "orig")
}

try_finally <- function(f){
  RSQLite::dbCommit(conn = conl,name = "orig")
}