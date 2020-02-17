try_handler <- function(e){
  message("Rolling back to orig savepoint...")
  RSQLite::dbRollback(conn = conl,name = "orig")
  message(e,"\n")
  return(FALSE)
}

try_finally <- function(){
  RSQLite::dbCommit(conn = conl,name = "orig")
  message("Done.")
}