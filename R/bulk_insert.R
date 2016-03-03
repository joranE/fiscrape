#' @import RSQLite
bulk_insert <- function(cn,sql,data){
  dbBegin(cn)
  dbGetPreparedQuery(cn,sql,bind.data=data)
  dbCommit(cn)
}
