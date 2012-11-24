bulk_insert <- function(cn,sql,data)
{
  dbBeginTransaction(cn)
  dbGetPreparedQuery(cn,sql,bind.data=data)
  dbCommit(cn)
}
