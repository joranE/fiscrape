bulk_insert <- function(cn,sql,data)
{
  #dbBeginTransation(cn)
  dbBegin(cn)
  dbGetPreparedQuery(cn,sql,bind.data=data)
  dbCommit(cn)
}
