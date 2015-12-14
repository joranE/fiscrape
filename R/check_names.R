#' @importFrom plyr ddply
checkNames <- function(x){
  #browser()
  fisids <- paste_in(x$fisid)
  con_local <- db_xc_local()
  res <- ss_query(con_local,sprintf("select 
                        fisid,
                        name,
                        count(distinct raceid) n 
                       from main 
                       where fisid in %s
                       group by fisid,name",fisids))
  dbDisconnect(con_local)
  res <- plyr::ddply(res,c("fisid"),function(x){x[which.max(x$n),]})
  x$name[match(res$fisid,x$fisid)] <- res$name
  x
}
