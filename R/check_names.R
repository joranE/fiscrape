#' @importFrom plyr ddply
#' @importFrom statskier paste_in
#' @importFrom statskier query
checkNames <- function(x){
  #browser()
  fisids <- paste_in(x$fisid)
  con <- dbConnect(MySQL(), 
                   dbname = databaseName, 
                   host = options()$mysql$host, 
                   port = options()$mysql$port, 
                   user = options()$mysql$user, 
                   password = options()$mysql$password)
  res <- query(con,"select 
                        fisid,
                        name,
                        count(distinct raceid) n 
                       from main 
                       where fisid in ",fisids,"
                       group by fisid,name")
  dbDisconnect(con)
  res <- ddply(res,.(fisid),function(x){x[which.max(x$n),]})
#   tbl <- table(res$fisid)
#   if (any(tbl > 1)){
#     res <- res[res$name %in% names(tbl)[tbl > 1],]
#   }
  x$name[match(res$fisid,x$fisid)] <- res$name
  x
}
