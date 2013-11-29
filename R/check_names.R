checkNames <- function(x){
  #browser()
  fisids <- my_paste(x$fisid)
  con <- dbConnect("SQLite","~/Dropbox/SkiingResults/fis_new.db")
  res <- my_query(con,"select 
                        fisid,
                        name,
                        count(distinct raceid) n 
                       from main 
                       where fisid IN (",fisids,")
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
