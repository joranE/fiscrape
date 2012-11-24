checkNames <- function(x){
  fisids <- my_paste(x$fisid)
  con <- dbConnect("SQLite","~/Dropbox/SkiingResults/fis_new.db")
  res <- my_query(con,"select fisid,name from main where fisid IN (",fisids,")")
  dbDisconnect(con)
  tbl <- table(res$fisid)
  if (any(tbl > 1)){
    res <- res[res$name %in% names(tbl)[tbl > 1],]
  }
  x$name[match(res$fisid,x$fisid)] <- res$name
  x
}
