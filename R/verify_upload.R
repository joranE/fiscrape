verifyUpload <- function(tbls){
  con <- dbConnect("SQLite","~/Dropbox/SkiingResults/fis_new.db")
  raceid <- tbls$raceid[1]
  test <- query(con,"select * from main where raceid = ",raceid)
  dbDisconnect(con)
  if (nrow(test) == nrow(tbls)){
    cat("\nUpload successful!\n")
  }
  else{
    cat("\nUpload failed!\n")
  }
}
