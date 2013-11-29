verifyUpload <- function(tbls){
  con <- db_xc()
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
