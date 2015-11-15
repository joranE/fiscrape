verifyUpload <- function(tbls){
  con <- statskier::db_xc()
  raceid <- tbls$raceid[1]
  test <- statskier::query(con,"select * from main where raceid = ",raceid)
  dbDisconnect(con)
  if (nrow(test) == nrow(tbls)){
    cat("\nUpload successful!\n")
  }
  else{
    cat("\nUpload failed!\n")
  }
}
