verify_upload <- function(tbls){
  con_local <- statskier2::db_xc_local()
  con_remote <- statskier2::db_xc_remote()
  raceid <- tbls$raceid[1]
  test_local <- statskier2::ss_query(con_local,sprintf("select * from main where raceid = %s",raceid))
  test_remote <- statskier2::ss_query(con_remote,sprintf("select * from main where raceid = %s",raceid))
  dbDisconnect(con_local)
  dbDisconnect(con_remote)
  if (nrow(test_local) == nrow(tbls) && nrow(test_remote) == nrow(tbls)){
    cat("\nUpload successful!\n")
  }else{
    cat("\nUpload failed!\n")
  }
}
