get_max_raceid <- function(){
  con_local <- statskier2::db_xc_local()
  con_remote <- statskier2::db_xc_remote()
  res_local <- statskier2::ss_query(con_local,"select max(raceid) mx from main")
  res_remote <- statskier2::ss_query(con_remote,"select max(raceid) mx from main")
  if (res_local$mx != res_remote$mx){
    stop("Max raceid in local/remote out of sync.")
  }
  dbDisconnect(con_local)
  dbDisconnect(con_remote)
  res_local$mx
}