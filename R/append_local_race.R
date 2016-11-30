#' @export
append_race <- function(race_data,url1 = NA,url2 = NA){
  con_remote <- statskier2::db_xc_remote()
  con_local <- statskier2::db_xc_local()
  
  new_raceid <- get_max_raceid()
  
  if (any(is.na(race_data$fisid))){
    race_data <- split(race_data,is.na(race_data$fisid))
    race_data[[1]] <- check_names(race_data[[1]])
    race_data <- do.call("rbind",race_data)
  }else{
    race_data <- check_names(race_data)
  }
  
  race_data$raceid <- new_raceid
  
  median_time <- data.frame(raceid = race_data$raceid[1],
                            median_time = median(race_data$time,na.rm = TRUE))
  
  race_url <- data.frame(raceid = race_data$raceid[1],
                         url1 = url2,
                         url2 = url1)
  
  cat("\nUploading...\n")
  #Remote upload
  check <- RMySQL::dbWriteTable(conn = con_remote,
                                name = "main",
                                value = race_data, 
                                row.names = FALSE, 
                                overwrite = FALSE, 
                                append = TRUE)
  if (!check){
    stop("Remote upload to main failed.")
  }
  
  check <- RMySQL::dbWriteTable(conn = con_remote,
                                name = "median_race_time",
                                value = median_time,
                                row.names = FALSE,
                                overwrite = FALSE,
                                append = TRUE)
  if (!check){
    stop("Remote upload to median_race_time failed.")
  }
  
  check <- RMySQL::dbWriteTable(conn = con_remote,
                                name = "race_url",
                                value = race_url,
                                row.names = FALSE,
                                overwrite = FALSE,
                                append = TRUE)
  if (!check){
    stop("Remote upload to race_url failed.")
  }
  
  #Local upload
  # sql <- sprintf("insert into main %s",paste_in(colnames(race_data),quote = FALSE))
  # sql <- paste(sql,"values",paste_in(rep("?",ncol(race_data)),quote = FALSE))
  # bulk_insert(cn = con_local,sql = sql,data = race_data)
  #Might be able to use this upon next RSQLite release after 1.0.0
  check <- RSQLite::dbWriteTable(conn = con_local,
                                 name = "main",
                                 value = race_data,
                                 row.names = FALSE,
                                 overwrite = FALSE,
                                 append = TRUE)
  if (!check){
    stop("Local upload to main failed.")
  }
  check <- RSQLite::dbWriteTable(conn = con_local,
                                 name = "median_race_time",
                                 value = median_time,
                                 row.names = FALSE,
                                 overwrite = FALSE,
                                 append = TRUE)
  if (!check){
    stop("Local upload to median_race_time failed.")
  }
  check <- RSQLite::dbWriteTable(conn = con_local,
                                 name = "race_url",
                                 value = race_url,
                                 row.names = FALSE,
                                 overwrite = FALSE,
                                 append = TRUE)
  if (!check){
    stop("Local upload to race_url failed.")
  }

  verify_upload(race_data)
  
}