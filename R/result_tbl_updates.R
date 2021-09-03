#' @export
dst_result_update <- function(new_data,schema = "test"){
  if (!all(c("eventid","compid") %in% colnames(new_data))){
    stop("'new_data' must contain eventid & compid as keys for the update join.")
  }
  
  #schema <- options()$fiscrape_schema
  cols_to_update <- setdiff(colnames(new_data),c("eventid","compid"))
  cols_to_update_b <- paste0("b.",cols_to_update)
  update_set <- paste(paste(cols_to_update,cols_to_update_b,sep = "="),collapse = ",")
  q <- sprintf("update %s.dst_result a set %s from %s.temp_update b where a.eventid = b.eventid and a.compid = b.compid",
               schema,update_set,schema)
  
  message("Writing new data to temp table...")
  rs <- dbWriteTable(conn = conl,
                     name = Id(schema = schema,table = "temp_update"),
                     value = new_data,
                     overwrite = TRUE)
  message("Performing update query...")
  rs <- dbSendQuery(conn = conl,statement = q)
  dbClearResult(rs)
  message("Removing temp table...")
  rs <- dbRemoveTable(conn = conl,name = Id(schema = schema,table = "temp_update"))
}