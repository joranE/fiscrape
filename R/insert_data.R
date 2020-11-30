insert_data <- function(data,tbl,conn){
  n <- nrow(data)
  RPostgres::dbWriteTable(conn = conn,
                        name = Id(schema = options()$fiscrape.schema,table = tbl),
                        value = data,
                        row.names = FALSE,
                        overwrite = FALSE,
                        append = TRUE)
  message(sprintf("Inserted %s row(s) into %s.",n,tbl))
}

