insert_data <- function(data,tbl,conn){
  n <- nrow(data)
  RSQLite::dbWriteTable(conn = conn,
                        name = tbl,
                        value = data,
                        row.names = FALSE,
                        overwrite = FALSE,
                        append = TRUE)
  message(sprintf("Inserted %s row(s) into %s.",n,tbl))
}

