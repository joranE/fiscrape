insert_data <- function(data,tbl,conn){
  RSQLite::dbWriteTable(conn = conn,
                        name = tbl,
                        value = data,
                        row.names = FALSE,
                        overwrite = FALSE,
                        append = TRUE)
  cat(sprtinf("\nINSERT of %s into %s complete.",data,tbl))
}

