insert_urls <- function(url_data,conn){
  RSQLite::dbWriteTable(conn = conn,
                        name = "event_url",
                        value = url_data,
                        row.names = FALSE,
                        overwrite = FALSE,
                        append = TRUE)
  cat("\nEvent URL insert complete.")
}