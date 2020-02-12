insert_event <- function(event_type = NULL,event_data = NULL,conn){
  tbl <- switch(event_type,Distance = "dst_event",Stage = "stg_event",Sprint = "spr_event")
  RSQLite::dbWriteTable(conn = conn,
                        name = tbl,
                        value = event_data,
                        row.names = FALSE,
                        overwrite = FALSE,
                        append = TRUE)
  cat("\nEvent INSERT complete.")
}