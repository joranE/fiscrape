insert_dst_result <- function(result_data,conn){
  RSQLite::dbWriteTable(conn = conn,
                        name = "dst_result",
                        value = result_data,
                        row.names = FALSE,
                        overwrite = FALSE,
                        append = TRUE)
  cat("\nDst result INSERT complete.")
}

insert_pur_times <- function(result_data,conn){
  RSQLite::dbWriteTable(conn = conn,
                        name = "dst_pur_comb_times",
                        value = result_data,
                        row.names = FALSE,
                        overwrite = FALSE,
                        append = TRUE)
  cat("\nDst pur time INSERT complete.")
}

insert_stg_result <- function(result_data,conn){
  RSQLite::dbWriteTable(conn = conn,
                        name = "stg_result",
                        value = result_data,
                        row.names = FALSE,
                        overwrite = FALSE,
                        append = TRUE)
  cat("\nStg result INSERT complete.")
}

insert_spr_qual_result <- function(result_data,conn){
  RSQLite::dbWriteTable(conn = conn,
                        name = "spr_qual_result",
                        value = result_data,
                        row.names = FALSE,
                        overwrite = FALSE,
                        append = TRUE)
  cat("\nSpr qual result INSERT complete.")
}

insert_spr_fin_result <- function(result_data,conn){
  RSQLite::dbWriteTable(conn = conn,
                        name = "spr_fin_result",
                        value = result_data,
                        row.names = FALSE,
                        overwrite = FALSE,
                        append = TRUE)
  cat("\nSpr final INSERT complete.")
}