#' @export
fiscrape_connect <- function(){
  con_info <- options()$statskier_pg_admin
  cn <- RPostgres::dbConnect(RPostgres::Postgres(),
                              host = con_info$host,
                              dbname = con_info$dbname,
                              user = con_info$user,
                              password = con_info$password,
                              port = con_info$port,
                              sslmode = con_info$sslmode)
  assign(x = "conl",value = cn,envir = .GlobalEnv)
}