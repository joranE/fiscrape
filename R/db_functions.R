#' Create Local XC DB Connection
#'
#' @import RSQLite
db_xc_local <- function(){
  dbConnect(SQLite(), options()$sqlite_path)
}

#' Create Remote XC DB Connection
#'
#' @import RMySQL
db_xc_remote <- function(){
  dbConnect(drv = RMySQL::MySQL(),
            dbname = options()$mysql$dbName,
            username = options()$mysql$user,
            password = options()$mysql$password,
            host = options()$mysql$host,
            port = options()$mysql$port)
}

#' Query XC Database
#'
#' @param con database connection
#' @param q character; SQL
#'
#' @import DBI
ss_query <- function(con,q){
  dbGetQuery(con,q)
}