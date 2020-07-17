`%ni%` <- Negate(`%in%`)

safe_html <- purrr::safely(.f = xml2::read_html)

safe_retry_read_html <- 
  purrr::possibly(~ xml2::read_html(httr::RETRY("GET", url = .x)), 
           otherwise = xml2::read_html("<html></html>"))

possibly_date <- purrr::possibly(.f = as.Date,otherwise = NA_character_)

.onLoad <- function(libname, pkgname){
  cn <- RSQLite::dbConnect(RSQLite::SQLite(),Sys.getenv("XC_DB"))
  # cn <- RPostgres::dbConnect(RPostgres::Postgres(),
  #                            host = "db-postgresql-sfo2-97367-do-user-1174283-0.a.db.ondigitalocean.com",
  #                            dbname = "statskier",
  #                            user = "doadmin",
  #                            password = "y9b5y2dt8ujoab32",
  #                            port = 25060,
  #                            sslmode = "require")
  assign(x = "conl",value = cn,envir = .GlobalEnv)
}