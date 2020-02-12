`%ni%` <- Negate(`%in%`)

conl <- RSQLite::dbConnect(RSQLite::SQLite(),
                                   dbname = "~/Dropbox/new-results-db/output/fis_results_prototype.db")

.onDetach <- function(libpath){
  RSQLite::dbDisconnect(conl)
}
