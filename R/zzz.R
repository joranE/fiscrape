`%ni%` <- Negate(`%in%`)

.onAttach <- function(libname,pkgname){
  src <- statskier2:::choose_src()
  options(statskier_src = src)
  invisible()
}

.onDetach <- function(libpath){
  if (!is.null(options()$statskier_src) && inherits(options()$statskier_src,"src_mysql")){
    DBI::dbDisconnect(options()$statskier_src$con)
  }
  invisible()
}