#' Delete Race from Database
#' 
#' Delete race data from both remote and local
#' databases from main, race_url and median_race_time
#' tables all in one go. Useful for when FIS fucks up the
#' results and you need to start over.
#' 
#' @param raceid integer
#' @export
delete_race <- function(raceid = NULL){
 conr <- statskier2::db_xc_remote()
 conl <- statskier2::db_xc_local()
 race <- statskier2::ss_query(conl,sprintf("select * from main where raceid = %s",raceid))
 cat("Preparing to delete the following race:\n")
 print(race[1,c('raceid','date','season','location','gender','length','tech','type','start','cat1','cat2')])
 choice <- menu(choices = c("Abort","Continue"),title = "Choose one")
 if (choice <= 1){
   cat("\nExiting and doing nothing...")
   return(NULL)
 }else{
   statskier2::ss_query(conr,sprintf("delete from main where raceid = %s",raceid))
   statskier2::ss_query(conr,sprintf("delete from race_url where raceid = %s",raceid))
   statskier2::ss_query(conr,sprintf("delete from median_race_time where raceid = %s",raceid))
   
   statskier2::ss_query(conl,sprintf("delete from main where raceid = %s",raceid))
   statskier2::ss_query(conl,sprintf("delete from race_url where raceid = %s",raceid))
   statskier2::ss_query(conl,sprintf("delete from median_race_time where raceid = %s",raceid))
 }
 dbDisconnect(conl)
 dbDisconnect(conr)
}

#' Update Race Info
#'
#' Update race info in both remote and local
#' databases for a specified raceid.
#'
#' @param raceid integer
#' @param ... named changes, e.g. \code{length = 30} or \code{cat1 = "WC"}
#' @export
update_race <- function(raceid,...){
  conr <- statskier2::db_xc_remote()
  conl <- statskier2::db_xc_local()
  race <- statskier2::ss_query(conl,sprintf("select * from main where raceid = %s",raceid))
  cat("Preparing to update the following race:\n")
  print(race[1,c('raceid','date','season','location','gender','length','tech','type','start','cat1','cat2')])
  changes <- list(...)
  if (length(changes) == 0){
    stop("No changes specified.")
  }
  changes <- lapply(changes,function(x) if (is.character(x)) {paste0("'",x,"'")} else {x})
  set_clause <- paste(names(changes),changes,sep = " = ",collapse = ", ")
  cat("with the following changes:\n")
  print(set_clause)
  choice <- menu(choices = c("Abort","Continue"),title = "Choose one")
  if (choice <= 1){
    cat("\nExiting and doing nothing...")
    return(NULL)
  }else{
    statskier2::ss_query(conr,sprintf("update main set %s where raceid = %s",set_clause,raceid))
    statskier2::ss_query(conl,sprintf("update main set %s where raceid = %s",set_clause,raceid))
  }
  dbDisconnect(conl)
  dbDisconnect(conr)
}

#' Update Person Info
#'
#' Update race info in both remote and local
#' databases for a specified raceid.
#'
#' @param fisid integer
#' @param ... named changes, e.g. \code{length = 30} or \code{cat1 = "WC"}
#' @export
update_person <- function(target_fisid,...){
  conr <- statskier2::db_xc_remote()
  conl <- statskier2::db_xc_local()
  person <- statskier2::ss_query(conl,
                     sprintf("select distinct fisid,name,gender,yob,nation from main where fisid = '%s'",
                             target_fisid))
  cat("Preparing to update the following person:\n")
  print(person)
  changes <- list(...)
  if (length(changes) == 0){
    stop("No changes specified.")
  }
  changes <- lapply(changes,function(x) if (is.character(x)) {paste0("'",x,"'")} else {x})
  set_clause <- paste(names(changes),changes,sep = " = ",collapse = ", ")
  cat("with the following changes:\n")
  print(set_clause)
  choice <- menu(choices = c("Abort","Continue"),title = "Choose one")
  if (choice <= 1){
    cat("\nExiting and doing nothing...")
    return(NULL)
  }else{
    statskier2::ss_query(conr,sprintf("update main set %s where fisid = '%s'",set_clause,target_fisid))
    statskier2::ss_query(conl,sprintf("update main set %s where fisid = '%s'",set_clause,target_fisid))
  }
  dbDisconnect(conl)
  dbDisconnect(conr)
}