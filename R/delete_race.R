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
#' @param target_gender character
#' @param target_compid integer
#' @param target_fisid character
#' @param ... named changes, e.g. \code{length = 30} or \code{cat1 = "WC"}
#' @export
update_person <- function(target_gender,target_compid,target_fisid,...){
  conr <- statskier2::db_xc_remote()
  conl <- statskier2::db_xc_local()
  person <- statskier2::ss_query(conl,
                     sprintf("select distinct fisid,name,gender,yob,nation 
                              from main 
                              where fisid = '%s' and gender = '%s' and compid = %s",
                             target_fisid,target_gender,target_compid))
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
    statskier2::ss_query(conr,
                         sprintf("update main set %s 
                                  where fisid = '%s' and gender = '%s' and compid = %s",
                                 set_clause,target_fisid,target_gender,target_compid))
    statskier2::ss_query(conl,
                         sprintf("update main set %s 
                                  where fisid = '%s' and gender = '%s' and compid = %s",
                                 set_clause,target_fisid,target_gender,target_compid))
  }
  dbDisconnect(conl)
  dbDisconnect(conr)
}

#' @export
fix_ath_names <- function(){
  conr <- statskier2::db_xc_remote()
  conl <- statskier2::db_xc_local()
  
  aths <- ss_query(con = conl,
                   q = "select gender,compid,fisid,name,max(date) max_date,
                              count(distinct raceid) n_race from main
                        where fisid is not null and compid is not null
                        group by gender,compid,fisid,name")
  
  ath_dups <- aths %>%
    group_by(gender,compid,fisid) %>%
    mutate(n_id = n_distinct(name)) %>%
    filter(n_id > 1) %>%
    ungroup() %>%
    arrange(gender,compid,fisid,desc(n_race),desc(max_date)) %>%
    mutate(split_var = paste(gender,compid,fisid,sep = "-"))
  
  ath_dups <- split(ath_dups,ath_dups$split_var)
  
  for (i in seq_along(ath_dups)){
    cur <- ath_dups[[i]]
    update_person(target_gender = cur$gender[1],
                  target_compid = cur$compid[1],
                  target_fisid = cur$fisid[1],
                  name = cur$name[1])
  }
}
