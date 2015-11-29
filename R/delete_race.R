#' Delete Race from Dataase
#' @export
delete_race <- function(raceid = NULL){
 conr <- options()$statskier_remote
 conl <- options()$statskier_local
 race <- statskier::query(conl,sprintf("select * from main where raceid = %s",raceid))
 cat("Preparing to delete the following race:\n")
 print(race[1,c('raceid','date','season','location','gender','length','tech','type','start','cat1','cat2')])
 choice <- menu(choices = c("Abort","Continue"),title = "Choose one")
 if (choice <= 1){
   cat("\nExiting and doing nothing...")
   return(NULL)
 }else{
   query(conr,sprintf("delete from main where raceid = %s",raceid))
   query(conr,sprintf("delete from race_url where raceid = %s",raceid))
   query(conr,sprintf("delete from median_race_time where raceid = %s",raceid))
   
   query(conl,sprintf("delete from main where raceid = %s",raceid))
   query(conl,sprintf("delete from race_url where raceid = %s",raceid))
   query(conl,sprintf("delete from median_race_time where raceid = %s",raceid))
 }
}