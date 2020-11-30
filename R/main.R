#' Run the screen scraper
#' 
#' The main function
#' 
#' @param update_bdays boolean
#' @param debug boolean
#' @import RPostgres
#' @export
fiscrape <- function(update_bdays = FALSE,debug = FALSE,schema = "public"){
  options(fiscrape.debug = debug)
  options(fiscrape.schema = schema)
  on.exit(expr = options(fiscrape.debug = FALSE,fiscrape.schema = "public"))
  
  message(sprintf("Operating on the schema '%s'.",options()$fiscrape.schema))
  
  #Main loop
  while(TRUE){
    cat("Make a selection: \n")
    
    selection <- menu(c("Enter New Race","Add Spr Final To Existing Race"))
    if (selection == 0){
      break
    }
    if (selection == 1){
      enter_new_race(update_bdays = update_bdays)
    }
    if (selection == 2){
      add_spr_final()
    }
  }
}