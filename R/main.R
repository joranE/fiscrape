#' Run the screen scraper
#' 
#' The main function
#' 
#' @param update_bdays boolean
#' @param debug boolean
#' @export
fiscrape <- function(update_bdays = FALSE,debug = FALSE){
  options(fiscrape.debug = debug)
  on.exit(options(fiscrape.debug = FALSE))
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