#' Run the screen scraper
#' 
#' The main function
#' 
#' @param \dots Ignored
#' @export
fiscrape <- function(...){
  #DB Connections
  con_remote <- db_xc_remote()
  con_local <- db_xc_local()
  
  #Main loop
  while(TRUE){
    cat("Make a selection: \n")
    
    selection <- menu(c('Enter New Race'))
    if (selection == 0){
      break
    }else{
      #Prompt for race info
      raceInfo <- gatherRaceInfo()
      raceInfo$season <- getSeason(raceInfo$date)
      
      #Scrape distance race
      if (raceInfo$type == 'Distance'){
        tbls <- dst_scrape(url = raceInfo$url,
                           raceInfo = raceInfo)
        race_url <- data.frame(raceid = tbls$raceid[1],
                               url1 = raceInfo$url,
                               url2 = NA)
        
        if (nrow(tbls) > 12){
          print(head(tbls))
          print(tail(tbls))
        }else{
          print(tbls)
        }
      }
      
      #Scrape stage race
      if (raceInfo$type == 'Stage'){
        tbls <- stage_scrape(url = raceInfo$url,
                             raceInfo = raceInfo)
        race_url <- data.frame(raceid = tbls$raceid[1],
                               url1 = raceInfo$url,
                               url2 = NA)
        if (nrow(tbls) > 12){
          print(head(tbls))
          print(tail(tbls))
        }else{
          print(tbls)
        }
      }
      
      #Scrape stage race
      if (raceInfo$type == 'Sprint'){
        #browser()
        qual <- spr_qual_scrape(url = raceInfo$url$qual,
                                raceInfo = raceInfo)
        final <- spr_final_scrape(url = raceInfo$url$final)
        tbls <- combine_qual_final(qual = qual,
                                   final = final)
        race_url <- data.frame(raceid = tbls$raceid[1],
                               url1 = raceInfo$url$qual,
                               url2 = raceInfo$url$final)
        if (nrow(tbls) > 12){
          print(head(tbls))
          print(tail(tbls))
        }else{
          print(tbls)
        }
      }
      
      median_time <- data.frame(raceid = tbls$raceid[1],
                                median_time = median(tbls$time,na.rm = TRUE))
      
      #Final check
      cat("\nDoes this look correct?")
      selection <- menu(c('Yes','No'))
      
      if (selection != 1){
        next
      }else{
        cat("\nUploading...\n")
        #Remote upload
        check <- RMySQL::dbWriteTable(conn = con_remote,
                                      name = "main",
                                      value = tbls, 
                                      row.names = FALSE, 
                                      overwrite = FALSE, 
                                      append = TRUE)
        if (!check){
          stop("Remote upload to main failed.")
        }
        
        check <- RMySQL::dbWriteTable(conn = con_remote,
                                      name = "median_race_time",
                                      value = median_time,
                                      row.names = FALSE,
                                      overwrite = FALSE,
                                      append = TRUE)
        if (!check){
          stop("Remote upload to median_race_time failed.")
        }
        
        check <- RMySQL::dbWriteTable(conn = con_remote,
                                      name = "race_url",
                                      value = race_url,
                                      row.names = FALSE,
                                      overwrite = FALSE,
                                      append = TRUE)
        if (!check){
          stop("Remote upload to race_url failed.")
        }
        
        #Local upload
        sql <- sprintf("insert into main %s",paste_in(colnames(tbls),quote = FALSE))
        sql <- paste(sql,"values",paste_in(rep("?",ncol(tbls)),quote = FALSE))
        bulk_insert(cn = con_local,sql = sql,data = tbls)
#         check <- RSQLite::dbWriteTable(conn = con_local,
#                                        name = "main",
#                                        value = tbls,
#                                        row.names = FALSE,
#                                        overwrite = FALSE,
#                                        append = TRUE)
#         if (!check){
#           stop("Local upload to main failed.")
#         }
        check <- RSQLite::dbWriteTable(conn = con_local,
                                       name = "median_race_time",
                                       value = median_time,
                                       row.names = FALSE,
                                       overwrite = FALSE,
                                       append = TRUE)
        if (!check){
          stop("Local upload to median_race_time failed.")
        }
        check <- RSQLite::dbWriteTable(conn = con_local,
                                       name = "race_url",
                                       value = race_url,
                                       row.names = FALSE,
                                       overwrite = FALSE,
                                       append = TRUE)
        if (!check){
          stop("Local upload to race_url failed.")
        }
      }
      
      verify_upload(tbls)
    }
  }
      
  dbDisconnect(con_local)
  dbDisconnect(con_remote)
}