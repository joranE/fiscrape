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
        print(head(tbls))
      }
      
      #Scrape stage race
      if (raceInfo$type == 'Stage'){
        tbls <- stage_scrape(url = raceInfo$url,
                             raceInfo = raceInfo)
        print(head(tbls))
      }
      
      #Scrape stage race
      if (raceInfo$type == 'Sprint'){
        #browser()
        qual <- spr_qual_scrape(url = raceInfo$url$qual,
                                raceInfo = raceInfo)
        final <- spr_final_scrape(url = raceInfo$url$final)
        tbls <- combine_qual_final(qual = qual,
                                   final = final)
        print(head(tbls))
      }
      
      #Final check
      cat("\nDoes this look correct?")
      selection <- menu(c('Yes','No'))
      
      if (selection != 1){
        next
      }else{
        cat("\nUploading...\n")
        check <- dbWriteTable(conn = con_remote,
                              name = "main",
                              value = tbls, 
                              row.names = FALSE, 
                              overwrite = FALSE, 
                              append = TRUE)
        median_time <- data.frame(raceid = tbls$raceid[1],
                                  median_time = median(tbls$time,na.rm = TRUE))
        dbWriteTable(conn = con_remote,
                     name = "median_race_time",
                     value = median_time,
                     row.names = FALSE,
                     overwrite = FALSE,
                     append = TRUE)
        race_url <- data.frame(raceid = tbls$raceid[1],url1 = raceInfo$url,url2 = NA)
        dbWriteTable(conn = con_remote,
                     name = "race_url",
                     value = race_url,
                     row.names = FALSE,
                     overwrite = FALSE,
                     append = TRUE)
        if (!check){
          stop("Remote upload failed.")
        }
        sql <- "insert into main (raceid,date,season,location,
        gender,length,tech,type,start,
        cat1,cat2,compid,fisid,name,yob,age,nation,
        rank,rankqual,time,fispoints) 
        values (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
        bulk_insert(con_local,sql,tbls)
        check <- dbWriteTable(conn = con_local,
                              name = "median_race_time",
                              value = median_time,
                              row.names = FALSE,
                              overwrite = FALSE,
                              append = TRUE)
        dbWriteTable(conn = con_local,
                     name = "race_url",
                     value = race_url,
                     row.names = FALSE,
                     overwrite = FALSE,
                     append = TRUE)
        
        if (!check){
          stop("Local upload failed.")
        }
      }
    }
  }
      
  dbDisconnect(con_local)
  dbDisconnect(con_remote)
}