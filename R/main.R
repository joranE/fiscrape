#' Run the screen scraper
#' 
#' The main function
#' 
#' @param update_bdays boolean
#' @param debug boolean
#' @export
fiscrape <- function(update_bdays = FALSE,debug = FALSE){
  #Main loop
  while(TRUE){
    cat("Make a selection: \n")
    
    selection <- menu(c('Enter New Race'))
    if (selection == 0){
      break
    }else{
      #Prompt for event info
      event_info <- gatherRaceInfo()
      event_info$season <- getSeason(event_info$date)
      
      #Scrape distance event
      if (event_info$type == 'Distance'){
        dst_data <- dst_scrape(url = event_info$url,
                               event_info = event_info)
        event_url <- data.frame(raceid = dst_data$event$raceid[1],
                                url1 = event_info$url,
                                url2 = NA)
        skiers <- process_skiers(dst_data[["skier"]],conl)
        
        dbBegin(conl,"orig")
        cond <- tryCatch({
          #Upload to:
          # event table
          # skier table (if new skiers)
          # dst_result table
          # dst_pur_comb_times table if applicable
          # event_url table
          insert_event(event_type = event_info$type,event_data = dst_data$event,conl)
          insert_dst_result(dst_data$result,conl)
          if (nrow(skiers) > 0){
            insert_skier(skiers,conl)
          }
          if (!is.null(dst_data$pur_times)){
            insert_pur_times(dst_data$pur_times,conl)
          }
        },error = try_handler,warning = try_handler,finally = try_finally)
      }
      
      #Scrape stage event
      if (event_info$type == 'Stage'){
        stg_data <- stg_scrape(url = event_info$url,
                               event_info = event_info)
        event_url <- data.frame(raceid = stg_data$event$raceid[1],
                                url1 = event_info$url,
                                url2 = NA)
        skiers <- process_skiers(stg_data[["skier"]],conl)
        
        dbBegin(conl,"orig")
        cond <- tryCatch({
          #Upload to:
          # event table
          # skier table (if new skiers)
          # stg_result table
          # stg_race_link table
          # event_url table
          insert_event(event_type = event_info$type,event_data = stg_data$event,conl)
          insert_stg_result(stg_data$result,conl)
          if (nrow(skiers) > 0){
            insert_skier(skiers,conl)
          }
        },error = try_handler,warning = try_handler,finally = try_finally)
      }
      
      #Scrape sprint event
      if (event_info$type == 'Sprint'){
        spr_qual_data <- NULL
        spr_fin_data_list <- NULL
        
        if (!is.na(event_info$url$qual)){
          spr_qual_data <- spr_qual_scrape(url = event_info$url$qual,
                                           event_info = event_info)
          skiers <- process_skiers(spr_qual_data[["skier"]],conl)
        }
        if (!is.na(event_info$url$final[1])){
          n_fin <- length(event_info$url$final)
          spr_fin_data_list <- vector(mode = "list",
                                length = n_fin)
          skiers_list <- vector(mode = "list",
                                length = n_fin)
          for (i in seq_len(n_fin)){
            spr_fin_data_list[[i]] <- spr_fin_scrape(url = event_info$url$final[i])
            skiers_list[[i]] <- process_skiers(spr_fin_data_list[[i]][["skier"]],conl)
          }
          
          skiers <- bind_rows(c(skiers,skiers_list)) %>%
            distinct()
          event <- bind_rows(c(spr_qual_data$event,lapply(spr_fin_data_list,'[[',"event"))) %>%
            distinct()
          
          dbBegin(conl,"orig")
          cond <- tryCatch({
            #Upload to:
            # event table
            # skier table (if new skiers)
            # spr_qual_result table
            # spr_fin_result table
            # event_url table
            insert_event(event_type = event_info$type,event_data = event,conl)
            if (nrow(skiers) > 0){
              insert_skier(skiers,conl)
            }
            
            if (!is.null(spr_qual_data)){
              insert_spr_qual_result(spr_qual_data$result,conl)
            }
            
            if (!is.null(spr_fin_data_list)){
              insert_spr_fin_result(spr_fin_data$result,conl)
            }
          },error = try_handler,warning = try_handler,finally = try_finally)
        }
        
        event_url <- data.frame(raceid = tbls$raceid[1],
                                url1 = event_info$url$qual,
                                url2 = event_info$url$final)
      }
    }
  }
}