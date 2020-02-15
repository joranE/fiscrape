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
    
    selection <- menu(c('Enter New Race'))
    if (selection == 0){
      break
    }else{
      #Prompt for event info
      event_info <- gather_event_info()
      event_info$season <- get_season(event_info$date)
      
      #Scrape distance event
      if (event_info$type == 'Distance'){
        dst_data <- dst_scrape(url = event_info$url$url,
                               event_info = event_info)
        if (!is.na(event_info$url$live_url)){
          dst_split_data <- dst_split_scrape(event_info$url$live_url,
                                             dst_data$race)
        }
        event_url <- data.frame(eventid = dst_data$event$eventid[1],
                                url_type = "DST",
                                url = event_info$url$url)
        skiers <- process_skiers(dst_data[["skier"]],conl,update_bdays)
        
        if (!options()$fiscrape.debug){
          dbBegin(conl,name = "orig")
          cond <- tryCatch({
            #Upload to:
            # event table
            # skier table (if new skiers)
            # dst_result table
            # dst_pur_comb_times table if applicable
            # split times (if applicaable)
            # event_url table
            message("Inserting event...")
            insert_data(dst_data$event,"dst_event",conl)
            message("Inserting results...")
            insert_data(dst_data$result,"dst_result",conl)
            if (nrow(skiers) > 0){
              skiers <- add_bdays(skiers)
              message("Inserting skiers...")
              insert_data(skiers,"skier",conl)
            }
            if (!is.null(dst_data$pur_times)){
              message("Inserting pursuit times...")
              insert_data(dst_data$pur_times,"dst_pur_comb_times",conl)
            }
            if (!is.na(event_info$linked_pursuit)){
              message("Inserting pursuit link...")
              pur_link <- data.frame(eventid = dst_data$event$eventid[1],
                                     pur_eventid = event_info$linked_pursuit)
              insert_data(pur_link,"dst_pur_link",conl)
            }
            message("Inserting event URLs...")
            insert_data(event_url,"event_urls",conl)
            if (exists("dst_split_data") && inherits(dst_split_data,"data.frame")){
              message("Inserting dst splits...")
              insert_data(dst_split_data,"dst_splits",conl)
            }
            TRUE
          },error = try_handler,warning = try_handler,finally = try_finally())
          if (!cond){
            stop("Insert failed.")
          }
        }else{
          browser()
        }
        
      }
      
      #Scrape stage event
      if (event_info$type == 'Stage'){
        stg_data <- stg_scrape(url = event_info$url,
                               event_info = event_info)
        event_url <- data.frame(eventid = stg_data$event$eventid[1],
                                url_type = "DST",
                                url = event_info$url)
        stg_event_link <- data.frame(ov_eventid = rep(stg_data$event$eventid[1],
                                                      times = length(event_info$linked_stages)),
                                     stg_eventid = event_info$linked_stages)
        skiers <- process_skiers(stg_data[["skier"]],conl,update_bdays)
        
        if (!options()$fiscrape.debug){
          dbBegin(conl,name = "orig")
          cond <- tryCatch({
            #Upload to:
            # event table
            # skier table (if new skiers)
            # stg_result table
            # stg_race_link table
            # event_url table
            insert_data(stg_data$event,"stg_event",conl)
            insert_data(stg_data$result,"stg_result",conl)
            insert_data(stg_event_link,"stg_event_link",conl)
            if (nrow(skiers) > 0){
              skiers <- add_bdays(skiers)
              insert_data(skiers,"skier",conl)
            }
            insert_data(event_url,"event_urls",conl)
            TRUE
          },error = try_handler,warning = try_handler,finally = try_finally())
          if (!cond){
            stop("Insert failed.")
          }
        }else {
          browser()
        }
        
      }
      
      #Scrape sprint event
      if (event_info$type == 'Sprint'){
        spr_qual_data <- NULL
        spr_fin_data_list <- NULL
        
        if (!is.na(event_info$url$qual)){
          n_qual <- length(c(na.omit(event_info$url$qual)))
          spr_qual_data <- spr_qual_scrape(url = event_info$url$qual,
                                           event_info = event_info)
          skiers <- process_skiers(spr_qual_data[["skier"]],conl,update_bdays)
        }
        if (!is.na(event_info$url$final[1])){
          n_fin <- length(c(na.omit(event_info$url$final)))
          spr_fin_data_list <- vector(mode = "list",
                                      length = n_fin)
          spr_fin_heat_list <- vector(mode = "list",
                                      length = n_fin)
          skiers_list <- vector(mode = "list",
                                length = n_fin)
          for (i in seq_len(n_fin)){
            spr_fin_data_list[[i]] <- spr_final_scrape(event_info,i)
            skiers_list[[i]] <- process_skiers(spr_fin_data_list[[i]][["skier"]],conl,update_bdays)
            if (!is.na(event_info$url$heats[i])){
              spr_fin_heat_list[[i]] <- spr_heat_scrape(url = event_info$url$heats[i],
                                                        race = spr_fin_data_list[[i]]$race)
            }
          }
          spr_fin_heat <- bind_rows(spr_fin_heat_list)
        }
        
        #browser()
        skiers <- bind_rows(c(skiers,bind_rows(skiers_list))) %>%
          distinct()
        event <- bind_rows(c(spr_qual_data$event,bind_rows(lapply(spr_fin_data_list,'[[',"event")))) %>%
          distinct()
        
        spr_url_types <- rep(c("SPQ","SPF"),times = c(n_qual,n_fin))
        spr_urls <- c(na.omit(c(event_info$url$qual,event_info$url$final)))
        event_url <- data.frame(eventid = rep(event$eventid[1],times = length(spr_urls)),
                                url_type = spr_url_types,
                                url = spr_urls)
        
        if (!options()$fiscrape.debug){
          dbBegin(conl,name = "orig")
          cond <- tryCatch({
            #Upload to:
            # event table
            # skier table (if new skiers)
            # spr_qual_result table
            # spr_fin_result table
            # heat time data (if applicable)
            # event_url table
            insert_data(event,"spr_event",conl)
            if (nrow(skiers) > 0){
              skiers <- add_bdays(skiers)
              insert_data(skiers,"skier",conl)
            }
            if (!is.null(spr_qual_data)){
              insert_data(spr_qual_data$result,"spr_qual_result",conl)
            }
            if (!is.null(spr_fin_data_list)){
              for (i in seq_len(n_fin))
                insert_data(spr_fin_data_list[[i]]$result,"spr_fin_result",conl)
            }
            if (nrow(spr_fin_heat) > 0){
              insert_data(spr_fin_heat,"spr_fin_heats",conl)
            }
            insert_data(event_url,"event_urls",conl)
            TRUE
          },error = try_handler,warning = try_handler,finally = try_finally())
          if (!cond){
            stop("Insert failed.")
          }
        }else {
          browser()
        }
      }
    }
  }
}