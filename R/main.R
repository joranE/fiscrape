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
        event_url <- data.frame(eventid_sq = NA_character_,
                                eventid_sf = NA_character_,
                                eventid = dst_data$event$eventid[1],
                                url_type = "DST",
                                url = event_info$url$url)
        skiers <- process_skiers(dst_data[["skier"]],conl,update_bdays)
        
        if (!options()$fiscrape.debug){
            #Upload to:
            # event table
            # skier table (if new skiers)
            # dst_result table
            # dst_pur_comb_times table if applicable
            # split times (if applicaable)
            # event_url table
          DBI::dbWithTransaction(conl,{
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
            insert_data(event_url,"event_url",conl)
            if (exists("dst_split_data") && inherits(dst_split_data,"data.frame") && !is.null(dst_split_data)){
              message("Inserting dst splits...")
              insert_data(dst_split_data,"dst_splits",conl)
              dst_split_data <- NULL
            }
          })
        }else{
          browser()
        }
        
      }
      
      #Scrape stage event
      if (event_info$type == 'Stage'){
        stg_data <- stg_scrape(url = event_info$url$url,
                               event_info = event_info)
        event_url <- data.frame(eventid_sq = NA_character_,
                                eventid_sf = NA_character_,
                                eventid = stg_data$event$eventid[1],
                                url_type = "DST",
                                url = event_info$url$url)
        stg_event_link <- data.frame(ov_eventid = rep(stg_data$event$eventid[1],
                                                      times = length(event_info$linked_stages)),
                                     stg_eventid = event_info$linked_stages)
        skiers <- process_skiers(stg_data[["skier"]],conl,update_bdays)

        if (!options()$fiscrape.debug){
            #Upload to:
            # event table
            # skier table (if new skiers)
            # stg_result table
            # stg_race_link table
            # event_url table
          DBI::dbWithTransaction(conl,{
            insert_data(stg_data$event,"stg_event",conl)
            insert_data(stg_data$result,"stg_result",conl)
            insert_data(stg_event_link,"stg_event_link",conl)
            if (nrow(skiers) > 0){
              skiers <- add_bdays(skiers)
              insert_data(skiers,"skier",conl)
            }
            insert_data(event_url,"event_url",conl)
          })
        }else {
          browser()
        }
        
      }
      
      #Scrape sprint event
      if (event_info$type == 'Sprint'){
        spr_qual_data <- NULL
        spr_fin_data_list <- NULL
        skiers_list <- NULL
        spr_fin_heat <- NULL
        n_fin <- length(c(na.omit(event_info$url$final)))
        
        if (!is.na(event_info$url$qual)){
          spr_qual_data <- spr_qual_scrape(url = event_info$url$qual,
                                           event_info = event_info)
          qual_skiers <- process_skiers(spr_qual_data[["skier"]],conl,update_bdays)
        }
        if (!is.na(event_info$url$final[1])){
          spr_fin_data_list <- vector(mode = "list",
                                      length = n_fin)
          spr_fin_heat_list <- vector(mode = "list",
                                      length = n_fin)
          fin_skiers <- vector(mode = "list",
                                length = n_fin)
          for (i in seq_len(n_fin)){
            spr_fin_data_list[[i]] <- spr_final_scrape(event_info,i)
            new_fin_skiers <- anti_join(spr_fin_data_list[[i]][["skier"]],
                                        qual_skiers,by = c("compid","fisid","name","yob"))
            if (nrow(new_fin_skiers) > 0){
              fin_skiers[[i]] <- process_skiers(new_fin_skiers,conl,FALSE)
            }else {
              fin_skiers[[i]] <- NULL
            }
            
            if (!is.na(event_info$url$heats[i])){
              spr_fin_heat_list[[i]] <- spr_heat_scrape(url = event_info$url$heats[i],
                                                        race = spr_fin_data_list[[i]]$race)
            }
          }
          if (any(!is.na(event_info$url$heats))){
            spr_fin_heat_list <- setNames(spr_fin_heat_list,LETTERS[seq_len(n_fin)])
            spr_fin_heat <- bind_rows(spr_fin_heat_list,.id = "sf_id") %>%
              mutate(eventid_sf = paste0(eventid,sf_id)) %>%
              select(-sf_id) %>%
              select(eventid_sf,everything())
          }
        }
        
        #browser()
        skiers <- bind_rows(qual_skiers,bind_rows(fin_skiers)) %>%
          distinct()
        event <- bind_rows(c(spr_qual_data$event,bind_rows(lapply(spr_fin_data_list,'[[',"event")))) %>%
          select(-format) %>%
          distinct()
        
        spr_url_types <- rep(c("SPQ","SPF"),times = c(1,n_fin))
        spr_urls <- c(na.omit(c(event_info$url$qual,event_info$url$final)))
        event_url <- data.frame(eventid_sq = paste0("SQ",event_info$url$qual),
                                eventid_sf = paste0(event_info$url$final,LETTERS[1:n_fin]),
                                eventid = rep(event$eventid[1],times = length(spr_urls)),
                                url_type = spr_url_types,
                                url = spr_urls)
        
        if (!options()$fiscrape.debug){
            #Upload to:
            # event table
            # skier table (if new skiers)
            # spr_qual_result table
            # spr_fin_result table
            # heat time data (if applicable)
            # event_url table
          DBI::dbWithTransaction(conl,{
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
            if (!is.null(spr_fin_heat) && nrow(spr_fin_heat) > 0){
              insert_data(spr_fin_heat,"spr_fin_heats",conl)
            }
            insert_data(event_url,"event_url",conl)
          })
        }else {
          browser()
        }
      }
    }
  }
}