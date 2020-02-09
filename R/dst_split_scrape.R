#' Scrape WC Sprint Heat Times
#' 
#' @export
dst_split_scrape <- function(){
  con_local <- statskier2::db_xc_local()
  maj_int <- tbl(src = con_local,"maj_int")
  while (TRUE){
    cat("Make a selection: \n")
    
    selection <- menu(c('Enter New Race'))
    if (selection == 0){
      break
    }else{
      #Prompt for race info
      dt <- readline(prompt = "Date: ")
      gndr <- switch(menu(c('Men','Women')),'Men','Women')
      
      race_info <- maj_int %>%
        filter(cat1 %in% c("WC","WSC","TDS","OWG") & 
                 date == dt & 
                 gender == gndr & 
                 type == "Distance") %>%
        select(raceid,date,gender,tech,length) %>%
        collect() %>%
        distinct()
      
      if (nrow(race_info) > 1){
        print(race_info)
        row <- readline(prompt = "Select row: ")
        race_info <- race_info[row,]
      }
      
      final_times <- maj_int %>%
        filter(raceid == !!race_info$raceid) %>%
        select(raceid,name,split_km = length,
               split_rank = rank,split_time = time) %>%
        collect() 
      
      urlFin <- readline(prompt  = "Splits URL: ")
      html <- read_html(x = urlFin)

      time_pts <- html %>%
        html_nodes("option") %>%
        html_text() %>%
        unique()
      time_pts <- time_pts[!grepl("Start",time_pts)]

      splits <- vector(mode = "list",length = length(time_pts))
      splits <- setNames(splits,time_pts)
      for (i in seq_along(time_pts)){
        if (i == length(splits)) val <- 99 else val <- i
        url_split <- gsub(pattern = ".htm",replacement = paste0("-",val,".htm"),urlFin,fixed = TRUE)
        splits[[i]] <- read_html(x = url_split) %>%
          parse_split_html()
      }

      all_splits <- bind_rows(splits,.id = "split_km") %>%
        mutate(split_km = gsub("km|Finish","",split_km),
              split_km = as.numeric(stringr::str_trim(split_km))) %>%
        mutate(dns = if_else(split_time == "DNS","Y","N"),
              dnf = if_else(split_time == "DNF","Y","N"),
              lapped = if_else(split_time == "LAPPED","Y","N")) %>%
        mutate(split_time = if_else(split_time %in% c("DNS","DNF","LAPPED"),"",split_time),
              split_time = if_else(grepl(pattern = "[a-zA-Z]",split_time),"",split_time)) %>%
        mutate(split_time = fiscrape:::convertTime(split_time,raceType = "Distance")) %>%
        mutate(raceid = final_times$raceid[1]) %>%
        mutate(split_rank = as.integer(split_rank)) %>%
        select(raceid,everything())
      
      # if (any(is.na(all_splits$raceid))){
      #   bad_match <- all_splits %>%
      #     filter(is.na(raceid)) %>%
      #     select(name,nation) %>%
      #     distinct()
      #   print(bad_match)
      #   stop("Unable to match these athletes by name.")
      # }
      
      pth <- "~/Dropbox/dst-splits/dst_splits.csv"
      if (!file.exists(pth)){
        write.table(x = all_splits,file = pth,sep = ",",quote = TRUE,row.names = FALSE)
      } else{
        write.table(x = all_splits,
                    file = pth,
                    sep = ",",
                    quote = TRUE,
                    row.names = FALSE,
                    col.names = FALSE,
                    append = TRUE)
      }
      
    }
    
  }
}

parse_split_html <- function(x){
  rank <- html_nodes(x,css = ".col_rank") %>% html_text()
  bib <- html_nodes(x,css = ".col_bib") %>% html_text()
  name <- html_nodes(x,css = ".col_name") %>% html_text()
  nsa <- html_nodes(x,css = ".col_nsa") %>% html_text()
  result <- html_nodes(x,css = ".col_result") %>% html_text()
  diff <- html_nodes(x,css = ".col_diff") %>% html_text()
  
  data.frame(split_rank = rank,
             name = name,
             nation = nsa,
             split_time = result,
             stringsAsFactors = FALSE)
}