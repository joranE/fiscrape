#' Scrape WC Sprint Heat Times
#' 
#' @export
spr_heat_scrape <- function(){
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
                 type == "Sprint") %>%
        select(raceid,date,gender,tech,length) %>%
        collect() %>%
        distinct()
      
      if (nrow(race_info) > 1){
        print(race_info)
        row <- readline(prompt = "Select row: ")
        race_info <- race_info[row,]
      }
      
      qual_times <- maj_int %>%
        filter(raceid == !!race_info$raceid) %>%
        select(raceid,name,rankqual,rank,qual_time = time) %>%
        collect()
      
      urlFin <- readline(prompt  = "Finals URL: ")
      url_base <- gsub(pattern = ".htm",replacement = "-%s-%s-99.htm",urlFin,fixed = TRUE)
      
      spr_heats <- vector(mode = "list",length = 8)
      heat_params <- list(c(1,1),c(1,2),c(1,3),c(1,4),c(1,5),c(2,1),c(2,2),c(3,1))
      for (i in seq_along(spr_heats)){
        url_q <- sprintf(url_base,heat_params[[i]][1],heat_params[[i]][2])
        spr_heats[[i]] <- read_html(x = url_q) %>%
          parse_heat_html(.,round = heat_params[[i]])
      }
      browser()
      spr_heats_clean <- bind_rows(spr_heats) %>%
        mutate(name = stringr::str_trim(name),
               heat_rank = if_else(rank == "",NA_character_,rank),
               qf = if_else(substr(heat,1,1) == "1",substr(heat,2,2),NA_character_),
               sf = if_else(substr(heat,1,1) == "2",substr(heat,2,2),NA_character_),
               fn = if_else(substr(heat,1,1) == "3",substr(heat,2,2),NA_character_),
               dns = if_else(time == "DNS","Y","N"),
               dnf = if_else(time == "DNF","Y","N"),
               ral = if_else(time == "RAL","Y","N"),
               ll = if_else(grepl("^LL",time),"Y","N")) %>%
        mutate(heat_rank = as.integer(heat_rank),
               time = gsub("^LL","",time),
               time = stringr::str_trim(time,side = "both"),
               time = if_else(time %in% c("DNF","DNS","RAL","DSQ"),"",time)) %>%
        mutate(time = fiscrape:::convertTime(time,raceType = "Sprint")) %>%
        select(-rank) %>%
        rename(heat_time = time)
      
      spr_heats_clean <- spr_heats_clean %>%
        left_join(qual_times,by = "name") %>%
        select(raceid,name,rankqual,qual_time,heat,everything())
      
      if (any(is.na(spr_heats_clean$raceid))){
        bad_match <- spr_heats_clean %>%
          filter(is.na(raceid)) %>%
          select(name,nation) %>%
          distinct()
        print(bad_match)
        stop("Unable to match these athletes by name.")
      }
      
      pth <- "~/Dropbox/spr-splits/spr_splits.csv"
      if (!file.exists(pth)){
        write.table(x = spr_heats_clean,file = pth,sep = ",",quote = TRUE,row.names = FALSE)
      } else{
        write.table(x = spr_heats_clean,
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

parse_heat_html <- function(x,round){
  rank <- html_nodes(x,css = ".col_rank") %>% html_text()
  bib <- html_nodes(x,css = ".col_bib") %>% html_text()
  name <- html_nodes(x,css = ".col_name") %>% html_text()
  nsa <- html_nodes(x,css = ".col_nsa") %>% html_text()
  result <- html_nodes(x,css = ".col_result") %>% html_text()
  diff <- html_nodes(x,css = ".col_diff") %>% html_text()
  
  data.frame(heat = paste(round,collapse = ""),
             rank = rank[-1],
             name = name[-1],
             nation = nsa[-1],
             time = result[-1],
             stringsAsFactors = FALSE)
}