#' Scrape WC Sprint Heat Times
#' 
#' @export
spr_heat_scrape <- function(url,race){
  message("Pulling spr final heats...")
  url_base <- gsub(pattern = ".htm",replacement = "-%s-%s-99.htm",url,fixed = TRUE)
  
  spr_heats <- vector(mode = "list",length = 8)
  heat_params <- list(c(1,1),c(1,2),c(1,3),c(1,4),c(1,5),c(2,1),c(2,2),c(3,1))
  for (i in seq_along(spr_heats)){
    url_q <- sprintf(url_base,heat_params[[i]][1],heat_params[[i]][2])
    spr_heats[[i]] <- read_html(x = url_q) %>%
      parse_heat_html(.,round = heat_params[[i]])
  }
  spr_heats_clean <- bind_rows(spr_heats) %>%
    mutate(name = stringr::str_trim(name),
           name = stringr::str_squish(name),
           heat_rank = if_else(rank == "",NA_character_,rank),
           qf = if_else(substr(heat,1,1) == "1",substr(heat,2,2),NA_character_),
           sf = if_else(substr(heat,1,1) == "2",substr(heat,2,2),NA_character_),
           fn = if_else(substr(heat,1,1) == "3",substr(heat,2,2),NA_character_),
           ll = if_else(grepl("^LL",time),"Y","N")) %>%
    mutate(heat_rank = as.integer(heat_rank),
           time = gsub("^LL","",time),
           time = stringr::str_trim(time,side = "both"),
           time = if_else(time %in% c("DNF","DNS","RAL","DSQ"),"",time)) %>%
    mutate(time = time_to_seconds(time)) %>%
    select(-rank) %>%
    rename(heat_time = time)
  
  #Check for mismatched names
  race_names <- select(race,eventid,compid,name)
  name_check <- spr_heats_clean %>%
    left_join(race_names,by = "name") %>%
    mutate(eventid = race$eventid[1]) %>%
    dplyr::mutate_if(.predicate = bit64::is.integer64,.funs = as.integer)
  if (any(is.na(name_check$compid))){
    print(arrange(race_names,name))
    name_check <- edit(name = name_check)
    spr_heats_clean <- name_check %>%
      select(-name,-nation) %>%
      select(eventid,compid,everything())
  } else {
    spr_heats_clean <- spr_heats_clean %>%
      left_join(race_names,by = "name") %>%
      select(-name,-nation) %>%
      select(eventid,compid,everything())
  }
  
  spr_heats_clean <- spr_heats_clean %>%
    select(eventid,compid,heat,everything())
  
}

parse_heat_html <- function(x,round){
  rank <- html_nodes(x,css = ".col_rank") %>% html_text()
  bib <- html_nodes(x,css = ".col_bib") %>% html_text()
  name <- html_nodes(x,css = ".col_name") %>% html_text()
  nsa <- html_nodes(x,css = ".col_nsa") %>% html_text()
  result <- html_nodes(x,css = ".col_result") %>% html_text()
  diff <- html_nodes(x,css = ".col_diff") %>% html_text()
  
  rank <- rank[-1]
  name <- name[-1]
  nsa <- nsa[-1]
  result <- result[-1]
  
  if (length(rank) == 0){
    rank <- name <- nsa <- result <- NA_character_
  }
  
  data.frame(heat = paste(round,collapse = ""),
             rank = rank,
             name = name,
             nation = nsa,
             time = result,
             stringsAsFactors = FALSE)
}