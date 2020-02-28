#' Scrape Distance Split Times
#' 
#' @export
dst_split_scrape <- function(url,race){
  message("Pulling dst split times...")
  html <- read_html(x = url)
  
  #Check if they are available
  noresult <- html %>%
    html_nodes(css = ".noresult")
  
  if (length(noresult) > 0){
    message("No live results available...")
    return(NULL)
  }
  
  time_pts_lab <- html %>%
    html_nodes("option") %>%
    html_text() %>%
    unique()
  time_pts_val <- html %>%
    html_nodes("option") %>%
    html_attrs() %>%
    unlist() %>%
    unique()
  time_pts <- data.frame(val = time_pts_val,
                         lab = time_pts_lab,stringsAsFactors = F) %>%
    filter(!grepl("^Start|^Bonus",lab))
  
  #Verify split time points
  print(time_pts)
  chc <- menu(choices = c("Yes","No"),title = "Are these split time points correct?")
  if (chc == 2){
    time_pts <- edit(time_pts)
  }
  
  splits <- vector(mode = "list",length = nrow(time_pts))
  splits <- setNames(splits,time_pts$lab)
  for (i in seq_len(nrow(time_pts))){
    url_split <- gsub(pattern = ".htm",
                      replacement = paste0("-",time_pts$val[i],".htm"),url,fixed = TRUE)
    splits[[i]] <- read_html(x = url_split) %>%
      parse_split_html()
  }
  
  #browser()
  all_splits <- bind_rows(splits,.id = "split_km") %>%
    mutate(name = stringr::str_trim(name),
           name = stringr::str_squish(name)) %>%
    mutate(split_unit = stringr::str_extract(split_km,"[a-zA-Z]+$"),
           split_km = gsub("[^0-9.]","",split_km),
           split_km = as.numeric(stringr::str_trim(split_km)),
           split_km = if_else(split_unit == "m",split_km / 1000,split_km)) %>%
    select(-split_unit) %>%
    mutate(split_time = if_else(grepl(pattern = "[a-zA-Z]",split_time),"",split_time)) %>%
    mutate(split_time = fiscrape::time_to_seconds(split_time)) %>%
    mutate(split_rank = as.integer(split_rank))
  
  all_splits <- all_splits %>%
    left_join(select(race,eventid,compid,name),by = "name") %>%
    select(-name,-nation) %>%
    select(eventid,compid,everything())
  
  if (any(is.na(all_splits$compid))){
    browser()
  }
  
  all_splits
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