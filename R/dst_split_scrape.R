#' Scrape WC Distance Split Times
#' 
#' @export
dst_split_scrape <- function(url,eventid){
  html <- read_html(x = url)
  
  time_pts <- html %>%
    html_nodes("option") %>%
    html_text() %>%
    unique()
  time_pts <- time_pts[!grepl("Start",time_pts)]
  
  splits <- vector(mode = "list",length = length(time_pts))
  splits <- setNames(splits,time_pts)
  for (i in seq_along(time_pts)){
    if (i == length(splits)) val <- 99 else val <- i
    url_split <- gsub(pattern = ".htm",replacement = paste0("-",val,".htm"),url,fixed = TRUE)
    splits[[i]] <- read_html(x = url_split) %>%
      parse_split_html()
  }
  
  all_splits <- bind_rows(splits,.id = "split_km") %>%
    mutate(split_km = gsub("km|Finish","",split_km),
           split_km = as.numeric(stringr::str_trim(split_km))) %>%
    mutate(split_time = if_else(split_time %in% c("DNS","DNF","LAPPED"),"",split_time),
           split_time = if_else(grepl(pattern = "[a-zA-Z]",split_time),"",split_time)) %>%
    mutate(split_time = fiscrape::time_to_seconds(split_time)) %>%
    mutate(eventid = eventid) %>%
    mutate(split_rank = as.integer(split_rank)) %>%
    select(eventid,everything())
  
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