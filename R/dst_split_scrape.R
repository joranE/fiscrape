#' Scrape Distance Split Times
#' 
#' @importFrom purrr safely
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
  
  time_pts_val <- time_pts_val[!grepl("bonus",time_pts_val)]
  
  if (length(time_pts_lab) != length(time_pts_val)){
    message("Split point values and labels have different lengths. Fixing...")
    #browser()
    n <- length(time_pts_val)
    time_pts_val <- time_pts_val[-c(2,3,n-1)]
  }
  time_pts <- data.frame(val = time_pts_val,
                         lab = time_pts_lab,stringsAsFactors = F) %>%
    filter(!grepl("^Start|^Bonus",lab))
  
  #Verify split time points
  print(time_pts)
  chc <- menu(choices = c("Yes","No"),title = "Are these split time points correct?")
  if (chc == 2){
    time_pts <- edit(time_pts)
    time_pts <- time_pts %>%
      filter(!is.na(val))
  }
  
  splits <- vector(mode = "list",length = nrow(time_pts))
  splits <- setNames(splits,time_pts$lab)
  read_html_safely <- purrr::safely(.f = xml2::read_html)
  for (i in seq_len(nrow(time_pts))){
    url_split <- gsub(pattern = ".htm",
                      replacement = paste0("-",time_pts$val[i],".htm"),url,fixed = TRUE)
    html <- read_html_safely(x = url_split)
    if (is.null(html$error)){
      splits[[i]] <- html$result %>%
        parse_split_html()
    }else {
      next
    }
  }
  
  all_splits <- bind_rows(splits,.id = "split_km") %>%
    mutate(name = stringr::str_trim(name),
           name = stringr::str_squish(name)) %>%
    mutate(split_unit = stringr::str_extract(split_km,"[a-zA-Z]+$"),
           split_km = gsub("[^0-9.]","",split_km),
           split_km = as.numeric(stringr::str_trim(split_km)),
           split_km = if_else(split_unit == "m",split_km / 1000,split_km)) %>%
    select(-split_unit) %>%
    mutate(split_time = if_else(grepl(pattern = "[a-zA-Z]",split_time),"",split_time)) %>%
    mutate(split_time = time_to_seconds(split_time)) %>%
    mutate(split_rank = as.integer(split_rank))
  
  all_splits <- all_splits %>%
    rename(split_name = name) %>%
    left_join(select(race,eventid,compid,race_name = name),by = c("split_name" = "race_name")) %>%
    select(eventid,compid,everything())
  
  if (all(is.na(all_splits$split_km))){
    message("All split_km values are NA.")
    browser()
  }
  
  if (any(is.na(all_splits$compid))){
    max_split <- max(all_splits$split_km)
    all_splits <- split(all_splits,is.na(all_splits$compid))
    n <- length(all_splits)
    final_posn <- filter(all_splits[[n]],split_km == max_split)
    final_posn_race_names <- race %>%
      filter(rank %in% final_posn$split_rank) %>%
      select(eventid,compid,rank) %>%
      left_join(select(final_posn,split_rank,split_name),by = c("rank" = "split_rank")) %>%
      select(eventid,compid,split_name) %>%
      filter(!is.na(compid)) %>%
      distinct()
    
    idx <- match(all_splits[[n]]$split_name,final_posn_race_names$split_name)
    all_splits[[n]]$eventid <- final_posn_race_names$eventid[idx]
    all_splits[[n]]$compid <- final_posn_race_names$compid[idx]
    
    if (any(is.na(all_splits[[n]]$compid))){
      message("Cannot match the skiers in 'all_splits[[2]]' to compids. They may be DNSs.")
      browser()
    }
    all_splits <- bind_rows(all_splits) %>%
      select(eventid,compid,split_km,split_rank,split_time)
  } else {
    all_splits <- all_splits %>%
      select(eventid,compid,split_km,split_rank,split_time)
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