stage_scrape <- function(url,raceInfo){
  #Load html
  page <- xml2::read_html(x = url)
  
  #Get competitor ids
  compids <- page %>% 
    html_nodes(xpath = "//*[contains(@data-link,'athlete-biography')]") %>% 
    html_attrs() %>%
    map(.x = .,.f = magrittr::extract2,"data-link") %>%
    stringr::str_extract("competitorid=[0-9]+") %>%
    stringr::str_replace("competitorid=","") %>%
    trim_compids()
  
  if (length(compids) == 0){
    compids <- page %>% 
      html_nodes(xpath = "//*[contains(@href,'athlete-biography')]") %>% 
      html_attrs() %>%
      map(.x = .,.f = magrittr::extract2,"href") %>%
      stringr::str_extract("competitorid=[0-9]+") %>%
      stringr::str_replace("competitorid=","") %>%
      trim_compids()
  }
  
  #Extract results table
  page_tbl <- page %>%
    html_nodes(css = ".g-row.justify-sb") %>%
    map(.f = compose(str_trim,html_text,html_children)) %>% 
    keep(~length(.x) > 5) %>%
    get_td_text_stage2()
    #map_dfr(.x = .,.f = compose(get_td_text_stage,html_children))
  #browser()
  #Final packaging
  page_tbl <- page_tbl %>%
    filter(grepl("[0-9]",stringr::str_trim(rank))) %>%
    select(rank,fisid,name,yob,nation,time) %>%
    mutate(rank = as.integer(stringr::str_trim(rank)),
           name = stringr::str_trim(name),
           yob = as.integer(yob),
           nation = stringr::str_trim(nation),
           time = stringr::str_trim(time),
           fispoints = NA_real_) %>%
    filter(!is.na(rank)) %>%
    mutate(compid = as.integer(compids[1:n()]),
           raceid = get_max_raceid() + 1,
           date = raceInfo[["date"]],
           season = raceInfo[["season"]],
           cat1 = raceInfo[["cat1"]],
           cat2 = raceInfo[["cat2"]],
           location = raceInfo[["location"]],
           gender = raceInfo[["gender"]],
           type = raceInfo[["type"]],
           start = raceInfo[["start"]],
           tech = raceInfo[["tech"]],
           length = raceInfo[["length"]],
           age = as.integer(substr(raceInfo[["date"]],1,4)) - yob,
           rankqual = NA_integer_,
           time = convertTime(time,raceInfo[["type"]])) %>%
    select(raceid,date,season,location,gender,length,tech,
           type,start,cat1,cat2,fisid,name,yob,age,
           nation,rank,rankqual,time,fispoints,compid)
  
  #Space for name check
  page_tbl <- check_names(page_tbl)
  
  #Age sanity check
  page_tbl <- page_tbl %>%
    mutate(age = ifelse(!is.na(age) & (age < 11 | age > 70),NA_integer_,age))
  
  page_tbl
}