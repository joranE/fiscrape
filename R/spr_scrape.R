spr_qual_scrape <- function(url,raceInfo){
  #Same as distance case except rename rank to rankqual
  result <- dst_scrape(url = url,raceInfo = raceInfo)
  result[["rankqual"]] <- result[["rank"]]
  result[["rank"]] <- NA
  result
}

spr_final_scrape <- function(url){
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
    keep(~length(.x) >= 5) %>%
    get_td_text_spr_final2()
    #map_dfr(.x = .,.f = compose(get_td_text_spr_final,html_children))
  
  page_tbl <- page_tbl %>%
    filter(grepl("[0-9]",stringr::str_trim(rank))) %>%
    select(rank,name,yob,nation) %>%
    mutate(rank = as.integer(stringr::str_trim(rank)),
           name = stringr::str_trim(name),
           yob = as.integer(yob),
           nation = stringr::str_trim(nation)) %>%
    filter(!is.na(rank)) %>%
    mutate(compid = as.integer(compids[1:n()])) %>%
    select(compid,name,yob,nation,rank)
  
  page_tbl
}

combine_qual_final <- function(qual,final){
  cn <- colnames(qual)
  result <- qual %>%
    select(-rank) %>%
    left_join(final[,c('compid','rank')],by = 'compid')
  result <- result[,cn]
  
  result
}