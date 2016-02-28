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
    html_nodes(xpath = "//a[contains(@href,'biography')]/@href") %>% 
    html_text() %>%
    stringr::str_extract("competitorid=[0-9]+") %>%
    stringr::str_replace("competitorid=","")
  
  #Extract results table
  page_tbl <- page %>%
    html_table(header = TRUE,fill = TRUE) %>%
    magrittr::extract2(2) 
  page_tbl <- page_tbl[,!is.na(colnames(page_tbl))]
  
  if ("Rank" %ni% colnames(page_tbl)){
    while(TRUE){
      print(head(page_tbl))
      cat("\nI don't think this was the right table.\n")
      selection <- readline(prompt = 'Try table number...')
      page_tbl <- page %>%
        html_table(header = TRUE,fill = TRUE) %>%
        magrittr::extract2(selection)
      print(head(page_tbl))
      selection <- menu(c('Yes','No'))
      if (selection == 1) break
    }
  }
  
  #Ran into this once, and the check for it lives on...
  page_tbl[] <- lapply(page_tbl,function(x) {stringr::str_trim(gsub("Â","",x))})
  
  page_tbl <- page_tbl %>%
    filter(grepl("[0-9]",stringr::str_trim(Rank))) %>%
    select(Rank,Name,Year,Nation) %>%
    mutate(Rank = as.integer(stringr::str_trim(Rank)),
           Name = stringr::str_trim(Name),
           Year = as.integer(Year),
           Nation = stringr::str_trim(Nation)) %>%
    filter(!is.na(Rank)) %>%
    rename(rank = Rank,
           name = Name,
           yob = Year,
           nation = Nation) %>%
    mutate(compid = compids[1:n()]) %>%
    select(compid,name,yob,nation,rank)
  
  page_tbl
}

combine_qual_final <- function(qual,final){
  cn <- colnames(qual)
  result <- qual %>%
    select(-rank) %>%
    left_join(final[,c('compid','rank')],by = 'compid')
  result <- result[,cn]
  
  #Space for name check
  
  result
}