strage_scrape <- function(url,raceInfo){
  #Load html
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
  
  if ("FIS Code" %in% colnames(page_tbl)){
    page_tbl <- page_tbl %>%
      rename(fisid = `FIS Code`)
  }else{
    page_tbl$fisid <- NA_character_
  }
  
  #Final packaging
  page_tbl <- page_tbl %>%
    filter(grepl("[0-9]",stringr::str_trim(Rank))) %>%
    select(Rank,fisid,Name,Year,Nation,Time) %>%
    mutate(Rank = as.integer(stringr::str_trim(Rank)),
           Name = stringr::str_trim(Name),
           Year = as.integer(Year),
           Nation = stringr::str_trim(Nation),
           Time = stringr::str_trim(Time),
           fispoints = NA_real_) %>%
    filter(!is.na(Rank)) %>%
    rename(rank = Rank,
           name = Name,
           yob = Year,
           nation = Nation,
           time = Time) %>%
    mutate(compid = as.integer(compids[1:n()]),
           raceid = getMaxRaceID() + 1,
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