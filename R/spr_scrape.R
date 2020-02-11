#' @export
spr_qual_scrape <- function(url,race_info){
  #Same as distance case except rename rank to rankqual
  spr_qual_out <- dst_scrape(url = url,race_info = race_info)
  spr_qual_out[["result"]] <- spr_qual_out[["result"]] %>%
    rename(rankqual = rank)
  spr_qual_out
}

spr_final_scrape <- function(url,race_info){
  #Load html
  page <- xml2::read_html(x = url)
  
  #Two attempts tp get competitor ids
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
  
  # All rows with sanctions
  page_tbl <- page %>%
    html_nodes(css = ".g-row.justify-sb,.g-xs-24.bold,.g-xs-24.container") %>%
    map(.f = row_text_extractor)
  
  #Remove garbarge leading rows, start with row beginning with 'Rank'
  first_row <- min(which(sapply(page_tbl,function(x) x[1] == "Rank")))
  page_tbl <- page_tbl[first_row:length(page_tbl)]
  any_notes <- any(lengths(page_tbl) == 1)
  
  # All rows without sanctions
  race <- page %>%
    html_nodes(css = ".g-row.justify-sb") %>%
    map(.f = row_text_extractor)
  cn <- keep(race,function(x) x[1] == "Rank")[[1]]
  race <- race %>%
    keep(~length(.) >= 5) %>%
    map(.f = function(x) setNames(x,cn[1:length(x)]))
  race <- race[-1]
  race <- race %>%
    setNames(.,compids)
  race <- bind_rows(!!!race,.id = "compid") %>%
    select(-Bib) %>%
    janitor::clean_names(.,case = "snake") %>%
    rename(fisid = fis_code,name = athlete,
           yob = year) %>%
    rename_at(.vars = vars(matches("fis_points")),.funs = function(x) "fispoints") %>%
    mutate(rank = as.integer(stringr::str_trim(rank)),
           notes = NA_character_)
  
  if (any_notes){
    # Add notes about DNS, DNF, DSQ, sanctions, etc.
    first_note <- min(which(lengths(page_tbl) == 1))
    notes <- page_tbl[first_note:length(page_tbl)]
    note_compids <- compids[(first_note - 1):length(compids)]
    
    #Split notes
    notes_list <- split(x = notes,f = cumsum(sapply(notes,find_note_headers)))
    notes_list <- setNames(lapply(notes_list,`[`,-1),lapply(notes_list,`[[`,1))
    notes_fisids <- lapply(notes_list,find_fisid)
    
    #Transfer DNS, DNF, etc info to notes column
    for (i in names(notes_list)){
      if (i == 'Sanctions'){
        sanctions <- sapply(notes_list[[i]][-1],tail,1)
        race$notes[race$fisid %in% notes_fisids[[i]]] <- sanctions
      }else {
        race$notes[race$fisid %in% notes_fisids[[i]]] <- i
      }
    }
  }
  
  #Final packaging
  race <- race %>%
    mutate(name = stringr::str_trim(name),
           yob = as.integer(yob),
           nation = stringr::str_trim(nation)) %>%
    mutate(raceid = get_max_raceid() + 1,
           date = race_info[["date"]],
           season = race_info[["season"]],
           cat1 = race_info[["cat1"]],
           cat2 = race_info[["cat2"]],
           location = race_info[["location"]],
           gender = race_info[["gender"]],
           tech = race_info[["tech"]],
           length = race_info[["length"]]) 
  
  skier <- race %>%
    select(compid,fisid,name,yob) %>%
    mutate(birth_date = NA_character_)
  event <- race %>%
    select(raceid,season,date,location,cat1,cat2,gender,length,tech) %>%
    distinct()
  result <- race %>%
    select(raceid,compid,nation,rank,notes)
  return(list(event = event,
              skier = skier,
              result = result))
}
