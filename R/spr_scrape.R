#' @export
spr_qual_scrape <- function(url,event_info,event_type = "Sprint"){
  #Same as distance case except rename rank to rankqual
  spr_qual_out <- dst_scrape(url = url,event_info = event_info,event_type = event_type)
  spr_qual_out[["result"]] <- spr_qual_out[["result"]] %>%
    rename(rankqual = rank) %>%
    mutate(eventid_sq = paste0("SQ",eventid)) %>%
    select(eventid_sq,everything())
  spr_qual_out
}

spr_final_scrape <- function(event_info,idx,override_eventid = NULL){
  message("Pulling spr final results...")
  #Load html
  page <- safe_retry_read_html(x = event_info$url$final[idx])
  
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
  
  #Remove Final, Semifinal, Quarterfinal & Qualification header rows
  flag_headers <- function(x) length(x) == 1 && x %in% c("Final","Semifinal","Quarterfinal","Qualification")
  page_tbl <- purrr::discard(.x = page_tbl,.p = flag_headers)
  
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
    select(-matches("Bib")) %>%
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
    
    names(notes_list) <- stringr::str_replace(names(notes_list),"2nd Run$|1st Run$","")
    names(notes_list) <- stringr::str_trim(names(notes_list),side = "both")
    
    notes_list <- purrr::imap(notes_list,build_notes)
    #Transfer DNS, DNF, etc info to notes column
    for (i in seq_along(notes_list)){
      cur_notes <- notes_list[[i]]
      cur_notes <- filter(cur_notes,fisid %in% race$fisid)
      if (nrow(cur_notes) == 0) {
        next
      }else {
        if (anyDuplicated(cur_notes$fisid)){
          cur_notes <- cur_notes %>%
            group_by(fisid) %>%
            summarise(notes = paste(notes,collapse = ", ")) %>%
            as.data.frame()
        }
        race$notes[race$fisid %in% cur_notes$fisid] <- cur_notes$notes
      }
    }
  }
  
  if (is.null(override_eventid)){
    ev_id <- get_max_eventid() + 1
  }else {
    ev_id <- override_eventid
  }
  
  #Final packaging
  race <- race %>%
    mutate(name = stringr::str_trim(name),
           name = stringr::str_squish(name),
           yob = as.integer(yob),
           nation = stringr::str_trim(nation)) %>%
    mutate(eventid = ev_id,
           eventid_sf = paste0(ev_id,LETTERS[idx]),
           date = event_info[["date"]],
           season = event_info[["season"]],
           cat1 = event_info[["cat1"]],
           cat2 = event_info[["cat2"]],
           spr_fin_cat = event_info$url$cat[idx],
           location = event_info[["location"]],
           gender = event_info[["gender"]],
           tech = event_info[["tech"]],
           length = event_info[["length"]]) 
  
  #race_penalty <- spr_race_penalty(result_data = race,event_date = event_info[["date"]])
  
  skier <- race %>%
    select(compid,fisid,name,yob) %>%
    mutate(compid = as.integer(compid),
           fisid = as.character(fisid),
           name = as.character(name),
           yob = as.integer(yob),
           birth_date = NA_character_)
  event <- race %>%
    select(eventid,season,date,location,cat1,cat2,gender,length,tech) %>%
    distinct()
  result <- race %>%
    select(eventid_sf,eventid,spr_fin_cat,compid,nation,rank,notes)
  return(list(event = event,
              skier = skier,
              result = result,
              race = race))
}
