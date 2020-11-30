#' @export
stg_scrape <- function(url,event_info,event_type){
  #Load html
  #browser()
  page <- xml2::read_html(x = url)
  
  #Two attempts tp get competitor ids
  compids <- page %>% 
    html_nodes(xpath = "//*[contains(@data-link,'athlete-biography')]") %>% 
    html_attrs() %>%
    purrr::map(.x = .,.f = magrittr::extract2,"data-link") %>%
    stringr::str_extract("competitorid=[0-9]+") %>%
    stringr::str_replace("competitorid=","") %>%
    trim_compids()
  
  if (length(compids) == 0){
    compids <- page %>% 
      html_nodes(xpath = "//*[contains(@href,'athlete-biography')]") %>% 
      html_attrs() %>%
      purrr::map(.x = .,.f = magrittr::extract2,"href") %>%
      stringr::str_extract("competitorid=[0-9]+") %>%
      stringr::str_replace("competitorid=","") %>%
      trim_compids()
  }
  
  # All rows with sanctions
  page_tbl <- page %>%
    html_nodes(css = ".g-row.justify-sb,.g-xs-24.bold,.g-xs-24.container") %>%
    purrr::map(.f = row_text_extractor)
  
  #Remove garbarge leading rows, start with row beginning with 'Rank'
  first_row <- min(which(sapply(page_tbl,function(x) x[1] == "Rank")))
  page_tbl <- page_tbl[first_row:length(page_tbl)]
  any_notes <- any(lengths(page_tbl) == 1)
  
  # All rows without sanctions
  race <- page %>%
    html_nodes(css = ".g-row.justify-sb") %>%
    purrr::map(.f = row_text_extractor)
  cn <- purrr::keep(race,function(x) x[1] == "Rank")[[1]]
  race <- race %>%
    purrr::keep(~length(.) >= 5) %>%
    purrr::map(.f = function(x) setNames(x,cn[1:length(x)]))
  race <- race[-1]
  race <- race %>%
    setNames(.,compids)
  race <- bind_rows(!!!race,.id = "compid") %>%
    janitor::clean_names(.,case = "snake") %>%
    rename(fisid = fis_code,name = athlete,
           yob = year) %>%
    rename_at(.vars = vars(matches("fis_points")),.funs = function(x) "fispoints") %>%
    mutate(rank = as.integer(stringr::str_trim(rank)),
           notes = NA_character_)
  
  if (!"fispoints" %in% colnames(race)){
    race$fispoints <- NA_real_
  }
  
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
      race$notes[race$fisid %in% notes_list[[i]]$fisid] <- notes_list[[i]]$notes
    }
  }
  
  #Final packaging
  race <- race %>%
    mutate(name = stringr::str_trim(name),
           name = stringr::str_squish(name),
           yob = as.integer(yob),
           nation = stringr::str_trim(nation),
           time = stringr::str_trim(time),
           fispoints = as.numeric(stringr::str_trim(fispoints))) %>%
    mutate(eventid = get_max_eventid() + 1,
           date = event_info[["date"]],
           season = event_info[["season"]],
           cat1 = event_info[["cat1"]],
           cat2 = event_info[["cat2"]],
           location = event_info[["location"]],
           site = NA_character_,
           gender = event_info[["gender"]],
           format = event_info[["format"]],
           tech = event_info[["tech"]],
           length = event_info[["length"]],
           time = time_to_seconds(time)) %>%
    mutate(pb = (time - min(time,na.rm = TRUE)) / min(time,na.rm = TRUE),
           pbm = (time - median(time,na.rm = TRUE)) / median(time,na.rm = TRUE),
           pbm_sd = sd(pbm,na.rm = TRUE),
           pbm_sd = if_else(is.na(time),NA_real_,pbm_sd))
  
  skier <- race %>%
    select(compid,fisid,name,yob) %>%
    mutate(compid = as.integer(compid),
           yob = as.integer(yob),
           birth_date = NA_character_)
  event <- race %>%
    select(eventid,season,date,cat1,cat2,gender,length,tech) %>%
    distinct()
  event_tags1 <- data.frame(eventid = race$eventid[1],
                            tag = event_info[["primary_tag"]],
                            primary_tag = "Y")
  n_tags <- length(event_info[["other_tags"]])
  if (n_tags > 0){
    event_tags2 <- data.frame(eventid = rep(race$eventid[1],n_tags),
                              tag = event_info[["other_tags"]],
                              primary_tag = rep("N",n_tags))
    event_tags <- dplyr::bind_rows(event_tags1,
                                   event_tags2)
  } else {
    event_tags <- event_tags1
  }
  result <- race %>%
    select(eventid,compid,nation,rank,time,pb,pbm,pbm_sd,fispoints,notes)
  return(list(event = event,
              event_tags = event_tags,
              skier = skier,
              result = result))
}