#' @import rvest
#' @import dplyr
#' @import xml2
#' @import purrr
#' @importFrom stringr str_trim
#' @importFrom stringr str_extract
#' @importFrom stringr str_replace
#' @importFrom stringr str_replace_all
#' @importFrom magrittr extract2
#' @export
dst_scrape <- function(url,event_info){
  message("Pulling dst or spr qual results...")
  #Load html
  page <- xml2::read_html(x = url)
  
  #Two attempts tp get competitor ids
  # First...
  compids <- page %>% 
    html_nodes(xpath = "//*[contains(@data-link,'athlete-biography')]") %>% 
    html_attrs() %>%
    map(.x = .,.f = magrittr::extract2,"data-link") %>%
    stringr::str_extract("competitorid=[0-9]+") %>%
    stringr::str_replace("competitorid=","") %>%
    trim_compids()
  
  # Second...
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
    rename_at(.vars = vars(ends_with("fis_points")),.funs = function(x) "fispoints") %>%
    mutate(rank = as.integer(stringr::str_trim(rank)),
           notes = NA_character_,
           eventid = get_max_eventid() + 1,
           compid = as.integer(compid))
  if ("fispoints" %ni% colnames(race)){
    race$fispoints <- NA_character_
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
  
  #Handle pursuit races with an overall time and a 'FIS points time' for that day
  # pur_time is the overall, combined time
  if ("fis_points_time" %in% colnames(race)){
    race <- race %>%
      rename(pur_time = time,
             time = fis_points_time,
             pur_rank = rank) %>%
      mutate(rank = as.integer(stringr::str_extract(rk,"[0-9]+")))
    pur_times <- race %>%
      mutate(pur_eventid = as.integer(eventid)) %>%
      select(pur_eventid,
             pur_compid = compid,
             pur_time) %>%
      mutate(pur_comb_time = time_to_seconds(pur_time)) %>%
      filter(!is.na(pur_comb_time)) %>%
      select(-pur_time)
  } else {
    pur_times <- NULL
  }
  
  #browser()
  #Final packaging
  race <- race %>%
    mutate(name = stringr::str_trim(name),
           name = stringr::str_squish(name),
           yob = as.integer(yob),
           nation = stringr::str_trim(nation),
           time = stringr::str_trim(time),
           fispoints = as.numeric(stringr::str_trim(fispoints))) %>%
    mutate(date = event_info[["date"]],
           season = event_info[["season"]],
           cat1 = event_info[["cat1"]],
           cat2 = event_info[["cat2"]],
           location = event_info[["location"]],
           gender = event_info[["gender"]],
           format = event_info[["format"]],
           tech = event_info[["tech"]],
           length = event_info[["length"]],
           time = time_to_seconds(time)) %>%
    mutate(pb = (time - min(time,na.rm = TRUE)) / min(time,na.rm = TRUE),
           pbm = (time - median(time,na.rm = TRUE)) / median(time,na.rm = TRUE),
           pbm_sd = sd(pbm,na.rm = TRUE),
           pbm_sd = if_else(is.na(time),NA_real_,pbm_sd))
  
  #race_penalty <- dst_race_penalty(result_data = race,event_date = event_info[["date"]])
  
  skier <- race %>%
    select(compid,fisid,name,yob) %>%
    mutate(compid = as.integer(compid),
           birth_date = NA_character_)
  event <- race %>%
    select(eventid,season,date,location,cat1,cat2,gender,length,format,tech) %>%
    distinct()
  result <- race %>%
    select(eventid,compid,nation,rank,time,pb,pbm,pbm_sd,fispoints,notes)
  return(list(event = event,
              skier = skier,
              result = result,
              pur_times = pur_times,
              race = race))
}

row_text_extractor <- function(x){
  cl <- rvest::html_attr(x,"class")
  if (!cl %in% c("g-row justify-sb","g-xs-24 bold","g-xs-24 container","container g-xs-24")){
    browser()
    stop("Encountered unknown row class in html:",cl)
  }
  if (cl %in% c("g-row justify-sb")){
    out <- x %>%
      html_children() %>%
      html_text() %>%
      stringr::str_trim()
  }
  if (cl %in% c("g-xs-24 bold","g-xs-24 container","container g-xs-24")){
    out <- x %>%
      html_text() %>%
      stringr::str_trim()
  }
  out
}

find_note_headers <- function(x){
  length(x) == 1 & grepl("start|finish|^sanction|disqualified|lapped",x[1],ignore.case = TRUE)
}

find_fisid <- function(x){
  u <- unlist(x)
  fisid <- u[grepl("[0-9]{7}",u)]
  if (length(fisid) == 0) return(NA_character_) 
  else return(fisid)
}

build_notes <- function(el,nm){
  fisids <- sapply(el,find_fisid)
  if (nm == "Sanctions"){
    notes <- sapply(el,tail,1)
  }else {
    notes <- rep(nm,length.out = length(fisids))
  }
  notes_df <- data.frame(fisid = fisids,
                         notes = notes,stringsAsFactors = FALSE)
  notes_df %>%
    filter(!is.na(fisid))
}