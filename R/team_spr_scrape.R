team_spr_scrape <- function(url,event_info,event_type = "Team"){
  page <- fiscrape:::safe_retry_read_html(x = url)
  
  #Use @href here not @data-link
  compids <- page %>% 
    html_elements(xpath = "//*[contains(@href,'athlete-biography')]") %>% 
    html_attrs() %>%
    purrr::map(.x = .,.f = magrittr::extract2,"href") %>%
    stringr::str_extract("competitorid=[0-9]+") %>%
    stringr::str_replace("competitorid=","")
  compids <- compids[-(1:3)]
  
  page_tbl <- page %>%
    html_nodes(css = ".g-row") %>%
    map(.f = ~html_children(.x) %>% html_text() %>% stringr::str_trim())
  
  flag_headers <- function(x) {
    length(x) == 1 && x %in% c("Final","Semifinal",
                               "Quarterfinal","Qualification")
  }
  page_tbl <- purrr::discard(.x = page_tbl,.p = flag_headers)
  
  first_row <- min(which(sapply(page_tbl,function(x) x[1] == "Rank")))
  page_tbl <- page_tbl[first_row:length(page_tbl)]
  
  any_notes <- any(lengths(page_tbl) == 1)
  
  race <- page %>%
    html_nodes(css = ".g-row") %>%
    map(.f = ~html_children(.x) %>% html_text() %>% stringr::str_trim())
  cn <- keep(race,function(x) x[1] == "Rank")[[1]]
  race <- race %>%
    purrr::keep(~length(.) >= 6) %>%
    purrr::map(.f = function(x) setNames(x,cn[1:length(x)]))
  race <- race[-1]
  race <- race %>%
    setNames(.,compids)
  
  empty_str_na <- function(x) if_else(x == '',NA_character_,x)
  
  race <- bind_rows(!!!race,.id = "compid") %>%
    janitor::clean_names(.,case = "snake") %>%
    rename(fisid = fis_code,yob = year) %>%
    rename_at(.vars = vars(matches("tot_time")),.funs = function(x) "time") %>%
    mutate(rank = as.integer(stringr::str_trim(rank)),
           notes = NA_character_) %>%
    tidyr::fill(rank,.direction = 'down') %>%
    mutate(across(where(is.character),empty_str_na)) %>%
    tidyr::fill(nation,time,diff_time,.direction = 'down')
  
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
}