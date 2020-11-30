add_spr_final <- function(){
  src_spr_ev <- tbl(conl,dbplyr::in_schema(options()$fiscrape.schema,"spr_event"))
  src_spr_fin <- tbl(conl,dbplyr::in_schema(options()$fiscrape.schema,"spr_fin_result"))
  .eventid <- readline(prompt = "Spr eventid: ")
  .eventid <- as.integer(.eventid)
  
  ev_info <- src_spr_ev %>%
    filter(eventid == .eventid) %>%
    collect()
  ev_fin <- src_spr_fin %>%
    filter(eventid == .eventid) %>%
    collect()
  
  max_idx <- max(stringr::str_extract(ev_fin$eventid_sf,"[A-Z]"))
  new_idx <- which(LETTERS == max_idx) + 1
  
  .cat <- readline(prompt = "Spr Final Cat: ")
  .url <- readline(prompt = "Spr Final URL: ")
  
  .event_info <- list(date = ev_info$date,
                      season = ev_info$season,
                      gender = ev_info$gender,
                      location = ev_info$location,
                      length = ev_info$length,
                      tech = ev_info$tech,
                      cat1 = ev_info$cat1,
                      cat2 = ev_info$cat2,
                      url = list(final = c(rep(NA,new_idx - 1),.url),
                                 cat = c(rep(NA,new_idx - 1),.cat))
  )
  
  spr_fin_res <- spr_final_scrape(event_info = .event_info,
                                  idx = new_idx,
                                  override_eventid = .eventid)
  browser()
  DBI::dbWithTransaction(conl,{
    insert_data(spr_fin_res$result,"spr_fin_result",conl)
  })
}