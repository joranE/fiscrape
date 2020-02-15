#' @export
lookup_skier_bday <- function(compid){
  skier_profile_url <- "https://www.fis-ski.com/DB/general/athlete-biography.html?sectorcode=CC&competitorid=%s"
  
  page <- read_html(x = sprintf(skier_profile_url,compid))
  node_info <- page %>%
    html_nodes("[id = 'Birthdate']") %>% 
    html_children() %>%
    html_text()
  bday <- node_info[2]
  
  .yob <- NA_integer_
  .birth_date <- NA_character_
  
  if (grepl("[0-9]{4}",bday)){
    type <- "yob"
  }
  if (grepl("[0-9]{2}-[0-9]{2}-[0-9]{4}",bday)){
    type <- "date"
  } else{
    type <- "missing"
  }
  
  if (type == "yob"){
    .yob <- bday
    .birth_date <- NA_character_
  }
  
  if (type == "date"){
    .birth_date <- as.character(as.Date(bday,"%d-%m-%Y"))
    .yob <- substr(.birth_date,1,4)
  }
  
  return(list(yob = .yob,birth_date = .birth_date))
}

#' @export
missing_bday <- function(skier_data,conl){
  src_skier <- tbl(conl,"skier")
  no_bday <- src_skier %>%
    filter(compid %in% local(skier_data$compid) & is.na(birth_date)) %>%
    collect()
  no_bday
}

#' @export
update_bdays <- function(skier_list,conl){
  message("Fetching bdays from athlete bios...")
  for (i in seq_len(nrow(skier_list))){
    bday <- lookup_skier_bday(compid = skier_list$compid[i])
    if (!is.na(bday$birth_date)){
      skier_list$birth_date[i] <- bday$birth_date
    }
  }
  
  skier_list_update <- skier_list %>%
    filter(!is.na(birth_date))
  if (!options()$fiscrape.debug){
    if (nrow(skier_list_update) > 0){
      message("Updating ",nrow(skier_list_update)," birth date(s)...")
      for (i in seq_len(nrow(skier_list_update))){
        compid <- skier_list_update$compid[i]
        bday <- skier_list_update$birth_date[i]
        q <- sprintf("update skier set birth_date = '%s' where compid = %s",bday,compid)
        RSQLite::dbBegin(conl,name = "bday")
        rs <- RSQLite::dbSendStatement(conl,q)
        RSQLite::dbClearResult(rs)
        RSQLite::dbCommit(conl,name = "bday")
      }
    }
  }else {
    browser()
  }
  
}

#' @export
add_bdays <- function(skier_list,conl){
  for (i in seq_len(nrow(skier_list))){
    bday <- lookup_skier_bday(compid = skier_list$compid[i])
    if (!is.na(bday$birth_date)){
      skier_list$birth_date[i] <- bday$birth_date
    }
  }
  skier_list
}