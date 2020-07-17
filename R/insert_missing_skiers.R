#' @export
insert_skiers <- function(compid,wait = 2){
  skier_profile_url <- "https://www.fis-ski.com/DB/general/athlete-biography.html?sectorcode=CC&competitorid=%s"
  
  skiers <- vector(mode = "list",length = length(compid))
  
  for (i in seq_along(compid)){
    message("Looking up compid ",compid[i])
    safe_html <- purrr::safely(xml2::read_html)
    page <- safe_html(x = sprintf(skier_profile_url,compid[i]))
    
    if (is.null(page$result)){
      message("Encountered a problem, skipping.")
      next
    }else{
      page <- page$result
    }
    
    #Birth date & yob
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
    
    #Skier name
    full_name <- page %>%
      html_nodes("[class = 'athlete-profile__name']") %>% 
      html_text() %>%
      stringr::str_squish()
    last_name <- page %>%
      html_nodes("[class = 'athlete-profile__lastname']") %>% 
      html_text() %>%
      stringr::str_squish()
    first_name <- stringr::str_squish(gsub(last_name,"",full_name))
    final_name <- paste(last_name,first_name)
    
    #fisid
    fisid <- page %>%
      html_nodes("[id = 'FIS Code']") %>% 
      html_children() %>%
      html_text() %>%
      magrittr::extract(2)
    
    bday_check_date <- as.character(Sys.Date())
    
    skier_row <- data.frame(compid = compid[i],
                            fisid = fisid,
                            name = final_name,
                            yob = .yob,
                            birth_date = .birth_date,
                            bday_check_date = bday_check_date,
                            stringsAsFactors = FALSE)
    skiers[[i]] <- skier_row
    Sys.sleep(wait)
  }
  
  all_skiers <- bind_rows(skiers)
  all_skiers
}