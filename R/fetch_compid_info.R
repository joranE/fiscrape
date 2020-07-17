fetch_compid_info <- function(compid){
  skier_profile_url <- "https://www.fis-ski.com/DB/general/athlete-biography.html?sectorcode=CC&competitorid=%s"
  
  page <- safe_html(x = sprintf(skier_profile_url,compid))
  if (is.null(page$result)){
    message("Athlete biography for compid ",compid," failed to load.")
    result <- data.frame(compid = compid,
                         fisid = NA_character_,
                         name = NA_character_,
                         yob = NA_character_,
                         birth_date = NA_character_,
                         bday_check_date = as.character(Sys.Date()),
                         stringsAsFactors = FALSE)
    return(result)
  }
  
  #Name
  last_name <- page$result %>% 
    html_nodes("[class = 'athlete-profile__lastname']") %>% 
    html_text() %>% 
    stringr::str_trim(side = "both") %>% 
    stringr::str_squish()
  
  full_name <- page$result %>% 
    html_nodes("[class = 'athlete-profile__name']") %>% 
    html_text() %>% 
    stringr::str_trim(side = "both") %>% 
    stringr::str_squish()
  
  first_name <- stringr::str_replace(string = full_name,pattern = last_name,replacement = "") %>%
    stringr::str_trim(side = "both") %>%
    stringr::str_squish()
  
  final_name <- paste(last_name,first_name)
  
  #fisid
  fisid <- page$result %>% 
    html_nodes("[id = 'FIS Code']") %>% 
    html_children() %>%
    html_text() 
  fisid <- fisid[2]
  
  #yob & birthdate
  node_info <- page$result %>%
    html_nodes("[id = 'Birthdate']") %>% 
    html_children() %>%
    html_text()
  bday <- node_info[2]
  
  .yob <- NA_integer_
  .birth_date <- NA_character_
  
  if (grepl("[0-9]{4}",bday)){
    type <- "yob"
  }else {
    if (grepl("[0-9]{2}-[0-9]{2}-[0-9]{4}",bday)){
      type <- "date"
    } else{
      type <- "missing"
    }
  }
  
  if (type == "yob"){
    .yob <- bday
    .birth_date <- NA_character_
  }
  
  if (type == "date"){
    .birth_date <- as.character(as.Date(bday,"%d-%m-%Y"))
    .yob <- substr(.birth_date,1,4)
  }
  
  result <- data.frame(compid = compid,
                       fisid = fisid,
                       name = final_name,
                       yob = .yob,
                       birth_date = .birth_date,
                       bday_check_date = as.character(Sys.Date()),
                       stringsAsFactors = FALSE)
  result
  
}