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