#' @export
get_event_site <- function(url){
  page <- safe_html(x = url)
  
  if (!is.null(page$result)){
    site <- page$result %>%
      html_nodes("[class = 'heading heading_l2 heading_white heading_off-sm-style']") %>% 
      html_text()
    
    site <- gsub(pattern = "\\([^)]*\\)",replacement = "",x = site)
    site <- stringr::str_trim(site)
  }else{
    message("Failed to load page ",url)
    site <- NA_character_
  }
  return(site)
}