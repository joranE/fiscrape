#' Some documentation
#' More documentation changes
#' @export
dst_race_penalty <- function(result_data,event_date){
  med_rnk <- median(result_data$rank,na.rm = TRUE)
  
  med_rnk_idx_top <- max(floor(med_rnk) - 2,1)
  med_rnk_idx_bot <- min(floor(med_rnk) + 2,nrow(result_data))
  pen_skiers <- result_data %>%
    filter(rank >= med_rnk_idx_top &
             rank <= med_rnk_idx_bot)
  
  #This won't work until db is pre-seeded with pbm_pts column & values
  pen_skier_hist <- tbl(conl,dbplyr::in_schema(options()$fiscrape.schema,"v_distance")) %>%
    filter(compid %in% local(pen_skiers$compid) & 
             date < event_date & 
             !is.na(pbm_pts)) %>%
    collect() %>%
    group_by(compid) %>%
    top_n(n = 5,wt = date)
  pen_skier_hist_maj_int <- filter(pen_skier_hist,primary_tag %in% c("wc","tds","owg","wsc"))
  pen_values <- c(pen_skier_hist$pbm_pts,
                  pen_skier_hist_maj_int$pbm_pts)
  
  if (nrow(pen_skier_hist) == 0 | all(is.na(pen_values))){
    race_penalty <- 4
  } else {
    race_penalty <- 0.9 * median(pen_values,na.rm = TRUE)
    race_penalty <- max(race_penalty,0.1)
  }
  return(race_penalty)
}

#' @export
spr_race_penalty <- function(result_data,event_date){
  med_rnk <- median(result_data$rank,na.rm = TRUE)
  
  med_rnk_idx_top <- max(floor(med_rnk) - 2,1)
  med_rnk_idx_bot <- min(floor(med_rnk) + 2,nrow(result_data))
  pen_skiers <- result_data %>%
    filter(rank >= med_rnk_idx_top &
             rank <= med_rnk_idx_bot)
  
  #This won't work until db is pre-seeded with pbm_pts column & values
  pen_skier_hist <- tbl(conl,dbplyr::in_schema(options()$fiscrape.schema,"v_sprint")) %>%
    filter(compid %in% local(pen_skiers$compid) & 
             date < event_date & 
             !is.na(pbm_pts)) %>%
    collect() %>%
    group_by(compid) %>%
    top_n(n = 5,wt = date)
  pen_skier_hist_maj_int <- filter(pen_skier_hist,primary_tag %in% c("wc","tds","owg","wsc"))
  pen_values <- c(pen_skier_hist$pbm_pts,
                  pen_skier_hist_maj_int$pbm_pts)
  
  if (nrow(pen_skier_hist) == 0 | all(is.na(pen_values))){
    race_penalty <- 4
  } else {
    race_penalty <- 0.9 * median(pen_values,na.rm = TRUE)
    race_penalty <- max(race_penalty,0.1)
  }
  return(race_penalty)
}