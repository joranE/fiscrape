#' @export
recalc_dst_pts <- function(){
  src_dst <- tbl(src = conl,"v_distance")
  src_dst_maj_int <- tbl(src = conl,"v_distance_maj_int")
  
  raw_dst_races <- src_dst %>%
    filter(!is.na(time)) %>%
    arrange(season,date,eventid) %>%
    collect() %>%
    as.data.frame()
  
  all_dst_races <- raw_dst_races %>%
    group_by(eventid) %>%
    mutate(race_size = n()) %>%
    as.data.frame() %>%
    mutate(pb_med_scl = if_else(primary_tag %in% c("wc","owg","wsc","tds"),pbm / pbm_sd,pbm / (1.5 * pbm_sd)),
           pb_med_scl = if_else(!primary_tag %in% c("wc","owg","wsc","tds") & season == "1975-1976",NA_real_,pb_med_scl),
           penalty = NA,
           pb_med_adj = NA) %>%
    mutate(maj_int = if_else(primary_tag %in% c("wc","wsc","owg","tds"),"Y","N")) %>%
    filter(!is.na(pb_med_scl))
  
  pts_hist <- all_dst_races %>%
    mutate(event_type = "Distance") %>%
    select(season,gender,event_type,date,eventid,cat1,fisid,rank,pb_med_adj)
  
  dst_nst <- all_dst_races %>%
    mutate(date2 = date,
           eventid2 = eventid) %>%
    arrange(season,date2,eventid2) %>%
    group_nest(season,date2,eventid2) %>%
    mutate(data = map(.x = data,.f = add_race_pen,.event_type = "Distance"))
  
  dst <- unnest(dst_nst,data) %>%
    as.data.frame()
  dst_sd_pen <- dst %>%
    filter(maj_int == "Y" | season >= "1992-1993") %>%
    select(eventid,pbm_sd,penalty) %>%
    distinct()
  
  dst_sd_pen
}

#' @export
recalc_spr_pts <- function(){
  src_spr <- tbl(src = conl,"v_sprint")
  src_spr_maj_int <- tbl(src = conl,"v_sprint_maj_int")
  
  raw_spr_races <- src_spr %>%
    filter(!is.na(time)) %>%
    arrange(season,date,eventid) %>%
    collect() %>%
    as.data.frame()
  
  all_spr_races <- raw_spr_races %>%
    group_by(eventid) %>%
    mutate(race_size = n()) %>%
    as.data.frame() %>%
    mutate(pb_med_scl = if_else(primary_tag %in% c("wc","owg","wsc","tds"),pbm / pbm_sd,pbm / (1.5 * pbm_sd)),
           pb_med_scl = if_else(!primary_tag %in% c("wc","owg","wsc","tds") & season == "1995-1996",NA_real_,pb_med_scl),
           penalty = NA,
           pb_med_adj = NA) %>%
    mutate(maj_int = if_else(primary_tag %in% c("wc","wsc","owg","tds"),"Y","N")) %>%
    filter(!is.na(pb_med_scl))
  
  pts_hist <- all_spr_races %>%
    mutate(event_type = "Sprint") %>%
    select(season,gender,event_type,date,eventid,cat1,fisid,rank,pb_med_adj)
  
  spr_nst <- all_spr_races %>%
    mutate(date2 = date,
           eventid2 = eventid) %>%
    arrange(season,date2,eventid2) %>%
    group_nest(season,date2,eventid2) %>%
    mutate(data = map(.x = data,.f = add_race_pen,.event_type = "Sprint"))
  
  spr <- unnest(spr_nst,data) %>%
    as.data.frame()
  spr_sd_pen <- spr %>%
    filter(maj_int == "Y" | season >= "1992-1993") %>%
    select(eventid,pbm_sd,penalty) %>%
    distinct()
  
  spr_sd_pen
}

#' @export
insert_recalc_med_pts <- function(dst,spr){
  all_pens <- bind_rows(dst,spr)
  RSQLite::dbWriteTable(conl,
                        name = "event_penalty",
                        value = all_pens,
                        row.names = FALSE,
                        overwrite = TRUE)
}

adj_mean <- function(x,adj = c(1.4,1.3,1.2,1.1,1)){
  mean(x) * adj[length(x)]
}

add_race_pen <- function(race,.event_type){
  pb$tick()
  race_tag <- race$primary_tag[1]
  maj_int_tag <- c("wc","owg","wsc","tds")
  
  if (race_tag %in% maj_int_tag){
    race_penalty <- 0
    n_maj_int <- NA
  } else{
    race_date <- race$date[1]
    
    if (.event_type == "Distance"){
      med_rnk <- median(race$rank,na.rm = TRUE)
    } else{
      med_rnk <- median(race$rankqual,na.rm = TRUE)
    }
    
    med_rnk_idx_top <- max(floor(med_rnk) - 2,1)
    med_rnk_idx_bot <- min(floor(med_rnk) + 2,nrow(race))
    
    if (.event_type == "Distance"){
      pen_skiers <- race[race$rank >= med_rnk_idx_top & race$rank <= med_rnk_idx_bot,]
    } else{
      pen_skiers <- race[race$rankqual >= med_rnk_idx_top & race$rankqual <= med_rnk_idx_bot,]
    }
    
    pen_skier_hist <- pts_hist %>%
      filter(fisid %in% pen_skiers$fisid & 
               date < race_date &
               event_type == .event_type & 
               !is.na(pb_med_adj)) %>%
      group_by(fisid) %>%
      top_n(n = 5,wt = date)
    pen_skier_hist_maj_int <- filter(pen_skier_hist,cat1 %in% maj_int_cat1)
    
    pen_values <- c(pen_skier_hist$pb_med_adj,pen_skier_hist_maj_int$pb_med_adj)
    
    n_maj_int <- sum(pen_skier_hist$cat1 %in% maj_int_cat1)
    
    if (nrow(pen_skier_hist) == 0 | all(is.na(pen_values))){
      race_penalty <- 4
    } else {
      race_penalty <- 0.9 * quantile(pen_values,probs = 0.5,na.rm = TRUE)
      race_penalty <- max(race_penalty,0.1)
    }
  }
  
  #cat("Race penalty:",race_penalty,"\n")
  
  race$penalty <- race_penalty
  race$n_maj_int <- n_maj_int
  race$pb_med_adj <- race$pb_med_scl + race_penalty
  
  pts_hist$penalty[pts_hist$eventid == race$eventid[1]] <<- race_penalty
  pts_hist$pb_med_adj[pts_hist$eventid == race$eventid[1]] <<- race$pb_med_scl + race_penalty
  
  race
}

pen_races <- function(nms,dt){
  dat <- dst %>%
    filter(name %in% nms & date < dt) %>%
    group_by(fisid) %>%
    top_n(5,date) %>%
    as.data.frame()
  dat
}

pts_compare <- function(.data,nm){
  .data %>%
    filter(name == nm) %>%
    select(name,date,maj_int,fispoints,pb_med_adj) %>%
    pivot_longer(cols = c("fispoints","pb_med_adj"),names_to = "pts_type",values_to = "pts") %>%
    ggplot(data = .,aes(x = as.Date(date),y = pts,color = maj_int)) + 
    facet_wrap(~pts_type,scale = "free_y") + 
    geom_point() +
    ggtitle(label = nm) +
    theme_bw()
}

pb_med_plot <- function(.data,nms,typ){
  .data %>%
    filter(name %in% nms & type == typ) %>%
    ggplot(data = .,aes(x = as.Date(date),y = pb_med_adj)) + 
    facet_wrap(~name) + 
    geom_point(aes(color = maj_int)) + 
    theme_bw()
}
