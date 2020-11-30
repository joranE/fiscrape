#' @export
dst_dup <- function(){
  v_dst <- tbl(src = conl,dbplyr::in_schema(options()$fiscrape.schema,"v_distance"))
  sec_plc <- v_dst %>%
    filter(!is.na(rank) & rank == 2 & !is.na(time)) %>%
    select(eventid,compid,fisid,rank,time,fispoints) %>%
    collect()
  
  possible_dup <- sec_plc %>%
    group_by(compid,fisid,time) %>%
    mutate(n = n()) %>%
    ungroup() %>%
    filter(n > 1) %>%
    as.data.frame() %>%
    arrange(compid,fisid,time,eventid)
  
  dup_grps <- possible_dup %>%
    group_by(compid,fisid,time) %>%
    group_split()
  
  message(sprintf("Found %s possible duplicates.",length(dup_grps)))
  
  for (i in seq_along(dup_grps)){
    cur <- dup_grps[[i]]
    comp <- v_dst %>%
      filter(eventid %in% local(cur$eventid) & rank <= 5) %>%
      select(eventid,gender,location,cat1,date,tech,length,rank,compid,fisid,name,time,fispoints) %>%
      arrange(rank,eventid) %>%
      collect() %>%
      as.data.frame()
    print(comp)
    m <- menu(choices = c("None",as.character(cur$eventid)),title = "Which event should we delete?")
    
    if (m == 1 || m == 0){
      next
    }else {
      ev_to_remove <- cur$eventid[m-1]
      delete_event(eventid = ev_to_remove)
    }
  }
  possible_dup
}

#' @export
spr_dup <- function(){
  v_dst <- tbl(src = conl,dbplyr::in_schema(options()$fiscrape.schema,"v_sprint"))
  sec_plc <- v_dst %>%
    filter(!is.na(rankqual) & rankqual == 2 & !is.na(time)) %>%
    select(eventid,compid,fisid,rankqual,time,fispoints) %>%
    collect()
  
  possible_dup <- sec_plc %>%
    group_by(compid,fisid,time) %>%
    mutate(n = n()) %>%
    ungroup() %>%
    filter(n > 1) %>%
    as.data.frame() %>%
    arrange(compid,fisid,time,eventid)
  
  dup_grps <- possible_dup %>%
    group_by(compid,fisid,time) %>%
    group_split()
  
  message(sprintf("Found %s possible duplicates.",length(dup_grps)))
  
  for (i in seq_along(dup_grps)){
    cur <- dup_grps[[i]]
    comp <- v_dst %>%
      filter(eventid %in% local(cur$eventid) & rankqual <= 5) %>%
      select(eventid,gender,location,cat1,date,tech,length,rankqual,compid,fisid,name,time,fispoints) %>%
      arrange(rankqual,eventid) %>%
      collect() %>%
      as.data.frame()
    print(comp)
    m <- menu(choices = c("None",as.character(cur$eventid)),title = "Which event should we delete?")
    
    if (m == 1 || m == 0){
      next
    }else {
      ev_to_remove <- cur$eventid[m-1]
      delete_event(eventid = ev_to_remove)
    }
  }
  possible_dup
}