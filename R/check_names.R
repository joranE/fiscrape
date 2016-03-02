check_names <- function(race){
  con_local <- db_xc_local()
  q_fisid <- "select fisid,name,count(distinct raceid) n 
              from main where fisid in %s group by fisid,name"
  q_compid <- "select compid,name,count(distinct raceid) n 
               from main where compid in %s group by compid,name"
  if (all(!is.na(race$fisid))){
    fisids <- paste_in(race$fisid)
    res <- ss_query(con_local,sprintf(q_fisid,fisids))
    
    res <- res %>%
      group_by(fisid) %>%
      top_n(1,wt = n)
    race$name[match(res$fisid,race$fisid)] <- res$name
    
    dbDisconnect(con_local)
    return(race)
  }else{
    if (all(!is.na(race$compid))){
      compids <- paste_in(race$compid)
      res <- ss_query(con_local,sprintf(q_compid,compids))
      
      res <- res %>%
        group_by(compid) %>%
        top_n(1,wt = n)
      race$name[match(res$compid,race$compid)] <- res$name
      
      dbDisconnect(con_local)
      return(race)
    }else{
      return(race)
    }
  }
}
