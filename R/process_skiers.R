#' @importFrom bit64 as.integer64
#' @export
process_skiers <- function(skier_list,update_bdays){
  src_skier <- tbl(conl,dbplyr::in_schema(options()$fiscrape.schema,"skier"))
  
  if (update_bdays){
    min_chk_date <- as.character(Sys.Date() - 365)
    no_bday <- src_skier %>%
      filter(compid %in% local(skier_list$compid) & 
               is.na(birth_date) & 
               bday_check_date < min_chk_date) %>%
      collect()
    skier_list_no_bday <- skier_list %>%
      filter(compid %in% no_bday$compid)

    if (nrow(skier_list_no_bday) > 0){
      update_bdays(skier_list_no_bday,conl)
      
      chk_dates <- skier_list_no_bday %>%
        select(compid) %>%
        mutate(bday_check_date = as.character(Sys.Date()))
      # RPostgres doesn't do named parameters
      chk_dates <- unname(as.list(chk_dates))
      DBI::dbWithTransaction(conl,{
        q <- "update skier set bday_check_date = $1 where compid = $2"
        rs <- RPostgres::dbSendStatement(conl,q)
        RPostgres::dbBind(rs,params = chk_dates)
        rows_aff <- RPostgres::dbGetRowsAffected(rs)
        RPostgres::dbClearResult(rs)
      })
    }
  }
  skier_list <- skier_list %>%
    mutate(compid = bit64::as.integer64(compid),
           fisid = as.character(fisid),
           name = as.character(name),
           yob = bit64::as.integer64(yob))
  # Potentially new athletes, some may just have name/yob variants
  new_skiers <- anti_join(skier_list,
                          src_skier,
                          by = c("compid","fisid","name","yob"),
                          copy = TRUE) 

  if (nrow(new_skiers) > 0){
    new_skiers <- new_skiers %>%
      mutate(new = FALSE)
    
    # Loop thru possibly new athletes, 
    # find potential matches in skier table
    # and update info as needed, or just use
    # existing record from skier table
    for (i in seq_len(nrow(new_skiers))){
      row <- new_skiers[i,]
      cap_name <- stringr::str_extract(string = new_skiers$name[i],"^[-A-Z]+")
      cap_name <- paste0("% ",cap_name," %")
      poss_match <- src_skier %>% 
        filter(compid %in% local(new_skiers$compid[i]) | 
                 fisid %in% local(new_skiers$fisid[i]) | 
                 name %like% cap_name) %>% 
        collect()
      skier_comp <- bind_rows(list(existing = poss_match,new = new_skiers[i,]),.id = "ref") %>%
        select(-birth_date)
      diff_vals <- names(skier_comp)[sapply(skier_comp,function(x) n_distinct(x) > 1)][-1]
      n <- nrow(skier_comp)
      
      if (n == 1){
        new_skiers$new[i] <- TRUE
        next
      }
      
      if (n >= 2){
        print(skier_comp)
        m <- menu(choices = seq_len(n - 1),
                  graphics = FALSE,
                  title = "\nWhich existing record is a match? (0 for no match)")
        
        #No match; potentially new skier
        if (m == 0){
          new_skiers$new[i] <- TRUE
          next
        }
        skier_comp <- skier_comp[c(m,n),]
        diff_vals <- names(skier_comp)[sapply(skier_comp,function(x) n_distinct(x) > 1)][-1]
        print(skier_comp %>% select(-new))
        s <- select.list(choices = diff_vals,
                         multiple = TRUE,
                         graphics = FALSE,
                         title = "\nWhich values (if any) should we update? (Type 'enter' for none.)")
        if (length(s) == 0) next
        ref_compid <- skier_comp$compid[1]
        updates <- as.list(skier_comp[2,s])
        update_skier(ref_compid = ref_compid,updates = updates,conn = conl)
        cat("\n")
      }
    }
    
    true_new_skiers <- new_skiers %>%
      filter(new) %>%
      select(-new)
    
    if (nrow(true_new_skiers) > 0){
      true_new_skiers <- true_new_skiers %>%
        mutate(bday_check_date = as.character(Sys.Date()))
      print(true_new_skiers)
      message("Saving these skiers to insert.")
    }else{
      # No new athletes found in skier table
      true_new_skiers <- data.frame(compid = integer(0),
                                    fisid = character(0),
                                    name = character(0),
                                    yob = integer(0),
                                    birth_date = character(0),
                                    bday_check_date = character(0),
                                    stringsAsFactors = FALSE)
      message("No new skiers to insert.")
    }
  }else {
    true_new_skiers <- data.frame(compid = integer(0),
                                  fisid = character(0),
                                  name = character(0),
                                  yob = integer(0),
                                  birth_date = character(0),
                                  bday_check_date = character(0),
                                  stringsAsFactors = FALSE)
    message("No new skiers to insert.")
  }
  
  true_new_skiers
}