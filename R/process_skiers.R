#' @export
process_skiers <- function(skier_list,conn,update_bdays){
  src_skier <- tbl(conl,"skier")
  
  if (update_bdays){
    no_bday <- src_skier %>%
      filter(compid %in% local(skier_list$compid) & is.na(birth_date)) %>%
      collect()
    skier_list_no_bday <- skier_list %>%
      filter(compid %in% no_bday$compid)
    if (nrow(skier_list_no_bday) > 0){
      update_bdays(skier_list_no_bday,conl)
    }
  }
  skier_list <- skier_list %>%
    mutate(compid = as.integer(compid),
           fisid = as.character(fisid),
           name = as.character(name),
           yob = as.integer(yob))
  new_skiers <- anti_join(skier_list,
                          src_skier,
                          by = c("compid","fisid","name","yob"),copy = TRUE)
  true_new_skiers <- list()
  
  for (i in seq_len(nrow(new_skiers))){
    row <- new_skiers[i,]
    cap_name <- stringr::str_extract(string = new_skiers$name[i],"^[-A-Z]+")
    cap_name <- paste0("%",cap_name,"%")
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
      true_new_skiers <- c(true_new_skiers,row)
      next
    }
    
    if (n == 2){
      print(skier_comp)
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
    
    if (n > 2){
      print(skier_comp)
      m <- menu(choices = seq_len(n - 1),
                graphics = FALSE,
                title = "\nWhich existing record is a match? (0 for no match)")
      
      #No match; potentially new skier
      if (m == 0){
        true_new_skiers <- c(true_new_skiers,row)
        next
      }
      skier_comp <- skier_comp[c(m,n),]
      diff_vals <- names(skier_comp)[sapply(skier_comp,function(x) n_distinct(x) > 1)][-1]
      print(skier_comp)
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
  true_new_skiers <- bind_rows(true_new_skiers)
  if (nrow(true_new_skiers) == 0){
    message("No new skiers to insert.")
  } else {
    print(true_new_skiers)
    message("Saving these skiers to insert.")
  }
  true_new_skiers
}