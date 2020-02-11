#Prototyping the process for screening skier records that don't match anything
# currently in the db
new_skiers <- anti_join(x[[2]],src_skier,by = c("compid","fisid","name","yob"),copy = TRUE)
true_new <- list()
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
  
  print(skier_comp)
  
  if (n == 2){
    s <- select.list(choices = diff_vals,
                     multiple = TRUE,
                     graphics = FALSE,
                     title = "Which values (if any) should we update?")
    if (length(s) == 0) next
    ref_compid <- skier_comp$compid[1]
    updates <- as.list(skier_comp[2,s])
    print(update_skier(ref_compid = ref_compid,updates = updates))
    cat("\n")
    Sys.sleep(0.5)
  }
  
  if (n > 2){
    m <- menu(choices = seq_len(n - 1),
              graphics = FALSE,
              title = "Which existing record is a match? (0 for no match)")
    
    #No match; potentially new skier
    if (m == 0){
      true_new <- c(true_new,row)
    }
    skier_comp <- skier_comp[c(m,n),]
    diff_vals <- names(skier_comp)[sapply(skier_comp,function(x) n_distinct(x) > 1)][-1]
    print(skier_comp)
    s <- select.list(choices = diff_vals,
                     multiple = TRUE,
                     graphics = FALSE,
                     title = "Which values (if any) should we update?")
    if (length(s) == 0) next
    ref_compid <- skier_comp$compid[1]
    updates <- as.list(skier_comp[2,s])
    print(update_skier(ref_compid = ref_compid,updates = updates))
    cat("\n")
    Sys.sleep(1)
  }
  true_new <- bind_rows(true_new)
  if (nrow(true_new) == 0){
    cat("\nNo new skiers to insert.")
  } else {
    print(true_new)
    cat("\nInserting these new skiers.")
  }
}

l <- list(compid = 1,fisid = "1234567",name = "Foo Bar",yob = 1987)
update_skier <- function(ref_compid,updates){
  updates_squish <- paste(purrr::imap(.x = updates,.f = squish),collapse = ",")
  q <- "update skier set %s where compid = %s"
  sprintf(q,updates_squish,ref_compid)
}
squish <- function(x,nm){
  if (is.character(x)) paste0(nm,"='",x,"'")
  else paste0(nm,"=",x)
}
