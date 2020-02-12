library(dplyr)
library(RSQLite)
conl <- RSQLite::dbConnect(RSQLite::SQLite(),
                           dbname = "~/Dropbox/new-results-db/output/fis_results_prototype.db")
src_skier <- tbl(conl,"skier")
#### Distance Races ####
#Interval start
race_info <- list(date = "2020-02-01",
                  season = "2019-2020",
                  gender = "Men",
                  type = "Distance",
                  location = "FIN",
                  length = 15,
                  tech = "C",
                  format = "Interval",
                  cat1 = "NC",
                  cat2 = NA)
url <- "https://www.fis-ski.com/DB/general/results.html?sectorcode=CC&raceid=35122"

x <- fiscrape:::dst_scrape(url = url,race_info = race_info)

#Mass start
race_info <- list(date = "2020-01-18",
                  season = "2019-2020",
                  gender = "Men",
                  type = "Distance",
                  location = "ITA",
                  length = 30,
                  tech = "C",
                  format = "Mass",
                  cat1 = "OPA",
                  cat2 = NA)
url <- "https://www.fis-ski.com/DB/general/results.html?sectorcode=CC&raceid=35393"

#Skiathlon
race_info <- list(date = "2019-12-07",
                  season = "2019-2020",
                  gender = "Women",
                  type = "Distance",
                  location = "NOR",
                  length = 15,
                  tech = "FC",
                  format = "Skiathlon",
                  cat1 = "WC",
                  cat2 = NA)
url <- "https://www.fis-ski.com/DB/general/results.html?sectorcode=CC&raceid=34341"

#Pursuit
race_info <- list(date = "2020-01-19",
                  season = "2019-2020",
                  gender = "Men",
                  type = "Distance",
                  location = "CZE",
                  length = 15,
                  tech = "C",
                  format = "Pursuit",
                  cat1 = "WC",
                  cat2 = NA)
url <- "https://www.fis-ski.com/DB/general/results.html?sectorcode=CC&raceid=34394"

#Mass start marathon
race_info <- list(date = "2020-01-26",
                  season = "2019-2020",
                  gender = "Women",
                  type = "Distance",
                  location = "ITA",
                  length = 70,
                  tech = "C",
                  format = "Mass",
                  cat1 = "ML",
                  cat2 = NA)
url <- "https://www.fis-ski.com/DB/general/results.html?sectorcode=CC&raceid=35698"

#Sprint qualifier
race_info <- list(date = "2020-02-07",
                  season = "2019-2020",
                  gender = "Women",
                  type = "Sprint",
                  location = "CAN",
                  length = 1.4,
                  tech = "F",
                  format = NA,
                  cat1 = "FIS",
                  cat2 = NA)
url <- "https://www.fis-ski.com/DB/general/results.html?sectorcode=CC&raceid=36112"

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