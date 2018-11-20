get_td_text_dst <- function(x){
  row <- str_trim(html_text(x))
  row <- setNames(row,c("rank","bib","fisid","name","yob","nation","time","time_back","fispoints"))
  as.data.frame(as.list(row),stringsAsFactors = FALSE)
}

get_td_text_pur <- function(x){
  row <- str_trim(html_text(x))
  row <- setNames(row,c("rank","bib","fisid","name","yob","nation","time",
                        "time_back","fispoints_time","fispoints_rank","fispoints"))
  as.data.frame(as.list(row),stringsAsFactors = FALSE)
}

get_td_text_spr_qual <- function(x){
  row <- str_trim(html_text(x))
  row <- setNames(row,c("rank","bib","fisid","name","yob","nation","time","time_back","fispoints"))
  as.data.frame(as.list(row),stringsAsFactors = FALSE)
}

get_td_text_spr_final <- function(x){
  row <- str_trim(html_text(x))
  row <- setNames(row,c("rank","bib","fisid","name","yob","nation"))
  as.data.frame(as.list(row),stringsAsFactors = FALSE)
}