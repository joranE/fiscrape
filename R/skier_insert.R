update_skier <- function(ref_compid,updates){
  updates_squish <- paste(purrr::imap(.x = updates,.f = squish),collapse = ",")
  q <- "update skier set %s where compid = %s"
  sprintf(q,updates_squish,ref_compid)
}

squish <- function(x,nm){
  if (is.character(x)) paste0(nm,"='",x,"'")
  else paste0(nm,"=",x)
}
