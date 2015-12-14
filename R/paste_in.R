paste_in <- function(x,quote=TRUE){
	#For names with apostrophes
 	x <- sub("'","''",x)
  	if (quote){
	  rs <- paste0("('",paste(x,collapse = "','"),"')")
  	}
  	else{
	  rs <- paste0("(",paste(x,collapse = ","),")")
  	}
	rs
}