convertTime <- function(x,raceType){
  x <- as.character(x)
  #Replace 'h' with ':'
  x <- gsub('h',':',x,fixed = TRUE)
  #Replace any leading : with ""
  x <- gsub("^:","",x)
  #Replace ',' with '.'
  x <- gsub(',','.',x,fixed = TRUE)
  
  #Split each time on ':'
  x <- strsplit(x,":")
  processTime <- function(t,raceType){
    if (length(t) == 0){return(NA)}
    if (as.numeric(t[1]) == 0){t <- t[-1]}
    n <- length(t)
    ind <- NA
    #if (raceType != 'Stage' & n == 3 & as.integer(t[1]) > 4){
    #  ind <- c(0.01,1,60)
    #}
    if (raceType == 'Sprint' & n == 3){
      ind <- c(0.01,1,60)
    }
    if (raceType != 'Stage' & n == 4){
      ind <- c(0.01,1,60,3600)
    }
    if (is.na(ind)){
      ind <- c(1,60,3600)
    }
    sum(as.numeric(t) * rev(ind[1:n]))
  }
  sapply(x,processTime,raceType = raceType)
}

#' @importFrom stringr str_count
#' @importFrom lubridate hms
#' @importFrom lubridate period_to_seconds
convert_time <- function(times){
  #Intended to catch "MM:SS.S" or "HH:MM:SS.S" formats
  pat <- "^([0-9]{1,2}:){1,2}([0-9]{2}\\.[0-9]{1})$"
  format_ok <- grepl(pattern = pat,x = times)
  
  if (!all(format_ok)){
    #Show what the times look like and have user specify format...?
    n_bad <- sum(!format_ok)
    cat(n_bad,"out of",length(times),"race times have an unusual format:\n")
    print(times[!format_ok])
    stop("Bad times need to be fixed.")
  }else{
    #Convert as usual
    one_colon <- stringr::str_count(times,":") == 1
    if (any(one_colon)){
      times[one_colon] <- paste0("00:",times[one_colon])
    }
    
    secs <- lubridate::period_to_seconds(lubridate::hms(times))
  }
  secs
}