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
  pat <- "^([0-9]{1,2}:){1,2}([0-9]{2}){1}(\\.[0-9]+)?$"
  format_ok <- grepl(pattern = pat,x = times)
  
  #Attempt to clean up bad formats
  attempts <- 0
  while (any(!format_ok)){
    n_bad <- sum(!format_ok)
    cat(n_bad,"out of",length(times),"race times have an unusual format:\n")
    print(times[!format_ok])
    parser_fun <- parse_fun()
    times[!format_ok] <- parser_fun(times[!format_ok])
    format_ok <- grepl(pattern = pat,x = times)
    if (attempts > 0){
      choice <- menu(choices = c("Try again","Abort"))
      if (choice == 2){
        stop("Aborting...")
      }
    }
    attempts <- attempts + 1
  }
  
  #Now everything should be ok to convert...
  one_colon <- stringr::str_count(times,":") == 1
  if (any(one_colon)){
    times[one_colon] <- paste0("00:",times[one_colon])
  }
  
  secs <- lubridate::period_to_seconds(lubridate::hms(times))
  
  secs
}

parse_fun <- function(){
  FUN <- readline(prompt = "Enter function to do further parsing: ")
  eval(parse(text = FUN))
}