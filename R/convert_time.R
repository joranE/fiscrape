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
    if (raceType != 'Stage' & n == 3 & as.integer(t[1]) > 4){
      ind <- c(0.01,1,60)
    }
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
