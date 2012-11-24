gatherRaceInfo <- function(){
  while(TRUE){
    cat("\nEnter race info: \n\n")
    dt <- readline(prompt = "Date: ")
    gender <- switch(menu(c('Men','Women')),'Men','Women')
    location <- toupper(readline(prompt = "Location: "))
    cat1 <- toupper(readline(prompt = "Cat1: "))
    cat2 <- toupper(readline(prompt = "Cat2: "))
    if (cat2 == 'JUNIOR'){cat2 <- 'Junior'}
    if (cat2 == 'NA'){cat2 <- NA}
    type <- switch(menu(c('Distance','Sprint')),'Distance','Sprint')
    if (type == 'Distance'){
      start <- switch(menu(c('Interval','Mass','Pursuit')),'Interval','Mass','Pursuit')
    }
    else{
      start <- NA
    }
    len <- as.numeric(readline(prompt = "Length: "))
    url <- readline(prompt = "URL: ")
    
    raceInfo <- list(cat1 = cat1,
                     cat2 = cat2,
                     cat3 = NA,
                     location = location,
                     type = type,
                     start = start,
                     length = len,
                     date = dt,
                     gender = gender,
                     url = url)
    cat("\n\nIs this correct?\n")
    cat("\nCat1      =",raceInfo$cat1)
    cat("\nCat2      =",raceInfo$cat2)
    cat("\nLocation  =",raceInfo$location)
    cat("\nDate      =",raceInfo$date)
    cat("\nGender    =",raceInfo$gender)
    cat("\nType      =",raceInfo$type)
    cat("\nStart     =",raceInfo$start)
    cat("\nLength    =",raceInfo$length)
    cat("\nURL       =",raceInfo$url)
    cat("\n")
    result <- menu(c('Correct','Incorrect'))
    if (result == 1){return(raceInfo)}
  }
}
