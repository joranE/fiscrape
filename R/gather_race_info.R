gatherRaceInfo <- function(){
  while(TRUE){
    cat("\nEnter race info: \n\n")
    
    dt <- readline(prompt = "Date: ")
    
    gender <- switch(menu(c('Men','Women')),'Men','Women')
    
    location <- toupper(readline(prompt = "Location: "))
    
    cat1 <- toupper(readline(prompt = "Cat1: "))
    cat2 <- toupper(readline(prompt = "Cat2: "))
    
    if (cat2 == 'JUNIOR'){
      cat2 <- 'Junior'
    }
    if (toupper(cat2) == 'NA'){
      cat2 <- NA
    }
    
    type <- switch(menu(c('Distance','Sprint','Stage')),'Distance','Sprint','Stage')
    
    if (type == 'Distance'){
      start <- switch(menu(c('Interval','Mass','Pursuit','Handicap')),'Interval','Mass','Pursuit','Handicap')
    }else{
      start <- NA
    }
    
    if (start == 'Pursuit' & !is.na(start)){
      tech <- 'FC'
    }else{
      tech <- switch(menu(c('Freestyle','Classic','Classic/Freestyle')),'F','C','FC')
    }
    
    len <- as.numeric(readline(prompt = "Length: "))
    
    if (type != 'Sprint'){
      url <- readline(prompt = "URL: ")
      if (grepl(pattern = "^https",x = url)){
        url <- gsub(pattern = "^https",replacement = "http",x = url)
      }
    }else{
      urlQual <- readline(prompt = "Qualification URL: ")
      urlFin <- readline(prompt  = "       Finals URL: ")
      
      if (grepl(pattern = "^https",x = urlQual)){
        urlQual <- gsub(pattern = "^https",
                        replacement = "http",
                        x = urlQual)
      }
      
      if (grepl(pattern = "^https",x = urlFin)){
        urlFin <- gsub(pattern = "^https",
                       replacement = "http",
                       x = urlFin)
      }
      
      if (toupper(urlQual) == "NA"){urlQual <- NA}
      if (toupper(urlFin) == "NA"){urlFin <- NA}
      
      url <- list(qual = urlQual,final = urlFin)
    }
    
    raceInfo <- list(cat1 = cat1,
                     cat2 = cat2,
                     location = location,
                     type = type,
                     start = start,
                     tech = tech,
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
    cat("\nTechnique =",raceInfo$tech)
    cat("\nLength    =",raceInfo$length)
    
    if (type != 'Sprint'){
      cat("\nURL       =",raceInfo$url)
    }else{
      cat("\nQual URL  =",raceInfo$url$qual)
      cat("\nFinal URL =",raceInfo$url$final)
    }
    
    cat("\n")
    result <- menu(c('Correct','Incorrect'))
    if (result == 1){return(raceInfo)}
  }
}
