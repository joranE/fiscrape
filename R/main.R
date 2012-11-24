#' Run the screen scraper
#' 
#' The main function
#' 
#' @param \dots Ignored
#' @export
ibuscrape <- function(...){
  require(XML)
  con <- dbConnect("SQLite","~/Dropbox/SkiingResults/ibu_update.db")
  while(TRUE){
    cat("Make a selection: \n")
    
    selection <- menu(c('Enter New Race'))
    if (selection == 0){break}
    else{
      raceInfo <- gatherRaceInfo()
      raceInfo$season <- getSeason(raceInfo$date)
      
      
      tbls <- readHTMLTable(raceInfo$url,
                            header = TRUE,
                            which = 5)
      if ("RESULT" %ni% colnames(tbls)){
        while(TRUE){
          print(tbls)
          cat("\nI don't think this was the right table.\n")
          selection <- readline(prompt = 'Try table number...')
          tbls <- readHTMLTable(raceInfo$url,
                                header = TRUE,
                                which = as.integer(selection))
          print(tbls)
          selection <- menu(c('Yes','No'))
          if (selection == 1) break
        }
      }
      
      colnames(tbls) <- c('rank','bib','name','nation','shooting','time','timeback')
      tbls$status <- NA
      dnf <- tbls$rank %in% c('DNF','DNS')
      tbls$status[dnf] <- tbls$rank[dnf]
      tbls$rank[dnf] <- NA
      
      tbls$rank <- as.integer(tbls$rank)
      tbls$bib <- NULL
      tbls$date <- raceInfo$date
      tbls$season <- raceInfo$season
      tbls$cat1 <- raceInfo$cat1
      tbls$cat2 <- raceInfo$cat2
      tbls$cat2 <- raceInfo$cat3
      tbls$location <- raceInfo$location
      tbls$gender <- raceInfo$gender
      tbls$type <- raceInfo$type
      tbls$start <- raceInfo$start
      tbls$length <- raceInfo$length
      tbls$raceid <- getMaxRaceID() + 1
      id <- getMaxID() + 1
      tbls$id <- seq(id,length.out = nrow(tbls))
      
      tbls <- tbls[,c('raceid','date','season','location','gender','length','tech',
                      'type','start','cat1','cat2','fisid','name','yob','age','nation',
                      'rank','rankqual','time','fispoints')]
      tbls$time <- convertTime(tbls$time,raceInfo$type)
      #browser()
      tbls <- checkNames(tbls)
      
      if (any(tbls$age < 11 | tbls$age > 70)){
        tbls$age[(tbls$age < 11 | tbls$age > 70)] <- NA
      }
      
      print(tbls)
      cat("\nDoes this look correct?")
      selection <- menu(c('Yes','No'))
      if (selection != 1){
        next
      }
      else{
        cat("\nUploading...\n")
        sql <- "insert into main (raceid,date,season,location,gender,length,tech,type,start,
          cat1,cat2,fisid,name,yob,age,nation,rank,rankqual,time,fispoints) 
          values (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
        bulk_insert(con,sql,tbls)
        verifyUpload(tbls)
      }
    }
    
  }
  dbDisconnect(con)
}
