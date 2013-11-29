#' Run the screen scraper
#' 
#' The main function
#' 
#' @param \dots Ignored
#' @export
fiscrape <- function(...){
  require(XML)
  con <- db_xc()
  while(TRUE){
    cat("Make a selection: \n")
    
    selection <- menu(c('Enter New Race'))
    if (selection == 0){break}
    else{
      raceInfo <- gatherRaceInfo()
      raceInfo$season <- getSeason(raceInfo$date)
      
      if (raceInfo$type != 'Sprint'){
        #browser()
        tbls <- readHTMLTable(raceInfo$url,
                              header = TRUE,
                              which = 2)
        tbls <- colwise(function(x) {stringr::str_trim(gsub("Â","",x))})(tbls)
        if ("Rank" %ni% colnames(tbls)){
          while(TRUE){
            print(tbls)
            cat("\nI don't think this was the right table.\n")
            selection <- readline(prompt = 'Try table number...')
            tbls <- readHTMLTable(raceInfo$url,
                                  header = TRUE,
                                  which = as.integer(selection))
            tbls <- colwise(function(x) {stringr::str_trim(gsub("Â","",x))})(tbls)
            print(tbls)
            selection <- menu(c('Yes','No'))
            if (selection == 1) break
          }
        }
        fp <- 'FIS Points' %in% colnames(tbls)
        bib <- 'Bib' %in% colnames(tbls)
        #browser()
        if (fp & bib){
          colnames(tbls) <- c('rank','bib','fisid','name','yob','nation','time','fispoints')
        }
        if (fp & !bib){
          colnames(tbls) <- c('rank','fisid','name','yob','nation','time','fispoints')
        }
        if (!fp){
          colnames(tbls) <- c('rank','bib','fisid','name','yob','nation','time')
          tbls$fispoints <- NA
        }
        
        tbls <- subset(tbls,rank != '' & !is.na(name))
        
        tbls$rank <- as.integer(tbls$rank)
        tbls$yob <- as.integer(tbls$yob)
        tbls$fispoints <- as.numeric(tbls$fispoints)
        tbls$bib <- NULL
        tbls$date <- raceInfo$date
        tbls$season <- raceInfo$season
        tbls$cat1 <- raceInfo$cat1
        tbls$cat2 <- raceInfo$cat2
        tbls$location <- raceInfo$location
        tbls$gender <- raceInfo$gender
        tbls$type <- raceInfo$type
        tbls$start <- raceInfo$start
        tbls$tech <- raceInfo$tech
        tbls$length <- raceInfo$length
        tbls$age <- as.integer(substr(raceInfo$date,1,4)) - tbls$yob
        tbls$raceid <- getMaxRaceID() + 1
        tbls$rankqual <- NA
        
        tbls <- tbls[,c('raceid','date','season','location','gender','length','tech',
                        'type','start','cat1','cat2','fisid','name','yob','age','nation',
                        'rank','rankqual','time','fispoints')]
        #browser()
        tbls$time <- convertTime(tbls$time,raceInfo$type)
        #browser()
        tbls <- checkNames(tbls)
        #browser()
        if (any(tbls$age < 11 | tbls$age > 70,na.rm = TRUE)){
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
      else{
        if (is.na(raceInfo$url$qual)) {tblsQual <- NULL}
        else{
          #browser()
          tblsQual <- readHTMLTable(raceInfo$url$qual,
                                    header = TRUE,
                                    which = 2)
          tblsQual <- colwise(function(x) {stringr::str_trim(gsub("Â","",x))})(tblsQual)
          if ("Rank" %ni% colnames(tblsQual)){
            while(TRUE){
              print(tblsQual)
              cat("\nI don't think this was the right table.\n")
              selection <- readline(prompt = 'Try table number...')
              tblsQual <- readHTMLTable(raceInfo$url$qual,
                                    header = TRUE,
                                    which = as.integer(selection))
              tblsQual <- colwise(function(x) {stringr::str_trim(gsub("Â","",x))})(tblsQual)
              print(tblsQual)
              selection <- menu(c('Yes','No'))
              if (selection == 1) break
            }
          }
        }
        if (is.na(raceInfo$url$final)){tblsFinal <- NULL}
        else{
          #browser()
          tblsFinal <- readHTMLTable(raceInfo$url$final,
                                     header = TRUE,
                                     which = 2)
          tblsFinal <- colwise(function(x) {stringr::str_trim(gsub("Â","",x))})(tblsFinal)
          if ("Rank" %ni% colnames(tblsFinal)){
            while(TRUE){
              print(tblsFinal)
              cat("\nI don't think this was the right table.\n")
              selection <- readline(prompt = 'Try table number...')
              tblsFinal <- readHTMLTable(raceInfo$url$final,
                                        header = TRUE,
                                        which = as.integer(selection))
              tblsFinal <- colwise(function(x) {stringr::str_trim(gsub("Â","",x))})(tblsFinal)
              print(tblsFinal)
              selection <- menu(c('Yes','No'))
              if (selection == 1) break
            }
          }
        }
        #browser()
        if (!is.null(tblsQual) & !is.null(tblsFinal)){
          tbls <- processSprints(tblsQual,tblsFinal,raceInfo)
        }
        if(is.null(tblsFinal)){
          tbls <- processSprintsQualOnly(tblsQual,raceInfo)
        }
        if(is.null(tblsQual)){
          tbls <- processSprintsFinalOnly(tblsFinal,raceInfo)
        }
        if (is.null(tblsQual) & is.null(tblsFinal)){
          cat("\nBoth urls returned missing tables!\n")
          next
        }
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
    
    }
  dbDisconnect(con)
}