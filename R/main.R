#' Run the screen scraper
#' 
#' The main function
#' 
#' @param \dots Ignored
#' @importFrom XML readHTMLTable
#' @importFrom stringr str_trim
#' @importFrom plyr colwise
#' @importFrom plyr rename
#' @export
fiscrape <- function(...){
  con_remote <- db_xc_remote()
  con_local <- db_xc_local()
  while(TRUE){
    cat("Make a selection: \n")
    
    selection <- menu(c('Enter New Race'))
    if (selection == 0){break}
    else{
      raceInfo <- gatherRaceInfo()
      raceInfo$season <- getSeason(raceInfo$date)
      
      if (raceInfo$type != 'Sprint'){
        #browser()
        download_time <- system.time(tbls <- XML::readHTMLTable(raceInfo$url,
                              header = TRUE,
                              which = 2))
        cat("\nDownload time:\n")
        print(download_time)
        tbls <- plyr::colwise(function(x) {stringr::str_trim(gsub("Â","",x))})(tbls)
        if ("Rank" %ni% colnames(tbls)){
          while(TRUE){
            print(tbls)
            cat("\nI don't think this was the right table.\n")
            selection <- readline(prompt = 'Try table number...')
            tbls <- XML::readHTMLTable(raceInfo$url,
                                  header = TRUE,
                                  which = as.integer(selection))
            tbls <- plyr::colwise(function(x) {stringr::str_trim(gsub("Â","",x))})(tbls)
            print(tbls)
            selection <- menu(c('Yes','No'))
            if (selection == 1) break
          }
        }
        rerank <- FALSE
        if ("Behind" %in% colnames(tbls)){
          tbls$Behind <- NULL
        }
        if (any(grepl('FIS Points Time',colnames(tbls)))){
          tbls$Time <- NULL
          tbls <- plyr::rename(tbls,c('FIS Points Time' = 'Time'))
          rerank <- TRUE
        }
        ind <- grepl('Rk',colnames(tbls))
        if (any(ind)){
          tbls <- tbls[,!ind]
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
        if (!fp & bib){
          colnames(tbls) <- c('rank','bib','fisid','name','yob','nation','time')
          tbls$fispoints <- NA
        }
        if (!fp & !bib){
          colnames(tbls) <- c('rank','fisid','name','yob','nation','time')
          tbls$fispoints <- NA
        }
        
        if (all(is.na(as.numeric(tbls$rank)))){
          tbls <- subset(tbls,!is.na(name))
          rerank <- TRUE
        } else{
          tbls <- subset(tbls,rank != '' & !is.na(name))
        }
        
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
        if (rerank){
          tbls <- subset(tbls,!is.na(time))
          tbls$rank <- rank(tbls$time,ties.method = "min")
        }
        #browser()
        name_check_time <- system.time(tbls <- checkNames(tbls))
        #cat("\nName check time:\n")
        #print(name_check_time)
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
          check <- dbWriteTable(conn = con_remote,
                                name = "main",
                                value = tbls, 
                                row.names = FALSE, 
                                overwrite = FALSE, 
                                append = TRUE)
          median_time <- data.frame(raceid = tbls$raceid[1],
                                    median_time = median(tbls$time,na.rm = TRUE))
          dbWriteTable(conn = con_remote,
                       name = "median_race_time",
                       value = median_time,
                       row.names = FALSE,
                       overwrite = FALSE,
                       append = TRUE)
          race_url <- data.frame(raceid = tbls$raceid[1],url1 = raceInfo$url,url2 = NA)
          dbWriteTable(conn = con_remote,
                       name = "race_url",
                       value = race_url,
                       row.names = FALSE,
                       overwrite = FALSE,
                       append = TRUE)
          if (!check){
            stop("Upload failed.")
          }
          sql <- "insert into main (raceid,date,season,location,
                                    gender,length,tech,type,start,
                                    cat1,cat2,fisid,name,yob,age,nation,
                                    rank,rankqual,time,fispoints) 
                              values (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
          bulk_insert(con_local,sql,tbls)
          dbWriteTable(conn = con_local,
                       name = "median_race_time",
                       value = median_time,
                       row.names = FALSE,
                       overwrite = FALSE,
                       append = TRUE)
          dbWriteTable(conn = con_local,
                       name = "race_url",
                       value = race_url,
                       row.names = FALSE,
                       overwrite = FALSE,
                       append = TRUE)
        }
      }
      else{
        if (is.na(raceInfo$url$qual)) {tblsQual <- NULL}
        else{
          #browser()
          tblsQual <- XML::readHTMLTable(raceInfo$url$qual,
                                    header = TRUE,
                                    which = 2)
          tblsQual <- plyr::colwise(function(x) {stringr::str_trim(gsub("Â","",x))})(tblsQual)
          if ("Rank" %ni% colnames(tblsQual)){
            while(TRUE){
              print(tblsQual)
              cat("\nI don't think this was the right table.\n")
              selection <- readline(prompt = 'Try table number...')
              tblsQual <- XML::readHTMLTable(raceInfo$url$qual,
                                    header = TRUE,
                                    which = as.integer(selection))
              tblsQual <- plyr::colwise(function(x) {stringr::str_trim(gsub("Â","",x))})(tblsQual)
              print(tblsQual)
              selection <- menu(c('Yes','No'))
              if (selection == 1) break
            }
          }
          if ("Behind" %in% colnames(tblsQual)){
            tblsQual$Behind <- NULL
          }
        }
        if (is.na(raceInfo$url$final)){tblsFinal <- NULL}
        else{
          #browser()
          tblsFinal <- XML::readHTMLTable(raceInfo$url$final,
                                     header = TRUE,
                                     which = 2)
          tblsFinal <- plyr::colwise(function(x) {stringr::str_trim(gsub("Â","",x))})(tblsFinal)
          if ("Rank" %ni% colnames(tblsFinal)){
            while(TRUE){
              print(tblsFinal)
              cat("\nI don't think this was the right table.\n")
              selection <- readline(prompt = 'Try table number...')
              tblsFinal <- XML::readHTMLTable(raceInfo$url$final,
                                        header = TRUE,
                                        which = as.integer(selection))
              tblsFinal <- plyr::colwise(function(x) {stringr::str_trim(gsub("Â","",x))})(tblsFinal)
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
          check <- dbWriteTable(conn = con_remote,
                                name = "main",
                                value = tbls, 
                                row.names = FALSE, 
                                overwrite = FALSE, 
                                append = TRUE)
          median_time <- data.frame(raceid = tbls$raceid[1],
                                    median_time = median(tbls$time,na.rm = TRUE))
          dbWriteTable(conn = con_remote,
                       name = "median_race_time",
                       value = median_time,
                       row.names = FALSE,
                       overwrite = FALSE,
                       append = TRUE)
          race_url <- data.frame(raceid = tbls$raceid[1],
                                 url1 = raceInfo$url$qual,
                                 url2 = raceInfo$url$final)
          dbWriteTable(conn = con_remote,
                       name = "race_url",
                       value = race_url,
                       row.names = FALSE,
                       overwrite = FALSE,
                       append = TRUE)
          if (!check){
            stop("Upload failed.")
          }
          sql <- "insert into main (raceid,date,season,location,
                                    gender,length,tech,type,start,
                                    cat1,cat2,fisid,name,yob,age,nation,
                                    rank,rankqual,time,fispoints) 
                              values (?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)"
          bulk_insert(con_local,sql,tbls)
          dbWriteTable(conn = con_local,
                       name = "median_race_time",
                       value = median_time,
                       row.names = FALSE,
                       overwrite = FALSE,
                       append = TRUE)
          dbWriteTable(conn = con_local,
                       name = "race_url",
                       value = race_url,
                       row.names = FALSE,
                       overwrite = FALSE,
                       append = TRUE)
        }
      }
    }
    
    }
  dbDisconnect(con_local)
  dbDisconnect(con_remote)
}