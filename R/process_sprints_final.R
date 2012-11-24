processSprintsFinalOnly <- function(tblsFinal,raceInfo){
  colnames(tblsFinal) <- c('rank','bib','fisid','name','yob',
                           'nation','time','fispoints')
  tblsFinal <- subset(tblsFinal,rank != '')
  
  tblsFinal$bib <- NULL
  tblsFinal$rank <- as.integer(tblsFinal$rank)
  tblsFinal$yob <- as.integer(tblsFinal$yob)
  tblsFinal$fispoints <- NA
  
  tblsFinal$rankqual <- NA
  
  tblsFinal$date <- raceInfo$date
  tblsFinal$season <- raceInfo$season
  tblsFinal$cat1 <- raceInfo$cat1
  tblsFinal$cat2 <- raceInfo$cat2
  tblsFinal$location <- raceInfo$location
  tblsFinal$gender <- raceInfo$gender
  tblsFinal$type <- raceInfo$type
  tblsFinal$start <- raceInfo$start
  tblsFinal$tech <- raceInfo$tech
  tblsFinal$length <- raceInfo$length
  tblsFinal$age <- as.integer(substr(raceInfo$date,1,4)) - tblsFinal$yob
  tblsFinal$raceid <- getMaxRaceID() + 1
  tbls <- tblsFinal[,c('raceid','date','season','location','gender','length','tech',
                       'type','start','cat1','cat2','fisid','name','yob','age','nation',
                       'rank','rankqual','time','fispoints')]
  #tbls$time <- convertTime(tbls$time,raceInfo$type)
  tbls$time <- NA
  tbls
  
}
