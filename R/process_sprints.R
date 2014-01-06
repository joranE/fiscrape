processSprints <- function(tblsQual,tblsFinal,raceInfo){
  #browser()
  colnames(tblsQual) <- c('rank','bib','fisid','name','yob',
                          'nation','time','fispoints')
  if (ncol(tblsFinal) == 8){
    colnames(tblsFinal) <- c('rank','bib','fisid','name','yob',
                             'nation','time','fispoints')
  }
  if (ncol(tblsFinal) == 7){
    colnames(tblsFinal) <- c('rank','bib','fisid','name','yob',
                             'nation','time')  
  }
  if (ncol(tblsFinal) == 6){
    colnames(tblsFinal) <- c('rank','bib','fisid','name','yob',
                             'nation')  
  }									    
  tblsQual <- subset(tblsQual,rank != '' & !is.na(name))
  tblsFinal <- subset(tblsFinal,rank != '' & !is.na(name))
  
  tblsQual$bib <- tblsFinal$bib <- NULL
  
  tblsQual$rank <- as.integer(tblsQual$rank)
  tblsFinal$rank <- as.integer(tblsFinal$rank)
  tblsQual$yob <- as.integer(tblsQual$yob)
  tblsQual$fispoints <- as.numeric(tblsQual$fispoints)
  
  tblsFinal <- tblsFinal[,1:3]
  colnames(tblsQual)[1] <- 'rankqual'
  
  tblsQual$rank <- tblsFinal$rank[match(tblsQual$fisid,tblsFinal$fisid)]
  tblsQual$date <- raceInfo$date
  tblsQual$season <- raceInfo$season
  tblsQual$cat1 <- raceInfo$cat1
  tblsQual$cat2 <- raceInfo$cat2
  tblsQual$location <- raceInfo$location
  tblsQual$gender <- raceInfo$gender
  tblsQual$type <- raceInfo$type
  tblsQual$start <- raceInfo$start
  tblsQual$tech <- raceInfo$tech
  tblsQual$length <- raceInfo$length
  tblsQual$age <- as.integer(substr(raceInfo$date,1,4)) - tblsQual$yob
  tblsQual$raceid <- getMaxRaceID() + 1
  tbls <- tblsQual[,c('raceid','date','season','location','gender','length','tech',
                      'type','start','cat1','cat2','fisid','name','yob','age','nation',
                      'rank','rankqual','time','fispoints')]
  tbls$time <- convertTime(tbls$time,raceInfo$type)
  tbls
}
