processSprintsQualOnly <- function(tblsQual,raceInfo){
  colnames(tblsQual) <- c('rankqual','bib','fisid','name','yob',
                          'nation','time','fispoints')
  tblsQual <- subset(tblsQual,rankqual != '')	
  tblsQual$bib <- NULL
  
  tblsQual$rank <- NA
  tblsQual$yob <- as.integer(tblsQual$yob)
  tblsQual$fispoints <- as.numeric(tblsQual$fispoints)
  
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
