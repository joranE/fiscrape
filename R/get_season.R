getSeason <- function(dt){
  yr <- as.integer(substr(dt,1,4))
  mo <- as.integer(substr(dt,6,7))
  if (mo >= 6){
    season <- paste(yr,"-",yr+1,sep = "")
  }
  else{
    season <- paste(yr-1,"-",yr,sep = "")
  }
  season
}
