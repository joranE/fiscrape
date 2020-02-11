library(RSQLite)
conl <- RSQLite::dbConnect(RSQLite::SQLite(),dbname = "~/Dropbox/new-results-db/output/fis_results_prototype.db")
src_skier <- tbl(conl,"skier")
#### Distance Races ####
#Interval start
race_info <- list(date = "2020-02-01",
                  season = "2019-2020",
                  gender = "Men",
                  type = "Distance",
                  location = "FIN",
                  length = 15,
                  tech = "C",
                  format = "Interval",
                  cat1 = "NC",
                  cat2 = NA)
url <- "https://www.fis-ski.com/DB/general/results.html?sectorcode=CC&raceid=35122"

#x <- fiscrape:::dst_scrape(url = url,raceInfo = race_info)

#Mass start
race_info <- list(date = "2020-01-18",
                  season = "2019-2020",
                  gender = "Men",
                  type = "Distance",
                  location = "ITA",
                  length = 30,
                  tech = "C",
                  format = "Mass",
                  cat1 = "OPA",
                  cat2 = NA)
url <- "https://www.fis-ski.com/DB/general/results.html?sectorcode=CC&raceid=35393"

#Skiathlon
race_info <- list(date = "2019-12-07",
                  season = "2019-2020",
                  gender = "Women",
                  type = "Distance",
                  location = "NOR",
                  length = 15,
                  tech = "FC",
                  format = "Skiathlon",
                  cat1 = "WC",
                  cat2 = NA)
url <- "https://www.fis-ski.com/DB/general/results.html?sectorcode=CC&raceid=34341"

#Pursuit
race_info <- list(date = "2020-01-19",
                  season = "2019-2020",
                  gender = "Men",
                  type = "Distance",
                  location = "CZE",
                  length = 15,
                  tech = "C",
                  format = "Pursuit",
                  cat1 = "WC",
                  cat2 = NA)
url <- "https://www.fis-ski.com/DB/general/results.html?sectorcode=CC&raceid=34394"

#Mass start marathon
race_info <- list(date = "2020-01-26",
                  season = "2019-2020",
                  gender = "Women",
                  type = "Distance",
                  location = "ITA",
                  length = 70,
                  tech = "C",
                  format = "Mass",
                  cat1 = "ML",
                  cat2 = NA)
url <- "https://www.fis-ski.com/DB/general/results.html?sectorcode=CC&raceid=35698"

#Sprint qualifier
race_info <- list(date = "2020-02-07",
                  season = "2019-2020",
                  gender = "Women",
                  type = "Sprint",
                  location = "CAN",
                  length = 1.4,
                  tech = "F",
                  format = NA,
                  cat1 = "FIS",
                  cat2 = NA)
url <- "https://www.fis-ski.com/DB/general/results.html?sectorcode=CC&raceid=36112"