library(dplyr)
library(RSQLite)
con <- RSQLite::dbConnect(RSQLite::SQLite(),
                           dbname = "~/Dropbox/new-results-db/output/fis_results_prototype.db")
src_skier <- tbl(conl,"skier")
src_event <- tbl(conl,"v_event")

#### Distance Races ####
#Interval start
event_info <- list(date = "2020-02-01",
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

#Mass start
event_info <- list(date = "2020-01-18",
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
event_info <- list(date = "2019-12-07",
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
event_info <- list(date = "2020-01-19",
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
event_info <- list(date = "2020-01-26",
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
event_info <- list(date = "2020-02-07",
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


x <- fiscrape:::dst_scrape(url = url,event_info = event_info)
missing_bday(x[["skier"]],conl)

#### Sprint Final Races ####
event_info <- gather_event_info()
event_info$season <- get_season(event_info$date)
x1 <- fiscrape:::spr_final_scrape(event_info,1)
x2 <- fiscrape:::spr_final_scrape(event_info,2)
x3 <- fiscrape:::spr_final_scrape(event_info,3)