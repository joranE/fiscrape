% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/delete_race.R
\name{delete_race}
\alias{delete_race}
\title{Delete Race from Database}
\usage{
delete_race(raceid = NULL)
}
\arguments{
\item{raceid}{integer}
}
\description{
Delete race data from both remote and local
databases from main, race_url and median_race_time
tables all in one go. Useful for when FIS fucks up the
results and you need to start over.
}

