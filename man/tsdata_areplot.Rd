% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{tsdata_areplot}
\alias{tsdata_areplot}
\title{creates time series for the area plot}
\usage{
tsdata_areplot(data, levs, nn = 1000)
}
\arguments{
\item{data}{data.frame aggregated data per region}

\item{levs}{order of statuses}

\item{nn}{minimum number of cases for the start date}
}
\value{
data.frame reshaped
}
\description{
creates time series for the area plot
}
\note{
starting date based on nn, first day with so many confirmed cases
}
