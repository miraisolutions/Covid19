% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_data.R
\name{get_datahub_fix_ch}
\alias{get_datahub_fix_ch}
\alias{get_datahub}
\title{Get timeseries full data from datahub adding CH hospitalised data from level 2}
\usage{
get_datahub_fix_ch(
  country = NULL,
  startdate = "2020-01-22",
  lev = 1,
  verbose = FALSE
)

get_datahub(
  country = NULL,
  startdate = "2020-01-22",
  lev = 1,
  verbose = FALSE,
  hosp = TRUE
)
}
\arguments{
\item{country}{character country, to chose with lev = 2}

\item{startdate}{character staring date}

\item{lev}{integer 1 for country level, 2 for reagions}

\item{verbose}{logical. Print data sources? Default FALSE (opposite from \code{covid19})}

\item{hosp}{logical. If TRUE hospitalised detailed data are retrieved. Default TRUE since release 2.3.1}
}
\value{
data tibble of confirmed, deaths, active and recovered etc Country.Region

data tibble of confirmed, deaths, active and recovered Country.Region
}
\description{
Get timeseries full data from datahub adding CH hospitalised data from level 2

Get timeseries full data from datahub
}
\details{
data sourced from https://github.com/covid19datahub/COVID19/

data sourced from https://github.com/covid19datahub/COVID19/
}
