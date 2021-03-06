% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_data.R
\name{get_pop_data}
\alias{get_pop_data}
\title{retrieves population data, matches countries with continents and subcontinents}
\usage{
get_pop_data()
}
\value{
global tibble of confirmed, deaths, active and recovered for each day by population
}
\description{
retrieves population data, matches countries with continents and subcontinents
}
\note{
additional demographic variables are available for further development
}
\examples{
\dontrun{
orig_data <- get_timeseries_full_data() \%>\%
              get_timeseries_by_contagion_day_data()
data <- orig_data \%>\% align_country_names()

}

}
