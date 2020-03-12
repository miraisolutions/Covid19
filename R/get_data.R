#' Data Urls confirmed_timeseries
#' @rdname DataUrls
#'
#' @export
confirmed_timeseries_csv_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"

#' Data Urls deaths_timeseries_csv_url
#' @rdname DataUrls
#'
#' @export
deaths_timeseries_csv_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"

#' Data Urls recovered_timeseries_csv_url
#' @rdname DataUrls
#'
#' @export
recovered_timeseries_csv_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv"

#' Data Urls daily_url
#' @rdname DataUrls
#'
#' @export
daily_url <- "https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data/csse_covid_19_daily_reports"

#' Get timeseries data
#' @rdname get_timeseries_data
#'
#' @return data list of confirmed, deaths and recovered, each for Province.State, Country.Region, Lat, Long, and day
#'
#' @export
get_timeseries_data <- function() {
  data <- list(
    confirmed = read.csv(file = confirmed_timeseries_csv_url),
    deaths = read.csv(file = deaths_timeseries_csv_url),
    recovered = read.csv(file = recovered_timeseries_csv_url)
  )
}
