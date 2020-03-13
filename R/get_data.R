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
daily_url <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/"

#' Get timeseries data
#' @rdname get_timeseries_data
#'
#' @return data list of confirmed, deaths and recovered, each for Province.State, Country.Region, Lat, Long, and day
#'
#' @export
get_timeseries_data <- function() {
  data <- list(
    confirmed = read.csv(file = confirmed_timeseries_csv_url, stringsAsFactors = FALSE),
    deaths = read.csv(file = deaths_timeseries_csv_url, stringsAsFactors = FALSE),
    recovered = read.csv(file = recovered_timeseries_csv_url, stringsAsFactors = FALSE)
  )
}

#' Get daily data
#' @rdname get_daily_data
#'
#' @param date string of format mm-dd-yyyy
#'
#' @return data data.frame
#'
#' @export
get_daily_data <- function(date) {
    data <- read.csv(file = paste0(daily_url, date, ".csv"), stringsAsFactors = FALSE)
}

#' Get timeseries single data
#' @rdname get_timeseries_single_data
#' @param type data to be returned. Character string. Expected values c("confirmed", "deaths", "recovered")
#'
#' @return data data.frame for Province.State, Country.Region, Lat, Long, and day
#'
#' @export
get_timeseries_single_data <- function(param) {
    data <- read.csv(file = eval(as.symbol(paste0(param,"_timeseries_csv_url"))), stringsAsFactors = FALSE)
}


#' Get timeseries full data
#' @rdname get_timeseries_full_data
#'
#' @return data tibble of confirmed, deaths, active and recovered, each for Province.State, Country.Region, Lat, Long, and day
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr left_join
#' @importFrom dplyr arrange
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr starts_with
#'
#' @export
get_timeseries_full_data <- function() {

  convert_date <- function(df){
    df %>%
      mutate(date = gsub("X", "0", date)) %>%
      mutate(date = as.Date(date, format = "%m.%d.%y")) %>%
      arrange(date)
  }

  confirmed <- get_timeseries_single_data("confirmed") %>%
    pivot_longer(cols = starts_with("X"), names_to = "date", values_to = "confirmed") %>%
    convert_date()
  deaths <- get_timeseries_single_data("deaths")  %>%
    pivot_longer(cols = starts_with("X"), names_to = "date", values_to = "deaths") %>%
    convert_date()
  recovered <- get_timeseries_single_data("recovered")  %>%
    pivot_longer(cols = starts_with("X"), names_to = "date", values_to = "recovered") %>%
    convert_date()

  join_by_cols <- c("Province.State", "Country.Region", "Lat", "Long", "date")

  data <- confirmed %>%
    left_join(deaths, by = join_by_cols) %>%
    left_join(recovered, by = join_by_cols) %>%
    mutate(active = confirmed - deaths - recovered)

}


#' Get data by day contagion
#' @rdname get_timeseries_by_contagion_day_data
#'
#' @param data data.frame
#'
#' @return data tibble of confirmed, deaths, active and recovered, each for Province.State, Country.Region, Lat, Long, contagion_day and day
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr case_when
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr select
#' @importFrom dplyr n
#'
#' @export
get_timeseries_by_contagion_day_data <- function(data) {

  data <- data %>%
    arrange(desc(Country.Region), date) %>%
    mutate(no_contagion = case_when(
      confirmed == 0 ~ 1,
      TRUE ~ 0
    )) %>%
    group_by(Province.State, Country.Region, Lat, Long) %>%
    mutate(incremental = seq(1:n())) %>%
    mutate(offset = sum(no_contagion)) %>%
    mutate(tmp = incremental - offset) %>%
    mutate(contagion_day = case_when(
      tmp < 0 ~ 0,
      TRUE ~ tmp
    )) %>%
    select(-incremental, -offset, -no_contagion, -tmp) %>%
    ungroup()

}

#' Global data timeseries
#' @rdname get_timeseries_global_data
#'
#' @param data data.frame
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr summarize
#'
#' @return global tibble of global confirmed, deaths, active and recovered, for each day
#'
#' @export
get_timeseries_global_data <- function(data){
  global <- data %>%
    group_by(date) %>%
    summarize(confirmed = sum(confirmed), deaths = sum(deaths), recovered = sum(recovered), active = sum(active)) %>%
    ungroup()
}

#' Country.Region data timeseries
#' @rdname get_timeseries_country_data
#'
#' @param data data.frame
#' @param country name of the country. Character string
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr summarize
#' @importFrom dplyr filter
#'
#' @return country_df tibble of by country confirmed, deaths, active and recovered for each day
#'
#' @export
get_timeseries_country_data <- function(data, country){
  country_df <- data %>%
    filter(Country.Region == country) %>%
    group_by(date) %>%
    summarize(confirmed = sum(confirmed), deaths = sum(deaths), recovered = sum(recovered), active = sum(active), contagion_day = max(contagion_day)) %>%
    ungroup()
}

#' Aggregate data by country
#' @rdname aggregate_country_data
#'
#' @param data data.frameing
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr filter
#' @importFrom dplyr summarize
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#'
#' @return country_df tibble of by country confirmed, deaths, active and recovered for each day
#'
#' @export
aggregate_country_data <- function(data){
  country_df <- data %>%
    filter( date == max(date)) %>%
    select(-Province.State, -Lat, -Long) %>%
    group_by(Country.Region) %>%
    summarize(confirmed = sum(confirmed), deaths = sum(deaths), recovered = sum(recovered), active = sum(active), contagion_day = max(contagion_day)) %>%
    arrange(desc(confirmed)) %>%
    ungroup()
}

#' Aggregate data by province
#' @rdname aggregate_province_timeseries_data
#'
#' @param data data.frameing
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr summarize
#'
#' @return df tibble of by country confirmed, deaths, active and recovered for each day
#'
#' @export
aggregate_province_timeseries_data <- function(data){
  df <- data %>%
    select(-Province.State) %>%
    group_by(Country.Region, date) %>%
    summarize(confirmed = sum(confirmed), deaths = sum(deaths), recovered = sum(recovered), active = sum(active), contagion_day = max(contagion_day)) %>%
    ungroup()
}

#' Province.State data timeseries
#' @rdname get_timeseries_province_data
#'
#' @param data data.frame
#' @param province name of the province Character string
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr summarize
#' @importFrom dplyr filter
#'
#' @return province_df tibble of by country confirmed, deaths, active and recovered for each day
#'
#' @export
get_timeseries_province_data <- function(data, province){
  province_df <- data %>%
    filter(Province.State == province) %>%
    group_by(date) %>%
    summarize(confirmed = sum(confirmed), deaths = sum(deaths), recovered = sum(recovered), active = sum(active)) %>%
    ungroup()
}

#' date data
#' @rdname get_date_data
#'
#' @param data data.frame
#' @param date date format yyyy-mm-dd
#'
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr summarize
#' @importFrom dplyr filter
#'
#' @return global tibble of confirmed, deaths, active and recovered for each day
#'
#' @export
get_date_data <- function(data, date){
  date_df <- data %>%
    group_by(date) %>%
    summarize(confirmed = sum(confirmed), deaths = sum(deaths), recovered = sum(recovered), active = sum(active)) %>%
    ungroup()
}
