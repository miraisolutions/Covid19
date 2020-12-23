# Data taken from here https://github.com/bumbeishvili/covid19-daily-data

#' Data Urls confirmed_timeseries
#' @rdname DataUrls
#'
#' @export
confirmed_timeseries_csv_url <- "https://raw.githubusercontent.com/bumbeishvili/covid19-daily-data/master/time_series_19-covid-Confirmed.csv"

#' Data Urls deaths_timeseries_csv_url
#' @rdname DataUrls
#'
#' @export
deaths_timeseries_csv_url <- "https://raw.githubusercontent.com/bumbeishvili/covid19-daily-data/master/time_series_19-covid-Deaths.csv"

#' Data Urls recovered_timeseries_csv_url
#' @rdname DataUrls
#'
#' @export
recovered_timeseries_csv_url <- "https://raw.githubusercontent.com/bumbeishvili/covid19-daily-data/master/time_series_19-covid-Recovered.csv"

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
#'
#' @param param character string
#'
#' @return data data.frame for Province.State, Country.Region, Lat, Long, and day
#'
#' @export
get_timeseries_single_data <- function(param) {
  data <- read.csv(file = eval(as.symbol(paste0(param,"_timeseries_csv_url"))), stringsAsFactors = FALSE)
}

#' named vector of french colonies to be added to France
#'
French.Colonies = c("Reunion", "Martinique", "French Guiana", "Guadeloupe", "St. Barth", "St Martin")

#' Get timeseries full data
#' @rdname get_timeseries_full_data
#'
#' @return data tibble of confirmed, deaths, active and recovered, each for Province.State, Country.Region, Lat, Long, and day
#'
#' @import dplyr
#' @import tidyr
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
    convert_date() %>%
    mutate(confirmed = round(confirmed)) # necessary for worldometers data, which is evenly spread across US states
  deaths <- get_timeseries_single_data("deaths")  %>%
    pivot_longer(cols = starts_with("X"), names_to = "date", values_to = "deaths") %>%
    convert_date() %>%
    mutate(deaths = round(deaths)) # necessary for worldometers data, which is evenly spread across US states
  recovered <- get_timeseries_single_data("recovered")  %>%
    pivot_longer(cols = starts_with("X"), names_to = "date", values_to = "recovered") %>%
    convert_date() %>%
    mutate(recovered = round(recovered)) # necessary for worldometers data, which is evenly spread across US states

  join_by_cols <- c("Province.State", "Country.Region", "Lat", "Long", "date")

  data <- confirmed %>%
    left_join(deaths, by = join_by_cols) %>%
    left_join(recovered, by = join_by_cols) %>%
    mutate_if(is.numeric, function(x){x = replace_na(x, 0)}) #%>% #control NAs
    #mutate(active = confirmed - deaths - recovered)

  sumcountries = function(data,tocountry, fromcountry) {
    message("Adding ", paste(fromcountry, collapse = ","), " to ", tocountry)
    data[data$Country.Region == tocountry, c("confirmed", "deaths", "recovered") ] =
      data %>% filter(Country.Region %in% c(tocountry,fromcountry ) ) %>% group_by(date) %>%
        select(date, confirmed, deaths, recovered) %>%
        summarize(confirmed = sum(confirmed), deaths = sum(deaths), recovered = sum(recovered)) %>%
        ungroup() %>% select(confirmed, deaths, recovered)
    # remove from countries
    data = data[!is.element(data$Country.Region,fromcountry), , drop = F]
    data
  }

  # rename some countries
  data$Country.Region[grepl("^Cura", data$Country.Region)] = "Curasao"

  # clean French colonies
  data = sumcountries(data, tocountry = "France", fromcountry = French.Colonies)
  # Add congo Brazzville to Congo
  data = sumcountries(data, tocountry = "Congo", fromcountry = c("Congo (Brazzaville)"))
  # Add "Cape Verde" "Cabo Verde"
  data = sumcountries(data, tocountry = "Cape Verde", fromcountry = c("Cabo Verde"))

  # compute active
  data = data %>%
       mutate(active = confirmed - deaths - recovered)

  data
}

#' Get timeseries full data from datahub adding CH hospitalised data from level 2
#' @rdname get_datahub
#'
#' @param country character country, to chose with lev = 2
#' @param startdate character staring date
#' @param lev integer 1 for country level, 2 for reagions
#' @param verbose logical. Print data sources? Default FALSE (opposite from \code{covid19})
#'
#' @details data sourced from https://github.com/covid19datahub/COVID19/
#'
#' @return data tibble of confirmed, deaths, active and recovered etc Country.Region
#'
#' @importFrom COVID19 covid19
#' @import dplyr
#'
#' @export
get_datahub_fix_ch <- function(country = NULL, startdate = "2020-01-22", lev = 1, verbose = FALSE) {

  orig_data <-
    get_datahub(country = country, startdate = startdate, lev = lev, verbose = verbose)

  orig_data_ch_2 <-
    get_datahub(country = "Switzerland", startdate = startdate, lev = 2, verbose = verbose)

  message("replace hosp data (",paste(.hosp_vars, collapse = ","), ") in lev1 dataset with lev2 swiss data")

  # aggregate hosp data at country level
  orig_data_ch_1 = orig_data_ch_2 %>%
    select(date, all_of(as.vector(.hosp_vars))) %>%
    group_by(date) %>%
    summarise_if(is.numeric, sum, na.rm = TRUE) %>% # todo, use only hospvars
    ungroup() %>%
    mutate(Country.Region = "Switzerland")
  orig_data_ch_1 = orig_data_ch_1[, c("Country.Region", setdiff(names(orig_data_ch_1),"Country.Region"))]

  if (!identical(orig_data_ch_1$date, orig_data$date[orig_data$Country.Region == "Switzerland"]))
    warning("Not same dates in lev1 and lev2 for CH")

  commondates = match(orig_data_ch_1$date ,orig_data$date[orig_data$Country.Region == "Switzerland"])

  orig_data[orig_data$Country.Region == "Switzerland" & (orig_data$date %in% orig_data_ch_1$date), .hosp_vars] <-
    orig_data_ch_1[(orig_data_ch_1$date %in% orig_data$date) , .hosp_vars]

  list(orig_data = orig_data, orig_data_ch_2 = orig_data_ch_2)
}


#' Get timeseries full data from datahub
#' @rdname get_datahub
#'
#' @param country character country, to chose with lev = 2
#' @param startdate character staring date
#' @param lev integer 1 for country level, 2 for reagions
#' @param verbose logical. Print data sources? Default FALSE (opposite from \code{covid19})
#' @param hosp logical. If TRUE hospitalised detailed data are retrieved. Default TRUE since release 2.3.1
#'
#' @details data sourced from https://github.com/covid19datahub/COVID19/
#'
#' @return data tibble of confirmed, deaths, active and recovered Country.Region
#'
#' @importFrom COVID19 covid19
#' @import dplyr
#'
#' @export
get_datahub = function(country = NULL, startdate = "2020-01-22", lev = 1, verbose = FALSE, hosp = TRUE) {
  message("get_datahub: country = ", country, "/ startdate = ", startdate, "/ level = ", lev)
  rawarg = FALSE
  if (!is.null(country)) {
    # remap country )
    country = recode(country,
                     "Republic of the Congo" = "Congo, the Democratic Republic of the",
                     "Vatican City" = "Holy See",
                     "South Korea" = "Korea, South",
                     "North Macedonia" = "Saint Vincent and the Grenadine",
                     "St. Vincent Grenadines" = "Saint Vincent and the Grenadines",
                     "UK" = "United Kingdom",
                     "USA" = "United States",
                     "U.S. Virgin Islands" = "U.S. Virgin Islands",
                     )
  }
  dataHub <- covid19(country = country, start = startdate, level = lev, verbose = verbose, raw = rawarg) # select level2 to add states
  # raw = FALSE then NAs replaced with 0s

  vars = c("date", "tests", "confirmed", "recovered", "deaths", "hosp", "stringency_index","population")
  if (hosp) {
    vars = append(vars, setdiff(.hosp_vars_datahub, vars), which(vars == "hosp"))
  }
  # if Hong Kong was chosen in country
  if ((is.null(dataHub) || nrow(dataHub) == 0) && lev == 1 && country == "Hong Kong") {
    message("Taking Hong Kong from chinese data from level 2")
    dataHub <- covid19("China",2, start = startdate, verbose = verbose, raw = rawarg) %>% ungroup() %>%
      select( administrative_area_level_2, !!vars) %>%
      rename(Country.Region = administrative_area_level_2) %>%
      filter(Country.Region == "Hong Kong")
  } else if (!is.null(dataHub) && nrow(dataHub) > 0) {
    adminvar = paste("administrative_area_level", lev, sep = "_")
    # select varaibles for backwards compatibility + some additional variables
    dataHub = dataHub %>% ungroup() %>% select(!!adminvar, #id, # at the moment removing ID
                                               !!vars) %>%
      rename(Country.Region = !!adminvar) # rename country variable

    if (lev == 1) {
      # recode some countries for backwards compatibility
      dataHub$Country.Region = dataHub$Country.Region %>%
        recode(
          "Congo, the Democratic Republic of the" = "Republic of the Congo",
          "Holy See" = "Vatican City",
          "Korea, South" = "South Korea",
          "Macedonia" = "North Macedonia",
          "Saint Vincent and the Grenadines" = "St. Vincent Grenadines",
          "United Kingdom" = "UK",
          "United States" = "USA",
          "Virgin Islands, U.S." = "U.S. Virgin Islands"
        )
    }
    # "Northern Mariana Islands" belongs to USA
    # "Virgin Islands, U.S." belongs to USA

    # if either china or hong kong missing
    if (FALSE) { # switch off handling of Hong Kong, too time consuming
      if (length(setdiff(c("Hong Kong","China"), dataHub$Country.Region))==1) {
        if (lev == 1) {
          message("Taking chinese data from level 2")
          dataMiss <- covid19("China",2, start = startdate, verbose = verbose) %>% ungroup() %>%
            select(administrative_area_level_1, administrative_area_level_2, !!vars) %>%
            rename(Country.Region = administrative_area_level_1)
          # separate Hong Kong
          if (is.null(country) && (!("Hong Kong" %in% dataHub$Country.Region))) {
            dataHG = dataMiss %>% filter(administrative_area_level_2 == "Hong Kong") %>%
              mutate(Country.Region = "Hong Kong") %>% select(-administrative_area_level_2)
            dataHub = rbind(dataHub, dataHG)
          }
          dataMiss = dataMiss %>% filter(administrative_area_level_2 != "Hong Kong") %>% # exclude hong kong
            group_by(Country.Region,date) %>%
            summarise_if(is.numeric, sum, na.rm = TRUE) %>%
            ungroup()

          dataHub = dataHub %>% filter(Country.Region != "China") # remove china in case it was present

          dataHub = rbind(dataHub, dataMiss) %>% arrange(Country.Region,date)
          #dataHub
        }
    }

    }
    if (lev == 2 && country == "China") {
        message("remove Hong Kong from China")
        dataHub = filter(dataHub, Country.Region != "Hong Kong")
    }
  }
  if (!is.null(dataHub) && nrow(dataHub) > 0) {
    # adjust recovered where they do not make sense, e.g. France lev 2
    #dataHub$recovered[is.na(dataHub$recovered)] = 0 #TODO: review, required only with COVID19 2.3.1
    dataHub$recovered = pmin(dataHub$recovered, dataHub$confirmed, na.rm = TRUE)
    dataHub$deaths    = pmin(dataHub$deaths, dataHub$confirmed, na.rm = TRUE)

    # compute active
    dataHub = dataHub %>%
      #mutate(active = confirmed - deaths - recovered) %>%
      mutate(active = confirmed - replace_na(deaths,0) - replace_na(recovered,0)) #%>%
      #mutate(active = replace_na(active, 0)) # TODO review

    # convert integers into numeric
    dataHub[,sapply(dataHub, class) == "integer"] = dataHub[,sapply(dataHub, class) == "integer"] %>% sapply(as.numeric)

    # take yesterday, data are updated hourly and they are complete around mid day, 40h later
    # regardless of the timezone, select the day 40h ago
    now = as.POSIXct(Sys.time()) # given time zone
    maxdate =  as.character(as.Date(now - 40*60*60))

    message("Maximum date set to: ", maxdate)
    #TODO: arrange should go descending, many rows could be filtered out for many countries#
    dataHub = dataHub %>% filter(date <= maxdate) %>% arrange(Country.Region, date)

    if (hosp) {
      message("Edit hosp data")
      # hospitalised must be
      dataHub$icuvent = replace_na(dataHub$vent,0)+replace_na(dataHub$icu,0) # check NAs
      dataHub$icuvent[is.na(dataHub$vent) & is.na(dataHub$icu) ] = NA # NA if both were Na
      dataHub$hosp = pmax(dataHub$hosp, dataHub$icuvent, na.rm = TRUE) # generally it can be lower
      dataHub = dataHub %>% select(-icu,-vent) # remove icu and vent
      if (any(dataHub$active < dataHub$hosp)) {
        warning(sum(dataHub$active < dataHub$hosp, na.rm = TRUE), " cases with active < hosp")
      }
      # fill NAs with 0 for hosp data

      # remove rows countries without data to reduce dataset
      #dataHub = dataHub[!is.na(dataHub$confirmed), , drop = FALSE]
      # remove countries without data
      if (F) {
        country_nodata = dataHub %>% group_by(Country.Region) %>% summarize(allnas = all(is.na(confirmed))) %>% ungroup() %>% filter(allnas == TRUE) %>% .[,1] %>% as.character()
        dataHub = dataHub[!(dataHub$Country.Region %in% country_nodata), , drop = FALSE]

      }
    }

  }  else {
    warning("Data not found for country = ", country, " startdate = ", startdate, " level = ", lev)
  }

  dataHub
}


#' Get data by day contagion
#' @rdname get_timeseries_by_contagion_day_data
#'
#' @param data data.frame
#'
#' @return data tibble of confirmed, deaths, active and recovered, each for Province.State, Country.Region, Lat, Long, contagion_day and day
#'
#' @import dplyr
#' @import tidyr
#'
#' @export
get_timeseries_by_contagion_day_data <- function(data) {

  if (!("tests" %in% names(data))) {
    data$tests = data$hosp = rep(0,nrow(data))
  }
  # Select column names for which new_vars to be created
  new_vars = intersect(setdiff(get_aggrvars(), "population"), names(data))

  new_calc = function(x) {
    x - replace_na(lag(x),0)
  }
  data1 <- data %>%
    arrange(desc(Country.Region), date) %>%
    mutate(no_contagion = case_when(
      confirmed == 0 ~ 1,
      TRUE ~ 0
    )) %>%
    group_by(Country.Region) %>%
    mutate(incremental = seq(1:n())) %>%
    mutate(offset = sum(no_contagion)) %>%
    mutate(tmp = incremental - offset) %>%
    mutate(contagion_day = case_when(
      tmp < 0 ~ 0,
      TRUE ~ tmp
    )) %>%
    select(-incremental, -offset, -no_contagion, -tmp) %>%
    # added replace_na around lag to avoid NA in fist position
    mutate(
      across(all_of(new_vars), new_calc, .names="new_{col}") # use all_of
    ) %>%
    # mutate(
    #   tot_hosp = cumsum(new_hosp, na.rm = TRUE),
    #   tot_icuvent = cumsum(new_icuvent, na.rm = TRUE),
    #   )
    # added replace_na around lag to avoid NA in fist position
    # mutate(new_confirmed = confirmed - replace_na(lag(confirmed),0),
    #     new_deaths = deaths - replace_na(lag(deaths),0),
    #     new_active = active - replace_na(lag(active),0),
    #     new_recovered = recovered - replace_na(lag(recovered),0),
    #     new_tests = tests - replace_na(lag(tests),0),
    #     new_hosp = hosp - replace_na(lag(hosp),0)
    #     # new_vent = vent - replace_na(lag(vent),0),
    #     # new_icu = icu - replace_na(lag(icu),0)
    #     ) %>%
    # mutate(new_confirmed = if_else(is.na(new_confirmed), 0, new_confirmed)) %>%
    # mutate(new_deaths = if_else(is.na(new_deaths), 0, new_deaths)) %>%
    # mutate(new_active = if_else(is.na(new_active), 0, new_active)) %>%
    # mutate(new_recovered = if_else(is.na(new_recovered), 0, new_recovered)) %>%
    ungroup()
  #TODO: remove all 0s confirmed from all countries
  #data1 = filter(data1, contagion_day != 0)
  data1
}

#' Global data timeseries
#' @rdname get_timeseries_global_data
#'
#' @param data data.frame
#' @param new logical, if TRUE then new variables are also considered
#'
#' @import dplyr
#'
#' @return global tibble of global confirmed, deaths, active and recovered, for each day
#'
#' @export
get_timeseries_global_data <- function(data, new = FALSE){
  vars =  intersect(get_aggrvars(), names(data))
  if (!new)
    vars = setdiff(vars, grep("new",vars, value = TRUE))
  vars = intersect(vars, names(data))
  data %>%
    group_by(date) %>%
    summarize_at(vars, sum, na.rm = TRUE) %>%
    ungroup()
}

#' Country.Region data timeseries
#' @rdname get_timeseries_country_data
#'
#' @param data data.frame
#' @param country name of the country. Character string
#'
#' @import dplyr
#'
#' @return country_df tibble of by country confirmed, deaths, active and recovered for each day
#'
#' @export
get_timeseries_country_data <- function(data, country){
  country_df <- data %>%
    filter(Country.Region == country) %>%
    group_by(date) %>%
    summarize(confirmed = sum(confirmed), deaths = sum(deaths), recovered = sum(recovered), active = sum(active), new_confirmed = sum(new_confirmed), new_deaths = sum(new_deaths), new_active = sum(new_active), new_recovered = sum(new_recovered), contagion_day = max(contagion_day)) %>%
    arrange(desc(date)) %>%
    ungroup()
}

#' Aggregate data by country
#' @rdname aggregate_country_data
#'
#' @param data data.frameing
#'
#' @import dplyr
#'
#' @return country_df tibble of by country confirmed, deaths, active and recovered for each day
#'
#' @export
aggregate_country_data <- function(data){
  country_df <- data %>%
    filter( date == max(date)) %>%
    select(-Province.State, -Lat, -Long) %>%
    group_by(Country.Region) %>%
    summarize(confirmed = sum(confirmed), deaths = sum(deaths), recovered = sum(recovered), active = sum(active), new_confirmed = sum(new_confirmed), new_deaths = sum(new_deaths), new_active = sum(new_active), new_recovered = sum(new_recovered), contagion_day = max(contagion_day)) %>%
    arrange(desc(confirmed)) %>%
    ungroup()
}

#' Aggregate data by province
#' @rdname aggregate_province_timeseries_data
#'
#' @param data data.frameing
#'
#' @import dplyr
#' @import tidyr
#'
#' @return df tibble of by country confirmed, deaths, active and recovered for each day
#'
#' @export
aggregate_province_timeseries_data <- function(data){
  df <- data %>%
    select(-Province.State) %>%
    group_by(Country.Region, date) %>%
    summarize(confirmed = sum(confirmed), deaths = sum(deaths), recovered = sum(recovered), active = sum(active), new_confirmed = sum(new_confirmed), new_deaths = sum(new_deaths), new_active = sum(new_active), new_recovered = sum(new_recovered), contagion_day = max(contagion_day)) %>%
    ungroup() %>%
    arrange(Country.Region, desc(date))
}

#' Add growth/lethality rates
#'
#' @param df data.frame
#' @param group character Country.Region or continent or subcontinent
#' @param time character date
#'
#' @import dplyr
#' @import tidyr
#'
#' @return df dataframe
#'
#' @export
add_growth_death_rate <- function(df, group = "Country.Region", time = "date"){

  df %>% #ungroup() %>%
    arrange(!!as.symbol(group), desc(!!as.symbol(time))) %>%
    group_by(.dots = group) %>%
    # mutate(daily_growth_factor_3 = replace_na(confirmed / lag(confirmed, n = 3), 1),
    #        daily_growth_factor_5 = replace_na(confirmed / lag(confirmed, n = 5), 1),
    #        daily_growth_factor_7 = replace_na(confirmed / lag(confirmed, n = 7), 1),
    #        daily_lethality_rate = replace_na(deaths / confirmed, 0)
    #        ) %>%
    # mutate(daily_growth_factor_3 = pmax(1, replace_na(zoo::rollapplyr(new_confirmed, 63, sum, partial=TRUE, align = "right") / zoo::rollapplyr(lag(new_confirmed,3), 60, sum, partial=TRUE, align = "right"),1)),
    #        daily_growth_factor_7 = pmax(1, replace_na(zoo::rollapplyr(new_confirmed, 67, sum, partial=TRUE, align = "right") / zoo::rollapplyr(lag(new_confirmed,7), 60, sum, partial=TRUE, align = "right"),1)),
    #        daily_growth_factor_14 = pmax(1, replace_na(zoo::rollapplyr(new_confirmed, 74, sum, partial=TRUE, align = "right") / zoo::rollapplyr(lag(new_confirmed,14), 60, sum, partial=TRUE, align = "right"),1)),
    #        daily_lethality_rate = pmax(0, replace_na(deaths / confirmed, 0)),
    # ) %>%
    mutate(growth_factor_3 = round(pmax(1, sum(head(new_confirmed, 63)) / sum(head(lead(new_confirmed,3), 60), na.rm = TRUE),1), digits = 3),
           growth_factor_7 = round(pmax(1, sum(head(new_confirmed, 67)) / sum(head(lead(new_confirmed,7), 60), na.rm = TRUE),1), digits = 3),
           growth_factor_14 = round(pmax(1, sum(head(new_confirmed, 74)) / sum(head(lead(new_confirmed,14), 60), na.rm = TRUE),1), digits = 3),
           lm_confirmed_rate_1M_pop = round(10^6*sum(head(new_confirmed, 30))/population, digits = 3)
    ) %>%
    #mutate_if(is.numeric, function(x){ifelse(x == "Inf",NA, x)} ) %>% # can be done just  later
    ungroup() %>%
    filter(date == max(date)) %>%
    mutate(growth_factor_3 = if_else(is.infinite(growth_factor_3),1, growth_factor_3),
    growth_factor_14 = if_else(is.infinite(growth_factor_14),1, growth_factor_14),
    growth_factor_7 = if_else(is.infinite(growth_factor_7),1, growth_factor_7)) %>%
    mutate_if(is.numeric, function(x){dplyr::na_if(x, Inf)} )

}


#' Province.State data timeseries
#' @rdname get_timeseries_province_data
#'
#' @param data data.frame
#' @param province name of the province Character string
#'
#' @import dplyr
#'
#' @return province_df tibble of by country confirmed, deaths, active and recovered for each day
#'
#' @export
get_timeseries_province_data <- function(data, province){
  province_df <- data %>%
    filter(Province.State == province) %>%
    group_by(date) %>%
    summarize(confirmed = sum(confirmed), deaths = sum(deaths), recovered = sum(recovered), active = sum(active), new_confirmed = sum(new_confirmed), new_deaths = sum(new_deaths), new_active = sum(new_active), new_recovered = sum(new_recovered), contagion_day = max(contagion_day)) %>%
    arrange(desc(date)) %>%
    ungroup()
}

#' date data
#' @rdname get_date_data
#'
#' @param data data.frame
#' @param date date format yyyy-mm-dd
#'
#' @import dplyr
#'
#' @return global tibble of confirmed, deaths, active and recovered for each day
#'
#' @export
get_date_data <- function(data, date){
  date_df <- data %>%
    group_by(date) %>%
    summarize(confirmed = sum(confirmed), deaths = sum(deaths), recovered = sum(recovered), active = sum(active), new_confirmed = sum(new_confirmed), new_deaths = sum(new_deaths), new_active = sum(new_active), new_recovered = sum(new_recovered), contagion_day = max(contagion_day)) %>%
    ungroup()
}


#' retrieves population data, matches countries with continents and subcontinents
#' @rdname get_pop_data
#'
#' @import dplyr
#' @import tidyr
#'
#' @note additional demographic variables are available for further development
#'
#' @return global tibble of confirmed, deaths, active and recovered for each day by population
#'
#' @examples
#' \dontrun{
#' orig_data <- get_timeseries_full_data() %>%
#'               get_timeseries_by_contagion_day_data()
#' data <- orig_data %>% align_country_names()
#'
#'}
#'
#' @export
get_pop_data <- function(){
  #Reference: https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population
  # pop_url <- "https://raw.githubusercontent.com/DrFabach/Corona/master/pop.csv"
  # pop_raw <- readLines(pop_url)
  # pop <- data.frame(pop_raw[2:length(pop_raw)], stringsAsFactors = FALSE) %>%
  #   setNames(pop_raw[1])
  # pop <- pop %>%
  #   separate(col = "Country;Population", into = c("Country.Region", "population"), sep = ";")
  # write.csv2(pop, "./inst/population_data/pop.csv")
  # population <- read.csv2(system.file("population_data/pop.csv", package = "Covid19"),stringsAsFactors = F) %>%
  #   select(-X)
  population <- read.csv2(system.file("population_data/popUN.csv", package = "Covid19Mirai"),stringsAsFactors = F)

  population$Country.Region <- population$Country.Region %>%
    # recode(
    #   "Ivory Coast" = "C\\u00f4te d'Ivoire",
    #   "DR Congo" = "Republic of the Congo",
    #   "United Arab Emirates" = "UAE",
    #   "East Timor" = "Timor-Leste" ,
    #   "Saint Vincent and the Grenadines" = "St. Vincent Grenadines",
    #   "Puerto Rico(US)" = "Puerto Rico",
    #   "Comoros" = "Mayotte",
    #   "Guam(US)" = "Guam",
    #   "Greenland(Denmark)" = "Greenland",
    #   "Eswatini" = "eSwatini",
    #   "Isle of Man(UK)" = "Channel Islands",
    #   "Central African Republic" = "CAR",
    #   "Cape Verde" = "Cabo Verde",
    #   "Antigua and Barbuda" = "Antigua and Barb.",
    #   "United States" = "United States of America"
    # )
  recode(
    "Ivory Coast" = "Cote d'Ivoire",
    "DR Congo" = "Republic of the Congo",
    "United Arab Emirates" = "UAE",
    "East Timor" = "Timor-Leste" ,
    "Saint Vincent and the Grenadines" = "St. Vincent Grenadines",
    "Puerto Rico(US)" = "Puerto Rico",
    "Comoros" = "Mayotte",
    "Guam(US)" = "Guam",
    "Greenland(Denmark)" = "Greenland",
    #"Eswatini" = "eSwatini", # already matching
    #"Isle of Man(UK)" = "Channel Islands",
    "Isle of Man(UK)" = "Isle of Man",
    "Central African Republic" = "CAR",
    #"Cape Verde" = "Cabo Verde", merged in data
    #"Antigua and Barbuda" = "Antigua and Barb.",
    #"United States" = "United States of America",
    "United States" = "USA",

    ##################################################
    "Bosnia and Herz." = "Bosnia and Herzegovina", # taken from reverse alignment
    "Ivory Coast" = "Cote d'Ivoire", # taken from reverse
    "Czechia" = "Czech Republic", # taken from reverse
    "Dominican Rep." = "Dominican Republic", # taken from reverse
    "Macedonia" = "North Macedonia", # taken from reverse
    "United Kingdom" = "UK", # taken from reverse
    "Vatican" = "Vatican City", # taken from reverse
    "Faeroe Is." = "Faeroe Islands",
    "Gibraltar(UK)" = "Gibraltar",
    "Saint Martin(France)" = "St Martin",
    "Cayman Islands(UK)" = "Cayman Islands",
    "New Caledonia(France)" = "New Caledonia",
    "F.S. Micronesia" = "Micronesia"
  )
  # rename non ASCII characters
  population$Country.Region[grepl("^St-Barth", population$Country.Region)] = "St. Barth"
  population$Country.Region[grepl("and Pr", population$Country.Region)] = "Sao Tome and Principe"
  population$Country.Region[grepl("^Cura", population$Country.Region)] = "Curasao"

  population$Country.Region = gsub("*\\(.*?\\) *","", population$Country.Region) # remove all text between brackets

  # sum french colonies

  population$population[population$Country.Region == "France"] =
    population$population[population$Country.Region == "France"] +
    sum(population$population[population$Country.Region %in% French.Colonies])
  # remove french colonies
  population = population[!(population$Country.Region %in% French.Colonies), , drop = F]

  population$population = as.numeric(population$population )
  population$PopulationUN = as.numeric(population$PopulationUN )
  #population$diff = (population$population - population$PopulationUN)/ population$Population *100
  population$population = population$PopulationUN # hwere when we need to change population with new data
  # the old population seems wrong for few countries
  population = population[, c("Country.Region", "continent", "subcontinent","population")]

  population
}

#' retrieves population data, matches countries with continents and subcontinents in line with get_datahub
#' @rdname get_pop_datahub
#'
#' @import dplyr
#' @import tidyr
#'
#' @note additional demographic variables are available for further development
#'
#' @return global tibble of confirmed, deaths, active and recovered for each day by population
#'
#' @examples
#' \dontrun{
#' orig_data <- get_timeseries_full_data() %>%
#'               get_timeseries_by_contagion_day_data()
#' data <- orig_data %>% align_country_names()
#'
#'}
#'
#' @export
get_pop_datahub <- function(){
  #Reference: https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population
  # pop_url <- "https://raw.githubusercontent.com/DrFabach/Corona/master/pop.csv"
  population <- read.csv2(system.file("population_data/popUN.csv", package = "Covid19Mirai"),stringsAsFactors = F)

  population$Country.Region <- population$Country.Region %>%
  recode(
    "Ivory Coast" = "Cote d'Ivoire", # ok
    "DR Congo" = "Republic of the Congo", #ok
    #"United Arab Emirates" = "UAE", same name
    "East Timor" = "Timor-Leste" , #ok
    "Saint Vincent and the Grenadines" = "St. Vincent Grenadines", #ok
    "Puerto Rico(US)" = "Puerto Rico", # ok
    #"Comoros" = "Mayotte", same name
    "Guam(US)" = "Guam", #ok
    "Greenland(Denmark)" = "Greenland", # missing in datahub anyway
    "Eswatini" = "Swaziland", # called Swaziland in new DF
    "Isle of Man(UK)" = "Channel Islands", # called Channel Islands in new DF
    #"Isle of Man(UK)" = "Isle of Man",
    #"Central African Republic" = "CAR", same name
    "United States" = "USA",

    ##################################################
    "Bosnia and Herz." = "Bosnia and Herzegovina",
    "Czechia" = "Czech Republic", # taken from reverse
    "Dominican Rep." = "Dominican Republic", # taken from reverse
    "Macedonia" = "North Macedonia", # taken from reverse
    "United Kingdom" = "UK", # taken from reverse
    "Vatican" = "Vatican City", # taken from reverse
    "Faeroe Is." = "Faeroe Islands", # possibly it is with DK in new DF
    "Gibraltar(UK)" = "Gibraltar", # possibly it is with UK in new DF
    "Saint Martin(France)" = "St Martin", # possibly it is with France in new DF
    "Cayman Islands(UK)" = "Cayman Islands", # possibly it is with UK in new DF
    "New Caledonia(France)" = "New Caledonia", # possibly it is with France in new DF
    "F.S. Micronesia" = "Micronesia"
  )
  # rename non ASCII characters
  population$Country.Region[grepl("^St-Barth", population$Country.Region)] = "St. Barth" # ok
  population$Country.Region[grepl("and Pr", population$Country.Region)] = "Sao Tome and Principe" # ok
  population$Country.Region[grepl("^Cura", population$Country.Region)] = "Curasao" # missing in new DF

  population$Country.Region = gsub("*\\(.*?\\) *","", population$Country.Region) # remove all text between brackets

  # # sum french colonies not needed, we do not take population from here
  #
  # population$population[population$Country.Region == "France"] =
  #   population$population[population$Country.Region == "France"] +
  #   sum(population$population[population$Country.Region %in% French.Colonies])
  # # remove french colonies
  # population = population[!(population$Country.Region %in% French.Colonies), , drop = F]

  population$population = as.numeric(population$population )
  population$PopulationUN = as.numeric(population$PopulationUN )
  #population$diff = (population$population - population$PopulationUN)/ population$Population *100
  population$populationOLD = population$PopulationUN # hwere when we need to change population with new data
  # populationOLD to be used for checks vs new DF
  population = population[, c("Country.Region", "continent", "subcontinent")]

  population
}

#' merge data
#' @rdname merge_pop_data
#'
#' @param data data.frame
#' @param popdata data.frame result of get_pop_data
#'
#' @import dplyr
#' @import tidyr
#'
#' @return global tibble of confirmed, deaths, active and recovered for each day by population
#'
#' @examples
#' \dontrun{
#' orig_data <- get_timeseries_full_data() %>%
#'               get_timeseries_by_contagion_day_data()
#' data <- orig_data %>% align_country_names()
#'
#' popdata <- get_pop_data()
#'
#'}
#' @export
merge_pop_data <- function(data, popdata) {
  data_pop <- data %>%
    #mutate(Country.Region = country_name) %>%
    left_join(popdata, by = "Country.Region") #%>%
    #mutate(population = if_else(population < 1000, NA_integer_, as.integer(population))) # set to NA very small countries
    #filter(!is.na(population) & population > 1000) # removing super small countries
    #filter(!is.na(population)) # not the right place

  #select(-country_name)

  data_pop
}

#' Select only countries which had at least n cases and outbreaks longer than w days
#'
#' @param df data.frame
#' @param n number of cases
#' @param w days of outbreak
#' @param group character Country.Region or continent or subcontinent
#' @importFrom rlang sym
select_countries_n_cases_w_days <- function(df, n, w, group = "Country.Region") {
  countries_filtered <- df %>%
    filter(confirmed > n) %>% #pick only those countries that have more than n cases
    group_by(.dots = group) %>%
    mutate(N = n()) %>%
    filter( N > w) %>% #pick only those countries that have had outbreak for more than w days
    ungroup() %>%
    select(!!group) %>%
    distinct()

  df_filtered <- df %>%
    filter(!!rlang::sym(group) %in% countries_filtered[[group]])

  df_filtered
}


#' Rescale contagion day for countries with at least n cases and outbreaks longer than w days
#'
#' @param df data.frame
#' @param n number of cases
#' @param w days of outbreak
#' @param group character Country.Region or continent or subcontinent
rescale_df_contagion <- function(df, n, w, group = "Country.Region"){
  df_rescaled <- df %>%
  select_countries_n_cases_w_days(n = n, w = w, group) %>%
    mutate(no_contagion = case_when( #drop rows where confirmed <- n
      confirmed < n ~ 1,
      TRUE ~ 0
    )) %>%
    filter(no_contagion == 0) %>%
    select(-no_contagion) %>%
    group_by(.dots = group) %>%
    mutate(contagion_day = contagion_day - min(contagion_day)) %>%
    ungroup()

  df_rescaled
}
