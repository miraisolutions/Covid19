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

  data1 <- data %>%
    arrange(desc(Country.Region), date) %>%
    mutate(no_contagion = case_when(
      confirmed == 0 ~ 1,
      TRUE ~ 0
    )) %>%
    #group_by(Province.State, Country.Region, Lat, Long) %>%
    group_by(Country.Region) %>%
    mutate(incremental = seq(1:n())) %>%
    mutate(offset = sum(no_contagion)) %>%
    mutate(tmp = incremental - offset) %>%
    mutate(contagion_day = case_when(
      tmp < 0 ~ 0,
      TRUE ~ tmp
    )) %>%
    select(-incremental, -offset, -no_contagion, -tmp) %>%
    mutate(new_confirmed = confirmed - lag(confirmed)) %>%
    mutate(new_deaths = deaths - lag(deaths)) %>%
    mutate(new_active = active - lag(active)) %>%
    mutate(new_recovered = recovered - lag(recovered)) %>%
    mutate(new_confirmed = if_else(is.na(new_confirmed), 0, new_confirmed)) %>%
    mutate(new_deaths = if_else(is.na(new_deaths), 0, new_deaths)) %>%
    mutate(new_active = if_else(is.na(new_active), 0, new_active)) %>%
    mutate(new_recovered = if_else(is.na(new_recovered), 0, new_recovered)) %>%
    ungroup()
  data1
}

#' Global data timeseries
#' @rdname get_timeseries_global_data
#'
#' @param data data.frame
#'
#' @import dplyr
#'
#' @return global tibble of global confirmed, deaths, active and recovered, for each day
#'
#' @export
get_timeseries_global_data <- function(data){
  global <- data %>%
    group_by(date) %>%
    summarize_at(c("confirmed", "deaths", "recovered", "active", "new_confirmed", "new_deaths", "new_active", "new_recovered"), sum) %>%
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

  df1 <- df %>% ungroup() %>%
    arrange(!!as.symbol(group), !!as.symbol(time)) %>%
    group_by(.dots = group) %>%
    mutate(daily_growth_factor_3 = replace_na(confirmed / lag(confirmed, n = 3), 1),
           daily_growth_factor_5 = replace_na(confirmed / lag(confirmed, n = 5), 1),
           daily_growth_factor_7 = replace_na(confirmed / lag(confirmed, n = 7), 1),
           daily_lethality_rate = replace_na(deaths / confirmed, 0)) %>%
    mutate_if(is.numeric, function(x){ifelse(x == "Inf",NA, x)} ) %>%
    ungroup()
  df2 <- df1 %>%
    group_by(.dots = group)  %>%
    # mutate(growth_factor = round(zoo::rollmeanr(daily_growth_factor, 7, align = "right", fill = 0), digits = 3)) %>%
    # mutate(death_rate = round(zoo::rollmeanr(daily_death_rate, 7, align = "right", fill = 0), digits = 3))  %>%
    mutate(growth_factor_3 = round(daily_growth_factor_3, digits = 3),
           growth_factor_5 = round(daily_growth_factor_5, digits = 3),
           growth_factor_7 = round(daily_growth_factor_5, digits = 3),
           lethality_rate = round(daily_lethality_rate, digits = 3)) %>%
    ungroup() %>%
    mutate_if(is.numeric, function(x){replace_na(x,0)} ) %>%
    mutate_if(is.numeric, function(x){ifelse(x == "Inf",NA, x)} ) %>%
    arrange(!!as.symbol(group), desc(!!as.symbol(time))) %>%
    select(-starts_with("daily_"))
  df2
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
  population <- read.csv2(system.file("population_data/popUN.csv", package = "Covid19"),stringsAsFactors = F)

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


#' Reescale contagion day for countries with at least n cases and outbreaks longer than w days
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
