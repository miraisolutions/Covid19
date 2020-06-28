# Note that the documentation link is not going to work because the pipe
# operator ultimately resides in `magrittr` and is only re-exported in `dplyr`.
#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`


#' Capitalize first letter of all words in a string
#'
#' @rdname capitalize_first_letter
#' @description Returns a string with all words of input string have the first letter capitalized.
#' @param x string
#' @return string
#' @export
capitalize_first_letter <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep = "", collapse = " ")
}

#' Capitalize names of a dataframe
#' @rdname capitalize_names_df
#' @description Returns a dataframe with names with the first letterof all words capitalized.
#' @param df dataframe
#' @return df
#' @export
capitalize_names_df <- function(df) {
  names(df) <- sapply(
    sapply(sapply(names(df), gsub, pattern = "\\.", replacement = " "), gsub, pattern = "_", replacement = " "),
    capitalize_first_letter)
  df
}


#' Basic plot Theme
#' @rdname basic_plot_theme
#' @import ggplot2
#' @export
basic_plot_theme <- function() {
  theme(
    plot.title = element_text(color = "grey45", size = 18, face = "bold.italic", hjust = 0.5),
    text = element_text(size = 12),
    panel.background = element_blank(),
    panel.grid.major = element_line(colour = "white", size = 0.1),
    line = element_line(size = 2.2),
    axis.line.x = element_line(color = "grey45", size = 0.5),
    axis.line.y = element_line(color = "grey45", size = 0.5),
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title =  element_blank(),
    legend.text = element_text(size = 10),
    legend.key = element_rect(fill = alpha("white", 0.0))
  )
}

#' Get table options.
#'@rdname getTableOptions
#' @description Returns option list for datatable.
#' @param scrollX Param to allow scrollX. Default TRUE
#' @param maxrowsperpage Maximum number of rows to display per page. Default 5.
#'
#' @export
getTableOptions <- function(scrollX = TRUE,
                            maxrowsperpage = 5) {
  options <- list(
    search = list(caseInsensitive = TRUE),
    searchHighlight = TRUE,
    processing = 0,
    scrollX = scrollX,
    pageLength = maxrowsperpage
  )
}

#' Color Palette
#'
#' @export
case_colors <- c(
  "confirmed" = "#dd4b39",
  "deaths" = "black",
  "recovered" = "#00a65a",
  "active" = "#3c8dbc"
)

#' Color Palette
#'
#' @export
new_case_colors <- c(
  "new_confirmed" = "#dd4b39",
  "new_deaths" = "black",
  "new_recovered" = "#00a65a",
  "new_active" = "#3c8dbc"
)

#' Color Palette
#'
#' @export
rate_colors <- c(
  #"growth_factor" = "#dd4b39",
  "growth_factor" = "chocolate3",
  "death_rate" = "grey30"
)

#' Color Palette
#'
#' @export
new_total_colors <- c(
  "total" = "#C8C8C8",
  "new" = "#ea8b5b"
)

#' load countries  data
#' @param destpath path to file
#'
#' @returns countries shapefile
#' @export
load_countries_data_map <- function(destpath = system.file("./countries_data", package = "Covid19Mirai")){
  # Resource https://www.naturalearthdata.com/downloads/50m-cultural-vectors/50m-admin-0-countries-2/
  url <- "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip"
  zip_path <- file.path(destpath,"ne_50m_admin_0_countries.zip")
  dsn_path <- file.path(destpath, "ne_50m_admin_0_countries")

  if (!file.exists(zip_path)) {
    download.file(url = url, destfile = zip_path)
    unzip(zip_path, exdir = dsn_path)
  }

  countries <- rgdal::readOGR(dsn = dsn_path,
                              layer = "ne_50m_admin_0_countries",
                              encoding = "utf-8", use_iconv = T,
                              verbose = FALSE)
  assign_new_level =  function(countrymap , lev, from, to, regexpress = FALSE) {
    if (regexpress) {
      if (!any(grepl(from, as.character(countrymap[[lev]]))))
        stop("wrong expression, not present in map: ", from)
      from = grep(from, as.character(countrymap[[lev]]), value = TRUE)
    }
    if (!any(countrymap[[lev]] == from))
      stop("wrong name, not present in map: ", from)
    levels(countrymap[[lev]])[levels(countrymap[[lev]])==from] <- to
    countrymap[[lev]][countrymap[[lev]] == from] = to
    countrymap
  }
  #rename continents
  countries = assign_new_level(countries, "CONTINENT", "North America", "Northern America")
  countries = assign_new_level(countries, "CONTINENT", "South America", "LatAm & Carib.")
  #rename NAME, i.e. countries
  #countries = assign_new_level(countries, "NAME", "Macao", "Macau")
  #countries = assign_new_level(countries, "NAME", "Macao", "Macau")
  countries = assign_new_level(countries, "NAME", "Macedonia", "North Macedonia")
  countries = assign_new_level(countries, "NAME", "Czechia", "Czech Republic")
  countries = assign_new_level(countries, "NAME", "Dominican Rep.", "Dominican Republic")
  countries = assign_new_level(countries, "NAME", "United Kingdom", "UK")
  countries = assign_new_level(countries, "NAME", "United States of America", "USA")
  countries = assign_new_level(countries, "NAME", "United Arab Emirates", "UAE")
  countries = assign_new_level(countries, "NAME", "^St-Barth", "St. Barth", regexpress = TRUE)
  countries = assign_new_level(countries, "NAME", "Faeroe Is.", "Faeroe Islands")
  countries = assign_new_level(countries, "NAME", "Bosnia and Herz.", "Bosnia and Herzegovina")
  countries = assign_new_level(countries, "NAME", "Vatican", "Vatican City")
  countries = assign_new_level(countries, "NAME", "St. Vin. and Gren.", "St. Vincent Grenadines")
  countries = assign_new_level(countries, "NAME", "Dem. Rep. Congo", "Republic of the Congo")
  #countries = assign_new_level(countries, "NAME", "Central African Rep.", "CAR")
  countries = assign_new_level(countries, "NAME", "Ivoire", "Cote d'Ivoire", regexpress = TRUE)
  countries = assign_new_level(countries, "NAME", "St-Martin", "St Martin")
  countries = assign_new_level(countries, "NAME", "Cayman Is.", "Cayman Islands")
  countries = assign_new_level(countries, "NAME", "Eq. Guinea", "Equatorial Guinea")
  countries = assign_new_level(countries, "NAME", "Central African Rep.", "CAR")
  countries = assign_new_level(countries, "NAME", "eSwatini", "Eswatini")
  countries = assign_new_level(countries, "NAME", "Cabo Verde", "Cape Verde")
  countries = assign_new_level(countries, "NAME", "S. Sudan", "South Sudan")
  countries = assign_new_level(countries, "NAME", "Fr. Polynesia", "French Polynesia")
  countries = assign_new_level(countries, "NAME", "Antigua and Barb.", "Antigua and Barbuda")
  countries = assign_new_level(countries, "NAME", "Cook Is.", "Cook Islands")
  countries = assign_new_level(countries, "NAME", "Falkland Is.", "Falkland Islands")
  countries = assign_new_level(countries, "NAME", "U.S. Virgin Is.", "U.S. Virgin Islands")

  countries
}

#' load countries data to match with get_datahub
#' @param destpath path to file
#'
#' @returns countries shapefile
#' @export
load_countries_datahub_map <- function(destpath = system.file("./countries_data", package = "Covid19Mirai")){
  # Resource https://www.naturalearthdata.com/downloads/50m-cultural-vectors/50m-admin-0-countries-2/
  url <- "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip"
  zip_path <- file.path(destpath,"ne_50m_admin_0_countries.zip")
  dsn_path <- file.path(destpath, "ne_50m_admin_0_countries")

  if (!file.exists(zip_path)) {
    download.file(url = url, destfile = zip_path)
    unzip(zip_path, exdir = dsn_path)
  }

  countries <- rgdal::readOGR(dsn = dsn_path,
                              layer = "ne_50m_admin_0_countries",
                              encoding = "utf-8", use_iconv = T,
                              verbose = FALSE)
  assign_new_level =  function(countrymap , lev, from, to, regexpress = FALSE) {
    if (regexpress) {
      if (!any(grepl(from, as.character(countrymap[[lev]]))))
        stop("wrong expression, not present in map: ", from)
      from = grep(from, as.character(countrymap[[lev]]), value = TRUE)
    }
    if (!any(countrymap[[lev]] == from))
      stop("wrong name, not present in map: ", from)
    levels(countrymap[[lev]])[levels(countrymap[[lev]])==from] <- to
    countrymap[[lev]][countrymap[[lev]] == from] = to
    countrymap
  }
  #rename continents
  countries = assign_new_level(countries, "CONTINENT", "North America", "Northern America")
  countries = assign_new_level(countries, "CONTINENT", "South America", "LatAm & Carib.")
  #rename NAME, i.e. countries
  #countries = assign_new_level(countries, "NAME", "Macao", "Macau")
  #countries = assign_new_level(countries, "NAME", "Macao", "Macau")
  countries = assign_new_level(countries, "NAME", "Macedonia", "North Macedonia")
  countries = assign_new_level(countries, "NAME", "Czechia", "Czech Republic")
  countries = assign_new_level(countries, "NAME", "Dominican Rep.", "Dominican Republic")
  countries = assign_new_level(countries, "NAME", "United Kingdom", "UK")
  countries = assign_new_level(countries, "NAME", "United States of America", "USA")
  #countries = assign_new_level(countries, "NAME", "United Arab Emirates", "UAE") # now has new name
  countries = assign_new_level(countries, "NAME", "^St-Barth", "St. Barth", regexpress = TRUE) # not needed but ok
  countries = assign_new_level(countries, "NAME", "Faeroe Is.", "Faeroe Islands")
  countries = assign_new_level(countries, "NAME", "Bosnia and Herz.", "Bosnia and Herzegovina")
  countries = assign_new_level(countries, "NAME", "Vatican", "Vatican City")
  countries = assign_new_level(countries, "NAME", "St. Vin. and Gren.", "St. Vincent Grenadines")
  countries = assign_new_level(countries, "NAME", "Dem. Rep. Congo", "Republic of the Congo")
  #countries = assign_new_level(countries, "NAME", "Central African Rep.", "CAR")
  countries = assign_new_level(countries, "NAME", "Ivoire", "Cote d'Ivoire", regexpress = TRUE)
  countries = assign_new_level(countries, "NAME", "St-Martin", "St Martin")
  countries = assign_new_level(countries, "NAME", "Cayman Is.", "Cayman Islands")
  countries = assign_new_level(countries, "NAME", "Eq. Guinea", "Equatorial Guinea")
  countries = assign_new_level(countries, "NAME", "Central African Rep.", "Central African Republic")
  countries = assign_new_level(countries, "NAME", "eSwatini", "Swaziland")
  countries = assign_new_level(countries, "NAME", "Cabo Verde", "Cape Verde")
  countries = assign_new_level(countries, "NAME", "S. Sudan", "South Sudan")
  countries = assign_new_level(countries, "NAME", "Fr. Polynesia", "French Polynesia")
  countries = assign_new_level(countries, "NAME", "Antigua and Barb.", "Antigua and Barbuda")
  countries = assign_new_level(countries, "NAME", "Cook Is.", "Cook Islands")
  countries = assign_new_level(countries, "NAME", "Falkland Is.", "Falkland Islands")
  countries = assign_new_level(countries, "NAME", "U.S. Virgin Is.", "U.S. Virgin Islands") # new name
  countries = assign_new_level(countries, "NAME", "St. Kitts and Nevis", "Saint Kitts and Nevis" ) # new name
  countries = assign_new_level(countries, "NAME", "Principe", "Sao Tome and Principe", regexpress = TRUE )
  countries = assign_new_level(countries, "NAME", "N. Mariana Is.", "Northern Mariana Islands")
  countries = assign_new_level(countries, "NAME", "Marshall Is.", "Marshall Iselands")
  countries = assign_new_level(countries, "NAME", "St. Pierre and Miquelon", "St. Pierre and Miquelon")

  countries
}

#' Sort type by max
#'
#' @param data data to be sorted
sort_type_by_max <- function(data) {
  c("active", "recovered", "deaths") %>% .[order(sapply(data[.], max))]
}

#' Sort type harcoded
#'
sort_type_hardcoded <- function() {
  c("recovered", "deaths", "active")
}

#' Round up to the next decine
#' Ref: https://stackoverflow.com/questions/6461209/how-to-round-up-to-the-nearest-10-or-100-or-x
#' @param x number to round
#' @returns rounded up number
#' @export
roundUp <- function(x) 10^ceiling(log10(x))

#' align countries names between population dataframe and orig_data dataframe
#' @param data data.frame
#' @export
align_country_names_pop <- function(data){
  data$Country.Region <- data$Country.Region %>%
    recode(
      #Note Congo (Brazzaville), Diamond Proncess, Guadeloupe, Martinique, Reunion,  present in orig_data not in population; and Tristan da Cunha(UK)  present in population, not in orig_data

      "Antigua and Barbuda" = "Antigua and Barb.",
      "Bosnia and Herzegovina" = "Bosnia and Herz.",
      #"Cape Verde" = "Cabo Verde",
      "Cote d'Ivoire" = "C\\u00f4te d'Ivoire",
      "Czech Republic" = "Czechia",
      "Dominican Republic" = "Dominican Rep.",
      "Eswatini" = "eSwatini",
      #"French Guiana" = "Guyana",
      "North Macedonia" = "Macedonia",
      "UK" = "United Kingdom",
      "USA" = "United States of America",
      "Vatican City" = "Vatican"
    )
  data
}

#' align countries names between population dataframe and orig_data dataframe
#' @param data data.frame
#' @export
align_country_names_pop_reverse <- function(data){
  data$Country.Region <- data$Country.Region %>%
    recode(
      #Note Congo (Brazzaville), Diamond Proncess, Guadeloupe, Martinique, Reunion,  present in orig_data not in population; and Tristan da Cunha(UK)  present in population, not in orig_data

      "Antigua and Barb." = "Antigua and Barbuda",
      "Bosnia and Herz." = "Bosnia and Herzegovina",
      #"Cabo Verde" = "Cape Verde",
      "C\\u00f4te d'Ivoire" = "Cote d'Ivoire",
      "Czechia" = "Czech Republic",
      "Dominican Rep." = "Dominican Republic",
      "eSwatini" = "Eswatini",
      #"Guyana" = "French Guiana",
      "Macedonia" = "North Macedonia",
      "United Kingdom" = "UK",
      "United States of America" = "USA",
      "Vatican" = "Vatican City"
    )
  data
}
#' clean ggplotly legend
#' @param .plotly_x ggplotly object
#' @param .extract_str regular expression
#' @import stringr
clean_plotly_leg <- function(.plotly_x, .extract_str) {
  # Inpects an x$data list in a plotly object, cleans up legend values where appropriate
  if ("legendgroup" %in% names(.plotly_x)) {
    # The list includes a legend group
    .plotly_x$legendgroup <- stringr::str_extract(.plotly_x$legendgroup, .extract_str)
    .plotly_x$name <- stringr::str_extract(.plotly_x$name, .extract_str)
  }
  .plotly_x
}
#' Aggregates data to continent or subcontinent (group)
#' @param data data.frame aggregated data per Country.Region
#' @param group character continent or subcontinent
#' @param time character date or contagion_day
#' @param allstatuses character vector of statuses to base the recomputation from: confirmed recovered deaths active
#'
#' @note growth and mortality variables must be recomputed after the aggregation
#' the return dataset renames the group column into Country.region to allow the following graphs to work
#'
#' @return data.frame aggregated at group level
#'
#' @importFrom rlang sym
#' @export
aggr_to_cont = function(data, group, time,
                        allstatuses = get_aggrvars()) {

  # popdata_cont = popdata %>% filter(!is.na(!!rlang::sym(group))) %>%
  #   group_by(.dots = group) %>%
  #   summarize(population = sum(population, na.rm = T))
  continent_data =    data %>% filter(!is.na(!!rlang::sym(group)))  %>%
    select(Country.Region, population, contagion_day, date, !!group, date, !!allstatuses) %>%
    mutate(population = as.numeric(population)) %>%
    group_by(.dots = c(time,group)) %>%
    summarise_at(c(allstatuses), sum, na.rm = TRUE) %>%
    add_growth_death_rate(group, time) %>%
    #left_join(popdata_cont[,c(group, "population")], by = group) %>% #TODO: why left_join not earlier?
    mutate(mortality_rate_1M_pop = round(10^6*deaths/population, digits = 3),
           prevalence_rate_1M_pop = round(10^6*confirmed/population, digits = 3),
           new_prevalence_rate_1M_pop = round(10^6*new_confirmed/population, digits = 3)) %>%
    rename(Country.Region = !!group) %>%
    get_timeseries_by_contagion_day_data()  %>%
    arrange(desc(date)) %>%   ungroup() %>%
    filter(!is.na(Country.Region)) # filter NAs off again for safety

  continent_data
}

#' creates time series for the area plot
#' @param data data.frame aggregated data per region
#' @param levs order of statuses
#' @param n minimum number of cases for the start date
#'
#' @note starting date based on n, first day with so many confirmed
#'
#' @return data.frame reshaped
#'
#' @import tidyr
tsdata_areplot <- function(data, levs, n = 1000) {
  data %>%
    #filter(date > date[min(which(confirmed>0))]) %>% #remove initial dates
    filter(confirmed > n) %>% #remove initial dates
    select(-starts_with("new_"), -confirmed) %>%
    pivot_longer(cols = -date, names_to = "status", values_to = "value") %>%
    mutate(status = factor(status, levels = levs)) %>%
    capitalize_names_df()
}
#' creates message of countries within subcountries
#' @param data data.frame aggregated data per region
#' @param area character main area
#' @param region character sub area
#'
#' @return list messages printing Country.Region within subcontinent
#'
message_subcountries <- function(data, area, region) {
  # remove where subcontinent could be NA
  list.countries = data[,c(area,region)] %>% filter(!is.na(!!rlang::sym(area))) %>% unique() %>%
    group_by(.dots = area) %>% group_split()
  lapply(list.countries, function(x)
    paste0("<b>",as.character(unique(x[[area]])),"</b>: ",
           paste(x[[region]], collapse = ",")))
}
#' Calculates growth vs prevalence factors
#' @param data data.frame aggregated data per region
#' @param growthvar growth factors
#' @param prevvar prevalence over 1 M
#'
#' @note factors created:
#' c('Low Growth and Prevalence', 'Low Growth - High Prevalence', 'High Growth - Low Prevalence', 'High Growth and Prevalence')
#'
#' @return factor vector
#'
growth_v_prev_calc <- function(data, growthvar,prevvar) {
  # compute stats for all growth factors
  med_growth = median(data[[growthvar]])
  med_prevalence = median(data[[prevvar]])

  labs = c("Low Growth and Prevalence",
           "Low Growth - High Prevalence",
           "High Growth - Low Prevalence",
           "High Growth and Prevalence" )

  val_cntry = rep(labs[2], nrow(data))
  val_cntry[data[[prevvar]] <= med_prevalence & data[[growthvar]]  <= med_growth ] = labs[1]
  val_cntry[data[[prevvar]] > med_prevalence & data[[growthvar]] > med_growth ] = labs[4]
  val_cntry[data[[prevvar]] <= med_prevalence & data[[growthvar]] > med_growth ] = labs[3]
  factor(val_cntry, levels = labs)
}

#' Rounds up numbers for labels in plots
#' @param maxv numeric max value
#'
#' @return numeric vector after ceiling()
#'
round_up = function(maxv) {
  dg = nchar(as.character(round(maxv)))
  if (dg == 1 && maxv>1)
    dg = 0
  ceiling(maxv/(10^(dg-1)))*10^(dg-1)
}
#' Derives number of digits for rounding
#' @param dg integer number of characters of figure, say 1000 = 4
#' @param maxv numeric max value
#' @param minxv numeric min value
#'
#' @return integer number of digit
#'
getdg_lab = function(dg,maxv,minxv) {
  if (dg >3)
    dglab = 0
  else if (dg == 1 && maxv <=1 && minxv>=0)
    dglab = 1 # rates are in 100
  else if (dg >0)
    dglab = dg - (c(-2,0,2))[dg]
  else
    stop("error wrong digits for rounding", dg)
  dglab
}
#' Derives text for plotly popups
#' @param x data
#' @param namvar character vector variable name
#'
#' @return text
#'
gen_text = function(x, namvar) {
  if (is.numeric(x)) {
    maxy = max(x, na.rm = TRUE)
    minxy = min(x, na.rm = TRUE)
    dg = nchar(as.character(round(max(abs(minxy),maxy))))
    #if(dg==1 && maxy<=1 && minxy>=0) {
    if(namvar %in% rate_vars) {
      text.pop = paste0(roundlab(x*100),"%")
    } else {
      text.pop = formatC(x, format = "f", big.mark = "'", digits  = getdg_lab(dg, maxy, minxy))
    }
    #x = replace("NA")
  } else
    text.pop = x
  text.pop
}

rate_vars <- c(
  "lethality_rate"
  )

#' Builds dataset to be used in modules merging pop_data with data
#' @param data data
#' @param popdata population data with continent info
#'
#' @return data
#'
build_data_aggr <- function(data, popdata) {
  orig_data_aggregate <- data %>%
    #aggregate_province_timeseries_data() %>% # not required anymore
    add_growth_death_rate() %>%
    arrange(Country.Region) %>%
    merge_pop_data(popdata) %>% # compute additional variables
    mutate(mortality_rate_1M_pop = round(10^6*deaths/population, digits = 3),
           prevalence_rate_1M_pop = round(10^6*confirmed/population, digits = 3),
           new_prevalence_rate_1M_pop = round(10^6*new_confirmed/population, digits = 3))
  orig_data_aggregate
}

#'Global definition of numeric aggregatable vars in dataset
#'
#' @return character vector
#'
get_aggrvars = function() {

  statuses <- c("confirmed", "deaths", "recovered", "active")
  # select all variables
  allstatuses = c(statuses, paste0("new_", statuses), "tests", "hosp", "population")
  allstatuses
}
