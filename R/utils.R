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

#' Color Palette vaccination vars
#'
#' @export
.vacc_colors <- c(
  "vaccines" = "#17a2ff",
  "people_vaccinated" = "lightblue",#"darkblue",
  "people_fully_vaccinated" = "darkblue", #"darkslategray"
  "vaccinated_rate" = "lightblue",#"darkblue",
  "fully_vaccinated_rate" = "darkblue" #"darkslategray"
)

#' Color Palette Hospitalizations vars
#'
#' @export
.hosp_colors <- c(
  "hosp" = "#08306B",
  "vent" = "midnightblue",#"darkblue",
  "icu" = "dimgrey", #"darkslategray"
  "icuvent" = "dimgrey"#"darkslategray"
)

#' Color Palette for Areaplot variables
#'
#' @export
.case_colors <- c(
  "confirmed" = "#dd4b39",
  "deaths" = "black",
  "recovered" = "#00a65a",
  "active" = "#3c8dbc",
  "hosp" = as.character(.hosp_colors["hosp"]),
  "vaccines" = as.character(.vacc_colors["vaccines"]) #"#24bbff"#,##24bbff "lightskyblue"#"cadetblue2"
)

#' Color Palette Tests vars
#'
#' @export
.tests_colors <- c(
  "tests" = "cadetblue",
  "positive_tests_rate" = "brown"#"darkblue",
)
#' Variables related to hospitalization
#' @export
.hosp_vars <- c(
  #c("hosp","vent","icu")
  c("Hospitalized" = "hosp", "Ventilated/Int.Care" = "icuvent")
)
#' Variables related to hospitalization in datahub
#' @export
.vacc_vars_datahub <- c(
  #c("hosp","vent","icu")
  c("Vaccine doses" = "vaccines", "Vaccinated" = "people_vaccinated",
    "Fully Vaccinated" = "people_fully_vaccinated")
)
#' Variables related to hospitalization in datahub
#' @export
.vacc_vars <- c(
  #c("hosp","vent","icu")
  c(.vacc_vars_datahub,
    c("% Vaccinated" = "vaccinated_rate",
    "% Fully Vaccinated" = "fully_vaccinated_rate") )
)



#' Variables related to hospitalization in covid19datahub
#' @export
.hosp_vars_datahub <- c(
  c("hosp","vent","icu")
)
#' Variables defining current state
#' @export
.current_vars <- c(
  unique(c(.hosp_vars_datahub, .hosp_vars, "active",
    paste0("new_", c(.hosp_vars_datahub, .hosp_vars, "active"))))
)

#' Add prefix new and last week variables
#'
#' @param cc vector \code{.case_colors}
#' @param prefix character, new or lw or both
#'
#' @export
prefix_var <- function(cc = .case_colors,  prefix = c("lw","pw","new")) {
  x = as.character(t(outer(prefix, cc, FUN = paste, sep = "_")))
  if (any(prefix == ""))
    x = gsub("^_","",x)
  x
}

#' Variables where negative values are allowed in map plot
#' @export
.neg_vars <- c(
  c("new_active","lw_active", .hosp_vars, prefix_var(.hosp_vars))
)

#' Variables used in nthcase_plot
.vars_nthcases_plot <-
  c(#"confirmed", "deaths", "recovered", "active", "hosp",
    names(.case_colors), setdiff(.hosp_vars, names(.case_colors)),
    prefix_var(names(.case_colors), "new"),
    prefix_var(setdiff(.hosp_vars, names(.case_colors)), "new"),
    setdiff(.vacc_vars, names(.case_colors)),
    #"new_confirmed", "new_deaths", "new_active",
    #"new_prevalence_rate_1M_pop",
    "tests","new_tests", #"new_tests_rate_1M_pop",
    "positive_tests_rate", "new_positive_tests_rate",
    #"vaccines","new_vaccines",
    #"lethality_rate", not interesting anymore
    "stringency_index"
    )

#' List of variable names to be used for map
#' @param vars variable name to be selected, if empty tehn all are returned
#' @details The name of the list component correspond to the variable label
#' @return list All variables, if vars is missing, or one variable.
varsNames <- function(vars) {

  newhosp = setdiff(.hosp_vars,names(.case_colors)) # review
  hospvars_1M_pop = paste(.hosp_vars,"rate_1M_pop",  sep = "_" )
  names(hospvars_1M_pop) = paste(names(.hosp_vars), "Rate 1M pop")

  newvacc = .vacc_vars[!.vacc_vars %in% names(.case_colors)]

  cases_1M_pop = paste(setdiff(names(.case_colors), c(.hosp_vars)),"rate_1M_pop",  sep = "_" ) # review

  allvars = c(names(.case_colors),
              prefix_var(names(.case_colors)),
              newhosp,
              prefix_var(newhosp),
              hospvars_1M_pop,
              prefix_var(hospvars_1M_pop),
              newvacc, # to add also prefix_var(newvacc) ?
              #prefix_var(grep("rate", newvacc, value = TRUE, invert = TRUE)),
              prefix_var(newvacc),
              cases_1M_pop,
              prefix_var(cases_1M_pop),
              "lm_confirmed_rate_1M_pop",
              paste("growth_factor", c(3,7,14), sep = "_"),
              "lethality_rate",
              prefix_var("lethality_rate"),
              #"deaths_rate_1M_pop",
              #prefix_var("mortality_rate_1M_pop"),
              "icuvent_rate_hosp",
              "hosp_rate_active",
              "stringency_index",
              # "vaccines",
              # prefix_var("vaccines"),
              # paste("vaccines", "rate_1M_pop", sep = "_"),
              # paste(prefix_var("vaccines"), "rate_1M_pop", sep = "_"),
              "vaccines_rate_pop",
              prefix_var("vaccines_rate_pop"),
              "tests",
              prefix_var("tests"),
              paste("tests", "rate_1M_pop", sep = "_"),
              paste(prefix_var("tests"), "rate_1M_pop", sep = "_"),
              "positive_tests_rate",
              prefix_var("positive_tests_rate"),
              "population", paste("growth_vs_prev", c(3,7,14), sep = "_"),
              paste("growth_vs_stringency", c(3,7,14), sep = "_"),
              "date", "AsOfDate")

  allvars = allvars %>%
    setNames(gsub("_", " ", allvars))
  #prevalence
  names(allvars)[grepl("confirmed_rate_1M_pop", allvars)] = gsub("confirmed","prevalence",names(allvars)[grepl("confirmed_rate_1M_pop", allvars)])
  names(allvars)[grepl("deaths_rate", allvars)] = gsub("deaths","mortality",names(allvars)[grepl("deaths_rate", allvars)])

  names(allvars)[grepl("vaccines", allvars)] = gsub("vaccines","vaccine doses",names(allvars)[grepl("vaccines", allvars)])

  for(hospvar in .hosp_vars) {
    names(allvars)[grepl(paste0(hospvar), unlist(allvars))] = gsub(hospvar, names(.hosp_vars)[.hosp_vars == hospvar], names(allvars)[grepl(paste0(hospvar), unlist(allvars))] )
  }
  names(allvars)[ allvars %in% hospvars_1M_pop] = names(hospvars_1M_pop)

  names(allvars)  = sapply(gsub("1M pop", "1M people", names(allvars)), capitalize_first_letter)
  names(allvars)  = gsub("^Lw", "Last Week", names(allvars))
  names(allvars)  = gsub("^Pw", "Past Week", names(allvars))

  names(allvars)  = gsub("^Lm", "Last Month", names(allvars))
  names(allvars)  = gsub("Rate ", "Over ", names(allvars))
  names(allvars)  = gsub("Over Pop$", "Over Population size", names(allvars))

  allvars = as.list(allvars)

  if (!missing(vars)){
    varnames = unlist(allvars)
    if (!all(vars %in% varnames)) {
      stop(paste(setdiff(vars, varnames), "invalid variable"))
    }
    res = allvars[match(vars, varnames)]
  }
  else
    res = allvars
  res
}
#' Variables defined as rate in map plot
.rate_vars <- c(
  grep("lethality", unlist(varsNames()), value = TRUE),
  grep("positive_tests_rate", unlist(varsNames()), value = TRUE),
  grep("hosp_rate_active", unlist(varsNames()), value = TRUE),
  grep("vaccines_rate_pop", unlist(varsNames()), value = TRUE),
  grep("vaccinated_rate$", unlist(varsNames()), value = TRUE),
  grep("icuvent_rate_hosp", unlist(varsNames()), value = TRUE)
)
#' Variables defined as 0-100 index in map plot
.index_vars <- c(
  grep("index", unlist(varsNames()), value = TRUE)
)
# Note that the documentation link is not going to work because the pipe
# operator ultimately resides in `magrittr` and is only re-exported in `dplyr`.
#' @importFrom dplyr %>%
#' @export
dplyr::`%>%`

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
    axis.text.x = element_text(size = 8.5, angle = 45, hjust = 1),
    axis.text.y = element_text(size = 8.5),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title =  element_blank(),
    legend.text = element_text(size = 8.5),
    legend.key = element_rect(fill = alpha("white", 0.0))
  )
}
#' Additional Theme for second line in plot
#' @rdname secondline_theme
#' @import ggplot2
#' @export
secondline_theme <- function() {
  theme(
    axis.line.y.right = element_line(color = "grey45", size = 0.5, linetype = "dashed")
    #axis.line.x.top = element_line(color = "grey45", size = 0.5, linetzpe = "dashed"),
  )
}
#' Remove axis labels
#' @rdname secondline_theme
#' @import ggplot2
#' @export
noaxislab_theme <- function() {
  theme(
    axis.title.x.bottom = element_blank(),
    axis.title.y.left = element_blank()
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
areaplot_vars <- function() {
  c("recovered", "deaths", "active")
}
#' Sort hosp type harcoded
#'
areaplot_hospvars <- function() {
  #c("hosp", "vent", "icu")
  as.vector(.hosp_vars) # order is the same
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
aggr_to_cont <- function(data, group, time,
                        allstatuses = intersect(get_aggrvars(), names(data))) {

  aggrvars = setdiff(intersect(get_aggrvars(), names(data)), "population")
  continent_data =    data %>% filter(!is.na(!!rlang::sym(group))) %>%
    select(Country.Region, population, contagion_day, date, !!group, date, !!allstatuses) %>%
    mutate(population = as.numeric(population)) %>%
    group_by(.dots = c(time,group)) %>%
    #group_by(across(all_of(time,group))) %>%
    summarise_at(c(allstatuses), sum, na.rm = TRUE) %>%
    #add_growth_death_rate(group, time) %>%
    #left_join(popdata_cont[,c(group, "population")], by = group) %>% #TODO: why left_join not earlier?
    mutate(
           ##mortality_rate_1M_pop = round(10^6*deaths/population, digits = 3),
           ##confirmed_rate_1M_pop = round(10^6*confirmed/population, digits = 3),
           ##new_confirmed_rate_1M_pop = round(10^6*new_confirmed/population, digits = 3),
           ##tests_rate_1M_pop = round(10^6*tests/population, digits = 3),
           ##new_tests_rate_1M_pop = round(10^6*new_tests/population, digits = 3),
            positive_tests_rate = rate_calc(confirmed, tests),
            new_positive_tests_rate = rate_calc(new_confirmed, new_tests),
            lethality_rate = rate_calc(deaths, tests, nawith0 = TRUE),
            hosp_rate_active = rate_calc(hosp, active),
            icuvent_rate_hosp = rate_calc(icuvent, hosp),
            vaccines_rate_pop = rate_calc(vaccines, population,cap = Inf, digits = 5),
            vaccinated_rate = rate_calc(people_vaccinated, population, cap = 1, digits = 5),
            fully_vaccinated_rate = rate_calc(people_fully_vaccinated, population, cap = 1, digits = 5)

           # positive_tests_rate = ifelse(tests == 0, NA, pmin(round(confirmed/tests, digits = 3),1)),
           # new_positive_tests_rate = ifelse(tests == 0, NA, pmin(round(new_confirmed/new_tests, digits = 3),1)),
           # lethality_rate = round(pmax(0, replace_na(deaths / confirmed, 0)), digits = 4),
           # #new_lethality_rate = round(pmax(0, replace_na(new_deaths / new_confirmed, 0)), digits = 3), # not making much sense
           # hosp_rate_active =  pmin(round(hosp/active, digits = 5), 1),
           # icuvent_rate_hosp =  pmin(round(icuvent/hosp, digits = 4), 1),
           # #new_hosp_rate_1M_pop = round(10^6*new_hosp/population, digits = 3), # no need for other hosp var
           # vaccines_rate_pop =  round(vaccines/population, digits = 5),# do not cap to 1
           #hosp_rate_1M_pop = round(10^6*hosp/population, digits = 3),
           #new_hosp_rate_1M_pop = round(10^6*new_hosp/population, digits = 3)
           #deaths_rate_hosp =  round(deaths/hosp, digits = 5) # not correct
    )  %>%   mutate( # add aggregated vars
      across(all_of(as.vector(aggrvars)), ~oneM_pop_calc(.x,pop = population), .names="{col}_rate_1M_pop") # use all_of
    )   %>%
    mutate_if(is.numeric, list(function(x) {
      ifelse(is.infinite(x),NA, x)
    })) %>%
    # mutate(positive_tests_rate = ifelse(is.infinite(as.numeric(positive_tests_rate)),NA, as.numeric(positive_tests_rate)),
    #        new_positive_tests_rate = ifelse(is.infinite(as.numeric(new_positive_tests_rate)),NA, new_positive_tests_rate),
    #        hosp_rate_active = ifelse(is.infinite(as.numeric(hosp_rate_active)),NA, hosp_rate_active),
    #        deaths_rate_hosp = ifelse(is.infinite(as.numeric(deaths_rate_hosp)),NA, deaths_rate_hosp)) %>%
    rename(Country.Region = !!group) %>%
    get_timeseries_by_contagion_day_data()  %>%
    arrange(desc(date)) %>%   ungroup() %>%
    filter(!is.na(Country.Region)) # filter NAs off again for safety

  continent_data
}

#' creates time series for the area plot
#' @param data data.frame aggregated data per region
#' @param levs order of statuses
#' @param nn minimum number of cases for the start date
#'
#' @note starting date based on nn, first day with so many confirmed cases
#'
#' @return data.frame reshaped
#'
#' @import tidyr
tsdata_areplot <- function(data, levs, nn = 1000) {

  # assumption, there are data with
  if (all(!data$confirmed>nn))
    warning("there are not enough cases")
  mindate = min(data$date[data$confirmed>nn], na.rm = TRUE)

  data = data %>% filter(date >= mindate)

  # data are in descending order, first dates at the end
  #idx = rowSums(data[, levs] > 0) == length(levs)
  idx = rowSums(data[, levs] > 0) >0

  data = data[idx, , drop = FALSE] # remove also in the middles and at th end, no problem

  data %>%
    select( date,!!levs) %>% #rename vars with labels
    #select(Country.Region, date, levs) %>%
    #renamevars() %>%
    pivot_longer(cols = -date, names_to = "status", values_to = "value") %>%
    #mutate(status = factor(status, levels = names(varlabels))) %>%
    mutate(status = factor(status, levels = levs)) %>%
    capitalize_names_df()
}
#' creates message of countries within sub countries
#' @param data data.frame aggregated data per region
#' @param area character main area
#' @param region character sub area
#'
#' @return list messages printing Country.Region within subcontinent
#'
message_subcountries <- function(data, area, region) {
  # remove where subcontinent could be NA
  list.countries = data[,c(area,region)] %>% filter(!is.na(!!rlang::sym(area))) %>% unique() %>%
    #group_by(across(all_of(area))) %>%
    group_by(.dots = area) %>% group_split()
  list.message = lapply(list.countries, function(x)
    paste0("<b>",as.character(unique(x[[area]])),"</b>: ",
           paste(x[[region]], collapse = ", ")))
  c("Continent Area composition: ", list.message)
}
#' Calculates growth vs prevalence, growth vs stringency factors
#' @param data data.frame aggregated data per region
#' @param yvar y variable, growth factors
#' @param xvar x variable, prevalence over 1 M or stringency index
#' @param yLab y variable label
#' @param xLab x variable label
#'
#' @note factors created:
#' c('Low Growth and Prevalence', 'Low Growth - High Prevalence', 'High Growth - Low Prevalence', 'High Growth and Prevalence')
#'
#' @return factor vector
#'
y_vs_x_calc <- function(data, yvar,xvar, yLab = "Growth", xLab = "Prevalence") {
  # compute stats for all growth factors
  xmed = data[[xvar]][data[[xvar]]!= 0]
  ymed = data[[yvar]][data[[yvar]]!= 0]

  med_y = median(ymed, na.rm = TRUE)
  med_x = median(xmed, na.rm = TRUE)

  labs = c(paste("Low", yLab,"and", xLab, collapse = " "),
           paste("Low", yLab," - High",xLab, collapse = " "),
           paste("High", yLab," - Low",xLab, collapse = " "),
           paste("High", yLab," and",xLab, collapse = " ") )

  val_cntry = "NA"
  val_cntry = rep(val_cntry, nrow(data))
  no_data = data[[xvar]]== 0 | data[[yvar]]== 0
  val_cntry[!(no_data) & data[[xvar]] > med_x & data[[yvar]] <= med_y ] = labs[2]
  val_cntry[!(no_data) & data[[xvar]] <= med_x & data[[yvar]]  <= med_y ] = labs[1]
  val_cntry[!(no_data) & data[[xvar]] > med_x & data[[yvar]] > med_y ] = labs[4]
  val_cntry[!(no_data) & data[[xvar]] <= med_x & data[[yvar]] > med_y ] = labs[3]
  factor(val_cntry, levels = labs)
}

#' Rounds up numbers for labels in plots
#' @param maxv numeric max value
#' @param down logical if TRUE then floor is used instead of ceiling
#'
#' @return numeric vector after ceiling()
#'
round_up = function(maxv, down = FALSE) {
  dg = nchar(as.character(round(maxv)))
  if (dg == 1 && maxv>1)
    dg = 0
  if(!down)
   res =   ceiling(maxv/(10^(dg-1)))*10^(dg-1)
  else
   res =   floor(maxv/(10^(dg-1)))*10^(dg-1)
  res
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
    if(namvar %in% .rate_vars) {
      text.pop = paste0(roundlab(x*100),"%")
    } else {
      text.pop = formatC(x, format = "f", big.mark = "'", digits  = getdg_lab(dg, maxy, minxy))
    }
    #x = replace("NA")
    if (any(text.pop != "NA")) {
      text.pop.num = text.pop[text.pop != "NA"]
      text.pop.num = gsub("\\.000$","", text.pop.num)
      text.pop.num = gsub("\\.00$","", text.pop.num)
      text.pop.num = gsub("\\.0$","", text.pop.num)
      text.pop[text.pop != "NA"] = text.pop.num
    }
  } else
    text.pop = x
  text.pop
}

#' Calculates ratio over 1M pop
#' @param x numeric data
#' @param pop numeric vector population size
#'
#' @return x numeric x rescaled by pop size
#'
oneM_pop_calc = function(x, pop) {
  # if (length(x) != length(pop))
  #   stop("different length x and pop")
  round(10^6*x/pop, digits = 3)
}

#' Calculates rates between variables
#' @param x numeric numerator
#' @param y numeric denominator
#' @param cap numeric cap, 1 or Inf for no cap
#' @param floor numeric floor, 0 or -Inf for no floor
#' @param digits numeric digits for rounding
#' @param nawith0 if TRUE then NAs are replaced with 0s
#'
#' @return x numeric rate
#'
rate_calc = function(x, y, cap = 1, floor = 0, digits = 4, nawith0 = FALSE) {
  # if (length(x) != length(pop))
  #   stop("different length x and pop")
  # remove latest NAs
  res = x/y
  res[is.infinite(res)] = NA
  res[is.nan(res)] = NA

  res = round(pmax(pmin(res,cap), floor),digits)
  if (nawith0)
    res = replace_na(res, 0)
 res
}

#' Calculates positive test rate for weekly data
#' @param conf numeric confirmed data last week
#' @param tests numeric tests data last week
#' @param digits numeric digits for rounding
#'
#' @return x numeric positive tests rate
#'
lw_positive_test_rate_calc = function(conf, tests, digits = 4) {
  # if (length(x) != length(pop))
  #   stop("different length x and pop")
  # remove latest NAs
  latesttests = cumsum(tests) != 0
  if (any(latesttests))
    ptr = pmin(1, round(sum(conf[latesttests])/sum(tests[latesttests]), digits = 3))
  else {
    ptr = Inf
  }
  ptr
}

#' Builds dataset to be used in modules merging pop_data with data
#' @param data data
#' @param popdata population data with continent info
#'
#' @return data
#'
#' @import dplyr
#' @export
build_data_aggr <- function(data, popdata) {
  orig_data_aggregate <- data %>%
    #aggregate_province_timeseries_data() %>% # not required anymore
    #add_growth_death_rate() %>%
    arrange(Country.Region)

  if (!missing(popdata)) {
    orig_data_aggregate = orig_data_aggregate %>%
      merge_pop_data(popdata)

  }
  # select variables that can be divided by population
  aggrvars = setdiff(intersect(get_aggrvars(), names(orig_data_aggregate)), "population")

  # orig_data_aggregate$positive_tests_rate = ifelse(orig_data_aggregate$tests == 0, NA,
  #                                                  pmin(round(orig_data_aggregate$confirmed/orig_data_aggregate$tests, digits = 4),1))
  # orig_data_aggregate$new_positive_tests_rate = ifelse(orig_data_aggregate$new_tests == 0, NA,
  #                                                      pmin(round(orig_data_aggregate$new_confirmed/orig_data_aggregate$new_tests, digits = 4),1))
  orig_data_aggregate = orig_data_aggregate %>%   mutate(
              across(all_of(as.vector(aggrvars)), ~oneM_pop_calc(.x,pop = population), .names="{col}_rate_1M_pop") # use all_of
          ) %>%
    #merge_pop_data(popdata) %>% # compute additional variables
    mutate(
           ##mortality_rate_1M_pop = round(10^6*deaths/population, digits = 3), ## keep mortality rate for convenience
           ##confirmed_rate_1M_pop = round(10^6*confirmed/population, digits = 3),
           ##new_confirmed_rate_1M_pop = round(10^6*new_confirmed/population, digits = 3),
           # tests_rate_1M_pop = round(10^6*tests/population, digits = 3),
           # new_tests_rate_1M_pop = round(10^6*new_tests/population, digits = 3),
           #positive_tests_rate = ifelse(tests == 0, NA, pmin(round(confirmed/tests, digits = 4),1)),
           positive_tests_rate = rate_calc(confirmed, tests),
           #positive_tests_rate = positive_test_rate_calc(confirmed, tests),
           #new_positive_tests_rate = ifelse(tests == 0 , NA, pmin(round(new_confirmed/new_tests, digits = 4),1)),
           new_positive_tests_rate = rate_calc(new_confirmed, new_tests),

           #new_positive_tests_rate = positive_test_rate_calc(confirmed, tests),
           #lethality_rate = round(pmax(0, replace_na(deaths / confirmed, 0)), digits = 4),
           lethality_rate = rate_calc(deaths, confirmed, nawith0 = TRUE),
           #new_lethality_rate = rate_calc(new_deaths, new_confirmed, nawith0 = TRUE),

           #new_lethality_rate = round(pmax(0, replace_na(new_deaths / new_confirmed, 0)), digits = 3),
           # hosp_rate_active =  pmin(round(hosp/active, digits = 5), 1),
           # icuvent_rate_hosp =  pmin(round(icuvent/hosp, digits = 4), 1),
           hosp_rate_active = rate_calc(hosp, active), # do not cap to 1
           icuvent_rate_hosp = rate_calc(icuvent, hosp), # do not cap to 1

           #vaccines_rate_pop =  round(vaccines/population, digits = 5), # do not cap to 1
           vaccines_rate_pop = rate_calc(vaccines, population, cap = Inf), # do not cap to 1
           vaccinated_rate = rate_calc(people_vaccinated, population, cap = 1),
           fully_vaccinated_rate = rate_calc(people_fully_vaccinated, population, cap = 1)
           #deaths_rate_hosp =  round(deaths/hosp, digits = 5) # not correct
           )  %>%   #mutate(
           #   across(all_of(as.vector(.hosp_vars)), ~oneM_pop_calc(.x,pop = population), .names="{col}_rate_1M_pop") # use all_of
           # ) %>%
    mutate_if(is.numeric, list(function(x) {
      ifelse(is.infinite(x),NA, x)
    }))

  classd = sapply(orig_data_aggregate, class)
  whichc = which(classd == "character")
  whichn = which(classd == "numeric")
  whichd = which(classd == "Date")
  whichl = which(classd == "logical") # for all NAs
  if (sum(sort(c(whichd, whichc, whichn, whichl))) != sum(1:ncol(orig_data_aggregate)))
    stop("Some columns are not character numeric and Date")

  orig_data_aggregate[, c(whichd, whichc, whichn, whichl)]
}

#' Computes last week variables from \code{build_data_aggr}
#' @param data data.frame
#' @param days integer 7 14 or 30,
#' @note Last week variables have prefix 'lw'
#'       Past week variables have prefix 'pw'
#'       Past month variables have prefix 'lm'
#' @details days = 7 then last week variables are computed.
#' days = 14 then past week variables from days -14 to -7 are computed.
#' days = 30 then last month variables are computed.
#'
#' @return data.frame with last/past week/month variables added
#'
#' @import dplyr
#' @export
lw_vars_calc <- function(data, days = 7) {
  if (length(unique(data$date))<7)
    stop("data must have at least 7 days")
  if (!days %in% c(7,14,30))
    stop("Allow 7 14 and 30 as last days in lw_vars_calc")

  # now = as.POSIXct(Sys.time()) # given time zone
  # LastDate =  as.Date(now - delay_date())
  LastDate <- get_asofdate(char = FALSE)
  # remove all countries without updates in the last week
  data = data %>%
      filter(AsOfDate > (LastDate-7))

  n.days = ifelse(days == 30, 30, 7)
  data7 = data %>% filter(date > (max(date, na.rm = TRUE)-days))# last week
  data7 = data7 %>% filter(date < (min(date, na.rm = TRUE)+n.days))# if days are 14 then select the previous week

  # check population:
  if(nrow(unique(data7[,c("Country.Region","population")]))> length(unique(data7$Country.Region))) {
    warning("inconsistent population variable, taking max for each Country")

    data7 = data7 %>% group_by(Country.Region) %>% mutate(maxpop = max(population)) %>% ungroup()

    data7$population =  data7$maxpop
    data7 = data7 %>%
      select(-maxpop)
  }
  aggr_vars = setdiff(get_aggrvars(),.current_vars) # remove variables that cannot be aggregated over a week
  aggr_vars = intersect(colnames(data7),aggr_vars)
  aggr_vars = grep("new", aggr_vars, value = TRUE) # select new ones
  if (length(aggr_vars) == 0)
    stop("new_ vars not available")
  data7vars = data7 %>% group_by(Country.Region) %>%
    summarise_at(aggr_vars, sum, na.rm = TRUE) %>% ungroup()
  # rename columns
  colnames(data7vars) = gsub("new","lw",colnames(data7vars))
  # calculate for hosp vars
  .lwcalc_diff <- function(x, date) {
    tdy <- max(date)
    startday <- min(date)
    x[date == tdy] - x[date == startday]
  }
  #varExtra <- c(.hosp_vars, setdiff(.vacc_vars_datahub, "vaccines"))
  varExtra <- .hosp_vars

  data7extra = data7 %>% group_by(Country.Region) %>%
    summarize(lw_positive_tests_rate = lw_positive_test_rate_calc(new_confirmed, new_tests),
              across(all_of(as.vector(varExtra)), ~.lwcalc_diff(.x, date), .names= "lw_{col}")) # last week of hospitalized

  # add back population
  data7vars = data7vars %>% left_join(unique(data7[,c("Country.Region","population"), drop = FALSE])) %>%
    right_join(data7extra, by = "Country.Region") # right join simply to have them on the right
  # save value for
  data7today <- data7 %>%
    filter( date == (AsOfDate - days +7))

  if (!identical(sort(data7today$Country.Region), sort(data7vars$Country.Region)))
    warning("Missing some countries in active lw calculation")
  data7vars <- data7vars %>%
    left_join(data7today[, c("Country.Region","active", "hosp")],
              by = c(c("Country.Region")))

  aggrvars <- setdiff(intersect(get_aggrvars(), names(data7vars)), c("population", "active", "hosp"))
  # compute rates
  data7vars = data7vars %>%
    mutate(
           #lw_confirmed_rate_1M_pop = round(10^6*lw_confirmed/population, digits = 3),
           # lw_tests_rate_1M_pop = round(10^6*lw_tests/population, digits = 3),
           #lw_positive_tests_rate = round(lw_confirmed/lw_tests, digits = 3), replaced by lw_positive_test_rate_calc
            lw_active = replace_na(lw_confirmed - lw_deaths - lw_recovered,0),

            lw_lethality_rate = rate_calc(lw_deaths, lw_confirmed, nawith0 = TRUE),
            lw_vaccines_rate_pop = rate_calc(lw_vaccines, population, cap = Inf, digits = 5),
            lw_vaccinated_rate = rate_calc(lw_people_vaccinated, population, cap = 1, digits = 5),
            lw_fully_vaccinated_rate = rate_calc(lw_people_fully_vaccinated, population, cap = 1, digits = 5),
            lw_hosp_rate_active = rate_calc(hosp, active, cap = Inf), # use current var

           # lw_lethality_rate = round(pmax(0, replace_na(lw_deaths / lw_confirmed, 0)), digits = 4),
           # lw_vaccines_rate_pop =  round(pmax(0, replace_na(lw_vaccines / population, 0)), digits = 5),
           #lw_mortality_rate = round(pmax(0, replace_na(lw_deaths / population, 0)), digits = 5)
           #lw_hosp_rate_active =  pmin(round(lw_hosp/lw_active, digits = 5), 1),
           #lw_icuvent_rate_hosp =  pmin(round(lw_icuvent/lw_hosp, digits = 4), 1),
           #lw_hosp_rate_1M_pop = round(10^6*lw_hosp/population, digits = 3)
    ) %>%
    select(- active, - hosp) %>% # remove active and hosp, required forlw_hosp_rate_active
    mutate(
      across(all_of(as.vector(aggrvars)), ~oneM_pop_calc(.x,pop = population), .names="{col}_rate_1M_pop") # use all_of
    ) %>%
    mutate_if(is.numeric, list(function(x) {
      ifelse(is.infinite(x),NA, x)
    }))

  if (days == 14)
    colnames(data7vars) = gsub("lw","pw",colnames(data7vars))
  if (days == 30)
    colnames(data7vars) = gsub("lw","lm",colnames(data7vars))
  data7vars
}

#'Global definition of numeric cumulative vars in dataset
#'
#' @return character vector
#'
get_cumvars = function() {
  c("confirmed", "deaths","recovered","tests", .vacc_vars_datahub)
}

#'Global definition of numeric aggregatable vars in dataset
#'
#' @return character vector
#'
get_aggrvars = function() {
  cumvars = get_cumvars()
  #statuses <- c("confirmed", "deaths", "recovered", "active")
  # select all variables
  allstatuses = c(cumvars, "active", as.vector(.hosp_vars))
  allstatuses = c(allstatuses, prefix_var(allstatuses), "population")
  allstatuses
}
#'Global definition of continent names for different purposes
#' @param idx integer, id of continent to be extracted: "Europe", "Asia", "Africa", "LatAm & Carib.", "Northern America", "Oceania"
#' @param contname character, name of continent to be extracted: "Europe", "Asia", "Africa", "LatAm & Carib.", "Northern America", "Oceania"
#' @return data.frame with continent info
#'
.getContinents = function(idx, contname){
  tabuicontinents = c("Europe", "Asia", "Africa", "Lat. America & Carib.", "Northern America", "Oceania")
  continents = c("Europe", "Asia", "Africa", "LatAm & Carib.", "Northern America", "Oceania")
  mainuicontinents = c("Europe", "Asia", "Africa", "LatAm", "NorthernAmerica", "Oceania")
  uicontinents = c("europe", "asia", "africa", "latam", "northernamerica", "oceania")
  adjective = c("europian", "asian", "african", "latin american", "northern america", "oceanian")

  res = data.frame(tab = tabuicontinents, names = continents, mainui = mainuicontinents, ui = uicontinents,
                   adjective = adjective, stringsAsFactors = FALSE)
  if (!missing(idx)) {
    res = res[idx,, drop = FALSE]
  }
  if (!missing(contname)) {
    res = res[res$names == contname,, drop = FALSE]
  }
  res
}


#'Message text, selection of confirmed cases
#' @param where character string, where text happens
#' @param ncases numeric string, minimum number of confirmed cases
#' @param suffix character string, end of text
#'
#' @return character vector
#'
message_conf_case = function(where = "Countries", ncases = 1000, suffix = "can be chosen") {
  paste("Only",where, "with more than", ncases, "confirmed cases", suffix,".")
}
#'Message text, selection of confirmed cases
#' @param ncases numeric string, minimum number of confirmed cases
#' @param var character string, variable used
#'
#' @return character vector
#'
message_firstday <- function(ncases, var = "confirmed") {
  paste("1st day is the day when", ncases ,var, "cases are reached.")
}
#'Message text, missing data
#' @param where character string, where text happens
#' @param what character string, text for missing variables
#'
#' @return character vector
#'
message_missing_data <- function(what = "Recovered, Hospitalized and Tests", where = "some countries and areas") {
  paste(what, "data can be partially/completely unavailable in our data source for",where, ".")
}

#'Message text, missing days for given countries in the data
#' @param data data.frame with Country data
#' @param sep character separator <br/>
#'
#' @return character vector with message
#'
message_missing_country_days = function(data, sep = "<br/>") {

  allcountries = unique(data$Country.Region)
  # not in the last day at all
  countries_notinlast = allcountries[!allcountries %in% unique(data[data$date == max(data$date), "Country.Region", drop = TRUE])]
  last10days = max(data$date) - 0:9
  vars_new = paste0("new_",get_cumvars())
  missdata = data[, c("Country.Region","date",vars_new)] %>%
    filter(date %in% last10days) %>% # take
    group_by(Country.Region,date) %>% #.[,get_cumvars()] %>%
    summarize(miss = sum(c_across(vars_new), na.rm = TRUE),
              allzero = miss == 0) %>%
    ungroup() %>% arrange(Country.Region, desc(date)) %>%
    filter(allzero)
  missdays = sapply(last10days,
         function(day){
           missdata$Country.Region[missdata$date == day]
         })
  names(missdays) = last10days
  missdays[[as.character(max(data$date))]] = unique(c(countries_notinlast, missdays[[as.character(max(data$date))]]))

  countries = unique(unlist(missdays))
  #msg = tags$li("Numbers in the latest days can be understimated for multiple reasons,  for example cases can be revised and updated with delay after few days.")
  msg = c("Latest Figures can be understimated for multiple reasons, for example cases can be revised and updated with delay after few days.")

  if (length(countries)>1) {
    country_miss = apply(sapply(countries, function(cc) {
      sapply(missdays, function(ccd) {
        cc %in% ccd
      })
    }), 2, function(df) {
      if (df[1]) # first day must be missing
        ifelse(all(df), length(df), min(which(!df)))
      else
        0
    })
    country = sapply(unique(country_miss), function(md) {
      names(country_miss)[country_miss == md]
    })
    names(country) = unique(country_miss)
    country = country[names(country) != "0"]
    country = country[sort(names(country))]
    ddays = names(country)
    ddays[ddays ==1] = ""
    daysstr = rep("days.", length(country))
    daysstr[ddays == ""] = "day."
    msgdays = paste("last", ddays, daysstr, sep = " ")
    textcountries = sapply(country, paste, collapse = ", ")
    msg1 = paste(textcountries, "have no data updates in",msgdays)
    n.countrys = as.vector(sapply(country, length))
    msg1[n.countrys == 1] = sapply(msg1[n.countrys == 1],function(x){
      gsub("have","has",x)
    })

    msg = c(msg,"In our dataset the following areas miss the most recent data:",
            paste("<li>", msg1, "</li>"))
  }

  msg = paste(msg, collapse = sep)
  msg

}
#'Message text, recovered
#' @param where character string, where text happens
#' @param what character string, text for missing variables
#'
#' @return character vector
#'
message_missing_recovered = function(what = "Recovered", where = "Some countries and areas") {
  paste(where ,"have stopped providing",what, "data.")
}
#'Message text, missing data
#' @param where character string, where text happens
#' @param what character string, text for missing variables
#' @param suffix character string, end of text
#'
#' @return character vector
#'
message_hosp_data <- function(what = "Hospitalized, Vaccine doses and Test", where = "some countries and areas", suffix = "where available") {
  paste(what, "data are updated with delay for", where, "in our data source", suffix, ".")
}
#' Color Palette for simple barplots
#'
#' @export
barplots_colors <- list(
  #"growth_factor" = "#dd4b39",
  "growth_factor" = list("uniform" = "chocolate3",
                         "calc" = c(col = "Oranges", rev = TRUE, skip = 2)
  ),
  "death_rate" = list("uniform" = "grey30",
                      "calc" = c(col = "Greys", rev = TRUE, skip = 2)
  ),
  "stringency" = list("uniform" = "grey30",
                      "calc" = c(col = "Greys", rev = TRUE, skip = 2)
  ),
  #"stringency" = c(col = "Greys", rev = TRUE, skip = 2),
  #"vaccines" = c(col = "Blues", rev = TRUE, skip = 2),
  "vaccines" = list("uniform" = .case_colors["vaccines"] ,
                    "calc" = c(col = "Blues", rev = TRUE, skip = 2)
  ),
  #"tests" = c(col = "Greens", rev = TRUE, skip = 2),
  "tests" = list("uniform" = "green" ,
                 "calc" = c(col = "Greens", rev = TRUE, skip = 2)
  ),
  #"hosp" = c(col = "Blues", rev = TRUE, skip = 2),
  "hosp" = list("uniform" = .case_colors["hosp"] ,
                "calc" = c(col = "Blues", rev = TRUE, skip = 2)
  ),
  #"confirmed" = c(col = "Reds", rev = TRUE, skip = 2)
  "confirmed" = list("uniform" = .case_colors["confirmed"] ,
                     "calc" = c(col = "Reds", rev = TRUE, skip = 2)
  )
)

#' Check whether data are available
#' @param data data.frame
#' @param var character string, variable name
#'
#' @return logical
#' @noRd
check_flag <- function(data, var) {
  Flag = TRUE
  # do not use stringency v is the same for all areas
  if (all(is.na(data[[var]])) || all(data[[var]] == 0, na.rm = TRUE) || length(table(data[[var]])) == 1)
    Flag = FALSE
  Flag
}
