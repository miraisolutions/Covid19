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
    text = element_text(size = 16),
    panel.background = element_blank(),
    axis.line.x = element_line(color = "grey45", size = 0.5),
    axis.line.y = element_line(color = "grey45", size = 0.5),
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    legend.title =  element_blank(),
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
  "growth_factor" = "#dd4b39",
  "death_rate" = "black"
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
#'
load_countries_data <- function(destpath = system.file("./countries_data", package = "Covid19")){
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
}

#' Sort type by max
#'
#' @param data data to be sorted
sort_type_by_max <- function(data) {
  c("active", "recovered", "deaths") %>% .[order(sapply(data[.], max))]
}

#' Sort type harcoded
#'
#' @param data data to be sorted
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
      "Cape Verde" = "Cabo Verde",
      "Cote d'Ivoire" = "C\\u00f4te d'Ivoire",
      "Czech Republic" = "Czechia",
      "Dominican Republic" = "Dominican Rep.",
      "Eswatini" = "eSwatini",
      "French Guiana" = "Guyana",
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
      "Cabo Verde" = "Cape Verde",
      "C\\u00f4te d'Ivoire" = "Cote d'Ivoire",
      "Czechia" = "Czech Republic",
      "Dominican Rep." = "Dominican Republic",
      "eSwatini" = "Eswatini",
      "Guyana" = "French Guiana",
      "Macedonia" = "North Macedonia",
      "United Kingdom" = "UK",
      "United States of America" = "USA",
      "Vatican" = "Vatican City"
    )
  data
}
