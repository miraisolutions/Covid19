if (interactive()) {
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(plotly)
  library(leaflet)
  library(leaflet.extras)

  library(shinycssloaders)
  library(RColorBrewer)
  library(COVID19)

  variable = "growth vs prevalence" # set variable
  #variable = "death rate" # set variable
  #variable = "prevalence rate" # set variable
  #variable = "active" # set variable
 #variable = "growth factor" # set variable
  #variable = "confirmed" # set variable

 sapply(file.path("R",list.files("R")), source)
  long_title <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit."
  ui <- fluidPage(
    tagList(
      Covid19Mirai:::golem_add_external_resources(),
      mod_map_area_calc_ui("map_area_country_ui")
    )
  )
  server <- function(input, output) {

    country = "Switzerland"

    area_data_2 <- get_datahub(country, lev = 2) %>%
      get_timeseries_by_contagion_day_data()

    area_data_2_aggregate <-
      build_data_aggr(area_data_2)
    area2_data_map = leaflet::gadmCHE

    data7_2_aggregate = lw_vars_calc(area_data_2_aggregate)

    # create datasets for maps merging today with data7
    data_area2 = area_data_2_aggregate %>% filter(date == max(date)) %>%
      left_join(data7_2_aggregate %>% select(-population))

    .adjustmap = function(spmap, country) {
      spmap$NAME = factor(spmap$NAME_1)

      spmap$NAME = gsub("\"", "", spmap$NAME)
      spmap$NAME = gsub("^", "", spmap$NAME, fixed = TRUE)
      spmap$NAME = gsub("`", "", spmap$NAME, fixed = TRUE)
      if (country == "Switzerland") {
        spmap$NAME = recode(spmap$NAME, "Basel-Landschaft" = "Baselland",
                            "Sankt Gallen" = "St.Gallen",
                            "Lucerne" = "Luzerne")
      }
      spmap
    }
    getmap <- function(country) {
      if (country == "Switzerland")
        area2_data_map = leaflet::gadmCHE
      else
        stop("Map for ", country, " not available")
      .adjustmap(area2_data_map, country)
    }
    area2_map = getmap(country)


    # area2_map = area2_data_map
    # area2_map$NAME = factor(area2_map$NAME_1)
    #
    # area2_map$NAME = gsub("\"", "", area2_map$NAME)
    # area2_map$NAME = gsub("^", "", area2_map$NAME, fixed = TRUE)
    # area2_map$NAME = gsub("`", "", area2_map$NAME, fixed = TRUE)
    # area2_map$NAME = recode(area2_map$NAME, "Basel-Landschaft" = "Baselland",
    #                         "Sankt Gallen" = "St.Gallen",
    #                        "Lucerne" = "Luzerne")
    # area2_map = .subsetmap(area2_data_map, ccname = "NAME_0",
    #                                      cc = country, lev2name = "NAME_1")

    callModule(mod_map_area_calc_server, "map_area_country_ui", df = data_area2,
               area2_map, area = country, variable = variable, max.pop = 0, countrymap = TRUE)
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

