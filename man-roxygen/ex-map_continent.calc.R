if (interactive()) {
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(plotly)
  library(leaflet)
  library(shinycssloaders)

  cont = "LatAm & Carib."
  cont = "Oceania"

  variable = "growth vs prevalence" # set variable
  variable = "death rate" # set variable
  variable = "prevalence rate" # set variable
  #variable = "active" # set variable
 #variable = "growth factor" # set variable

 sapply(file.path("R",list.files("R")), source)
  long_title <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit."
  ui <- fluidPage(
    tagList(
      Covid19Mirai:::golem_add_external_resources(),
      Covid19Mirai:::mod_map_cont_calc_ui("map_cont_calc_ui")
    )
  )
  server <- function(input, output) {

    orig_data <- reactive({ get_datahub() %>%
        get_timeseries_by_contagion_day_data()
    })

    pop_data = get_pop_datahub()
    orig_data_aggregate = reactive({ build_data_aggr(orig_data(), pop_data)})

    countries_data_map <- Covid19Mirai:::load_countries_datahub_map(destpath = system.file("./countries_data", package = "Covid19Mirai"))

    orig_data_aggregate_cont <- reactive({
      orig_data_aggregate() %>% filter(continent == cont)
    })

    callModule(mod_map_cont_cal_server, "map_cont_calc_ui", orig_data_aggregate = orig_data_aggregate_cont,  countries_data_map,
               cont = cont, variable = variable)
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

