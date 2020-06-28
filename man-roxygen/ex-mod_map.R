if (interactive()) {
  library(shiny)
  library(dplyr)
  long_title <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit."
  ui <- fluidPage(
    tagList(
      Covid19Mirai:::golem_add_external_resources(),
      Covid19Mirai:::mod_map_ui("map_ui")
    )
  )


  server <- function(input, output, session) {

    countries_data_map <- load_countries_datahub_map(destpath = system.file("./countries_data", package = "Covid19Mirai"))


    orig_data <- reactive({ get_datahub() %>%
        get_timeseries_by_contagion_day_data()
    })

    pop_data = get_pop_datahub()
    orig_data_aggregate = reactive({ build_data_aggr(orig_data(), pop_data)})

    callModule(mod_map_server, "map_ui", orig_data_aggregate, countries_data_map)
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

# if (interactive()) {
#   library(shiny)
#   library(leaflet)
#   ui <- fluidPage(
#     leafletOutput("myMap", width = "100%", height = "800px")
#   )
#   server <- function(input, output, session) {
#     countries_data <- load_countries_data(destpath = "./inst")
#     map <- leaflet(data = countries_data) %>%
#       setView(0, 30, zoom = 3) %>%
#       addPolygons(data = countries_data)
#     output$myMap <- renderLeaflet({map})
#   }
#   runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
# }
