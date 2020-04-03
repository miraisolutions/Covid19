if (interactive()) {
  library(shiny)
  long_title <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit."
  ui <- fluidPage(
    tagList(
      Covid19:::golem_add_external_resources(),
      Covid19:::mod_map_ui("map_ui")
    )
  )
  server <- function(input, output, session) {
    orig_data <- reactive({
      orig_data <- get_timeseries_full_data() %>%
        get_timeseries_by_contagion_day_data()
    })
    callModule(Covid19:::mod_map_server, "map_ui", orig_data)
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
