if (interactive()) {
  library(shiny)
  library(Covid19)
  library(dplyr)
  long_title <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit."
  ui <- fluidPage(
    tagList(
      Covid19:::golem_add_external_resources(),
      Covid19:::mod_map_ui("map_ui")
    )
  )


  server <- function(input, output, session) {

    pop_data = get_pop_data()
    countries_data_map <- load_countries_data_map(destpath = system.file("./countries_data", package = "Covid19"))


    orig_data_aggregate <- reactive({
      orig_data <- get_timeseries_full_data() %>%
        get_timeseries_by_contagion_day_data()
      orig_data_aggregate <- orig_data %>%
        aggregate_province_timeseries_data() %>%
        add_growth_death_rate() %>%
        arrange(Country.Region) %>%
        #align_country_names_pop() %>%
        merge_pop_data(pop_data) %>% # compute additional variables
        #align_country_names_pop_reverse() %>%
        mutate(mortality_rate_1M_pop = round(10^6*deaths/population, digits = 3),
               prevalence_rate_1M_pop = round(10^6*confirmed/population, digits = 3),
               new_prevalence_rate_1M_pop = round(10^6*new_confirmed/population, digits = 3))
      orig_data_aggregate
    })
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
