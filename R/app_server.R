#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import dplyr
#' @import tidyr
#' @import markdown
#'
#' @noRd
app_server <- function(input, output, session) {

  # Params ----
  n <- 1000 #  min number of cases for a country to be considered. Default 1000
  w <- 7 # number of days of outbreak. Default 7

  # Data ----
  # map
  countries_data_map <- load_countries_data_map(destpath = system.file("./countries_data", package = "Covid19"))

  orig_data <- reactive({
    get_timeseries_full_data() %>%
      get_timeseries_by_contagion_day_data()
  })

  pop_data = get_pop_data()

  orig_data_aggregate <- reactive({
    orig_data_aggregate <- orig_data() %>%
      aggregate_province_timeseries_data() %>%
      add_growth_death_rate() %>%
      arrange(Country.Region) %>%
      align_country_names_pop() %>%
      merge_pop_data(pop_data) %>% # compute additional variables
      align_country_names_pop_reverse() %>%
      mutate(mortality_rate_1M_pop = round(10^6*deaths/population, digits = 3),
             prevalence_rate_1M_pop = round(10^6*confirmed/population, digits = 3),
             new_prevalence_rate_1M_pop = round(10^6*new_confirmed/population, digits = 3))
    orig_data_aggregate
  })

  output$last_update <- renderText({
    paste0("Last updated: ",
           max(orig_data()$date)
    )
  })

  data_filtered <- reactive({
    orig_data_aggregate() %>%
      rescale_df_contagion(n = n, w = w)
  })

  countries <- reactive({
    data_filtered() %>%
      select(Country.Region) %>%
      distinct()
  })


  # continents <- reactive({
  #   data_filtered() %>%
  #     select(continent) %>%
  #     distinct()
  # })

  # Modules ----
  callModule(mod_global_server, "global", orig_data = orig_data, orig_data_aggregate = orig_data_aggregate, countries_data_map)
  callModule(mod_continent_comparison_server, "continent_comparison", orig_data_aggregate = orig_data_aggregate, n = n, w = w, pop_data = pop_data)

  # select continents in tabs
  continents = c("Europe", "Asia", "Africa", "LatAm & Carib.", "Northern America", "Oceania")
  mainuicontinents = c("Europe", "Asia", "Africa", "LatAm", "NorthernAmerica", "Oceania")
  uicontinents = c("europe", "asia", "africa", "latam", "northernamerica", "oceania")
  for (i.cont in 1:length(continents)) {
    callModule(mod_continent_server, paste(mainuicontinents[i.cont], "comparison", sep = "_"),
               orig_data_aggregate = orig_data_aggregate, n = n, w = w,
               pop_data = pop_data, countries_data_map = countries_data_map,
               cont = continents[i.cont], uicont = uicontinents[i.cont])

  }


  callModule(mod_country_server, "country", orig_data_aggregate = orig_data_aggregate, data_filtered = data_filtered, countries = countries, n = n, w = w)
  callModule(mod_country_comparison_server, "country_comparison", orig_data_aggregate = orig_data_aggregate, data_filtered = data_filtered, countries = countries, n = n, w = w)

  # Modal ----
  # what is new pop-up
  observeEvent(input$btn_whatsnew, {
    showModal(modalDialog(
      title = "What's New:",
      includeMarkdown("./NEWS.md"),
      footer = modalButton("Dismiss"),
      size = "l",
      easyClose = TRUE,
      fade = FALSE
    ))
  })

}
