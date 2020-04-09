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

  orig_data <- reactive({
    get_timeseries_full_data() %>%
      get_timeseries_by_contagion_day_data()
  })

  orig_data_aggregate <- reactive({
    orig_data_aggregate <- orig_data() %>%
      aggregate_province_timeseries_data() %>%
      add_growth_death_rate() %>%
      arrange(Country.Region)
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

  # Modules ----
  callModule(mod_global_server, "global", orig_data = orig_data, orig_data_aggregate = orig_data_aggregate)
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

  # alert popup regarding data source
  showModal(modalDialog(
    title = "ALERT: Temporary Data Source",
    includeMarkdown(system.file("additional_files/alert.md", package = "Covid19")),
    footer = modalButton("Dismiss"),
    size = "l",
    easyClose = TRUE,
    fade = FALSE
  ))
}
