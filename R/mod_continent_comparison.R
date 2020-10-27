#' continent_comparison UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom shinycssloaders withSpinner
mod_continent_comparison_ui <- function(id){
  ns <- NS(id)
  tagList(
    # div(
    #   textOutput(ns("from_nth_case"))
    # ),
    withSpinner(uiOutput(ns("lineplots_cont"))),
    fluidRow(
      column(5,
             withSpinner(uiOutput(ns("rateplots_cont")))
      ),
      column(7,
             withSpinner(uiOutput(ns("lines_points_plots_cont")))
             )
    ),
    fluidRow(
      column(6,
             withSpinner(uiOutput(ns("scatterplot_plots_cont")))
      ),
      column(6,
             withSpinner(uiOutput(ns("status_stackedbarplot_cont")))
      )
    ),
    mod_add_table_ui(ns("add_table_cont"))
  )
}

#' continent_comparison Server Function
#'
#' @param orig_data_aggregate data.frame
#' @param nn min number of cases for a country to be considered. Default 1000
#' @param w number of days of outbreak. Default 7
#'
#' @import dplyr
#'
#' @noRd
mod_continent_comparison_server <- function(input, output, session, orig_data_aggregate, nn = 1000, w = 7, pop_data){
  ns <- session$ns

  # aggregate data to continent
  continent_data <- aggr_to_cont(orig_data_aggregate, "continent", "date")

  continents = unique(continent_data$Country.Region)

  # create data for comparison with common starting point
  continent_data_filtered <- continent_data %>%
      rescale_df_contagion(n = nn, w = w)

  output$lineplots_cont <- renderUI({
    tagList(
      h2("Continents Comparison"),
      mod_lineplots_day_contagion_ui(ns("lineplots_day_contagion_cont"))
    )
  })

  callModule(mod_lineplots_day_contagion_server, "lineplots_day_contagion_cont", continent_data, nn = nn)

  # Rate plots ----
  output$rateplots_cont <- renderUI({
    mod_growth_death_rate_ui(ns("rate_plots_cont"))
  })

  callModule(mod_growth_death_rate_server, "rate_plots_cont", continent_data_filtered, nn = nn, n_highligth = length(continents), istop = FALSE)

  # Line with bullet plot

  output$lines_points_plots_cont <- renderUI({
    mod_compare_nth_cases_plot_ui(ns("lines_points_plots_cont"), tests = FALSE, hosp = FALSE, selectvar = "new_prevalence_rate_1M_pop")
  })

  callModule(mod_compare_nth_cases_plot_server, "lines_points_plots_cont", continent_data, nn = nn, w = w,
             n_highligth = length(continents), istop = FALSE)

  # scatterplot
  output$scatterplot_plots_cont <- renderUI({
    mod_scatterplot_ui(ns("scatterplot_plots_cont"))
  })

  callModule(mod_scatterplot_server, "scatterplot_plots_cont", continent_data_filtered, nmed = nn, n_highligth = length(continents), istop = FALSE, countries = continents)

  output$status_stackedbarplot_cont <- renderUI({
    mod_stackedbarplot_ui(ns("status_stackedbarplot_cont"))
  })
  callModule(mod_stackedbarplot_status_server, "status_stackedbarplot_cont", continent_data_filtered, n_highligth = length(continents), istop = FALSE)

  # tables ----
  callModule(mod_add_table_server, "add_table_cont", continent_data_filtered, maxrowsperpage = 10)



}
