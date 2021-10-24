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
    #withSpinner(uiOutput(ns("lineplots_cont"))),
    div(h4("Continents Comparison"), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
    mod_lineplots_day_contagion_ui(ns("lineplots_day_contagion_cont")),
    fluidRow(
      column(5,
             #withSpinner(uiOutput(ns("rateplots_cont")))
             withSpinner(mod_barplot_ui(ns("rate_plots_cont")))
      ),
      column(7,
             # withSpinner(uiOutput(ns("lines_points_plots_cont")))
             # )
      withSpinner(mod_compare_nth_cases_plot_ui(ns("lines_points_plots_cont"), istop = FALSE, nn = 1000, tests = FALSE, hosp = FALSE, selectvar = "new_confirmed", oneMpop = TRUE, vax = TRUE))
      )
    ),
    fluidRow(
      column(6,
             withSpinner(mod_scatterplot_ui(ns("scatterplot_plots_cont")))
      ),
      column(6,
             withSpinner(mod_barplot_ui(ns("barplot_vax_index_cont"), plot1 = "ui_vaccines", plot2 = NULL))
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
#' @example ex-ex-continents.R
#'
#' @import dplyr
#'
#' @noRd
mod_continent_comparison_server <- function(input, output, session, orig_data_aggregate, nn = 1000, w = 7, pop_data){
  ns <- session$ns

  # aggregate data to continent
  message("start mod_continent_comparison_server")
  continent_data <- aggr_to_cont(orig_data_aggregate, "continent", "date")

  continents = unique(continent_data$Country.Region)

  # create data for comparison with common starting point
  continent_data_filtered <- continent_data %>%
      rescale_df_contagion(n = nn, w = w)

  continent_data_filtered_today = continent_data_filtered %>%
    add_growth_death_rate()

  lw_continent_data_filtered =  lw_vars_calc(continent_data_filtered)
  pw_continent_data_filtered =  lw_vars_calc(continent_data_filtered, 14)

  continent_data_filtered_today = continent_data_filtered_today  %>%
    left_join(lw_continent_data_filtered %>% select(-population))  %>%
    left_join(pw_continent_data_filtered %>% select(-population))

  callModule(mod_lineplots_day_contagion_server, "lineplots_day_contagion_cont", continent_data, nn = nn, statuses = c("confirmed", "deaths", "vaccines", "active"))

  # Rate plots ----

  callModule(mod_barplot_server, "rate_plots_cont", continent_data_filtered_today, n_highlight = length(continents), istop = FALSE,
             g_palette = list("plot_1" = graph_palette[1:length(continents)], #barplots_colors$stringency,
                              "plot_2" = graph_palette[1:length(continents)],
                              calc = FALSE),
             sortbyvar = FALSE)

  # Line with bullet plot

  callModule(mod_compare_nth_cases_plot_server, "lines_points_plots_cont", continent_data, nn = nn,
             n_highlight = length(continents), tests = FALSE, hosp = FALSE, istop = FALSE, oneMpop = TRUE, vax = TRUE)

  # scatterplot

  callModule(mod_scatterplot_server, "scatterplot_plots_cont", continent_data_filtered_today, nmed = nn, n_highlight = length(continents), istop = FALSE, countries = continents)


  callModule(mod_barplot_server, "barplot_vax_index_cont", continent_data_filtered_today,
             n_highlight = length(continents), istop = FALSE,
             plottitle = c("Vaccination Status"),
             g_palette = list("plot_1" = graph_palette[1:length(continents)], #barplots_colors$stringency,
                              calc = FALSE),
             sortbyvar = FALSE
             )

  # tables ----
  callModule(mod_add_table_server, "add_table_cont", continent_data_filtered, maxrowsperpage = 10)



}
