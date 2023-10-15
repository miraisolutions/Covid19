#' continent_comparison UI Function
#'
#' @description A shiny Module.
#'
#' @param id Internal parameters for {shiny}.
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
      column(12,
             withSpinner(mod_group_plot_ui(ns("cmp_confirmed_cont"), type = "confirmed"))
      )
    ),
    fluidRow(
      column(6,
             # withSpinner(uiOutput(ns("lines_points_plots_cont")))
             # )
             withSpinner(mod_compare_nth_cases_plot_ui(ns("lines_points_plots_cont"), istop = FALSE, nn = 1000, tests = FALSE, hosp = FALSE, selectvar = "new_confirmed", oneMpop = TRUE, vax = TRUE))
             #withSpinner(mod_compare_timeline_plot_ui(ns("lines_points_plots_cont"), titles = 1:2, tests = FALSE, hosp = FALSE, strindx = FALSE, nn = 1, oneMpop = TRUE, vax = TRUE))

      ),
      column(6,
             #withSpinner(uiOutput(ns("rateplots_cont")))
             withSpinner(mod_barplot_ui(ns("rate_plots_cont"), text = FALSE))
      )
    ),
    fluidRow(
      column(12,
             withSpinner(mod_group_plot_ui(ns("cmp_vax_cont"), type = "vaccines"))
      )
    ),
    mod_add_table_ui(ns("add_table_cont"))
  )
}

#' continent_comparison Server Function
#'
#' @param orig_data_aggregate data.frame
#' @param conts_data list of data.frames
#' @param nn min number of cases for a country to be considered. Default 1000
#' @param w number of days of outbreak. Default 7
#' @param pop_data data.frame
#'
#' @example ex-ex-continents.R
#'
#' @import dplyr
#'
#' @noRd
mod_continent_comparison_server <- function(input, output, session, orig_data_aggregate, conts_data, nn = 1000, w = 7, pop_data){
  ns <- session$ns

  # aggregate data to continent
  message("start mod_continent_comparison_server")
  # continent_data <- aggr_to_cont(orig_data_aggregate, "continent", "date")
  continent_data <- conts_data$continent_data

  continents = unique(continent_data$Country.Region)

  # create data for comparison with common starting point
  continent_data_filtered <- conts_data$continent_data_filtered

  continent_data_filtered_today <- conts_data$continent_data_filtered_today


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

  # callModule(mod_compare_timeline_plot_server, "lines_points_plots_cont", continent_data , istop = FALSE,
  #            tests = FALSE, hosp = FALSE, strindx = FALSE, nn = 1, oneMpop = FALSE, vax = TRUE)
  #

  # scatterplot

  #callModule(mod_scatterplot_server, "scatterplot_plots_cont", continent_data_filtered_today, nmed = nn, n_highlight = length(continents), istop = FALSE, countries = continents)

  callModule(mod_group_plot_server, "cmp_confirmed_cont", data_today = continent_data_filtered_today, type = "confirmed", istop = FALSE,
             scatterplotargs = list(countries = continents, nmed = nn),
             barplotargs = list(g_palette = list("plot_1" = graph_palette[1:length(continents)], calc = FALSE)),
             tests = FALSE

  )


  callModule(mod_group_plot_server, "cmp_vax_cont", data_today = continent_data_filtered_today, type = "vaccines", istop = FALSE,
             scatterplotargs = list(countries = continents, nmed = nn),
             barplotargs = list(pickvariable = list("plot_1" = "lm_confirmed_rate_1M_pop"),
                                g_palette = list("plot_1" = graph_palette[1:length(continents)], calc = FALSE),
                                sortbyvar = FALSE)
  )

  # tables ----
  callModule(mod_add_table_server, "add_table_cont", continent_data_filtered, maxrowsperpage = 10)

}
