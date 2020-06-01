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
    div(
      #selectInput(label = "Countries", inputId = ns("select_countries"), choices = NULL, selected = NULL, multiple = TRUE),
      textOutput(ns("from_nth_case"))
    ),
    #withSpinner(uiOutput(ns("barplots"))),
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
#' @param orig_data_aggregate reactive data.frame
#' @param data_filtered reactive data.frame
#' @param countries reactive data.frame
#' @param n min number of cases for a country to be considered. Default 1000
#' @param w number of days of outbreak. Default 7
#'
#' @import dplyr
#'
#' @noRd
mod_continent_comparison_server <- function(input, output, session, orig_data_aggregate, data_filtered, n = 1000, w = 7, pop_data){
  ns <- session$ns

  statuses <- c("confirmed", "deaths", "recovered", "active")
  # select all variables
  allstatuses = c(statuses, paste0("new_", statuses))

  continents = reactive({unique(orig_data_aggregate()$continent)})

  continent_pop_data = pop_data %>% filter(!is.na(continent)) %>%
    group_by(continent) %>%
    summarize(population = sum(as.numeric(population), rm.na = T))


  prepdata = function(data, group, time) {
    continent_data =    data %>%
      select(Country.Region, population, contagion_day, date, continent, date, !!allstatuses) %>%
      mutate(population = as.numeric(population)) %>%
      group_by_(.dots = time, group) %>%
      #group_by(time, continent) %>%
      summarise_at(c(allstatuses), sum, na.rm = TRUE) %>%
      add_growth_death_rate(group, time) %>%
      left_join(continent_pop_data, by = group) %>%
      mutate(mortality_rate_1M_pop = round(10^6*deaths/population, digits = 3),
             prevalence_rate_1M_pop = round(10^6*confirmed/population, digits = 3),
             new_prevalence_rate_1M_pop = round(10^6*new_confirmed/population, digits = 3)) %>%
      rename(Country.Region = continent) %>%
      get_timeseries_by_contagion_day_data()  %>%
      arrange(desc(date))
    continent_data
  }
  continent_data <- reactive({prepdata(orig_data_aggregate(), "continent", "date")})

  continent_data_filtered <- reactive({prepdata(data_filtered(), "continent", "date")})


  output$lineplots_cont <- renderUI({
    tagList(
      h2("Continents Comparison"),
      mod_lineplots_day_contagion_ui(ns("lineplots_day_contagion_cont"))
    )
  })

  callModule(mod_lineplots_day_contagion_server, "lineplots_day_contagion_cont", continent_data_filtered)

  # Rate plots ----
  output$rateplots_cont <- renderUI({
    mod_growth_death_rate_ui(ns("rate_plots_cont"), n_highligth = length(continents()))
  })

  callModule(mod_growth_death_rate_server, "rate_plots_cont", continent_data_filtered, n = n, n_highligth = length(continents()), istop = F)

  # Line with bullet plot

  output$lines_points_plots_cont <- renderUI({
    mod_compare_nth_cases_plot_ui(ns("lines_points_plots_cont"))
  })

  callModule(mod_compare_nth_cases_plot_server, "lines_points_plots_cont", continent_data_filtered, n = n, n_highligth = length(continents()), istop = F)

  # scatterplot
  output$scatterplot_plots_cont <- renderUI({
    mod_scatterplot_ui(ns("scatterplot_plots_cont"))
  })

  callModule(mod_scatterplot_server, "scatterplot_plots_cont", continent_data_filtered, n = n, n_highligth = length(continents()), istop = F, countries = continents)


  output$status_stackedbarplot_cont <- renderUI({
    mod_stackedbarplot_ui(ns("status_stackedbarplot_cont"))
  })
  callModule(mod_stackedbarplot_status_server, "status_stackedbarplot_cont", continent_data_filtered, n = n, n_highligth = length(continents()), istop = F)

  # tables ----
  callModule(mod_add_table_server, "add_table_cont", continent_data_filtered, maxrowsperpage = 10)

#   observeEvent(input$select_countries,{
#     if (input$select_countries != "") {
#       # Data ----
#       all_countries_data <- reactive({data_filtered() %>%
#           filter(contagion_day > 0) %>%
#           arrange(desc(date))
#       })
#       countries_data <- reactive({all_countries_data() %>%
#           filter(Country.Region %in% input$select_countries) %>%
#           arrange(desc(date))
#       })
#
#
#       output$from_nth_case <- renderText({
#         paste0("Only countries with more than ", n, " confirmed cases, and outbreaks longer than ", w, " days considered. Contagion day 0 is the first day with more than ", n ," cases.")
#       })
#
#       # Bar plots ----
#
#       output$barplots <- renderUI({
#         lapply(input$select_countries, function(country){
#           tagList(
#             h2(country),
#             mod_bar_plot_day_contagion_ui(ns(paste0(country,"_bar_plot_day_contagion")))
#           )
#         })
#       })
#
#       lapply(input$select_countries, function(country){
#         country_data <- reactive({countries_data() %>%
#             filter(Country.Region %in% country)})
#         callModule(mod_bar_plot_day_contagion_server, paste0(country,"_bar_plot_day_contagion"), country_data)
#       })
#     }
#
#     # Line plots ----
#     output$lineplots <- renderUI({
#       tagList(
#         h2("Countries Comparison"),
#         mod_lineplots_day_contagion_ui(ns("lineplots_day_contagion"))
#       )
#     })
#
#     callModule(mod_lineplots_day_contagion_server, "lineplots_day_contagion", countries_data)
#
#     # Rate plots ----
#     output$rateplots <- renderUI({
#       mod_growth_death_rate_ui(ns("rate_plots"), n_highligth = length(input$select_countries))
#     })
#
#     callModule(mod_growth_death_rate_server, "rate_plots", countries_data, n = n, n_highligth = length(input$select_countries), istop = F)
#
#     # Line with bullet plot
#
#     output$lines_points_plots <- renderUI({
#       mod_compare_nth_cases_plot_ui(ns("lines_points_plots"))
#     })
#
#     callModule(mod_compare_nth_cases_plot_server, "lines_points_plots", countries_data, n = n, n_highligth = length(input$select_countries), istop = F)
#
#     inputcountries = reactive({input$select_countries}) # pass countries to plot below
#     output$scatterplot_plots <- renderUI({
#       mod_scatterplot_ui(ns("scatterplot_plots"))
#     })
#
#     callModule(mod_scatterplot_server, "scatterplot_plots", all_countries_data, n = n, n_highligth = length(input$select_countries), istop = F, countries = inputcountries)
#
#     output$status_stackedbarplot <- renderUI({
#       mod_stackedbarplot_ui(ns("status_stackedbarplot"))
#     })
#     callModule(mod_stackedbarplot_status_server, "status_stackedbarplot", countries_data, n = n, n_highligth = length(input$select_countries), istop = F)
#
#     # tables ----
#     callModule(mod_add_table_server, "add_table_countries", countries_data, maxrowsperpage = 10)
#   })

}
