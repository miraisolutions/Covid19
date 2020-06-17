#' global UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom DT DTOutput
#' @importFrom plotly plotlyOutput
mod_global_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$head(tags$style(HTML(".small-box {width: 300px; margin: 20px;}"))),
    mod_caseBoxes_ui(ns("count-boxes")),
    fluidRow(
      mod_map_ui(ns("map_ui"))
    ),
    hr(),
    fluidRow(
      column(6,
             div(h4("Global Covid-19 time evolution"), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
             mod_plot_log_linear_ui(ns("plot_log_area_global"))
      ),
      column(6,
             div(h4("Confirmed cases for top 5 countries"), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
             mod_plot_log_linear_ui(ns("plot_log_linear_top_n"))
      )
    ),
    hr(),
    fluidRow(
      column(6,
             mod_compare_nth_cases_plot_ui(ns("plot_compare_nth"))
      ),
      column(6,
             mod_growth_death_rate_ui(ns("plot_growth_death_rate"))
      )
    ),
    hr(),
    fluidRow(
      column(6,
             mod_scatterplot_ui(ns("plot_scatterplot_glob"))
             ),
      column(6,
             mod_stackedbarplot_ui(ns("plot_stackedbarplot_status"))
      )
    ),
    hr(),
    mod_add_table_ui(ns("add_table_world"))
  )
}

#' global Server Function
#'
#' @param orig_data reactive data.frame
#' @param orig_data_aggregate reactive data.frame
#' @param data_filtered reactive data.frame from contagion day n
#' @param countries_data data.frame sp for mapping
#'
#' @import dplyr
#' @import tidyr
#' @importFrom DT renderDT
#' @importFrom DT datatable
#' @importFrom plotly renderPlotly
#' @importFrom plotly ggplotly
#'
#' @noRd
mod_global_server <- function(input, output, session, orig_data, orig_data_aggregate, data_filtered, countries_data_map){
  ns <- session$ns

  # Datasets ----

  global <- reactive({
    global <- orig_data() %>%
      get_timeseries_global_data()
    global
  })

  global_today <- reactive({
    global() %>%
      filter(date == max(date))
  })

  orig_data_aggregate_today <- reactive({
    orig_data_aggregate() %>%
      filter( date == max(date))
  })

  world <- reactive({
    orig_data_aggregate_today() %>%
      arrange(desc(confirmed) )
  })

  world_top_5_today <- reactive({
    world() %>%
      head(5)
  })

  world_top_5_confirmed <- reactive({
    orig_data_aggregate() %>%
      filter(Country.Region %in% world_top_5_today()$Country.Region) %>%
      select(Country.Region, date, confirmed)
  })
  # Boxes ----
  callModule(mod_caseBoxes_server, "count-boxes", global_today)

  # map ----

  callModule(mod_map_server, "map_ui", orig_data_aggregate, countries_data_map)

  # plots ----
  levs <- sort_type_hardcoded()

  df_global = reactive({
    tsdata_areplot(global(),levs, 1000) # start from day qith |1000
  })


  callModule(mod_plot_log_linear_server, "plot_log_area_global", df = df_global, type = "area")

  # > line plot top 5
  df_top_n <- reactive({
    world_top_5_confirmed() %>%
      mutate(status = as.factor(Country.Region)) %>%
      mutate(value = confirmed) %>%
      capitalize_names_df()
  })

  callModule(mod_plot_log_linear_server, "plot_log_linear_top_n", df = df_top_n, type = "line")

  # > comparison plot from day of nth contagion

  callModule(mod_compare_nth_cases_plot_server, "plot_compare_nth", data_filtered)

  # > growth_death_rate
  callModule(mod_growth_death_rate_server, "plot_growth_death_rate", orig_data_aggregate)

  # > scatterplot prevalence vs growth
  callModule(mod_scatterplot_server, "plot_scatterplot_glob", orig_data_aggregate)

  # > stacked barplot with status split
  callModule(mod_stackedbarplot_status_server, "plot_stackedbarplot_status", orig_data_aggregate)

  # tables ----
  callModule(mod_add_table_server, "add_table_world", world)

}


