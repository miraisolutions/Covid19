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
             mod_plot_log_linear_ui(ns("plot_area_global"))
      ),
      column(6,
             div(h4("Confirmed cases for top 5 countries"), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
             mod_plot_log_linear_ui(ns("plot_log_linear_top_n"), area = FALSE)
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
#' @param orig_data data.frame
#' @param orig_data_aggregate data.frame
#' @param data_filtered data.frame from contagion day n
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

  # global_today <-
  #   orig_data %>%
  #     filter(date == max(date))

  # world time series
   total <-
     orig_data %>%
      get_timeseries_global_data()

   total_today <-
     total %>%
        filter(date == max(date))

  # countries today
  orig_data_aggregate_today <-
    orig_data_aggregate %>%
      filter( date == max(date))

  world <-
    orig_data_aggregate_today %>%
      arrange(desc(confirmed) )
  # todp 5 countries today by confirmed
  world_top_5_today <-
    world %>%
      head(5)

  world_top_5_confirmed <-
    orig_data_aggregate %>%
      filter(Country.Region %in% world_top_5_today$Country.Region) %>%
      select(Country.Region, date, confirmed)

  # Boxes ----
  callModule(mod_caseBoxes_server, "count-boxes", total_today)

  # map ----

  callModule(mod_map_server, "map_ui", orig_data_aggregate, countries_data_map)

  # plots ----
  levs <- sort_type_hardcoded()

  n = 1000 # define areaplot start
  df_global =
    tsdata_areplot(total,levs, n) # start from day with >1000

  callModule(mod_plot_log_linear_server, "plot_area_global", df = df_global, type = "area")

  # > line plot top 5
  #df_top_n <-
    # create factors with first top confirmed
  countries_order =  world_top_5_confirmed %>% filter(date == max(date)) %>%
    arrange(desc(confirmed)) %>%
     .[,"Country.Region"] %>% as.vector()

  # start lineplot from a later date
  mindate = min(world_top_5_confirmed$date[world_top_5_confirmed$confirmed>n])

  df_top_n = world_top_5_confirmed %>% filter(date > mindate) %>% # take only starting point where greater than 1000
    mutate(status = factor(Country.Region, levels = countries_order[, "Country.Region", drop = T])) %>%
    mutate(value = confirmed) %>%
    capitalize_names_df()
  # lineplot of top 5 countries confirmed cases with date x axis
  callModule(mod_plot_log_linear_server, "plot_log_linear_top_n", df = df_top_n, type = "line")

  # > comparison plot from day of nth contagion

  callModule(mod_compare_nth_cases_plot_server, "plot_compare_nth", data_filtered)

  # > growth_death_rate
  callModule(mod_growth_death_rate_server, "plot_growth_death_rate", orig_data_aggregate)

  # > scatterplot prevalence vs growth, nmed = 10000 by default
  callModule(mod_scatterplot_server, "plot_scatterplot_glob", orig_data_aggregate_today)

  # > stacked barplot with status split
  callModule(mod_stackedbarplot_status_server, "plot_stackedbarplot_status", orig_data_aggregate)

  # tables ----
  callModule(mod_add_table_server, "add_table_world", world)

}


