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
    fluidRow(
      column(6,
             div(h3("Global Covid-19 time evolution"), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
             mod_plot_log_linear_ui(ns("plot_log_area_global"))
             ),
      column(6,
             div(h3("Confirmed cases for top 5 countries"), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
             mod_plot_log_linear_ui(ns("plot_log_linear_top_n"))
             )
    ),
    mod_add_table_ui(ns("add_table_world"))
  )
}

#' global Server Function
#'
#' @param orig_data reactive data.frame
#'
#' @import dplyr
#' @import tidyr
#' @importFrom DT renderDT
#' @importFrom DT datatable
#' @importFrom plotly renderPlotly
#' @importFrom plotly ggplotly
#'
#' @noRd
mod_global_server <- function(input, output, session, orig_data){
  ns <- session$ns

  # Datasets ----

  global <- reactive({
    orig_data() %>%
      get_timeseries_global_data() %>%
      select(-ends_with("rate"))
  })

  global_today <- reactive({
    global() %>%
      filter(date == max(date))
  })

  world <- reactive({
    orig_data() %>%
      aggregate_country_data()
  })

  world_top_5 <- reactive({
    world() %>%
      head(5)
  })

  world_top_5_confirmed <- reactive({
    orig_data() %>%
      aggregate_province_timeseries_data() %>%
      filter(Country.Region %in% world_top_5()$Country.Region) %>%
      select(Country.Region, date, confirmed)
  })

  # Boxes ----
  callModule(mod_caseBoxes_server, "count-boxes", global_today)

  # map ----

  callModule(mod_map_server, "map_ui", world)

  # plots ----

  levs <- reactive(
    rev(sort_type_by_max(global_today()))
  )

  df_global <- reactive({
    global() %>%
      select(-starts_with("new_")) %>%
      select( -confirmed) %>%
      pivot_longer(cols = -date, names_to = "status", values_to = "value") %>%
      mutate(status = factor(status, levels = levs())) %>%
      capitalize_names_df()
  })

  callModule(mod_plot_log_linear_server, "plot_log_area_global", df = df_global, type = "area")

  df_top_n <- reactive({
    world_top_5_confirmed() %>%
      mutate(status = as.factor(Country.Region)) %>%
      mutate(value = confirmed) %>%
      capitalize_names_df()
  })

  callModule(mod_plot_log_linear_server, "plot_log_linear_top_n", df = df_top_n, type = "line")

  # tables ----
  callModule(mod_add_table_server, "add_table_world", world)

}


