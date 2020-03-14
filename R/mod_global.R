#' global UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinydashboard valueBoxOutput
#' @importFrom DT DTOutput
#' @importFrom plotly plotlyOutput
mod_global_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$head(tags$style(HTML(".small-box {width: 300px; margin: 20px;}"))),
    fluidRow(
      column(3,valueBoxOutput(ns("confirmed"))),
      column(3,valueBoxOutput(ns("death"))),
      column(3,valueBoxOutput(ns("recovered"))),
      column(3,valueBoxOutput(ns("active")))
    ),
    fluidRow(
      div(DTOutput(ns("dt_global")), style = "margin: 50px;")
    ),
    fluidRow(
      column(4,
             div(h3("Global Covid-19 time evolution - log scale"), align = "center"),
             plotlyOutput(ns("global_line_plot"))),
      column(4,
             div(h3("Confirmed cases for top 10 countries - log scale"), align = "center"),
             plotlyOutput(ns("top_n_line_plot"))),
      column(4,
             div(DTOutput(ns("dt_top10")), style = "margin: 20px;"))
    )
  )
}

#' global Server Function
#'
#' @param orig_data reactive data.frame
#'
#' @importFrom shinydashboard valueBox
#' @importFrom shinydashboard renderValueBox
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr starts_with
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
      get_timeseries_global_data()
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
  output$confirmed <- renderValueBox({
    valueBox("Confirmed",
             global_today()$confirmed,
             color = "red",
             width = 3)
  })
  output$death <- renderValueBox({
    valueBox("Deaths",
             global_today()$deaths,
             color = "black",
             width = 3)
  })
  output$recovered <- renderValueBox({
    valueBox("Recovered",
             global_today()$recovered,
             color = "green",
             width = 3)
  })
  output$active <- renderValueBox({
    valueBox("Active",
             global_today()$active,
             color = "orange",
             width = 3)
  })

  # plots ----
  output$global_line_plot <- renderPlotly({
    df <- global() %>%
      pivot_longer(cols = -date, names_to = "status", values_to = "value") %>%
      mutate(status = as.factor(status)) %>%
      mutate(value = log10(value)) %>%
      capitalize_names_df()

    df %>% time_evol_line_plot() %>% add_log_scale() %>% ggplotly()
  })

  output$top_n_line_plot <- renderPlotly({
    df <- world_top_5_confirmed() %>%
      mutate(status = as.factor(Country.Region)) %>%
      mutate(value = confirmed) %>%
      mutate(value = log10(value)) %>%
      capitalize_names_df()

    df %>% time_evol_line_plot() %>% add_log_scale() %>% ggplotly()
  })

  # tables ----
  output$dt_global <- renderDT(
    datatable(global(),
              rownames = FALSE,
              selection = "single",
              filter = 'bottom',
              escape = FALSE,
              plugins = 'natural',
              options = getTableOptions())
  )

  output$dt_top10 <- renderDT(
    datatable(world(),
              rownames = FALSE,
              selection = "single",
              filter = 'bottom',
              escape = FALSE,
              plugins = 'natural',
              options = getTableOptions())
  )

}


