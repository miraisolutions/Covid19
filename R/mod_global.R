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
      column(3, valueBoxOutput(ns("confirmed"))),
      column(3,valueBoxOutput(ns("death"))),
      column(3,valueBoxOutput(ns("recovered"))),
      column(3,valueBoxOutput(ns("active")))
    ),
    fluidRow(
      column(6,
             div(h3("Global Covid-19 time evolution"), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
             plotlyOutput(ns("global_line_plot"))
             ),
      column(6,
             div(h3("Confirmed cases for top 10 countries - log scale"), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
             plotOutput(ns("top_n_line_plot"))
             )
    ),
    div(DTOutput(ns("dt_top10")), style = "margin-left: 20px;margin-right: 20px;")
  )
}

#' global Server Function
#'
#' @param orig_data reactive data.frame
#'
#' @importFrom shinydashboard valueBox
#' @importFrom shinydashboard renderValueBox
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr starts_with
#' @importFrom tidyr ends_with
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
             color = "light-blue",
             width = 3)
  })

  # plots ----
  output$global_line_plot <- renderPlotly({
    df <- global() %>%
      select(- starts_with("new_")) %>%
      select( - confirmed) %>%
      pivot_longer(cols = -date, names_to = "status", values_to = "value") %>%
      mutate(status = as.factor(status)) %>%
      capitalize_names_df()

    df %>% time_evol_area_plot() %>% ggplotly() #%>% add_log_scale() %>% ggplotly()
  })

  output$top_n_line_plot <- renderPlot({
    df <- world_top_5_confirmed() %>%
      mutate(status = as.factor(Country.Region)) %>%
      mutate(value = confirmed) %>%
      capitalize_names_df()

    df %>% time_evol_line_plot() %>% add_log_scale() #%>% add_log_scale() %>% ggplotly()
  })

  # tables ----
  output$dt_top10 <- renderDT(
    datatable(world(),
              rownames = FALSE,
              selection = "single",
              #filter = 'bottom',
              escape = FALSE,
              plugins = 'natural',
              options = getTableOptions())
  )

}


