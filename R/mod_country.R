#' country UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom plotly plotlyOutput
mod_country_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(label = "Country", inputId = ns("select_country"), choices = NULL, selected = NULL),

    fluidRow(
      column(6,
             column(6,
                    div(h3("Confirmed"), align = "center", style = "red"),
                    plotlyOutput(ns("bar_confirmed"), height = "300px")
             ),
             column(6,
                    div(h3("Active"), align = "center", style = "orange"),
                    plotlyOutput(ns("bar_active"), height = "300px")
             ),
             column(6,
                    div(h3("Deaths"), align = "center", style = "black"),
                    plotlyOutput(ns("bar_deaths"), height = "300px")
             ),
             column(6,
                    div(h3("Recovered"), align = "center", style = "green"),
                    plotlyOutput(ns("bar_recovered"), height = "300px")
             )
      ),
      column(6,
             div(h3("Covid-19 time evolution"), align = "center", style = "green"),
             plotlyOutput(ns("line_plot"), height = "300px"),
             div(DTOutput(ns("dt_country")), style = "margin: 50px;")
      )
    )
  )
}

#' country Server Function
#'
#' @param orig_data reactive data.frame
#'
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @importFrom dplyr distinct
#' @importFrom dplyr mutate
#' @importFrom dplyr arrange
#' @importFrom dplyr desc
#' @importFrom tidyr pivot_longer
#' @importFrom tidyr starts_with
#' @importFrom plotly renderPlotly
#'
#' @noRd
mod_country_server <- function(input, output, session, orig_data){
  ns <- session$ns

  countries <- reactive({
    orig_data() %>%
      select(Country.Region) %>%
      distinct()
  })

  observe(
    updateSelectInput(session, "select_country", choices = countries()$Country.Region, selected = "Switzerland")
  )

  observeEvent(input$select_country, {

    # Data ----
    country_data <- reactive({orig_data() %>%
        aggregate_province_timeseries_data() %>%
        filter(Country.Region == input$select_country) %>%
        arrange(desc(date))
    })

    confirmed_data <- reactive({
      country_data() %>%
        select(Country.Region, contagion_day, confirmed) %>%
        mutate(status = as.factor(Country.Region)) %>%
        mutate(value = confirmed) %>%
        capitalize_names_df()
    })

    deaths_data <- reactive({
      country_data() %>%
        select(Country.Region, contagion_day, deaths) %>%
        mutate(status = as.factor(Country.Region)) %>%
        mutate(value = deaths) %>%
        capitalize_names_df()
    })

    active_data <- reactive({
      country_data() %>%
        select(Country.Region, contagion_day, active) %>%
        mutate(status = as.factor(Country.Region)) %>%
        mutate(value = active) %>%
        capitalize_names_df()
    })

    recovered_data <- reactive({
      country_data() %>%
        select(Country.Region, contagion_day, recovered) %>%
        mutate(status = as.factor(Country.Region)) %>%
        mutate(value = recovered) %>%
        capitalize_names_df()
    })

    # tables ----
    output$dt_country <- renderDT(
      datatable(country_data(),
                rownames = FALSE,
                selection = "single",
                # filter = 'bottom',
                escape = FALSE,
                plugins = 'natural',
                options = getTableOptions(maxrowsperpage = 5))
    )

    # plots ----

    output$line_plot <- renderPlotly({
      df <- country_data() %>%
        select(-Country.Region, -contagion_day) %>%
        pivot_longer(cols = -date, names_to = "status", values_to = "value") %>%
        mutate(status = as.factor(status)) %>%
        capitalize_names_df()

      df %>% time_evol_line_plot() %>% add_log_scale() %>% ggplotly()
    })

    output$bar_confirmed <- renderPlotly({
      confirmed_data() %>% from_contagion_day_bar_plot() %>% ggplotly()
    })
    output$bar_deaths <- renderPlotly({
      deaths_data() %>% from_contagion_day_bar_plot() %>% ggplotly()
    })
    output$bar_active <- renderPlotly({
      active_data() %>% from_contagion_day_bar_plot() %>% ggplotly()
    })
    output$bar_recovered <- renderPlotly({
      recovered_data() %>% from_contagion_day_bar_plot() %>% ggplotly()
    })
  })



}



