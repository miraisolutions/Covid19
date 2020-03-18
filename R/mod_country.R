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

    tags$head(tags$style(HTML(".small-box {width: 300px; margin: 20px;}"))),
    fluidRow(
      column(3, valueBoxOutput(ns("confirmed"))),
      column(3,valueBoxOutput(ns("death"))),
      column(3,valueBoxOutput(ns("recovered"))),
      column(3,valueBoxOutput(ns("active")))
    ),

    fluidRow(
      uiOutput(ns("barplots"))
    ),
    fluidRow(
      column(6,
             div(h3("Total Cases - log scale"), align = "center",
                 plotOutput(ns("line_plot"), height = "400px")
             )
      ),
      column(6,
             mod_add_table_ui(ns("add_table_country"))
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
#' @importFrom tidyr ends_with
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
        filter(Country.Region %in% input$select_country) %>%
        filter(contagion_day > 0) %>%
        select(-ends_with("rate")) %>%
        arrange(desc(date))
    })

    country_data_today <- reactive({
      country_data() %>%
        filter(date == max(date))
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

    confirmed_data_new <- reactive({
      country_data() %>%
        select(Country.Region, contagion_day, new_confirmed) %>%
        mutate(status = as.factor(Country.Region)) %>%
        mutate(value = new_confirmed) %>%
        capitalize_names_df()
    })

    deaths_data_new <- reactive({
      country_data() %>%
        select(Country.Region, contagion_day, new_deaths) %>%
        mutate(status = as.factor(Country.Region)) %>%
        mutate(value = new_deaths) %>%
        capitalize_names_df()
    })

    active_data_new <- reactive({
      country_data() %>%
        select(Country.Region, contagion_day, new_active) %>%
        mutate(status = as.factor(Country.Region)) %>%
        mutate(value = new_active) %>%
        capitalize_names_df()
    })

    recovered_data_new <- reactive({
      country_data() %>%
        select(Country.Region, contagion_day, new_recovered) %>%
        mutate(status = as.factor(Country.Region)) %>%
        mutate(value = new_recovered) %>%
        capitalize_names_df()
    })

    # Boxes ----
    output$confirmed <- renderValueBox({
      valueBox("Confirmed",
               country_data_today()$confirmed,
               color = "red",
               width = 3)
    })
    output$death <- renderValueBox({
      valueBox("Deaths",
               country_data_today()$deaths,
               color = "black",
               width = 3)
    })
    output$recovered <- renderValueBox({
      valueBox("Recovered",
               country_data_today()$recovered,
               color = "green",
               width = 3)
    })
    output$active <- renderValueBox({
      valueBox("Active",
               country_data_today()$active,
               color = "light-blue",
               width = 3)
    })

    # tables ----
    callModule(mod_add_table_server, "add_table_country", country_data)

    # plots ----

    output$line_plot <- renderPlot({
      df <- country_data() %>%
        select(-Country.Region, -contagion_day) %>%
        select(-starts_with("new")) %>%
        pivot_longer(cols = -date, names_to = "status", values_to = "value") %>%
        mutate(status = as.factor(status)) %>%
        capitalize_names_df()

      df %>% time_evol_area_plot() %>% add_log_scale() %>% fix_colors()
    })

    output$barplots <- renderUI({
          mod_bar_plot_day_contagion_ui(ns("bar_plot_day_contagion"))

    })

    callModule(mod_bar_plot_day_contagion_server, "bar_plot_day_contagion", country_data)

  })



}



