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
      column(3,
             div(h3("Tot Confirmed Cases from 1st day of Contagion"), align = "center"),
             plotlyOutput(ns("bar_confirmed"), height = "300px")
      ),
      column(3,
             div(h3("Tot Active from 1st day of Contagion"), align = "center"),
             plotlyOutput(ns("bar_active"), height = "300px")
      ),
      column(3,
             div(h3("Tot Deaths from 1st day of Contagion"), align = "center"),
             plotlyOutput(ns("bar_deaths"), height = "300px")
      ),
      column(3,
             div(h3("Tot Recovered from 1st day of Contagion"), align = "center"),
             plotlyOutput(ns("bar_recovered"), height = "300px")
      ),
      column(3,
             div(h3("New Confirmed Cases from 1st day of Contagion"), align = "center"),
             plotlyOutput(ns("bar_confirmed_new"), height = "300px")
      ),
      column(3,
             div(h3("New Active from 1st day of Contagion"), align = "center"),
             plotlyOutput(ns("bar_active_new"), height = "300px")
      ),
      column(3,
             div(h3("New Deaths from 1st day of Contagion"), align = "center"),
             plotlyOutput(ns("bar_deaths_new"), height = "300px")
      ),
      column(3,
             div(h3("New Recovered from 1st day of Contagion"), align = "center"),
             plotlyOutput(ns("bar_recovered_new"), height = "300px")
      )
    ),
    fluidRow(
      column(3,
             div(h3("Total Cases - log scale"), align = "center",
                 plotOutput(ns("line_plot"), height = "400px")
             )
      ),
      column(3,
             div(h3("New Cases"), align = "center",
                 plotOutput(ns("line_plot_new"), height = "400px")
             )
      ),
      column(6,
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
        filter(Country.Region == input$select_country) %>%
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

    output$line_plot <- renderPlot({
      df <- country_data() %>%
        select(-Country.Region, -contagion_day) %>%
        select(-starts_with("new")) %>%
        pivot_longer(cols = -date, names_to = "status", values_to = "value") %>%
        mutate(status = as.factor(status)) %>%
        capitalize_names_df()

      df %>% time_evol_area_plot() %>% add_log_scale() %>% fix_colors()
    })

    output$line_plot_new <- renderPlot({
      df <- country_data() %>%
        select(-Country.Region, -contagion_day) %>%
        select(date,starts_with("new_")) %>%
        pivot_longer(cols = -date, names_to = "status", values_to = "value") %>%
        mutate(status = as.factor(status)) %>%
        capitalize_names_df()

      df %>% time_evol_area_plot() %>% fix_colors()
    })

    output$bar_confirmed <- renderPlotly({
      confirmed_data() %>% from_contagion_day_bar_plot() %>% remove_legend() %>% ggplotly()
    })
    output$bar_deaths <- renderPlotly({
      deaths_data() %>% from_contagion_day_bar_plot() %>% remove_legend() %>% ggplotly()
    })
    output$bar_active <- renderPlotly({
      active_data() %>% from_contagion_day_bar_plot() %>% remove_legend() %>% ggplotly()
    })
    output$bar_recovered <- renderPlotly({
      recovered_data() %>% from_contagion_day_bar_plot() %>% remove_legend() %>% ggplotly()
    })
    output$bar_confirmed_new <- renderPlotly({
      confirmed_data_new() %>% from_contagion_day_bar_plot() %>% remove_legend() %>% ggplotly()
    })
    output$bar_deaths_new <- renderPlotly({
      deaths_data_new() %>% from_contagion_day_bar_plot() %>% remove_legend() %>% ggplotly()
    })
    output$bar_active_new <- renderPlotly({
      active_data_new() %>% from_contagion_day_bar_plot() %>% remove_legend() %>% ggplotly()
    })
    output$bar_recovered_new <- renderPlotly({
      recovered_data_new() %>% from_contagion_day_bar_plot() %>% remove_legend() %>% ggplotly()
    })

  })



}



