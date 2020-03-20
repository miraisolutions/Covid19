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

    mod_caseBoxes_ui(ns("count-boxes")),

    fluidRow(
      uiOutput(ns("barplots"))
    ),
    fluidRow(
      column(6,
             # div(h3("Total Cases"), align = "center",
                 mod_plot_log_linear_ui(ns("plot_log_linear_tot"))
             # )
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

    # Boxes ----
    callModule(mod_caseBoxes_server, "count-boxes", country_data_today)

    # tables ----
    callModule(mod_add_table_server, "add_table_country", country_data)

    # plots ----

    df_tot <- reactive({
      country_data() %>%
        select(-Country.Region, -contagion_day) %>%
        select(-starts_with("new"), -confirmed) %>%
        pivot_longer(cols = -date, names_to = "status", values_to = "value") %>%
        mutate(status = factor(status, levels = c("active", "recovered", "deaths"))) %>%
        capitalize_names_df()
    })

    callModule(mod_plot_log_linear_server, "plot_log_linear_tot", df = df_tot, type = "area")


    output$barplots <- renderUI({
      mod_bar_plot_day_contagion_ui(ns("bar_plot_day_contagion"))
    })

    callModule(mod_bar_plot_day_contagion_server, "bar_plot_day_contagion", country_data)

  })



}



