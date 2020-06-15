#' country UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom shinycssloaders withSpinner
#' @importFrom plotly plotlyOutput
mod_country_ui <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(label = "Country", inputId = ns("select_country"), choices = NULL, selected = NULL),

    tags$head(tags$style(HTML(".small-box {width: 300px; margin: 20px;}"))),
    mod_caseBoxes_ui(ns("count-boxes")),

    fluidRow(
      withSpinner(uiOutput(ns("barplots")))
    ),
    hr(),
    fluidRow(
      column(6,
             br(),
             div( h4("Total cases"), align = "center",
             div(style = "visibility: hidden", radioButtons("dummy1", "", choices = "dummy")),
             withSpinner(mod_plot_log_linear_ui(ns("plot_log_linear_tot")))
             )
      ),
      column(6,
             withSpinner(mod_compare_nth_cases_plot_ui(ns("lines_points_plots")))
      )
    ),
    mod_add_table_ui(ns("add_table_country"))
  )
}

#' country Server Function
#'
#' @param orig_data_aggregate reactive data.frame
#' @param data_filtered reactive data.frame
#' @param countries reactive data.frame
#' @param n min number of cases for a country to be considered. Default 1000
#' @param w number of days of outbreak. Default 7
#'
#' @import dplyr
#' @import tidyr
#' @importFrom plotly renderPlotly
#'
#' @noRd
mod_country_server <- function(input, output, session, orig_data_aggregate, data_filtered, countries, n = 1000, w = 7){
  ns <- session$ns

  observe(
    updateSelectInput(session, "select_country", choices = sort(countries()$Country.Region), selected = "Switzerland")
  )

  observeEvent(input$select_country, {

    # Data ----
    country_data <- reactive({data_filtered() %>%
        filter(Country.Region %in% input$select_country) %>%
        filter(contagion_day > 0) %>%
        arrange(desc(date))
    })

    country_data_today <- reactive({
      country_data() %>%
        filter(date == max(date))
    })

    # Boxes ----
    callModule(mod_caseBoxes_server, "count-boxes", country_data_today)

    # tables ----
    callModule(mod_add_table_server, "add_table_country", country_data,  maxrowsperpage = 10)

    # plots ----

    levs <- sort_type_hardcoded()

    global <- reactive({
      country_data() %>%
        get_timeseries_global_data()
    })
    df_tot = reactive({
      tsdata_areplot(global(),levs, 1000) # start from day with >1000
    })
    callModule(mod_plot_log_linear_server, "plot_log_linear_tot", df = df_tot, type = "area")


    output$barplots <- renderUI({
      mod_bar_plot_day_contagion_ui(ns("bar_plot_day_contagion"))
    })

    callModule(mod_compare_nth_cases_plot_server, "lines_points_plots", country_data, n = n,istop = F)

    callModule(mod_bar_plot_day_contagion_server, "bar_plot_day_contagion", country_data)

  })



}



