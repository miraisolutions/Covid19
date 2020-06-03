#' continent_comparison UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param uicont character continent or region.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom shinycssloaders withSpinner
mod_continent_ui <- function(id, uicont){
  ns <- NS(id)
  tagList(
    tags$head(tags$style(HTML(".small-box {width: 300px; margin: 20px;}"))),
   # mod_caseBoxes_ui(ns(paste("count-boxes")),
    mod_caseBoxes_ui(ns(paste("count-boxes", uicont , sep = "_"))),
    hr(),
    div(
      #selectInput(label = "Countries", inputId = ns("select_countries"), choices = NULL, selected = NULL, multiple = TRUE),
      textOutput(ns(paste("from_nth_case", uicont , sep = "_")))
    ),
    #withSpinner(uiOutput(ns(paste("barplots", uicont , sep = "_")))),
    withSpinner(uiOutput(ns(paste("lineplots_cont", uicont , sep = "_")))),
    fluidRow(
      column(5,
             withSpinner(uiOutput(ns(paste("rateplots_cont", uicont , sep = "_"))))
      ),
      column(7,
             withSpinner(uiOutput(ns(paste("lines_points_plots_cont", uicont , sep = "_"))))
             )
    ),
    fluidRow(
      column(6,
             withSpinner(uiOutput(ns(paste("scatterplot_plots_cont", uicont , sep = "_"))))
      ),
      column(6,
             withSpinner(uiOutput(ns(paste("status_stackedbarplot_cont", uicont , sep = "_"))))
      )
    ),
    mod_add_table_ui(ns(paste("add_table_cont", uicont , sep = "_")))
    )
}

#' continent Server Function
#'
#' @param orig_data_aggregate reactive data.frame with data from 1 continent
#' @param data_filtered reactive data.frame
#' @param n min number of cases for a country to be considered. Default 1000
#' @param w number of days of outbreak. Default 7
#' @param pop_data data.frame population
#' @param cont character continent or subcontinent name
#' @param uicont character continent or subcontinent name of ui
#' @import dplyr
#'
#' @noRd
mod_continent_server <- function(input, output, session, orig_data_aggregate, data_filtered, n = 1000, w = 7, pop_data, cont, uicont){
  ns <- session$ns

  statuses <- c("confirmed", "deaths", "recovered", "active")
  # select all variables
  allstatuses = c(statuses, paste0("new_", statuses))

  orig_data_aggregate_cont <- reactive({
    orig_data_aggregate() %>% filter(continent == cont)})
  data_filtered_cont <- reactive({data_filtered() %>% filter(continent == cont)})

  subcontinents = reactive({unique(orig_data_aggregate_cont()$subcontinent)})

  continent_pop_data =  pop_data %>% filter(!is.na(continent) & continent %in% cont) %>%
    group_by(continent) %>%
    summarize(population = sum(population, rm.na = T))

  subcontinent_pop_data =  pop_data %>% filter(!is.na(continent) & continent %in% cont) %>%
    group_by(subcontinent) %>%
    summarize(population = sum(population, rm.na = T))

    continent_data <- reactive({aggr_to_cont(orig_data_aggregate_cont(), "continent", "date",
                                           continent_pop_data, allstatuses)})


  subcontinent_data <- reactive({aggr_to_cont(orig_data_aggregate_cont(), "subcontinent", "date",
                                              subcontinent_pop_data, allstatuses)})

  subcontinent_data_filtered <- reactive({aggr_to_cont(data_filtered_cont(), "subcontinent", "date",
                                                       subcontinent_pop_data, allstatuses)})

  continent_data_today <- reactive({
    continent_data() %>%
      filter(date == max(date))
  })

  # Boxes ----
  callModule(mod_caseBoxes_server, paste("count-boxes", uicont , sep = "_"), continent_data_today)

  output[[paste("from_nth_case", uicont , sep = "_")]]<- renderText({
    paste0("Only Areas with more than ", n, " confirmed cases, and outbreaks longer than ", w, " days considered. Contagion day 0 is the first day with more than ", n ," cases.")
  })

  output[[paste("lineplots_cont", uicont , sep = "_")]] <- renderUI({
    tagList(
      h2("Macro Area Comparison"),
      mod_lineplots_day_contagion_ui(ns("lineplots_day_contagion_cont"))
    )
  })

  callModule(mod_lineplots_day_contagion_server,
             "lineplots_day_contagion_cont",
             subcontinent_data_filtered)

  # Rate plots ----
  output[[paste("rateplots_cont", uicont , sep = "_")]] <- renderUI({
    mod_growth_death_rate_ui(ns("rate_plots_cont"), n_highligth = length(subcontinents()))
  })

  callModule(mod_growth_death_rate_server, "rate_plots_cont", subcontinent_data_filtered, n = n, n_highligth = length(subcontinents()), istop = F)

  # Line with bullet plot

  output[[paste("lines_points_plots_cont", uicont , sep = "_")]] <- renderUI({
    mod_compare_nth_cases_plot_ui(ns("lines_points_plots_cont"))
  })

  callModule(mod_compare_nth_cases_plot_server, "lines_points_plots_cont", subcontinent_data_filtered, n = n, n_highligth = length(subcontinents()), istop = F)

  # scatterplot
  output[[paste("scatterplot_plots_cont", uicont , sep = "_")]] <- renderUI({
    mod_scatterplot_ui(ns("scatterplot_plots_cont"))
  })

  callModule(mod_scatterplot_server, "scatterplot_plots_cont",
             subcontinent_data_filtered, n = n, n_highligth = length(subcontinents()),
             istop = F, countries = subcontinents)


  output[[paste("status_stackedbarplot_cont", uicont , sep = "_")]] <- renderUI({
    mod_stackedbarplot_ui(ns("status_stackedbarplot_cont"))
  })
  callModule(mod_stackedbarplot_status_server, "status_stackedbarplot_cont",
             subcontinent_data_filtered, n = n, n_highligth = length(subcontinents()), istop = F)

  # tables ----
  callModule(mod_add_table_server, paste("add_table_cont", uicont , sep = "_"),
             subcontinent_data_filtered, maxrowsperpage = 10)


}
