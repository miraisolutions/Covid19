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
             withSpinner(mod_compare_nth_cases_plot_ui(ns("lines_points_plots_tot")))
      )
    ),
    # hr(),
    # actionButton(ns("addarea"), "Add Regional plots"),
    hr(),
    #uiOutput(ns("subarea")),
    tags$div(id = "subarea"),
    mod_add_table_ui(ns("add_table_country"))
  )
}

areaUI = function(id){
  ns = NS(id)
  message("areaUI:", ns("plot_growth_death_rate_area2"))
    tagList(
      div(id = id,
           div(h4("Country split at level 2"), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
           hr(),
           fluidRow(
             #id = 'area_plots_1',
             column(6,
                    div(h4("Covid-19 time evolution"), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
                    mod_plot_log_linear_ui(ns("plot_log_area_area_2"))

             ),
             column(6,
                    div(h4("Confirmed cases for top 5 Areas"), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
                    mod_plot_log_linear_ui(ns("plot_log_linear_top_n_area2"))
             )
           ),
           hr(),
           fluidRow(
             #id = 'area_plots_2',

             column(6,
                    mod_compare_nth_cases_plot_ui(ns("plot_compare_nth_area2"), actives = FALSE)
             ),
             column(6,
                    mod_growth_death_rate_ui(ns("plot_growth_death_rate_area2"))
             )
           ),
           hr(),
           fluidRow(
             column(6,
                    mod_scatterplot_ui(ns("plot_scatterplot_area_2"))
             ),
             column(6,
                    mod_stackedbarplot_ui(ns("plot_stackedbarplot_status_area2"))
             )
           )
       )
  )
}
#' country Server Function
#'
#' @param data_filtered data.frame
#' @param countries reactive character vector
#' @param n min number of cases for a country to be considered. Default 1000
#' @param w number of days of outbreak. Default 7
#'
#' @import dplyr
#' @import tidyr
#' @importFrom plotly renderPlotly
#'
#' @noRd
mod_country_server <- function(input, output, session, data_filtered, countries, n = 1000, w = 7){
  ns <- session$ns
  observe(
    updateSelectInput(session, "select_country", choices = sort(countries()$Country.Region), selected = "Switzerland")
  )
  lev2id <- reactiveVal(0) # for removeUI
  moduleOuts = reactiveValues()

  observeEvent(input$select_country, {


    # Data ----
    country_data <-  data_filtered %>%
        filter(Country.Region %in% req(input$select_country)) %>%
        filter(contagion_day > 0) %>%
        arrange(desc(date))

    country_data_today <- country_data %>%
        filter(date == max(date))

    # Boxes ----
    callModule(mod_caseBoxes_server, "count-boxes", country_data_today)

    # tables ----
    callModule(mod_add_table_server, "add_table_country", country_data,  maxrowsperpage = 10)
    # plots ----
    levs <- sort_type_hardcoded()
    country_data_area = country_data
    if (sum(country_data$hosp)>0) {
      message("Adding hospitalised data for ", req(input$select_country))
      levs = c(levs, "hosp")
      country_data_area$active = country_data_area$active - country_data_area$hosp
    }

    df_tot = tsdata_areplot(country_data_area,levs, 1000) # start from day with >1000

    callModule(mod_plot_log_linear_server, "plot_log_linear_tot", df = df_tot, type = "area")


    output$barplots <- renderUI({
      mod_bar_plot_day_contagion_ui(ns("bar_plot_day_contagion"))
    })

    callModule(mod_compare_nth_cases_plot_server, "lines_points_plots_tot", country_data , n = n, w = w, istop = F)


    callModule(mod_bar_plot_day_contagion_server, "bar_plot_day_contagion", country_data)

  #  })
  # # ##### country split within areas #############################################

  #   # Data ----
    area_data_2 = get_datahub(country = req(input$select_country), stardate = "2020-01-15", lev = 2, verbose = FALSE)
    #area_data_2 = get_datahub(country = "UK", stardate = "2020-01-15", lev = 2, verbose = FALSE)
    if (!is.null(area_data_2)) {
      # insert UI components
      if (lev2id() == 0) {
        lev2id(lev2id()+1)
        message('Level 2 data present: insertUI for area plots ', lev2id())

        insertUI(paste0("#","subarea"),
                 'afterEnd',
                 ui = areaUI(ns(paste0("area",lev2id()))),#,
                 #ui = areaUI(paste0("area",lev2id())), # it works
                 session = session,
                  immediate = TRUE
                 )
      }

      n2 = n/10 # start from 100 cases

      area_data_2 = area_data_2 %>%
        get_timeseries_by_contagion_day_data()

      area_data_2_aggregate <-
        build_data_aggr(area_data_2)

      data_2_filtered <-
        area_data_2_aggregate %>%
        rescale_df_contagion(n = n2, w = w) # take where 100 confirmed

      # list of ares to be used in the UI
      areas <- reactive({
        data_2_filtered %>%
          select(Country.Region) %>%
          distinct() %>% .$Country.Region
      })

      area_2_today <-
        area_data_2 %>%
        filter(date == max(date))

      total_2 <-
        area_2_today %>%
        get_timeseries_global_data() # possibly not needed


      area_data_2_aggregate_today <-
        area_data_2_aggregate %>%
        filter( date == max(date))

      area_2 <-
        area_data_2_aggregate_today %>%
        arrange(desc(confirmed) )

      area_2_top_5_today <-
        area_2 %>%
        head(5)

      area_2_top_5_confirmed <-
        area_data_2_aggregate %>%
        filter(Country.Region %in% area_2_top_5_today$Country.Region) %>%
        select(Country.Region, date, confirmed)

      # plots ----
      levs <- sort_type_hardcoded()

      df_area_2 = tsdata_areplot(area_data_2,levs, n2) # start from day with |100

      callModule(mod_plot_log_linear_server, paste(paste0("area",lev2id()),"plot_log_area_area_2", sep = "-"), df = df_area_2, type = "area")


      # > line plot top 5
      #df_top_n <-
      # create factors with first top confirmed
      countries_order =  area_2_top_5_confirmed %>% filter(date == max(date)) %>%
        arrange(desc(confirmed)) %>%
        #arrange(!!as.symbol(input$radio_indicator)) %>%
        .[,"Country.Region"] %>% as.vector()

      mindate = min(area_2_top_5_confirmed$date[area_2_top_5_confirmed$confirmed>n2])

      df_top_n = area_2_top_5_confirmed %>% filter(date > mindate) %>% # take only starting point where greater than 1000
        mutate(status = factor(Country.Region, levels = countries_order[, "Country.Region", drop = T])) %>%
        mutate(value = confirmed) %>%
        capitalize_names_df()

      callModule(mod_plot_log_linear_server, paste(paste0("area",lev2id()),"plot_log_linear_top_n_area2", sep = "-"), df = df_top_n, type = "line")

      # > comparison plot from day of nth contagion
      callModule(mod_compare_nth_cases_plot_server, paste(paste0("area",lev2id()),"plot_compare_nth_area2", sep = "-"), orig_data_aggregate = data_2_filtered, n = n2)

      # > growth_death_rate, INPUT IS NOT CORRECT
      callModule(mod_growth_death_rate_server, paste(paste0("area",lev2id()),"plot_growth_death_rate_area2", sep = "-"), df = area_data_2_aggregate, n = n2)

      # > scatterplot prevalence vs growth
      callModule(mod_scatterplot_server, paste(paste0("area",lev2id()),"plot_scatterplot_area_2", sep = "-"), df = area_data_2_aggregate, istop = FALSE, n = n2, countries = areas())

      # > stacked barplot with status split
      callModule(mod_stackedbarplot_status_server, paste(paste0("area",lev2id()),"plot_stackedbarplot_status_area2", sep = "-"), df = area_data_2_aggregate, n = n2)

    } else{
      message("remove level 2 UI for ", req(input$select_country))

      removeUI(
        selector = paste0("#",ns(paste0("area",lev2id()))),
        #selector = paste0("#area",lev2id()), # it works
        immediate = TRUE
      )
      lev2id(0)
    }

  })


}



