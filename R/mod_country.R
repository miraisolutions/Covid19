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
mod_country_ui <- function(id){
  ns <- NS(id)
  tagList(
    hr(),
    div(
      uiOutput(ns("from_nth_case"))
    ),
    hr(),
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
             withSpinner(mod_plot_log_linear_ui(ns("plot_area_tot"), area = TRUE))
             )
      ),
      column(6,
             withSpinner(mod_compare_nth_cases_plot_ui(ns("lines_points_plots_tot")))
      )
    ),
    hr(),
    tags$div(id = "subarea"),
    mod_add_table_ui(ns("add_table_country"))
  )
}

#' Level 2 areas UI function
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom shinycssloaders withSpinner
areaUI = function(id){
  ns = shiny::NS(id)
    tagList(
      div(id = id,
           div(h3("Country split at level 2"), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
           hr(),
           #div(h5("Some countries have unreliable or inconsistent data at regional level. They may not match those at Country Level or they may miss information."), align = "left", style = "margin-top:20px; margin-bottom:20px;"),
           div(
              uiOutput(ns("from_nth_case_area2"))
           ),
           hr(),
           fluidRow(
             column(6,
                    div(h4("Covid-19 time evolution"), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
                    #mod_plot_log_linear_ui(ns("plot_area_area2"))
                    withSpinner(uiOutput(ns("plot_area_area2")))
             ),
             column(6,
                    div(h4("Confirmed cases for top 5 Areas"), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
                    #mod_plot_log_linear_ui(ns("plot_log_linear_top_n_area2"))
                    withSpinner(uiOutput(ns("plot_log_linear_top_n_area2")))
                 )
           ),
           hr(),
           fluidRow(

             column(6,
                    #mod_compare_nth_cases_plot_ui(ns("plot_compare_nth_area2"), actives = FALSE)
                    withSpinner(uiOutput(ns("plot_compare_nth_area2")))

                    ),
             column(6,
                    #mod_growth_death_rate_ui(ns("plot_growth_death_rate_area2"))
                    withSpinner(uiOutput(ns("plot_growth_death_rate_area2")))
                    )
           ),
           hr(),
           fluidRow(
             column(6,
                    #mod_scatterplot_ui(ns("plot_scatterplot_area_2"))
                    withSpinner(uiOutput(ns("plot_scatterplot_area_2")))

             ),
             column(6,
                    mod_stackedbarplot_ui(ns("plot_stackedbarplot_status_area2"))
             )
           ),
          hr(),
          mod_add_table_ui(ns("add_table_area2"))
       )
  )
}
#' country Server Function
#'
#' @param data data.frame
#' @param countries reactive character vector
#' @param n min number of cases for used to filter country data
#' @param w number of days of outbreak. Default 7
#' @param n.select min number of cases for a country to be considered in selectInput.
#'
#' @import dplyr
#' @import tidyr
#' @import shiny
#'
#' @noRd
mod_country_server <- function(input, output, session, data, countries, n = 1, w = 7, n.select = 1000){
  ns <- session$ns
  observe(
    updateSelectInput(session, "select_country", choices = sort(countries()$Country.Region), selected = "Switzerland")
  )
  lev2id <- reactiveVal(0) # for removeUI


  output$from_nth_case<- renderUI({
    HTML(paste(paste0("Only Countries with more than ", n.select, " confirmed cases can be chosen."),
               paste0("Some countries are not providing Recovered data."),
         paste0("Contagion day 0 is the day when ", n ," confirmed cases are reached."), sep = "<br/>"))
  })

  observeEvent(input$select_country, {

    message("process country ", req(input$select_country))
    # Data ----
    country_data <-  data %>%
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
    #n_country = select_n(country_data_area$confirmed,n)
    message("n for ", req(input$select_country), " = ", n)
    # for country plot start from the beginning
    df_tot = tsdata_areplot(country_data_area,levs, n) # start from day with >n

    callModule(mod_plot_log_linear_server, "plot_area_tot", df = df_tot, type = "area")

    output$barplots <- renderUI({
      mod_bar_plot_day_contagion_ui(ns("bar_plot_day_contagion"))
    })

    callModule(mod_compare_nth_cases_plot_server, "lines_points_plots_tot", country_data , n = n, w = w, istop = F)

    callModule(mod_bar_plot_day_contagion_server, "bar_plot_day_contagion", country_data)

  #  })
  # # ##### country split within areas #############################################

  #   # Data ----
    area_data_2 = get_datahub(country = req(input$select_country), stardate = "2020-01-22", lev = 2, verbose = FALSE)
    if (!is.null(area_data_2) && nrow(area_data_2) >0) {
      # insert UI components
      if (lev2id() == 0) {
        lev2id(lev2id()+1)
        id = paste0("area",lev2id())
        message("id insert = ", id)

        #lev2id(lev2id()+1)
        message('Level 2 data present: insertUI for area plots ', lev2id())

        insertUI(paste0("#","subarea"),
                 'afterEnd',
                 #ui = areaUI(ns(paste0("area",lev2id()))),#,  good for example
                 ui = areaUI(ns(id)),#,  good for example
                 session = session,
                  immediate = TRUE
                 )
      } else
        id = paste0("area",lev2id())
      area2id <<-id

      # works in example
      message("id lev2 = ", id)
      callModule(mod_country_area_server, id, datahub_2 = area_data_2, n2 = max(1,n/10))

    } else{
      message("remove level 2 UI for ", req(input$select_country))
      id = area2id
      # remove the ui generated previously
      message("remove id = ", id )
      removeUI(
        selector = paste0("#",ns(id)),
        #selector = paste0("#area",lev2id()), # it works
        immediate = TRUE
      )
      lev2id(0)
    }
  })
}
#' country Server Function for level 2
#'
#' @param datahub_2 data.frame with level 2 countries
#' @param n2 min number of cases for a country to be considered. Default n
#' @param w number of days of outbreak. Default 7
#'
#' @import dplyr
#' @import tidyr
#' @import shiny
#'
#' @noRd
mod_country_area_server <- function(input, output, session, datahub_2, n2 = 1, w = 7) {
  ns <- session$ns

  message("mod_country_area_server n2 = ", n2)
  area_data_2 = datahub_2 %>%
    get_timeseries_by_contagion_day_data()

  area_data_2_aggregate <-
    build_data_aggr(area_data_2)

  data_2_filtered <-
    area_data_2_aggregate %>%
    rescale_df_contagion(n = n2, w = w) # take where 100 confirmed

  data_2_filtered_today = data_2_filtered %>% filter(date == max(date))

  # list of ares to be used in the UI
  areas <- reactive({
    data_2_filtered %>%
      select(Country.Region) %>%
      distinct() #%>% .$Country.Region
  })

  area_data_2_aggregate_today <-
    area_data_2_aggregate %>%
    filter( date == max(date))

  area_2_top_5_today <-
    area_data_2_aggregate_today %>%
    arrange(desc(confirmed)) %>%
    head(5)

  area_2_top_5_confirmed <-
    area_data_2_aggregate %>%
    filter(Country.Region %in% area_2_top_5_today$Country.Region) %>%
    select(Country.Region, date, confirmed)

  output$from_nth_case_area2<- renderUI({
    HTML(paste(
      "Some countries have unreliable or inconsistent data at regional level. They may not match those at Country Level or they may miss information.",
      paste0("Some countries or some regions within countries are not providing Recovered data"),
      paste0("Contagion day 0 is the day when ", n2 ," confirmed cases are reached."), sep = "<br/>"))
  })

  # plots ----
  levs <- sort_type_hardcoded()
  df_area_2 = purrr::map(unique(area_data_2$Country.Region),
    function(un) {
      dat = tsdata_areplot(area_data_2[area_data_2$Country.Region == un, ], levs, 0) #n = 0 for area plot
      dat$Country.Region = rep(un, nrow(dat))
      dat
      })
  df_area_2 = Reduce("rbind",df_area_2)
  # create factors with first top confirmed
  countries_order =  area_2_top_5_confirmed %>% filter(date == max(date)) %>%
    arrange(desc(confirmed)) %>%
    .[,"Country.Region"] %>% as.vector()
  output[["plot_area_area2"]] <- renderUI({
    mod_plot_log_linear_ui(ns("plot_area_area2"), select = TRUE, area = TRUE)
  })
  callModule(mod_plot_log_linear_server, "plot_area_area2", df = df_area_2, type = "area" , countries = areas)

  # > line plot top 5

  mindate = min(area_2_top_5_confirmed$date[area_2_top_5_confirmed$confirmed>n2])

  df_top_n = area_2_top_5_confirmed %>% filter(date > mindate) %>% # take only starting point where greater than n
    mutate(status = factor(Country.Region, levels = countries_order[, "Country.Region", drop = T])) %>%
    mutate(value = confirmed) %>%
    capitalize_names_df()

  output[["plot_log_linear_top_n_area2"]] <- renderUI({
    mod_plot_log_linear_ui(ns("log_linear_top_n_area2"), area = FALSE)
  })
  callModule(mod_plot_log_linear_server, "log_linear_top_n_area2", df = df_top_n, type = "line")

  # > comparison plot from day of nth contagion

  output[["plot_compare_nth_area2"]] <- renderUI({
    mod_compare_nth_cases_plot_ui(ns("lines_plots_area2"))
  })
  callModule(mod_compare_nth_cases_plot_server, "lines_plots_area2", df = data_2_filtered, n = n2, istop = TRUE)

  # > growth_death_rate,
  output[["plot_growth_death_rate_area2"]] <- renderUI({
    mod_growth_death_rate_ui(ns("rate_plots_area2"))
  })
  callModule(mod_growth_death_rate_server, "rate_plots_area2", df = area_data_2_aggregate, n = n2)

  # > scatterplot prevalence vs growth
  output[["plot_scatterplot_area_2"]] <- renderUI({
    mod_scatterplot_ui(ns("scatterplot_plots_area2"))
  })
  areasC <- reactive({
    areas() %>% .$Country.Region
  })
  # use data_2_filtered to filter out areas with few cases
  callModule(mod_scatterplot_server, "scatterplot_plots_area2", df = data_2_filtered_today, istop = FALSE, nmed = n2, countries = areasC())

  # > stacked barplot with status split, use data_2_filtered_today
  callModule(mod_stackedbarplot_status_server, "plot_stackedbarplot_status_area2", df = data_2_filtered, n = n2)

  # prepare data for table with country data
  area_data_2_aggregate_tab = area_data_2_aggregate %>% # only data from today
    filter(date == max(date)) %>%
    arrange(desc(confirmed) )
  callModule(mod_add_table_server, "add_table_area2",
             area_data_2_aggregate_tab, maxrowsperpage = 10)
}

# select_n <- function(var, n = 100) {
#   ifelse(max(var) > 10000,
#                      n,
#                      ifelse(max(var) > 1000,
#                             n/10,
#                             ifelse(max(var) > 100,
#                                    n/100,
#                                    1)))
# }
