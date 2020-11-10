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
    mod_caseBoxes_ui(ns("count-boxes_hosp"), hosp = TRUE),
    hr(),
    fluidRow(
      withSpinner(uiOutput(ns("barplots")))
    ),
    hr(),
    fluidRow(
      column(6,
             br(),
             div( h4("Covid-19 time evolution"), align = "center",
             div(style = "visibility: hidden", radioButtons("dummy1", "", choices = "dummy")),
             withSpinner(mod_plot_log_linear_ui(ns("plot_area_tot"), area = TRUE))
             )
      ),
      column(6,
             div( h4("Time evolution of Hospitalised cases"), align = "center",
                  div(style = "visibility: hidden", radioButtons("dummy1", "", choices = "dummy")),
                  withSpinner(mod_plot_log_linear_ui(ns("plot_areahosp_tot"), area = TRUE))
             )
      )
    ),
    hr(),
    fluidRow(
      column(6,
            #withSpinner(mod_compare_nth_cases_plot_ui(ns("lines_points_plots_tot"), tests = TRUE, hosp = TRUE))
            withSpinner(uiOutput(ns("lines_points_plots_tot")))

      )#,
      # column(6,
      #        withSpinner(mod_compare_nth_cases_plot_ui(ns("lines_points_plots_tot"), tests = TRUE, hosp = TRUE))
      # )
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
#' @param tab logical, if TRUE then also the data table ui is called
#' @noRd
#'
#' @import shiny
#' @importFrom shinycssloaders withSpinner
areaUI = function(id, tab = TRUE){
  ns = shiny::NS(id)
   tg = tagList(
      div(id = id,
           hr(),
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
                    withSpinner(uiOutput(ns("plot_area_area2")))
             ),
             # column(6,
             #        div(h4("Confirmed cases for top 5 Areas"), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
             #        withSpinner(uiOutput(ns("plot_log_linear_top_n_area2")))
             #     )
             column(6,
                    div(h4("Time evolution of Hospitalised cases"), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
                    withSpinner(uiOutput(ns("plot_areahosp_area2")))
             )
           ),
           hr(),
           fluidRow(

             column(6,
                    withSpinner(uiOutput(ns("plot_compare_nth_area2")))

                    ),
             column(6,
                    withSpinner(uiOutput(ns("plot_scatterplot_area_2")))
                    )
           ),
           hr(),
          fluidRow(
            column(12,
                   withSpinner(uiOutput(ns("plot_growth_death_rate_area2")))
            )
          ),
           fluidRow(
             column(12,

                    mod_stackedbarplot_ui(ns("plot_stackedbarplot_status_area2")
                )
             )
          )
       )
  )
  if(tab) {
    tg = tagList(
      div(id = id,
          tg,
          hr(),
          mod_add_table_ui(ns("add_table_area2"))
      )
    )
  }
   tg

}
#' country Server Function
#'
#' @param data data.frame
#' @param countries reactive character vector
#' @param nn min number of cases for used to filter country data
#' @param w number of days of outbreak. Default 7
#' @param n.select min number of cases for a country to be considered in selectInput.
#'
#' @import dplyr
#' @import tidyr
#' @import shiny
#'
#' @noRd
mod_country_server <- function(input, output, session, data, countries, nn = 100, w = 7, n.select = 1000){
  ns <- session$ns
  message("mod_country_server")
  observe(
    updateSelectInput(session, "select_country", choices = sort(countries()$Country.Region), selected = "USA")
  )
  lev2id <- reactiveVal(0) # for removeUI


  output$from_nth_case<- renderUI({
    HTML(paste(paste0("Only Countries with more than ", n.select, " confirmed cases can be chosen."),
               paste0("Some countries are not providing Recovered data."),
         paste0("1st day is the day when ", nn ," confirmed cases are reached."), sep = "<br/>"))
  })

  observeEvent(input$select_country, {

    message("process country page ", req(input$select_country))
    # Data ----
    country_data <-  data %>%
        filter(Country.Region %in% req(input$select_country)) %>%
        filter(contagion_day > 0) %>%
        arrange(desc(date))

    #   # Data ----
    area_data_2 = get_datahub(country = req(input$select_country), lev = 2, verbose = FALSE)

    if (!is.null(area_data_2) && nrow(area_data_2) >0) {
     # adjust hospitalised data if we have better in lev 2
      if (sum(area_data_2$hosp, na.rm = TRUE) > sum(country_data$hosp, na.rm = TRUE)*1.5) {
        message("Update Lev 1 hospitalised data based on lev2 fo country ", req(input$select_country))
        # aggregate hosp data at country level
        area_data_1 = area_data_2 %>% select(date, all_of(as.vector(.hosp_vars))) %>%
          filter(date >= min(country_data$date)) %>% # filter to align dates with country data
          group_by(date) %>%
          summarise_if(is.numeric, sum, na.rm = TRUE) %>% #
          ungroup() %>%
          mutate(Country.Region = req(input$select_country)) %>%
          arrange(desc(date))
        area_data_1 = area_data_1[, c("Country.Region", setdiff(names(area_data_1),"Country.Region"))]
        if (!identical(area_data_1$date, country_data$date))
          warning("wrong dates in lev1 and lev2 for ", req(input$select_country))
        country_data[country_data$date %in% area_data_1$date, .hosp_vars] <- area_data_1[area_data_1$date %in% country_data$date, .hosp_vars]
        country_data = country_data %>%
          get_timeseries_by_contagion_day_data() %>%  # recompute new variables
          build_data_aggr() # recompute other variables
        }
    }
    hospflag = sum(country_data$hosp, na.rm = TRUE) > 0

    country_data_today <- country_data %>%
        filter(date == max(date))

    lw_country_data = lw_vars_calc(country_data)

    # create datasets for box merging today with data7
    lw_country_data_today = country_data %>% filter(date == max(date)) %>%
      left_join(lw_country_data %>% select(-population))

    # Boxes ----
    callModule(mod_caseBoxes_server, "count-boxes", country_data_today)

    # Boxes ----
    callModule(mod_caseBoxes_server, "count-boxes_hosp", lw_country_data_today, hosp = TRUE)

    # tables ----
    callModule(mod_add_table_server, "add_table_country", country_data,  maxrowsperpage = 10)
    # plots ----
    levs <- areaplot_vars()
    country_data_area = country_data
    active_hosp = FALSE
    if (sum(country_data$hosp, na.rm = TRUE)>0) {
      message("Adding hospitalised data in areaplot for ", req(input$select_country))
      levs = c(levs, "hosp")
      active_hosp = TRUE
    }
    #n_country = select_n(country_data_area$confirmed,n)
    message("n for ", req(input$select_country), " = ", nn)
    # for country plot start from the beginning
    df_tot = tsdata_areplot(country_data_area,levs, nn) # start from day with >nn

    callModule(mod_plot_log_linear_server, "plot_area_tot", df = df_tot, type = "area", active_hosp = active_hosp)

    # plots ----
    levs <- areaplot_hospvars()
    # for country plot start from the beginning
    df_hosp = tsdata_areplot(country_data,levs, 1) # start from day with >nn

    callModule(mod_plot_log_linear_server, "plot_areahosp_tot", df = df_hosp, type = "area", hosp = TRUE)

    output$barplots <- renderUI({
      mod_bar_plot_day_contagion_ui(ns("bar_plot_day_contagion"))
    })

    output[["lines_points_plots_tot"]] <- renderUI({
      mod_compare_nth_cases_plot_ui(ns("lines_plots_country"), tests = FALSE, hosp = hospflag, selectvar = "new_confirmed", oneMpop = FALSE)
    })

    callModule(mod_compare_nth_cases_plot_server, "lines_plots_country", country_data , nn = nn, w = w, istop = FALSE)

    callModule(mod_bar_plot_day_contagion_server, "bar_plot_day_contagion", country_data, nn = nn)

  #  })
  # # ##### country split within areas #############################################


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

      area_data_2 = area_data_2 %>%
        get_timeseries_by_contagion_day_data()

      area_data_2_aggregate <-
        build_data_aggr(area_data_2)
      # works in example
      message("id lev2 = ", id)
      callModule(mod_country_area_server, id, data = area_data_2_aggregate, n2 = max(1,nn/10))

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
#' @param area_data_2_aggregate data.frame with level 2 countries
#' @param n2 min number of cases for a country to be considered. Default n
#' @param w number of days of outbreak. Default 7
#' @param tab logical, if TRUE then also the data table module is called
#'
#' @import dplyr
#' @import tidyr
#' @import shiny
#'
#' @noRd
mod_country_area_server <- function(input, output, session, data, n2 = 1, w = 7, tab = TRUE) {
  ns <- session$ns

  message("mod_country_area_server n2 = ", n2)
  # area_data_2 = datahub_2 %>%
  #   get_timeseries_by_contagion_day_data()
  #
  # area_data_2_aggregate <-
  #   build_data_aggr(area_data_2)

  data_2_filtered <-
    data %>%
    rescale_df_contagion(n = n2, w = w) # take where 100 confirmed

  data_2_filtered_today = data_2_filtered %>% filter(date == max(date))

  # list of ares to be used in the UI
  areas <- reactive({
    data_2_filtered %>%
      select(Country.Region) %>%
      distinct() #%>% .$Country.Region
  })

  area_data_2_aggregate_today <-
    data %>%
    filter( date == max(date))

  area_2_top_5_today <-
    area_data_2_aggregate_today %>%
    arrange(desc(confirmed)) %>%
    head(5)

  area_2_top_5_confirmed <-
    data %>%
    filter(Country.Region %in% area_2_top_5_today$Country.Region) %>%
    select(Country.Region, date, confirmed)

  output$from_nth_case_area2<- renderUI({
    HTML(paste(
      "Some countries have unreliable or inconsistent data at regional level. They may not match those at Country Level or they may miss information.",
      paste0("Some countries or some regions within countries are not providing Recovered data."),
      paste0("1st day is the day when ", n2 ," confirmed cases are reached."), sep = "<br/>"))
  })

  # plots ----
  # Area plot
  levs <- areaplot_vars()
  data_area = data
  active_hosp = FALSE
  if (sum(data$hosp, na.rm = TRUE)>0) {
    message("Adding hospitalised data for areaplot")
    levs = c(levs, "hosp")
    active_hosp = TRUE
  }

  df_area_2 = purrr::map(unique(data_area$Country.Region),
    function(un) {
      dat = tsdata_areplot(data_area[data_area$Country.Region == un, ], levs, nn = n2) #n = 0 for area plot
      dat$Country.Region = rep(un, nrow(dat))
      dat
      })
  df_area_2 = Reduce("rbind",df_area_2)


  output[["plot_area_area2"]] <- renderUI({
    mod_plot_log_linear_ui(ns("plot_area2_area2"), select = TRUE, area = TRUE)
  })
  callModule(mod_plot_log_linear_server, "plot_area2_area2", df = df_area_2, type = "area" , countries = areas, active_hosp = active_hosp)

  # Area plot hospitalised ----
  levs <- areaplot_hospvars()

  df_area_2 = purrr::map(unique(data$Country.Region),
                         function(un) {
                           dat = tsdata_areplot(data[data$Country.Region == un, ], levs, nn = 1) #n = 0 for area plot
                           dat$Country.Region = rep(un, nrow(dat))
                           dat
                         })
  df_area_2 = Reduce("rbind",df_area_2)

  output[["plot_areahosp_area2"]] <- renderUI({
    mod_plot_log_linear_ui(ns("plot_areahosp2_area2"), select = TRUE, area = TRUE)
  })
  callModule(mod_plot_log_linear_server, "plot_areahosp2_area2", df = df_area_2, type = "area" , countries = areas, hosp = TRUE)


  # > line plot top 5

  if (FALSE) {
    mindate = min(area_2_top_5_confirmed$date[area_2_top_5_confirmed$confirmed>n2], na.rm = TRUE)

    # create factors with first top confirmed
    countries_order =  area_2_top_5_confirmed %>% filter(date == max(date)) %>%
      arrange(desc(confirmed)) %>%
      .[,"Country.Region"] %>% as.vector()
    df_top_n = area_2_top_5_confirmed %>% filter(date > mindate) %>% # take only starting point where greater than n
      mutate(status = factor(Country.Region, levels = countries_order[, "Country.Region", drop = T])) %>%
      mutate(value = confirmed) %>%
      capitalize_names_df()
    output[["plot_log_linear_top_n_area2"]] <- renderUI({
      mod_plot_log_linear_ui(ns("log_linear_top_n_area2"), area = FALSE)
    })
    callModule(mod_plot_log_linear_server, "log_linear_top_n_area2", df = df_top_n, type = "line")

  }

  # > comparison plot from day of nth contagion
  hospflag = sum(data$hosp, na.rm = TRUE) > 0
  output[["plot_compare_nth_area2"]] <- renderUI({
    mod_compare_nth_cases_plot_ui(ns("lines_plots_area2"), tests = FALSE, hosp = hospflag, selectvar = "new_prevalence_rate_1M_pop")
  })
  callModule(mod_compare_nth_cases_plot_server, "lines_plots_area2", df = data, nn = n2, istop = TRUE, n_highligth = min(5,length(unique(data$Country.Region))))

  # > growth_death_rate,
  output[["plot_growth_death_rate_area2"]] <- renderUI({
    mod_growth_death_rate_ui(ns("rate_plots_area2"))
  })
  callModule(mod_growth_death_rate_server, "rate_plots_area2", df = data, nn = n2, istop = FALSE, n_highligth = length(unique(data$Country.Region)))

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
  callModule(mod_stackedbarplot_status_server, "plot_stackedbarplot_status_area2", df = data_2_filtered, istop = FALSE, n_highligth = length(unique(data_2_filtered$Country.Region)), active_hosp = active_hosp)

  if(tab) {
    # prepare data for table with country data
    area_data_2_aggregate_tab = data %>% # only data from today
      filter(date == max(date)) %>%
      arrange(desc(confirmed) )

    callModule(mod_add_table_server, "add_table_area2",
               area_data_2_aggregate_tab, maxrowsperpage = 10)

  }

}
