#' country UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param n.select min number of cases for a country to be considered in selectInput.
#' @param nn min number of cases for used to filter country data
#'
#' @noRd
#'
#' @import shiny
#' @importFrom shinycssloaders withSpinner
mod_country_ui <- function(id, nn = 1000, n.select = 1000){
  ns <- NS(id)
  from_nth_case_msg = paste(message_conf_case("Countries",n.select),
                            #paste0("Only Countries with more than ", n.select, " confirmed cases can be chosen."),
                            message_firstday(nn),
                            #paste0("1st day is the day when ", nn ," confirmed cases are reached."),
                            message_missing_recovered(),
                            message_missing_data(),
                            #paste0("Recovered, Hospitalised and Tests data can be partially/completely unavailable in our data source for some countries and areas."),
                            #paste0("Hospitalised and test data are updated with delay for some countries and areas in our data source where available."),
                            message_hosp_data(),
                            sep = "<br/>")

  tagList(
    hr(),
    div(
      HTML(from_nth_case_msg)
      #uiOutput(ns("from_nth_case"))
    ),
    hr(),
    selectInput(label = "Country", inputId = ns("select_country"), choices = NULL, selected = NULL),

    tags$head(tags$style(HTML(".small-box {width: 300px; margin: 20px;}"))),
    mod_caseBoxes_ui(ns("count-boxes")),
    mod_caseBoxes_ui(ns("count-boxes_hosp"), hosp = TRUE),
    hr(),
    fluidRow(
      #withSpinner(uiOutput(ns("barplots")))
      withSpinner( mod_bar_plot_day_contagion_ui(ns("bar_plot_day_contagion")))
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
      ),
      column(6,
             withSpinner(mod_vaccines_text_ui(ns("vaccines_text_plot")))
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
#' @param tab logical, if TRUE then also the data table ui is called
#' @param stringency logical, if TRUE then also the barplot stringency ui is called
#' @param vaxflag logical, if TRUE then also the barplot of vaccines ui is called
#' @param n2 min number of cases for a country to be considered. Default n
#'
#' @noRd
#'
#' @import shiny
#' @importFrom shinycssloaders withSpinner
areaUI = function(id, tab = TRUE, stringency = TRUE, vaxflag = TRUE, n2 = 100){
  ns = shiny::NS(id)
  from_nth_case_area2_msg = paste(
    "Some countries have unreliable or inconsistent data at regional level in our data source.",
    "They may not match those at Country Level or they may miss information.",
    #paste0("Some countries or some regions within countries are not providing Recovered data."),
    message_missing_data(),
    message_firstday(n2),
    #paste0("1st day is the day when ", n2 ," confirmed cases are reached."),
    message_hosp_data(where = "some areas"),
    sep = "<br/>")


   tg = tagList(
      div(id = id,
           hr(),
           div(h3("Country split at level 2"), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
           hr(),
           #div(h5("Some countries have unreliable or inconsistent data at regional level. They may not match those at Country Level or they may miss information."), align = "left", style = "margin-top:20px; margin-bottom:20px;"),
           div(
             HTML(from_nth_case_area2_msg)
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
           hr(),
           fluidRow(
             column(12,
                    mod_stackedbarplot_ui(ns("plot_stackedbarplot_status_area2"))
             )
           ),
           #hr(),
           # fluidRow(
           #  column(12,
           #         mod_barplot_ui(ns("plot_barplot_stringency_area2"), plot1 = "ui_stringency", plot2 = NULL)
           #  )
           # )
          #tags$div(id = "plot_barplot_stringency_area2"),
       )
  )
  if(stringency) {
    tg = tagList(
      div(id = id,
          tg,
          hr(),
          tags$div(id = "plot_barplot_stringency_area2"),
      )
    )
  }
  if(vaxflag) {
     tg = tagList(
       div(id = id,
           tg,
           hr(),
           tags$div(id = "barplot_vax_index_area2"),
       )
     )
   }
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
mod_country_server <- function(input, output, session, data, countries, nn = 1000, w = 7, n.select = 1000){
  ns <- session$ns
  message("mod_country_server")
  observe(
    updateSelectInput(session, "select_country", choices = sort(countries()$Country.Region), selected = "USA")
  )
  lev2id <- reactiveVal(0) # for removeUI

  observeEvent(input$select_country, {

    message("process country page ", req(input$select_country))
    strid  <- reactiveVal(0) # for removeUI of stringency index
    vaxid  <- reactiveVal(0) # for removeUI of vaccines barplot
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

        country_data = country_data[, !grepl("^new", names(country_data))]
        country_data = country_data %>%
          get_timeseries_by_contagion_day_data() %>%  # recompute new variables
          build_data_aggr() # recompute other variables
        }
    }
    hospflag = sum(country_data$hosp, na.rm = TRUE) > 0
    vaxflag = sum(country_data$vaccines, na.rm = TRUE) > 0
    message("hospflag = ", hospflag)
    message("vaxflag = ", vaxflag)

    country_data_today <- country_data %>%
      add_growth_death_rate()

    lw_country_data = lw_vars_calc(country_data)

    # create datasets for box merging today with data7
    country_data_today = country_data_today %>%
      left_join(lw_country_data %>% select(-population))

    # Boxes ----
    vaxarg = NULL
    if (vaxflag)
      vaxarg = "recovered"
    callModule(mod_caseBoxes_server, "count-boxes", country_data_today, vax = vaxarg)

    # Boxes ----
    callModule(mod_caseBoxes_server, "count-boxes_hosp", country_data_today, hosp = TRUE)

    statuseslineplot = c("confirmed", "deaths", "recovered", "active")
    if (vaxflag)
      statuseslineplot = c("confirmed", "deaths", "vaccines", "active")

    callModule(mod_bar_plot_day_contagion_server, "bar_plot_day_contagion", country_data, nn = nn, statuses = statuseslineplot)

    # tables ----
    callModule(mod_add_table_server, "add_table_country", country_data %>% arrange(desc(date)), maxrowsperpage = 10)
    # plots ----
    levs <- areaplot_vars()
    country_data_area = country_data
    active_hosp = FALSE
    if (sum(country_data$hosp, na.rm = TRUE)>0) {
      message("Adding hospitalised data in areaplot for ", req(input$select_country))
      levs = c(levs, "hosp")
      active_hosp = TRUE
    }
    message("n for ", req(input$select_country), " = ", nn)
    # for country plot start from the beginning
    df_tot = tsdata_areplot(country_data_area,levs, nn) # start from day with >nn

    callModule(mod_plot_log_linear_server, "plot_area_tot", df = df_tot, type = "area", active_hosp = active_hosp)

    # plots ----
    levs <- areaplot_hospvars()
    # for country plot start from the beginning
    df_hosp = tsdata_areplot(country_data,levs, 1) # start from day with >nn

    callModule(mod_plot_log_linear_server, "plot_areahosp_tot", df = df_hosp, type = "area", hosp = TRUE)


    output[["lines_points_plots_tot"]] <- renderUI({
      mod_compare_nth_cases_plot_ui(ns("lines_plots_country"), nn = nn, istop = FALSE, tests = TRUE, hosp = hospflag, strindx = FALSE, selectvar = "new_confirmed", oneMpop = FALSE, vax = vaxflag)
    })

    callModule(mod_compare_nth_cases_plot_server, "lines_plots_country", country_data , tests = TRUE, hosp = hospflag, strindx = TRUE, nn = nn, istop = FALSE, oneMpop = FALSE, vax = vaxflag)#, secondline = "stringency_index")

    if (vaxflag) {
      callModule(mod_vaccines_text_server, "vaccines_text_plot", country_data, country_data_today)
    } else {
      callModule(mod_novaccines_text_server,"vaccines_text_plot", country = req(input$select_country))
    }

  # # ##### country split within areas #############################################

    #area2id = "subarea"
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
                 ui = areaUI(ns(id), n2 = max(1,nn/10)),#,  good for example
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
      callModule(mod_country_area_server, id, data = area_data_2_aggregate, n2 = max(1,nn/10), strid_arg = strid, vaxid_arg = vaxid, country = req(input$select_country))

    } else{
      #id = area2id
      if (exists("area2id") && lev2id() == 1) {

        message("remove level 2 UI for ", req(input$select_country))

        id <<- area2id

        # remove the ui generated previously
        message("remove id = ", id )
        removeUI(
          selector = paste0("#",ns(id)),
          #selector = paste0("#area",lev2id()), # it works
          immediate = TRUE
        )
        lev2id(0)
      }
    }
  })
}
#' country Server Function for level 2
#'
#' @param area_data_2_aggregate data.frame with level 2 countries
#' @param n2 min number of cases for a country to be considered. Default n
#' @param w number of days of outbreak. Default 7
#' @param tab logical, if TRUE then also the data table module is called
#' @param stringencyFlag logical, if TRUE then also stringency barplot module is called
#' @param vaccinesFlag logical, if TRUE then also vaccines barplot module is called
#' @param strid_arg reactive value numeric, it tells what country is being loaded with stringency plot, first one = 0
#' @param vaxid_arg reactive value numeric, it tells what country is being loaded with v ploaccinest, first one = 0
#'
#' @param country character, name of level 1 country
#'
#' @import dplyr
#' @import tidyr
#' @import shiny
#'
#' @noRd
mod_country_area_server <- function(input, output, session, data, n2 = 1, w = 7, tab = TRUE, stringencyFlag = TRUE, vaccinesFlag = TRUE, strid_arg,vaxid_arg, country = NULL) {
  ns <- session$ns

  message("mod_country_area_server n2 = ", n2, " Country = " ,country)

  data_2_filtered <-
    data %>%
    rescale_df_contagion(n = n2, w = w) # take where 100 confirmed

  data_2_filtered_today = data_2_filtered %>%
      filter(date == max(date))

  data_today = data %>%
    add_growth_death_rate()

  lw_data =  lw_vars_calc(data)

  data_today = data_today  %>%
    left_join(lw_data %>% select(-population))


  # list of ares to be used in the UI
  areas <- #reactive({
    data_2_filtered %>%
      select(Country.Region) %>%
      distinct() #%>% .$Country.Region
  #})

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
  callModule(mod_plot_log_linear_server, "plot_area2_area2", df = df_area_2, type = "area" , countries = reactive(areas), active_hosp = active_hosp)

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
  callModule(mod_plot_log_linear_server, "plot_areahosp2_area2", df = df_area_2, type = "area" , countries = reactive(areas), hosp = TRUE)


  # > line plot top 5

  if (FALSE) {
    mindate = min(area_2_top_5_confirmed$date[area_2_top_5_confirmed$confirmed>n2], na.rm = TRUE)

    # create factors with first top confirmed
    countries_order =  area_2_top_5_confirmed %>% filter(date == max(date)) %>%
      arrange(desc(confirmed)) %>%
      .[,"Country.Region"] %>% as.vector()
    df_top_n = area_2_top_5_confirmed %>% filter(date >= mindate) %>% # take only starting point where greater than n
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
  oneMpopflag = TRUE
  if (all(is.na(data_today$population)))
    oneMpopflag = FALSE

  strFlag = TRUE
  # do not use stringency v is the same for all areas
  if (all(is.na(data_today$stringency_index)) || all(data_today$stringency_index == 0, na.rm = TRUE) || length(table(data_today$stringency_index)) == 1)
    strFlag = FALSE

  vaxFlag = TRUE
  # do not use stringency v is the same for all areas
  if (all(is.na(data_today$vaccines)) || all(data_today$vaccines == 0, na.rm = TRUE) || length(table(data_today$vaccines)) == 1)
    vaxFlag = FALSE

  testsflag = TRUE
  if (all(is.na(data_today$tests)) || all(data_today$tests == 0, na.rm = TRUE) || length(table(data_today$tests)) == 1)
    testsflag = FALSE

  message("hospflag: ", hospflag, "/ oneMpopflag: ", oneMpopflag,"/ strFlag: ", strFlag,"/ testsflag: ", testsflag, "/ vaxFlag: ", vaxFlag)
 # paste0("lines_plots_area2_",country) because  of problems with selectInputID after USA page. not solved, TBD
  output[["plot_compare_nth_area2"]] <- renderUI({
    mod_compare_nth_cases_plot_ui(ns(paste0("lines_plots_area2_",country)), nn = n2, istop = FALSE, tests = testsflag, hosp = hospflag, strindx = strFlag,vax = vaxFlag, selectvar = "new_confirmed", oneMpop = oneMpopflag, areasearch = TRUE)
  })
  callModule(mod_compare_nth_cases_plot_server, paste0("lines_plots_area2_",country), df = data, nn = n2,  istop = FALSE, tests = testsflag, hosp = hospflag, strindx = strFlag ,vax = vaxFlag,
             n_highligth = length(unique(data$Country.Region)), oneMpop = oneMpopflag, areasearch = TRUE)

  # growth_death_rate,
  output[["plot_growth_death_rate_area2"]] <- renderUI({
    mod_barplot_ui(ns("rate_plots_area2"))
  })
  callModule(mod_barplot_server, "rate_plots_area2", df = data_today, istop = FALSE,
             n_highligth = length(unique(data_today$Country.Region)))


  areasC <-
    areas %>% .$Country.Region
  #})
  # > scatterplot prevalence vs growth

  # use data_2_filtered to filter out areas with few cases
  # choose x var
  xvar = "confirmed_rate_1M_pop"
  varsSelextX = NULL
  if (all(is.na(data_today$population))) {
    varsSelextX = list(label = div(style = "font-size:10px","Select (x) Total Confirmed Cases"),
                 choices = list("Over one week" = "lw_confirmed",
                                #"Over 1 month" = "lm_confirmed",
                                "Total" = "confirmed"),
                 selected = "confirmed")
    xvar = "confirmed"
  }
  output[["plot_scatterplot_area_2"]] <- renderUI({
    mod_scatterplot_ui(ns("scatterplot_plots_area2"), varsx = varsSelextX )
  })
  callModule(mod_scatterplot_server, "scatterplot_plots_area2", df = data_today, istop = FALSE, nmed = n2, countries = areasC, xvar = xvar)

  # reconsider hosp flag as of today, hosp data may have been removed or not updated as of today
  # > stacked barplot with status split, use data_2_filtered_today
  callModule(mod_stackedbarplot_status_server, "plot_stackedbarplot_status_area2", df = data_today, istop = FALSE, n_highligth = length(unique(data_today$Country.Region)), active_hosp = active_hosp)


  ######################
  # Barplot stringency is conditional on having stringency data
  if (stringencyFlag) {
    message("strid_arg: ", strid_arg())

    if (strFlag) {

      # insert UI components
      if (strid_arg() == 0) { # country has changed
        if (exists("stridx2id")) {
          message("remove stringency id = ", stridx2id )

          removeUI(
            selector = paste0("#",ns(stridx2id)),
            #selector = paste0("#area",lev2id()), # it works
            immediate = TRUE
          )
        }

        strid_arg(strid_arg()+1)
        id = paste0("area_stringency",strid_arg())
        message("id stringency insert = ", id)

        message('Level 2 Stringency Index present: insertUI for barplot ', strid_arg())

        insertUI(paste0("#","plot_barplot_stringency_area2"),
                 'afterEnd',
                 #ui = areaUI(ns(paste0("area",lev2id()))),#,  good for example
                 ui = mod_barplot_ui(ns(id), plot1 = "ui_stringency", plot2 = NULL), #,  good for example
                 session = session,
                 immediate = TRUE
        )
      } else
        id = paste0("strid_arg",strid_arg())
      stridx2id <<-id
      message("id call stringency module = ", id)

      # > barplot stringency
      callModule(mod_barplot_server, id, data_today, n_highligth = length(unique(data_today$Country.Region)), istop = FALSE,
                 plottitle = c("Stringency Index"),
                 g_palette = list("plot_1" = barplots_colors$stringency,
                                  calc = TRUE),
                 pickvariable = list("plot_1" = "confirmed_rate_1M_pop")) # pick top 10 confirmed countries
    } else{
      if (exists("stridx2id")) {
        message("remove level 2 Stringency Index UI barplot")
        id = stridx2id
        # remove the ui generated previously
        message("remove stringency id = ", id )
        removeUI(
          selector = paste0("#",ns(id)),
          immediate = TRUE
        )
        strid_arg(0)
      }
    }

  }

  ######################
  # Barplot vaccines is conditional on having vaccines data
  if (vaccinesFlag) {
    message("vaxid_arg: ", vaxid_arg())
    if (vaxFlag) {

      # insert UI components
      if (vaxid_arg() == 0) {
        if (exists("vaxidx2id")) {
          message("remove vaccines id = ", vaxidx2id )

          removeUI(
            selector = paste0("#",ns(vaxidx2id)),
            #selector = paste0("#area",lev2id()), # it works
            immediate = TRUE
          )
        }

        vaxid_arg(vaxid_arg()+1)
        id = paste0("area_vaccines",strid_arg())
        message("id vaccines insert = ", id)

        message('Level 2 Vaccines data present: insertUI for barplot ', vaxid_arg())

        insertUI(paste0("#","barplot_vax_index_area2"),
                 'afterEnd',
                 #ui = areaUI(ns(paste0("area",lev2id()))),#,  good for example
                 ui = mod_barplot_ui(ns(id), plot1 = "ui_vaccines", plot2 = NULL), #,  good for example
                 session = session,
                 immediate = TRUE
        )
      } else
        id = paste0("vaxid_arg",vaxid_arg())
      vaxidx2id <<-id
      message("id call vaccines module = ", id)

      # > barplot stringency
      callModule(mod_barplot_server, id, data_today, n_highligth = length(unique(data_today$Country.Region)), istop = FALSE,
                 plottitle = c("Vaccinations"),
                 g_palette = list("plot_1" = barplots_colors$vaccines,
                                  calc = TRUE),
                 pickvariable = list("plot_1" = "confirmed_rate_1M_pop")) # pick top 10 confirmed countries
    } else{
      if (exists("vaxidx2id")) {
        message("remove vaccines id = ", vaxidx2id )
        id = vaxidx2id
        # remove the ui generated previously
        removeUI(
          selector = paste0("#",ns(id)),
          #selector = paste0("#area",lev2id()), # it works
          immediate = TRUE
        )
        vaxid_arg(0)
      }
    }

  }
  if(tab) {
    # prepare data for table with country data
    area_data_2_aggregate_tab = data %>% # only data from today
      filter(date == max(date)) %>%
      arrange(desc(confirmed) )

    callModule(mod_add_table_server, "add_table_area2",
               area_data_2_aggregate_tab, maxrowsperpage = 10)

  }

}
