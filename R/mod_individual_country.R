#' ind_country UI Function
#'
#' @description A shiny Module.
#'
#' @param id, Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom shinycssloaders withSpinner
mod_ind_country_ui <- function(id){
  ns <- NS(id)
  n2 <- 10
  tagList(
    hr(),
    tags$head(tags$style(HTML(".small-box {width: 300px; margin: 20px;}"))),
    mod_caseBoxes_ui(ns("ind_count-boxes")),
    mod_caseBoxes_ui(ns("ind_count-boxes_hosp"), hosp = TRUE),
    hr(),
    div( h4("COVID-19 Dashboard Switzerland"), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
    hr(),
    div(
      htmlOutput(ns("ind_from_nth_case")), class = "bodytext"
    ),
    #hr(),
    div(
      htmlOutput(ns("ind_missing_days")), class = "bodytext"
    ),
    hr(),
    fluidRow(
     # withSpinner(uiOutput(ns("ind_barplots")))
      withSpinner(mod_bar_plot_day_contagion_ui(ns("ind_bar_plot_day_contagion")))

    ),
    hr(),
    fluidRow(
      column(6,
             br(),
             div( h4("COVID-19 evolution over time"), align = "center",
             div(style = "visibility: hidden", radioButtons("dummy1", "", choices = "dummy")),
             withSpinner(mod_plot_log_linear_ui(ns("ind_plot_area_tot"), area = TRUE))
             )
      ),
      column(6,
             br(),
             div( h4("Evolution over time of Hospitalizations"), align = "center",
                  div(style = "visibility: hidden", radioButtons("dummy1", "", choices = "dummy")),
                  withSpinner(mod_plot_log_linear_ui(ns("ind_plot_areahosp_tot"), area = TRUE))
             )
      )
    ),
    fluidRow(
      column(6,
             #withSpinner(mod_compare_nth_cases_plot_ui(ns("ind_lines_points_plots_tot"),istop = FALSE,nn = 1, tests = TRUE, hosp = TRUE, strindx = FALSE, oneMpop = FALSE, vax = TRUE))
             withSpinner(mod_compare_timeline_plot_ui(ns("ind_lines_points_plots_tot"), titles = 1:2, istop = FALSE, tests = TRUE, hosp = TRUE, strindx = TRUE, oneMpop = FALSE, vax = TRUE, nn = 100))
      ),
      column(6,
             withSpinner(mod_vaccines_text_ui(ns("ind_vaccines_text_plot")))
      )
    ),
    hr(),
    mod_add_table_ui(ns("ind_add_table_country")), # table at country level

    hr(),
    actionButton(ns("button_2nd_level"), p("Click to open report:",br(), "Cantonal Level."), class = "button-style"),
    div(h4("Country Report at Cantonal level"), align = "center", class = "sectiontitle"),
    uiOutput(ns("ch_2nd_level_ui")),
    hr(),
    actionButton(ns("button_2nd_level_maps"), p("Click to open Heat Maps report:",br(), "Switzerland at Cantonal Level."), class = "button-style"),
    uiOutput(ns("ch_2nd_level_maps_ui")),

    hr()
    )
}
#' Level 2 areas UI function for maps
#' @description A shiny Module.
#'
#' @param id, Internal parameters for {shiny}.
#' @param country, character name of main country
#'
#' @noRd
#'
#' @import shiny
#' @importFrom shinycssloaders withSpinner
areamapUI <- function(id, country){
  ns = shiny::NS(id)
  message("areamapUI")
  tagList(
    div(id = id,
        hr(),
        div(h4(paste("Country Heat Maps within", country)), align = "center", style = "sectiontitle"),
        hr(),
        fluidRow(
          column(6,
                 withSpinner(mod_map_area_calc_ui(ns("map_ind_confirmed")))
                 #withSpinner(uiOutput(ns("map_countries_confirmed")))
          ),
          column(6,
                 #withSpinner(mod_map_area_calc_ui(ns("map_ind_active")))
                 withSpinner(mod_map_area_calc_ui(ns("map_ind_vaccines")))
                 #withSpinner(uiOutput(ns("map_countries_active")))
          )
        ),
        fluidRow(
          column(6,
                 withSpinner(mod_map_area_calc_ui(ns("map_ind_growthvsprev")))
                 #withSpinner(uiOutput(ns("map_countries_growthvsprev")))
          ),
          column(6,
                 withSpinner(mod_map_area_calc_ui(ns("map_ind_growth")))
                 #withSpinner(uiOutput(ns("map_countries_growth")))
          )
        ),
        fluidRow(
          column(6,
                 withSpinner(mod_map_area_calc_ui(ns("map_ind_hosp")))
                 #withSpinner(uiOutput(ns("map_countries_hosp")))
          ),
          column(6,
                 withSpinner(mod_map_area_calc_ui(ns("map_ind_death")))
                 #withSpinner(uiOutput(ns("map_countries_death")))
          )
        )
    )
  )
}
#' ind_country Server Function
#'
#' @param data data.frame with Country Level data
#' @param data2 data.frame with Country Level 2 data
#' @param countries reactive character vector
#' @param nn min number of cases for used to filter country data
#' @param w number of days of outbreak. Default 7
#'
#' @import dplyr
#' @import tidyr
#' @import shiny
#'
#' @noRd
mod_ind_country_server <- function(input, output, session, data, data2 = NULL, country , nn = 1, w = 7){
  ns <- session$ns

  message("mod_ind_country_server")
  output$ind_from_nth_case<- renderText({
    HTML(paste(
         message_missing_data("Recovered and Tests",where = "most of Cantons"),
         #message_firstday(nn),
         message_hosp_data(where = "Cantons"), sep = "<br/>"))
  })

  message("process individual country ", country)
  # Data ----
  country_data <-  data %>%
      filter(Country.Region %in% country) %>%
      filter(contagion_day > 0) %>%
      arrange(desc(date))

  output$ind_missing_days <- renderText({
    HTML(
      message_missing_country_days(country_data)
    )})

  lw_country_data = lw_vars_calc(country_data)
  pw_country_data = lw_vars_calc(country_data, 14)

  # create datasets for box merging today with data7
  country_data_today = country_data %>% add_growth_death_rate() %>%
    left_join(lw_country_data %>% select(-population)) %>%
    left_join(pw_country_data %>% select(-population))

  # country_data_today <- country_data %>%
  #     filter(date == max(date))
  vaxflag = check_flag(country_data, "vaccines")
  message("vaxflag = ", vaxflag)
  vaxarg = NULL
  if (vaxflag)
    vaxarg = "recovered"

  # Boxes ----
  callModule(mod_caseBoxes_server, "ind_count-boxes", country_data_today, vax = vaxarg)

  # Boxes ----
  callModule(mod_caseBoxes_server, "ind_count-boxes_hosp", country_data_today, hosp = TRUE)


  statuseslineplot = c("confirmed", "deaths", "recovered", "active")
  if (vaxflag)
    statuseslineplot = c("confirmed", "deaths", "vaccines", "active")

  callModule(mod_bar_plot_day_contagion_server, "ind_bar_plot_day_contagion", country_data, nn = nn, statuses = statuseslineplot)

  # tables ----
  callModule(mod_add_table_server, "ind_add_table_country", country_data,  maxrowsperpage = 10)
  # plots ----
  levs <- areaplot_vars()
  country_data_area = country_data
  active_hosp = FALSE
  if (sum(country_data$hosp, na.rm = TRUE)>0) {
    message("Adding hospitalized data for areaplot for ", country)
    levs = c(levs, "hosp")
    active_hosp = TRUE
  }
  message("n for ", country, " = ", nn)

  # for country plot start from the beginning
  df_tot = tsdata_areplot(country_data_area, levs, nn = nn) # start from day with >nn

  callModule(mod_plot_log_linear_server, "ind_plot_area_tot", df = df_tot, type = "area", active_hosp = active_hosp)

  # for country plot start from the beginning
  levs <- areaplot_hospvars()

  df_hosp = tsdata_areplot(country_data_area, levs, nn = 1) # start from day with >nn

  callModule(mod_plot_log_linear_server, "ind_plot_areahosp_tot", df = df_hosp, type = "area", hosp = TRUE)


  #callModule(mod_compare_nth_cases_plot_server, "ind_lines_points_plots_tot", country_data, nn = nn, istop = FALSE, tests = TRUE, hosp = TRUE, strindx = TRUE, oneMpop = FALSE, vax = vaxflag)#, secondline = "stringency_index")
  callModule(mod_compare_timeline_plot_server, "ind_lines_points_plots_tot", country_data , istop = FALSE, tests = TRUE, hosp = TRUE, strindx = TRUE, nn = nn, oneMpop = FALSE, vax = vaxflag)#, secondline = "stringency_index")

  callModule(mod_vaccines_text_server, "ind_vaccines_text_plot", country_data, country_data_today)

# # ##### country split within areas #############################################

  #   # Data ----
  if (is.null(data2)) {
    area_data_2 = get_datahub(country = country, lev = 2, verbose = FALSE)

    area_data_2 = area_data_2 %>%
      get_timeseries_by_contagion_day_data()

  } else
    area_data_2 = data2


  area_data_2_aggregate <-
    build_data_aggr(area_data_2)


  observeEvent(input$button_2nd_level, {

    output$ch_2nd_level_ui <- renderUI({
      tagList(
        div(
          HTML(from_nth_case_area2_msg(n2)), class = "bodytext"
        ),
        #hr(),
        div(
          htmlOutput(ns("ind_missing_days_area2")), class = "bodytext"
        ),
        fluidRow(
          column(12,
                 #withSpinner(mod_group_plot_ui(ns("ind_country_hosp"), type = "hosp", infotext = FALSE, titlesection = FALSE))
                 withSpinner(mod_group_plot_ui(ns("ind_country_confirmed_2"), type = "confirmed"))
          )
        ),
        hr(),
        fluidRow(
          column(6,
                 div(h4("COVID-19 evolution over time"), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
                 #withSpinner(uiOutput(ns("plot_area_area2")))
                 withSpinner( mod_plot_log_linear_ui(ns("plot_ind_area2"), select = TRUE, area = TRUE))
          ),
          column(6,
                 withSpinner(uiOutput(ns("plot_compare_nth_ind_area2")))

          )
        ),
        fluidRow(
          column(12,
                 #withSpinner(mod_group_plot_ui(ns("ind_country_hosp"), type = "hosp", infotext = FALSE, titlesection = FALSE))
                 withSpinner(mod_group_plot_ui(ns("ind_country_hosp_2"), type = "hosp", infotext = TRUE, titlesection = TRUE))

          )
        ),
        fluidRow(
          column(6,
                 div(h4("Evolution over time of Hospitalizations"), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
                 #withSpinner(uiOutput(ns("plot_areahosp_area2")))
                 withSpinner(mod_plot_log_linear_ui(ns("plot_areahosp2_ind_area2"), select = TRUE, area = TRUE))
          ),
          column(6,
                 withSpinner(uiOutput(ns("plot_compare_hosp_nth_ind_area2")))
          ),
        ),
        fluidRow(
          column(12,
                 withSpinner(mod_group_plot_ui(ns("ind_country_vax_2"), type = "vaccines"))
          )
        ),
        # hr(),
        # withSpinner(uiOutput(ns("maps_ind_subarea"))),
        hr(),
        mod_add_table_ui(ns("add_table_subarea"))

      )
    })

    # DATA area 2

    area_data_2_aggregate_today = area_data_2_aggregate %>%
      add_growth_death_rate()

    lw_data_2 =  lw_vars_calc(area_data_2_aggregate)
    pw_data_2 =  lw_vars_calc(area_data_2_aggregate, 14)

    area_data_2_aggregate_today = area_data_2_aggregate_today  %>%
      left_join(lw_data_2 %>% select(-population)) %>%
      left_join(pw_data_2 %>% select(-population))

    areas <- #reactive({
      area_data_2_aggregate_today %>%
      filter(confirmed > 10) %>%
      select(Country.Region) %>%
      distinct() %>% .$Country.Region

    # output$ind_subarea <- renderUI({
    #   areaUI(ns("ind_country_subarea"), tab = FALSE, stringency = FALSE, vaxflag = FALSE)
    #   #areaUI("ind_country_subarea")
    # })
    output$ind_missing_days_area2 <- renderText({
      HTML(
        message_missing_country_days(area_data_2)
      )})

    n2 <- 10

    # callModule(mod_country_area_server, "ind_country_subarea", data = area_data_2_aggregate, n2 = n2, tab = FALSE, hospitalFlag = FALSE, stringencyFlag = FALSE, vaccinesFlag = FALSE, country = "Switzerland")
    testsflag = check_flag(area_data_2_aggregate, "tests")

    # confirmed session
    relevant_countries = unique(area_data_2_aggregate_today$Country.Region[area_data_2_aggregate_today$confirmed>n2])

    callModule(mod_group_plot_server, "ind_country_confirmed_2", area_data_2_aggregate_today, type = "confirmed", istop = FALSE,
               # scatterplotargs = list(nmed = n2, countries = relevant_countries),
               tests = testsflag,
               scatterplotargs = list(nmed = 10),
               barplotargs = list(#pickvariable = list("plot_1" = "lm_confirmed_rate_1M_pop")
                 sortbyvar = TRUE))

    levs <- areaplot_vars()
    data_area = area_data_2
    active_hosp = FALSE
    hospflag = FALSE
    if (sum(data$hosp, na.rm = TRUE)>0) {
      message("Adding hospitalized data for areaplot")
      levs = c(levs, "hosp")
      active_hosp = TRUE
      hospflag = TRUE
    }
    oneMpopflag = check_flag(area_data_2, "population")

    # relevant_countries = unique(data_area$Country.Region[data_area$confirmed>n2])
    df_area_2 = purrr::map(relevant_countries,
                           function(un) {
                             dat = tsdata_areplot(area_data_2[area_data_2$Country.Region == un, ], levs, nn = n2) #n = 0 for area plot
                             dat$Country.Region = rep(un, nrow(dat))
                             dat
                           })
    df_area_2 = Reduce("rbind",df_area_2)


    areas <- #reactive({
      area_data_2 %>%
      select(Country.Region) %>%
      distinct() #%>% .$Country.Region

    callModule(mod_plot_log_linear_server, "plot_ind_area2", df = df_area_2, type = "area" , countries = reactive(areas), active_hosp = active_hosp)


    levs <- areaplot_hospvars()
    #
    #relevant_countries = unique(data_area$Country.Region[data_area$confirmed>1])
    #
    df_area_2 = purrr::map(relevant_countries,
                           function(un) {
                             dat = tsdata_areplot(area_data_2[area_data_2$Country.Region == un, ], levs, nn = 1) #n = 0 for area plot hosp, do not filter
                             dat$Country.Region = rep(un, nrow(dat))
                             dat
                           })
    df_area_2 = Reduce("rbind",df_area_2)

    callModule(mod_plot_log_linear_server, "plot_areahosp2_ind_area2", df = df_area_2, type = "area" , countries = reactive(areas), hosp = TRUE)



    strFlag = check_flag(area_data_2_aggregate, "stringency_index")
    # # do not use stringency v is the same for all areas

    vaxFlag = check_flag(area_data_2_aggregate, "vaccines")
    # do not use vax is the same for all areas

    message("hospflag: ", hospflag, "/ oneMpopflag: ", oneMpopflag,"/ strFlag: ", strFlag,"/ testsflag: ", testsflag, "/ vaxFlag: ", vaxFlag)

    # paste0("lines_plots_area2_",country) because  of problems with selectInputID after USA page. not solved, TBD
    output[["plot_compare_nth_ind_area2"]] <- renderUI({
      mod_compare_nth_cases_plot_ui(ns(paste0("lines_plots_ind_area2_",country)), vars = setdiff(.vars_nthcases_plot, prefix_var(.hosp_vars, c("", "new"))),
                                    nn = n2, istop = FALSE, tests = testsflag, hosp = FALSE, strindx = strFlag,vax = vaxFlag, selectvar = "new_confirmed", oneMpop = oneMpopflag, areasearch = TRUE)
    })

    callModule(mod_compare_nth_cases_plot_server, paste0("lines_plots_ind_area2_",country), df = area_data_2_aggregate, nn = n2,  istop = FALSE, tests = testsflag, hosp = FALSE, strindx = strFlag ,vax = vaxFlag,
               n_highlight = length(unique(area_data_2_aggregate$Country.Region)), oneMpop = oneMpopflag, areasearch = TRUE,
               vars = setdiff(.vars_nthcases_plot, prefix_var(.hosp_vars, c("", "new"))))

    output[["plot_compare_hosp_nth_ind_area2"]] <- renderUI({
      mod_compare_nth_cases_plot_ui(ns(paste0("lines_plots_hosp_ind_area2_",country)), vars = intersect(.vars_nthcases_plot, prefix_var(.hosp_vars, c("", "new"))),
                                    nn = n2, istop = FALSE, tests = FALSE, hosp = hospflag, strindx = FALSE,vax = vaxFlag, selectvar = "new_hosp", oneMpop = oneMpopflag, areasearch = TRUE)
    })
    callModule(mod_compare_nth_cases_plot_server, paste0("lines_plots_hosp_ind_area2_",country), df = area_data_2_aggregate, nn = n2,  istop = FALSE, tests = FALSE, hosp = hospflag, strindx = FALSE ,vax = vaxFlag,
               n_highlight = length(unique(area_data_2_aggregate$Country.Region)), oneMpop = oneMpopflag, areasearch = TRUE,
               vars = intersect(.vars_nthcases_plot, prefix_var(.hosp_vars, c("", "new"))))

    callModule(mod_group_plot_server, "ind_country_hosp_2", data_today = area_data_2_aggregate_today, nn = n2, type = "hosp", istop = FALSE,
               scatterplotargs = list(nmed = n2),
               barplotargs = list(#pickvariable = list("plot_1" = "lm_confirmed_rate_1M_pop")
                 sortbyvar = TRUE))

    callModule(mod_group_plot_server, "ind_country_vax_2", data_today = area_data_2_aggregate_today, nn = n2, type = "vaccines", istop = FALSE,
               scatterplotargs = list(nmed = n2),
               barplotargs = list(#pickvariable = list("plot_1" = "lm_confirmed_rate_1M_pop")
                 sortbyvar = TRUE))


    # output$maps_ind_subarea <- renderUI({
    #   areamapUI(ns("maps_subarea"), country)
    # })
    # callModule(mod_country_area_maps_server, "maps_subarea", data = area_data_2_aggregate, country = country)
    #
    # # prepare data for table with country data
    area_data_2_aggregate_tab = area_data_2_aggregate %>% # only data from today
      filter(date == AsOfDate) %>%
      arrange(desc(confirmed) )

    # add tables UIs
    callModule(mod_add_table_server, "add_table_subarea",
               area_data_2_aggregate_tab, maxrowsperpage = 10)


  }, once = TRUE)

  observeEvent(input$button_2nd_level_maps, {

    output$ch_2nd_level_maps_ui <- renderUI({
      tagList(
        hr(),
       #withSpinner(uiOutput(ns("maps_ind_subarea")))
       areamapUI(ns("maps_subarea"), country)

       )

    })


    callModule(mod_country_area_maps_server, "maps_subarea", data = area_data_2_aggregate, country = country)

    # prepare data for table with country data
    area_data_2_aggregate_tab = area_data_2_aggregate %>% # only data from today
      filter(date == AsOfDate) %>%
      arrange(desc(confirmed) )
  }, once = TRUE)


}
#' ind_country Server Function
#'
#' @param data data.frame
#' @param country character vector
#' @import dplyr
#' @import tidyr
#' @import shiny
#'
#' @noRd
mod_country_area_maps_server <- function(input, output, session, data, country){
  ns <- session$ns

  message("mod_country_area_maps_server")

  message("get map of ", country)
  .adjustmap = function(spmap, country) {
    spmap$NAME = factor(spmap$NAME_1)

    spmap$NAME = gsub("\"", "", spmap$NAME)
    spmap$NAME = gsub("^", "", spmap$NAME, fixed = TRUE)
    spmap$NAME = gsub("`", "", spmap$NAME, fixed = TRUE)
    if (country == "Switzerland") {
      spmap$NAME = recode(spmap$NAME, "Basel-Landschaft" = "Baselland",
                          "Sankt Gallen" = "St.Gallen",
                          "Lucerne" = "Luzerne")
    }
    spmap
  }
  .getmap <- function(country) {
    if (country == "Switzerland")
      area2_data_map = leaflet::gadmCHE
    else
      stop("Map for ", country, " not available")
    .adjustmap(area2_data_map, country)
  }
  area2_map = .getmap(country)

  message("process at level 2 individual country ", country)
  # Data ----
  country_data_today <-  data %>%
    add_growth_death_rate()
    #filter(Country.Region %in% country) %>%
    # filter(contagion_day > 0) %>%
    # arrange(desc(date))

  # Compute Last week variables

  data7_aggregate = lw_vars_calc(data)
  data14_aggregate = lw_vars_calc(data, 14)

  # create datasets for maps merging today with data7
  data_maps = country_data_today %>% #filter(date == max(date)) %>%
    right_join(data7_aggregate %>% select(-population)) %>%
    right_join(data14_aggregate %>% select(-population))
  # #maps confirmed

  callModule(mod_map_area_calc_server, "map_ind_confirmed", df = data_maps,  area2_map,
             area = country, variable = "confirmed", max.pop = 0, countrymap = TRUE)

  #maps vaccines
  callModule(mod_map_area_calc_server, "map_ind_vaccines", df = data_maps,  area2_map,
             area = country, variable = "vaccines", max.pop = 0, countrymap = TRUE)

  #maps growth vs prev
  callModule(mod_map_area_calc_server, "map_ind_growthvsprev", df = data_maps,  area2_map,
             area = country, variable = "growth vs prev", max.pop = 0, countrymap = TRUE)

  #maps prevalence
  callModule(mod_map_area_calc_server, "map_ind_growth", df = data_maps,  area2_map,
             area = country, variable = "growth factor", max.pop = 0, countrymap = TRUE)

  #maps death

  callModule(mod_map_area_calc_server, "map_ind_death", df = data_maps,  area2_map,
             area = country, variable = "death", max.pop = 0, countrymap = TRUE)

  #maps hosp

  callModule(mod_map_area_calc_server, "map_ind_hosp", df = data_maps,  area2_map,
             area = country, variable = "hospitalized", max.pop = 0, countrymap = TRUE)

  #maps hosp per population

  # callModule(mod_map_area_calc_server, "map_ind_hosp_1M_pop", df = data_maps,  area2_map,
  #            area = country, variable = "hospitalized over 1M", max.pop = 0, countrymap = TRUE)
}


