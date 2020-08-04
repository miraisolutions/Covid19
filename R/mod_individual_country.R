#' ind_country UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom shinycssloaders withSpinner
mod_ind_country_ui <- function(id){
  ns <- NS(id)
  tagList(
    hr(),
    tags$head(tags$style(HTML(".small-box {width: 300px; margin: 20px;}"))),
    mod_caseBoxes_ui(ns("ind_count-boxes")),
    hr(),
    div(
      uiOutput(ns("ind_from_nth_case"))
    ),
    fluidRow(
      withSpinner(uiOutput(ns("ind_barplots")))
    ),
    hr(),
    fluidRow(
      column(6,
             br(),
             div( h4("Total cases"), align = "center",
             div(style = "visibility: hidden", radioButtons("dummy1", "", choices = "dummy")),
             withSpinner(mod_plot_log_linear_ui(ns("ind_plot_area_tot"), area = TRUE))
             )
      ),
      column(6,
             withSpinner(mod_compare_nth_cases_plot_ui(ns("ind_lines_points_plots_tot")))
      )
    ),
    hr(),
    mod_add_table_ui(ns("ind_add_table_country")), # table at country level
    hr(),
    withSpinner(uiOutput(ns("ind_subarea"))),
    #tags$div(id = ns("ind_subarea")),

    hr(),
    #withSpinner(uiOutput(ns("maps_ind_subarea")))
    #tags$div(id = ns("maps_ind_subarea")),
    tagList(
      div(#id = id,
        hr(),
        #div(h3(paste("Country Heat Maps within", country)), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
        div(h3("Country Heat Maps within Switzerland"), align = "center", style = "margin-top:20px; margin-bottom:20px;"),

        hr(),
        fluidRow(
          column(6,
                 withSpinner(uiOutput(ns("map_countries_confirmed")))
          ),
          column(6,
                 withSpinner(uiOutput(ns("map_countries_active")))
          )
        ),
        fluidRow(
          column(6,
                 withSpinner(uiOutput(ns("map_countries_growthvsprev")))
          ),
          column(6,
                 withSpinner(uiOutput(ns("map_countries_prev")))
          )
        ),
        fluidRow(
          column(6,
                 withSpinner(uiOutput(ns("map_countries_growth")))
          ),
          column(6,
                 withSpinner(uiOutput(ns("map_countries_death")))
          )
        )
      )
    )

  )
}
#' Level 2 areas UI function for maps
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom shinycssloaders withSpinner
areamapUI = function(id, country){
  ns = NS(id)
  #uiarea = "swiss"
  tagList(
    div(#id = id,
        hr(),
        div(h3(paste("Country Heat Maps within", country)), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
        hr(),
        fluidRow(
          column(6,
                 withSpinner(uiOutput(ns("map_countries_confirmed")))
          ),
          column(6,
                 withSpinner(uiOutput(ns("map_countries_active")))
          )
        ),
        fluidRow(
          column(6,
                 withSpinner(uiOutput(ns("map_countries_growthvsprev")))
          ),
          column(6,
                 withSpinner(uiOutput(ns("map_countries_prev")))
          )
        ),
        fluidRow(
          column(6,
                 withSpinner(uiOutput(ns("map_countries_growth")))
          ),
          column(6,
                 withSpinner(uiOutput(ns("map_countries_death")))
          )
        )
    )
  )
}
#' ind_country Server Function
#'
#' @param data data.frame
#' @param countries reactive character vector
#' @param n min number of cases for used to filter country data
#' @param w number of days of outbreak. Default 7
#'
#' @import dplyr
#' @import tidyr
#' @import shiny
#'
#' @noRd
mod_ind_country_server <- function(input, output, session, data, country, n = 1, w = 7){
  ns <- session$ns

  message("mod_ind_country_server")
  output$ind_from_nth_case<- renderUI({
    HTML(paste(
         paste0("Some Cantons are not providing Recovered data."),
         paste0("Contagion day 0 is the day when ", n ," confirmed cases are reached."), sep = "<br/>"))
  })

  message("process individual country ", country)
  # Data ----
  country_data <-  data %>%
      filter(Country.Region %in% country) %>%
      filter(contagion_day > 0) %>%
      arrange(desc(date))

  country_data_today <- country_data %>%
      filter(date == max(date))

  # Boxes ----
  callModule(mod_caseBoxes_server, "ind_count-boxes", country_data_today)

  # tables ----
  callModule(mod_add_table_server, "ind_add_table_country", country_data,  maxrowsperpage = 10)
  # plots ----
  levs <- sort_type_hardcoded()
  country_data_area = country_data
  if (sum(country_data$hosp)>0) {
    message("Adding hospitalised data for ", country)
    levs = c(levs, "hosp")
    country_data_area$active = country_data_area$active - country_data_area$hosp
  }
  #n_country = select_n(country_data_area$confirmed,n)
  message("n for ", country, " = ", n)
  # for country plot start from the beginning
  df_tot = tsdata_areplot(country_data_area,levs, n) # start from day with >n

  callModule(mod_plot_log_linear_server, "ind_plot_area_tot", df = df_tot, type = "area")

  output$ind_barplots <- renderUI({
    mod_bar_plot_day_contagion_ui(ns("ind_bar_plot_day_contagion"))
  })

  callModule(mod_compare_nth_cases_plot_server, "ind_lines_points_plots_tot", country_data , n = n, w = w, istop = F)

  callModule(mod_bar_plot_day_contagion_server, "ind_bar_plot_day_contagion", country_data)

# # ##### country split within areas #############################################

#   # Data ----
  area_data_2 = get_datahub(country = country, lev = 2, verbose = FALSE)

  area_data_2 = area_data_2 %>%
    get_timeseries_by_contagion_day_data()

  area_data_2_aggregate <-
    build_data_aggr(area_data_2)

  output$ind_subarea <- renderUI({
    areaUI(ns("ind_country_subarea"))
  })
  callModule(mod_country_area_server, "ind_country_subarea", area_data_2_aggregate = area_data_2_aggregate, n2 = max(1,n/10))

  # output$maps_ind_subarea <- renderUI({
  #   areamapUI(ns("maps_ind_country_subarea"), country)
  # })
  # callModule(mod_country_area_maps_server, "maps_ind_country_subarea", data = area_data_2_aggregate, country = country)

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
  getmap <- function(country) {
    if (country == "Switzerland")
      area2_data_map = leaflet::gadmCHE
    else
      stop("Map for ", country, " not available")
    .adjustmap(area2_data_map, country)
  }
  area2_map = getmap(country)

  message("process individual country ", country)
  # area_data_2 = data %>%
  #   get_timeseries_by_contagion_day_data()
  #
  # area_data_2_aggregate <-
  #   build_data_aggr(area_data_2)

  # Data ----
  country_data <-  data %>%
    #filter(Country.Region %in% country) %>%
    filter(contagion_day > 0) %>%
    arrange(desc(date))

  # country_data_today <- country_data %>%
  #   filter(date == max(date))

  uiarea = "swiss"
  # Compute Last week variables

  data7_aggregate_cont = lw_vars_calc(country_data)

  # create datasets for maps merging today with data7
  data_maps = country_data %>% filter(date == max(date)) %>%
    left_join(data7_aggregate_cont %>% select(-population))

  #maps confirmed
  output[["map_countries_confirmed"]] <- renderUI({
    mod_map_area_calc_ui(ns("map_countries_confirmed"))
  })
  callModule(mod_map_area_calc_server, "map_countries_confirmed", df = data_maps,  area2_map,
             area = country, variable = "confirmed")

  #maps active
  output[["map_countries_active"]] <- renderUI({
    mod_map_area_calc_ui(ns("map_countries_active"))
  })
  callModule(mod_map_area_calc_server, "map_countries_active", df = data_maps,  area2_map,
             area = country, variable = "active")

  #maps growth vs prev
  output[["map_countries_growthvsprev"]] <- renderUI({
    mod_map_area_calc_ui(ns("map_countries_growthvsprev"))
  })
  callModule(mod_map_area_calc_server, "map_countries_growthvsprev", df = data_maps,  area2_map,
             area = country, variable = "growth vs prev")

  #maps prevalence
  output[["map_countries_prev"]] <- renderUI({
    mod_map_area_calc_ui(ns("map_countries_prev"))
  })
  callModule(mod_map_area_calc_server, "map_countries_prev", df = data_maps,  area2_map,
             area = country, variable = "prevalence rate")
  #maps growth
  output[["map_countries_growth"]] <- renderUI({
    mod_map_area_calc_ui(ns("map_countries_growth"))
  })
  callModule(mod_map_area_calc_server, "map_countries_growth", df = data_maps,  area2_map,
             area = country, variable = "growth factor")

  #maps death
  output[["map_countries_death"]] <- renderUI({
    mod_map_area_calc_ui(ns("map_countries_death"))
  })
  callModule(mod_map_area_calc_server, "map_countries_death", df = data_maps,  area2_map,
             area = country, variable = "death")

  # add mapp UIs
}
#' ind_country Server Function
#'
#' @param data data.frame
#' @param country character vector
#'
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
  getmap <- function(country) {
    if (country == "Switzerland")
      area2_data_map = leaflet::gadmCHE
    else
      stop("Map for ", country, " not available")
    .adjustmap(area2_data_map, country)
  }
  area2_map = getmap(country)

  message("process individual country ", country)
  # area_data_2 = data %>%
  #   get_timeseries_by_contagion_day_data()
  #
  # area_data_2_aggregate <-
  #   build_data_aggr(area_data_2)

  # Data ----
  country_data <-  data %>%
    #filter(Country.Region %in% country) %>%
    filter(contagion_day > 0) %>%
    arrange(desc(date))

  # country_data_today <- country_data %>%
  #   filter(date == max(date))

  uiarea = "swiss"
  # Compute Last week variables

  data7_aggregate_cont = lw_vars_calc(country_data)

  # create datasets for maps merging today with data7
  data_maps = country_data %>% filter(date == max(date)) %>%
    left_join(data7_aggregate_cont %>% select(-population))

  #maps confirmed
  output[["map_countries_confirmed"]] <- renderUI({
    mod_map_area_calc_ui(ns("map_countries_confirmed"))
  })
  callModule(mod_map_area_calc_server, "map_countries_confirmed", df = data_maps,  area2_map,
             area = country, variable = "confirmed")

  #maps active
  output[["map_countries_active"]] <- renderUI({
    mod_map_area_calc_ui(ns("map_countries_active"))
  })
  callModule(mod_map_area_calc_server, "map_countries_active", df = data_maps,  area2_map,
             area = country, variable = "active")

  #maps growth vs prev
  output[["map_countries_growthvsprev"]] <- renderUI({
    mod_map_area_calc_ui(ns("map_countries_growthvsprev"))
  })
  callModule(mod_map_area_calc_server, "map_countries_growthvsprev", df = data_maps,  area2_map,
             area = country, variable = "growth vs prev")

  #maps prevalence
  output[["map_countries_prev"]] <- renderUI({
    mod_map_area_calc_ui(ns("map_countries_prev"))
  })
  callModule(mod_map_area_calc_server, "map_countries_prev", df = data_maps,  area2_map,
             area = country, variable = "prevalence rate")
  #maps growth
  output[["map_countries_growth"]] <- renderUI({
    mod_map_area_calc_ui(ns("map_countries_growth"))
  })
  callModule(mod_map_area_calc_server, "map_countries_growth", df = data_maps,  area2_map,
             area = country, variable = "growth factor")

  #maps death
  output[["map_countries_death"]] <- renderUI({
    mod_map_area_calc_ui(ns("map_countries_death"))
  })
  callModule(mod_map_area_calc_server, "map_countries_death", df = data_maps,  area2_map,
             area = country, variable = "death")

}


