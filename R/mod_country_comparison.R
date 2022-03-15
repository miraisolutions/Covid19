#' country_comparison UI Function
#'
#' @description A shiny Module.
#'
#' @param id, Internal parameters for {shiny}.
#' @param nn min number of cases for used to filter country data
#'
#' @noRd
#'
#' @import shiny
#' @importFrom shinycssloaders withSpinner
mod_country_comparison_ui <- function(id, nn = 1000){
  ns <- NS(id)
  from_nth_case_msg = paste(
    message_conf_case("Areas", nn),
    #paste0("Only Countries with more than ", n.select, " confirmed cases can be chosen."),
    message_missing_recovered(),
    message_missing_data("Recovered and Tests", "some countries"),
    #paste0("1st day is the day when ", nn ," confirmed cases are reached.")
    #message_firstday(nn),
    sep = "<br/>")


  tagList(
    div(
      hr(),
      div(
        HTML(from_nth_case_msg), class = "bodytext"
        #uiOutput(ns("from_nth_case"))
      ),
      hr(),
      div(class = "plottext", style = "font-size:14px",
          selectInput(label = "Select Countries", inputId = ns("select_countries"), choices = NULL, selected = NULL, multiple = TRUE)
          )
    ),
    #tagList(
      div(h4("Countries Comparison"), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
      mod_lineplots_day_contagion_ui(ns("lineplots_day_contagion")),
    #),
    fluidRow(
      column(6,
             withSpinner(uiOutput(ns("lines_points_plots")))
      ),
      column(6,
             withSpinner(mod_barplot_ui(ns("rate_plots"), text = FALSE))

      )
    ),
    fluidRow(
      column(12,
             withSpinner(mod_group_plot_ui(ns("cmp_countries_conf"), type = "confirmed"))
      )
    ),
    fluidRow(
      column(12,
             withSpinner(mod_group_plot_ui(ns("cmp_countries_hosp"), type = "hosp"))
      )
    ),
    fluidRow(
      column(12,
             withSpinner(mod_group_plot_ui(ns("cmp_countries_str"), type = "stringency"))
      )
    ),
    fluidRow(
      column(12,
             withSpinner(mod_group_plot_ui(ns("cmp_countries_vax"), type = "vaccines"))
      )
    ),
    mod_add_table_ui(ns("add_table_countries"))
  )
}

#' country_comparison Server Function
#'
#' @param data data.frame with countries
#' @param nn min number of cases for used to filter country data
#' @param w number of days of outbreak. Default 7
#' @param n.select min number of cases for a country to be considered in selectInput.
#'
#' @example ex-country-comparison.R
#'
#' @import dplyr
#'
#' @noRd
mod_country_comparison_server <- function(input, output, session, data, countries, nn = 1000, w = 7, n.select){
  ns <- session$ns

  # Data ----

  observe(
    updateSelectInput(session, "select_countries", choices = sort(countries()$Country.Region), selected = c("Switzerland", "Italy"))
  )
  # output$from_nth_case<- renderUI({
  #   HTML(paste(
  #              message_conf_case("Areas", nn),
  #     #paste0("Only Countries with more than ", n.select, " confirmed cases can be chosen."),
  #              #paste0("Some countries are not providing Recovered data."),
  #              message_missing_data("Recovered and Tests", "some countries"),
  #              #paste0("1st day is the day when ", nn ," confirmed cases are reached.")
  #               message_firstday(nn),
  #     sep = "<br/>"))
  # })

  all_countries_data <- data %>%
    filter(contagion_day > 0) %>%
    arrange(desc(date))

  all_countries_data_today = all_countries_data %>%
    add_growth_death_rate()

  lw_all_countries_data =  lw_vars_calc(all_countries_data)
  pw_all_countries_data =  lw_vars_calc(all_countries_data, 14)


  all_countries_data_today = all_countries_data_today  %>%
    left_join(lw_all_countries_data %>% select(-population))  %>%
    left_join(pw_all_countries_data %>% select(-population))


  observeEvent(input$select_countries,{
    if (input$select_countries != "") {
      # Data ----

      countries_data <- all_countries_data %>%
          filter(Country.Region %in% input$select_countries) #%>%
          #arrange(desc(date))

      # idx = order(match(countries_data$Country.Region, input$select_countries))
      # countries_data = countries_data[idx, ]

      countries_data_today = all_countries_data_today %>%
        filter(Country.Region %in% input$select_countries)


      vaxflag = sum(countries_data$vaccines, na.rm = TRUE) > 0

      statuseslineplot = c("confirmed", "deaths", "recovered", "active")
      if (vaxflag)
        statuseslineplot = c("confirmed", "deaths", "vaccines", "active")

      callModule(mod_lineplots_day_contagion_server, "lineplots_day_contagion", countries_data, nn = nn, statuses = statuseslineplot)

      # Rate plots ----
      # output$rateplots <- renderUI({
      #   mod_barplot_ui(ns("rate_plots"))
      # })

      callModule(mod_barplot_server, "rate_plots", countries_data_today, n_highlight = length(input$select_countries), istop = FALSE,
                 g_palette = list("plot_1" = graph_palette[1:length(input$select_countries)], #barplots_colors$stringency,
                                  "plot_2" = graph_palette[1:length(input$select_countries)],
                                  calc = FALSE),
                 sortbyvar = FALSE)


      # Line with bullet plot

      output$lines_points_plots <- renderUI({
        mod_compare_nth_cases_plot_ui(ns("lines_points_plots"), istop = FALSE, nn = nn, tests = TRUE, hosp = TRUE, strindx = TRUE, selectvar = "new_confirmed", oneMpop = TRUE, vax = vaxflag)
      })

      callModule(mod_compare_nth_cases_plot_server, "lines_points_plots", countries_data, nn = nn, n_highlight = length(input$select_countries),
                 istop = FALSE, tests = TRUE, hosp = TRUE, strindx = TRUE,  oneMpop = TRUE, vax = vaxflag)


      inputcountries = reactive({input$select_countries}) # pass countries to plot below

      # set nmed to 10000 like in global page, istop == FALSE
      # callModule(mod_scatterplot_server, "scatterplot_plots", all_countries_data_today, nmed = 10000, n_highlight = length(input$select_countries), istop = FALSE, countries = inputcountries())
      #
      #
      # callModule(mod_stackedbarplot_status_server, "status_stackedbarplot", countries_data_today, n_highlight = length(input$select_countries), istop = FALSE)
      callModule(mod_group_plot_server, "cmp_countries_conf", data_today = countries_data_today, type = "confirmed", istop = FALSE,
                 scatterplotargs = list(countries = inputcountries(), nmed = nn),
                 barplotargs = list(pickvariable = list("plot_1" = "lm_confirmed_rate_1M_pop"),
                                    g_palette = list("plot_1" = graph_palette[1:length(input$select_countries)],                                                      calc = FALSE),
                                    sortbyvar = FALSE)
      )
      callModule(mod_group_plot_server, "cmp_countries_hosp", data_today = countries_data_today, type = "hosp", istop = FALSE,
                 scatterplotargs = list(countries = inputcountries(), nmed = nn),
                 barplotargs = list(pickvariable = list("plot_1" = "lm_confirmed_rate_1M_pop"),
                                    g_palette = list("plot_1" = graph_palette[1:length(input$select_countries)],                                                      calc = FALSE),
                                    sortbyvar = FALSE)
      )
      # set nmed to 10000 like in global page, istop == FALSE
      #callModule(mod_scatterplot_server, "scatterplot_stringency", all_countries_data_today, nmed = 10000, n_highlight = length(input$select_countries), istop = FALSE, countries = inputcountries(), xvar = "stringency_index", growth = FALSE, fitted = FALSE)


      # # > barplot stringency
      # callModule(mod_barplot_server, "barplot_stringency_index", countries_data_today, n_highlight = length(input$select_countries), istop = FALSE,
      #            plottitle = c("Stringency Index"),
      #            g_palette = list("plot_1" = graph_palette[1:length(input$select_countries)], #barplots_colors$stringency,
      #                             calc = FALSE),
      #            sortbyvar = FALSE)
      #
      # #scatterplot vax versus vars
      # callModule(mod_scatterplot_server, "scatterplot_vax_vars",
      #            all_countries_data_today, nmed = nn, n_highlight = length(input$select_countries),
      #            istop = FALSE, countries = inputcountries(), xvar = "vaccines_rate_pop", growth = FALSE, fitted = FALSE)
      callModule(mod_group_plot_server, "cmp_countries_vax", data_today = countries_data_today, type = "vaccines", istop = FALSE,
                 scatterplotargs = list(countries = inputcountries(), nmed = nn),
                 barplotargs = list(pickvariable = list("plot_1" = "lm_confirmed_rate_1M_pop"),
                                    g_palette = list("plot_1" = graph_palette[1:length(input$select_countries)],  calc = FALSE),
                                    sortbyvar = FALSE)
                 )

      callModule(mod_group_plot_server, "cmp_countries_str", data_today = countries_data_today, type = "stringency", istop = FALSE,
                 scatterplotargs = list(countries = inputcountries(), nmed = nn),
                 barplotargs = list(pickvariable = list("plot_1" = "lm_confirmed_rate_1M_pop"),
                                    g_palette = list("plot_1" = graph_palette[1:length(input$select_countries)],  calc = FALSE),
                                    sortbyvar = FALSE))

      #barplot vax

      # callModule(mod_barplot_server, "barplot_vax_index", countries_data_today,
      #            n_highlight = length(input$select_countries), istop = FALSE,
      #            plottitle = c("Vaccination Status"),
      #            g_palette = list("plot_1" = graph_palette[1:length(input$select_countries)],
      #                             calc = FALSE),
      #            sortbyvar = FALSE)


      # tables ----
      callModule(mod_add_table_server, "add_table_countries", countries_data, maxrowsperpage = 10)
    }
  })

}
