  if (interactive()) {

    #sapply(file.path("R",list.files("R")), source)
    #devtools::load_all()
    ui <- fluidPage(
      tagList(
        Covid19Mirai:::golem_add_external_resources(),
        mod_compare_nth_cases_plot_ui("plot_compare_nth", nn = 1000)
      )
    )
    server <- function(input, output, session) {
      # Data ----
      orig_data_with_ch = readRDS(file =  file.path(system.file("./datahub", package = "Covid19Mirai"),rds_data))

      orig_data_aggregate       = orig_data_with_ch$orig_data_aggregate
      orig_data_ch_2  = orig_data_with_ch$orig_data_ch_2

      nn = 1000;
     #  data_filtered <- #reactive({
     #    orig_data_aggregate %>%
     #      rescale_df_contagion(n = n, w = w)
     # # })
      callModule(mod_compare_nth_cases_plot_server, "plot_compare_nth", orig_data_aggregate, nn = nn)
    }
    runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
  }

if (interactive()) {


  ui <- fluidPage(
    tagList(
      Covid19Mirai:::golem_add_external_resources(),
      mod_compare_nth_cases_plot_ui("lines_points_plots", istop = FALSE, oneMpop = TRUE, tests = TRUE)
    )
  )
  server <- function(input, output, session) {
    # Data ----
    DATA <- readRDS(system.file("datahub/DATA.rds", package = "Covid19Mirai"))
    orig_data_aggregate <- DATA$orig_data_aggregate

    n = 1000;

    countries = c("Switzerland", "Italy", "France")

    countries_data <- #reactive({
      countries_data <- orig_data_aggregate %>%
        filter(Country.Region %in% countries)
    #})
    callModule(mod_compare_nth_cases_plot_server, "lines_points_plots", countries_data,
               nn = n, n_highlight = length(countries), istop = FALSE, oneMpop = TRUE, tests = TRUE)
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}


if (interactive()) {

  ui <- fluidPage(
    tagList(
      Covid19Mirai:::golem_add_external_resources(),
      mod_compare_nth_cases_plot_ui("lines_points_plots", strindx = FALSE, istop = FALSE)
    )
  )
  server <- function(input, output, session) {
    # Data ----
    DATA <- readRDS(system.file("datahub/DATA.rds", package = "Covid19Mirai"))
    orig_data_aggregate <- DATA$orig_data_aggregate

    n = 1000
    countries = c("Switzerland")

    countries_data <-
      countries_data <- orig_data_aggregate %>%
        filter(Country.Region %in% countries)

    callModule(mod_compare_nth_cases_plot_server, "lines_points_plots", countries_data,
               nn = n, n_highlight = length(countries), istop = FALSE, strindx = TRUE, secondline = "stringency_index")
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}


if (interactive()) {
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(plotly)
  library(ggplot2)

  ui <- fluidPage(
    tagList(
      Covid19Mirai:::golem_add_external_resources(),
      mod_compare_nth_cases_plot_ui("lines_points_plots", istop = FALSE, actives = FALSE, tests = TRUE, hosp = TRUE, selectvar = "new_confirmed", oneMpop = TRUE, strindx = TRUE)
    )
  )
  server <- function(input, output, session) {
    # Data ----
    orig_data <- get_datahub(country = "USA", lev = 2) %>%
      get_timeseries_by_contagion_day_data()

    # selected_country_data <- readRDS(system.file("datahub/Selected_Country.rds", package = "Covid19Mirai"))$area_data_2

    #pop_data = get_pop_datahub()
    #orig_data_aggregate = build_data_aggr(orig_data, pop_data)
    orig_data_aggregate = build_data_aggr(orig_data)

    n = 1000
    countries = c("Italy","USA")
    countries = unique(orig_data_aggregate$Country.Region)

    countries_data <-
      countries_data <- orig_data_aggregate %>%
      filter(Country.Region %in% countries)

    callModule(mod_compare_nth_cases_plot_server, "lines_points_plots", countries_data,
               nn = n, n_highlight = length(countries), istop = FALSE, actives = FALSE, tests = TRUE, hosp = TRUE, oneMpop = TRUE, strindx = TRUE)#, secondline = "stringency_index")
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}
# search country option

if (interactive()) {

  ui <- fluidPage(
    tagList(
      Covid19Mirai:::golem_add_external_resources(),
      uiOutput("lines_points_plots_ui", istop = FALSE)
    )
  )
  server <- function(input, output, session) {
    # Data ----
    DATA <- readRDS(system.file("datahub/DATA.rds", package = "Covid19Mirai"))
    orig_data_aggregate <- DATA$orig_data_aggregate

    n = 1000
    countries_data <-
      countries_data <- orig_data_aggregate %>%
      filter(continent %in% "Europe")

    output[["lines_points_plots_ui"]] <- renderUI({
      mod_compare_nth_cases_plot_ui("lines_points_plots", tests = TRUE, hosp = TRUE, actives = FALSE, strindx = TRUE, selectvar = "new_confirmed", oneMpop = TRUE, areasearch = TRUE)
    })

    callModule(mod_compare_nth_cases_plot_server, "lines_points_plots", countries_data,
               nn = n, n_highlight = length(countries_data$Country.Region), istop = FALSE, actives = FALSE, tests = TRUE, hosp = TRUE, oneMpop = TRUE, strindx = TRUE, areasearch = TRUE)#, secondline = "stringency_index")
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}


if (interactive()) {

  ui <- fluidPage(
    tagList(
      Covid19Mirai:::golem_add_external_resources(),
      uiOutput("lines_points_plots_ui", istop = FALSE)
    )
  )
  server <- function(input, output, session) {
    # Data ----
    orig_data <- get_datahub(country = "Germany", lev = 2) %>%
      get_timeseries_by_contagion_day_data()

    orig_data_aggregate = build_data_aggr(orig_data)

    n = 1000


    output[["lines_points_plots_ui"]] <- renderUI({
      mod_compare_nth_cases_plot_ui("lines_points_plots", tests = TRUE, hosp = FALSE, actives = FALSE, strindx = TRUE, selectvar = "new_confirmed", oneMpop = FALSE, areasearch = TRUE)
    })

    callModule(mod_compare_nth_cases_plot_server, "lines_points_plots", orig_data_aggregate,
               nn = n, n_highlight = length(orig_data_aggregate$Country.Region), istop = FALSE, actives = FALSE, tests = TRUE, hosp = FALSE, oneMpop = FALSE, strindx = TRUE, areasearch = TRUE)#, secondline = "stringency_index")
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}



if (interactive()) {

    ui <- fluidPage(
      tagList(
        Covid19Mirai:::golem_add_external_resources(),
        uiOutput("lines_points_plots_ui", istop = FALSE)
      )
    )
    server <- function(input, output, session) {
      # Data ----
      orig_data <- get_datahub(country = "USA", lev = 2) %>%
        get_timeseries_by_contagion_day_data()

      orig_data_aggregate = build_data_aggr(orig_data)

      n = 1000


      output[["lines_points_plots_ui"]] <- renderUI({
        mod_compare_nth_cases_plot_ui("lines_points_plots", tests = FALSE, hosp = TRUE, actives = FALSE, strindx = FALSE, vax = TRUE, selectvar = "new_confirmed", oneMpop = TRUE, areasearch = TRUE)
      })

      callModule(mod_compare_nth_cases_plot_server, "lines_points_plots", orig_data_aggregate,
                 nn = n, n_highlight = length(orig_data_aggregate$Country.Region), istop = FALSE, actives = FALSE, tests = FALSE, vax = TRUE, hosp = TRUE, oneMpop = TRUE, strindx = FALSE, areasearch = TRUE)#, secondline = "stringency_index")
    }
    runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
  }


