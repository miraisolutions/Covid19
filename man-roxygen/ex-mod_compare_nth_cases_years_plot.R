if (interactive()) {

    #devtools::load_all()
    ui <- fluidPage(
      tagList(
        Covid19Mirai:::golem_add_external_resources(),
        mod_compare_nth_cases_years_plot_ui("plot_compare_nth")
      )
    )
    server <- function(input, output, session) {
      # Data ----
      # orig_data <- get_datahub() %>%
      #     get_timeseries_by_contagion_day_data()

      DATA <- readRDS(system.file("datahub/DATA.rds", package = "Covid19Mirai"))
      orig_data_aggregate <- DATA$orig_data_aggregate


      country_data = orig_data_aggregate %>% dplyr::filter(Country.Region == "Italy") # possibly with select also multiple countries possible
      df = country_data


      callModule(mod_compare_nth_cases_years_plot_server, "plot_compare_nth", country_data)
    }
    runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

if (interactive()) {

    #devtools::load_all()
    ui <- fluidPage(
      tagList(
        Covid19Mirai:::golem_add_external_resources(),
        mod_compare_timeline_plot_ui("plot_compare_nth", actives = TRUE, tests = TRUE, hosp = TRUE, strindx = TRUE, vax = TRUE)
      )
    )
    server <- function(input, output, session) {
      # Data ----
      DATA <- readRDS(system.file("datahub/DATA.rds", package = "Covid19Mirai"))
      orig_data_aggregate <- DATA$orig_data_aggregate


      country_data = orig_data_aggregate %>% filter(Country.Region == "Israel") # possibly with select also multiple countries possible
      df = country_data


      callModule(mod_compare_timeline_plot_server, "plot_compare_nth", country_data, actives = TRUE, tests = TRUE, hosp = TRUE, strindx = TRUE, vax = TRUE)
    }
    runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
  }
