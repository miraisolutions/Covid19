if (interactive()) {
 # devtools::load_all()

  ui <- fluidPage(
    tagList(
      Covid19Mirai:::golem_add_external_resources(),
      mod_stackedbarplot_ui("plot")
    )
  )
  server <- function(input, output, session) {
    # Data ----
    orig_data <- readRDS(system.file("datahub/DATA.rds", package = "Covid19Mirai"))$orig_data

    pop_data = get_pop_datahub()
    orig_data_aggregate = build_data_aggr(orig_data, pop_data)

    callModule(mod_stackedbarplot_status_server, "plot", orig_data_aggregate, active_hosp = TRUE, istop = TRUE)

  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

