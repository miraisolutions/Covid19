if (interactive()) {
  #devtools::load_all()


  #sapply(file.path("R",list.files("R")), source)
  #devtools::load_all()
  long_title <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit."
  ui <- fluidPage(
    mod_global_ui("global")
  )
  server <- function(input, output) {

    DATA <- readRDS(system.file("datahub/DATA.rds", package = "Covid19Mirai"))
    orig_data_aggregate <- DATA$orig_data_aggregate

    countries_data_map = DATA$countries_data_map

    callModule(mod_global_server, "global",
               orig_data_aggregate, countries_data_map)
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

