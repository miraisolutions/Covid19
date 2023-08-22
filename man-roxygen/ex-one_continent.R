if (interactive()) {

  #sapply(file.path("R",list.files("R")), source)
  #devtools::load_all()

  long_title <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit."
  cont = "Europe"
  uicont = tolower(cont)
  ui <- fluidPage(
    tabPanel("Continents",
             tabsetPanel(
               tabPanel(cont,
                        id = "tab_global",
    mod_continent_ui("cont_comparison", uicont)
               )))
  )
  server <- function(input, output) {

    DATA <- readRDS(system.file("datahub/DATA.rds", package = "Covid19Mirai"))
    orig_data_aggregate <- DATA$orig_data_aggregate
    pop_data <- DATA$pop_data
    countries_data_map <- DATA$countries_data_map
    n = 1000; w = 7
    # data_filtered <- reactive({
    #   orig_data_aggregate_cont() %>%
    #     rescale_df_contagion(n = n, w = w)
    # })

    callModule(mod_continent_server, "cont_comparison", orig_data_aggregate = orig_data_aggregate,
               countries_data_map, nn = n, w = w, pop_data, cont = cont, uicont = uicont)
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

