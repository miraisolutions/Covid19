if (interactive()) {


  #sapply(file.path("R",list.files("R")), source)
  #devtools::load_all()
  long_title <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit."
  ui <- fluidPage(
    tabPanel("Continents",
             tabsetPanel(
               tabPanel("Summary",
                        id = "tab_global",
    mod_continent_comparison_ui("continent_comparison")
               )))
  )
  server <- function(input, output) {

    # Data ----
    orig_data_aggregate <- readRDS(system.file("datahub/DATA.rds", package = "Covid19Mirai"))$orig_data_aggregate

    n = 1000; w = 7
    # data_filtered <-
    #   orig_data_aggregate %>%
    #     Covid19Mirai:::rescale_df_contagion(n = n, w = w)


    # countries <- reactive({
    #   data_filtered() %>%
    #     select(Country.Region) %>%
    #     distinct()
    # })
    callModule(mod_continent_comparison_server, "continent_comparison", orig_data_aggregate = orig_data_aggregate, n = n, w = w, pop_data)
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

