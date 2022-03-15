if (interactive()) {
  #devtools::load_all()

  long_title <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit."
  ui <- fluidPage(
    mod_country_ui("country")
  )
  server <- function(input, output) {

    orig_data_with_ch = readRDS(file =  file.path(system.file("./datahub", package = "Covid19Mirai"),rds_data))

    orig_data_aggregate       = orig_data_with_ch$orig_data_aggregate
    orig_data_ch_2  = orig_data_with_ch$orig_data_ch_2

    n = 1000; w = 7

    data_filtered <-
      orig_data_aggregate %>%
        Covid19Mirai:::rescale_df_contagion(n = n, w = w)

    countries <- reactive({
      data_filtered %>%
        select(Country.Region) %>%
        distinct()
    })

    callModule(mod_country_server, "country",
               data = data_filtered, countries = countries, nn = n,  w = w)
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

