if (interactive()) {
 # devtools::load_all()
  long_title <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit."
  ui <- fluidPage(
    tagList(
      Covid19Mirai:::golem_add_external_resources(),
      mod_caseBoxes_ui("boxes", outputui = TRUE)
    )
  )
  server <- function(input, output, session) {

    DATA <- readRDS(system.file("datahub/DATA.rds", package = "Covid19Mirai"))
    orig_data_aggregate <- DATA$orig_data_aggregate

    country_data = orig_data_aggregate %>%
      dplyr::filter(Country.Region == "France")

    country_data_today <- country_data %>%
      dplyr::filter(date == AsOfDate)

    lw_country_data = lw_vars_calc(country_data)
    pw_country_data =  lw_vars_calc(country_data, 14)

    country_data_today = country_data_today %>%
      dplyr::left_join(lw_country_data %>% dplyr::select(-population))  %>%
      dplyr::left_join(pw_country_data %>% dplyr::select(-population))

    vaxarg = "recovered"

    callModule(mod_caseBoxes_server, "boxes", counts = country_data_today, vax = vaxarg, renderui = TRUE)
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

if (interactive()) {
  # devtools::load_all()
  long_title <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit."
  ui <- fluidPage(
    tagList(
      Covid19Mirai:::golem_add_external_resources(),
      mod_caseBoxes_ui("boxes", hosp = TRUE)
    )
  )
  server <- function(input, output, session) {

    orig_data <- readRDS(system.file("datahub/DATA.rds", package = "Covid19Mirai"))$orig_data


    DATA <- readRDS(system.file("datahub/DATA.rds", package = "Covid19Mirai"))
    orig_data_aggregate <- DATA$orig_data_aggregate

    country_data = orig_data_aggregate %>%
      dplyr::filter(Country.Region == "France")

    country_data_today <- country_data %>%
      dplyr::filter(date == AsOfDate)

    lw_country_data = lw_vars_calc(country_data)
    pw_country_data =  lw_vars_calc(country_data, 14)

    country_data_today = country_data_today %>%
      dplyr::left_join(lw_country_data %>% dplyr::select(-population))  %>%
      dplyr::left_join(pw_country_data %>% dplyr::select(-population))


    callModule(mod_caseBoxes_server, "boxes", counts = country_data_today, hosp = TRUE)
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}
