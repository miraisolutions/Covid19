if (interactive()) {
  #devtools::load_all()

  ui <- fluidPage(
    tagList(
      Covid19Mirai:::golem_add_external_resources(),
      mod_scatterplot_ui("plot")
    )
  )
  server <- function(input, output, session) {

    # Data ----
    DATA <- readRDS(system.file("datahub/DATA.rds", package = "Covid19Mirai"))
    orig_data_aggregate <- DATA$orig_data_aggregate

    lw_orig_data_aggregate =  lw_vars_calc(orig_data_aggregate)

    orig_data_aggregate_today = orig_data_aggregate %>%
      add_growth_death_rate()
    orig_data_aggregate_today = orig_data_aggregate_today  %>%
      left_join(lw_orig_data_aggregate %>% select(-population))

    inputcountries = c("Italy","Germany", "Switzerland", "Sweden", "UK", "France", "Spain", "Russia") # example with countries

    callModule(mod_scatterplot_server, "plot", orig_data_aggregate_today, countries = inputcountries, nmed = 10000, wmed = 7, n_highlight = 5, istop = FALSE)

  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}
# stringency index
if (interactive()) {
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(plotly)
  varsSelecty = list(label = "Select (x) Total Confirmed Cases",
                     choices = list("Over one week" = "lw_confirmed",
                                    #"Over 1 month" = "lm_confirmed",
                                    "Total" = "confirmed"),
                     selected = "confirmed")
  ui <- fluidPage(
    tagList(
      Covid19Mirai:::golem_add_external_resources(),

      mod_scatterplot_ui("plot", growth = FALSE)
    )
  )
  server <- function(input, output, session) {

    # Data ----
    DATA <- readRDS(system.file("datahub/DATA.rds", package = "Covid19Mirai"))
    orig_data_aggregate <- DATA$orig_data_aggregate


    lw_orig_data_aggregate =  lw_vars_calc(orig_data_aggregate)

    orig_data_aggregate_today = orig_data_aggregate %>%
      add_growth_death_rate()
    orig_data_aggregate_today = orig_data_aggregate_today  %>%
      left_join(lw_orig_data_aggregate %>% select(-population))


    inputcountries = c("Italy","Germany", "Switzerland", "Sweden", "UK", "France", "Spain", "Russia") # example with countries

    callModule(mod_scatterplot_server, "plot", orig_data_aggregate_today, countries = inputcountries, nmed = 10000, wmed = 7, n_highlight = 5, istop = FALSE, xvar = "stringency_index", growth = FALSE, fitted = FALSE)

  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

# stringency index vs other variables
if (interactive()) {
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(plotly)

  ui <- fluidPage(
    tagList(
      Covid19Mirai:::golem_add_external_resources(),
      mod_scatterplot_ui("plot", growth = FALSE)
    )
  )
  server <- function(input, output, session) {

    # Data ----
    DATA <- readRDS(system.file("datahub/DATA.rds", package = "Covid19Mirai"))
    orig_data_aggregate <- DATA$orig_data_aggregate

    lw_orig_data_aggregate =  lw_vars_calc(orig_data_aggregate)

    orig_data_aggregate_today = orig_data_aggregate %>%
      add_growth_death_rate()
    orig_data_aggregate_today = orig_data_aggregate_today  %>%
      left_join(lw_orig_data_aggregate %>% select(-population))

    #inputcountries = c("Italy","Germany", "Switzerland", "Sweden", "UK", "France", "Spain", "Russia") # example with countries

    callModule(mod_scatterplot_server, "plot", orig_data_aggregate_today, countries = inputcountries, nmed = 10000, wmed = 7, n_highlight = 10, istop = TRUE, xvar = "stringency_index", growth = FALSE, fitted = FALSE)

  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

if (interactive()) {
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(plotly)

  ui <- fluidPage(
    tagList(
      Covid19Mirai:::golem_add_external_resources(),
      Covid19Mirai:::mod_scatterplot_ui("plot_oceania")
    )
  )
  server <- function(input, output, session) {
    DATA <- readRDS(system.file("datahub/DATA.rds", package = "Covid19Mirai"))
    orig_data_aggregate <- DATA$orig_data_aggregate

    cont = "Oceania"

    subcontinent_pop_data =  pop_data %>% filter(!is.na(continent) & continent %in% cont) %>%
      group_by(subcontinent) %>%
      summarize(population = sum(population, na.rm = T))

    orig_data_aggregate_cont <- reactive({
      orig_data_aggregate %>% filter(continent == cont)})
    # select all variables
    allstatuses = get_aggrvars()
    subcontinent_data <- reactive({aggr_to_cont(orig_data_aggregate_cont(), "subcontinent", "date",
                                                subcontinent_pop_data, allstatuses)})
    subcontinents = reactive({unique(orig_data_aggregate_cont()$subcontinent)})


    callModule(mod_scatterplot_server, "plot_oceania", subcontinent_data, istop = F, countries = subcontinents)

  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}



# vaccinaction index vs other variables
if (interactive()) {
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(plotly)

  ui <- fluidPage(
    tagList(
      Covid19Mirai:::golem_add_external_resources(),
      mod_scatterplot_ui("plot", growth = FALSE)
    )
  )
  server <- function(input, output, session) {

    # Data ----
    DATA <- readRDS(system.file("datahub/DATA.rds", package = "Covid19Mirai"))
    orig_data_aggregate <- DATA$orig_data_aggregate

    lw_orig_data_aggregate =  lw_vars_calc(orig_data_aggregate)

    orig_data_aggregate_today = orig_data_aggregate %>%
      add_growth_death_rate()
    orig_data_aggregate_today = orig_data_aggregate_today  %>%
      left_join(lw_orig_data_aggregate %>% select(-population))

    #inputcountries = c("Italy","Germany", "Switzerland", "Sweden", "UK", "France", "Spain", "Russia") # example with countries
    orig_data_aggregate_today = orig_data_aggregate_today %>% filter(continent == "Europe" & population > 1000000)

    inputcountries = unique(orig_data_aggregate_today$Country.Region)

    callModule(mod_scatterplot_server, "plot", orig_data_aggregate_today, countries = inputcountries, nmed = 10000, wmed = 7, n_highlight = 10, istop = FALSE, xvar = "vaccines_rate_pop", growth = FALSE, fitted = TRUE)

  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}



# vaccinaction index vs other variables
if (interactive()) {
  library(shiny)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  library(plotly)

  ui <- fluidPage(
    tagList(
      Covid19Mirai:::golem_add_external_resources(),
      mod_scatterplot_ui("plot", growth = FALSE, hospvars = "keep")
    )
  )
  server <- function(input, output, session) {

    # Data ----
    DATA <- readRDS(system.file("datahub/DATA.rds", package = "Covid19Mirai"))
    orig_data_aggregate <- DATA$orig_data_aggregate

    lw_orig_data_aggregate =  lw_vars_calc(orig_data_aggregate)

    orig_data_aggregate_today = orig_data_aggregate %>%
      add_growth_death_rate()
    orig_data_aggregate_today = orig_data_aggregate_today  %>%
      left_join(lw_orig_data_aggregate %>% select(-population))

    #inputcountries = c("Italy","Germany", "Switzerland", "Sweden", "UK", "France", "Spain", "Russia") # example with countries
    orig_data_aggregate_today = orig_data_aggregate_today %>% filter(continent == "Europe" & population > 1000000)

    inputcountries = unique(orig_data_aggregate_today$Country.Region)

    callModule(mod_scatterplot_server, "plot", orig_data_aggregate_today, countries = inputcountries, nmed = 10000, wmed = 7, n_highlight = 10, istop = FALSE, growth = FALSE, fitted = FALSE, xvar = "lm_confirmed_rate_1M_pop")

  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}


