if (interactive()) {
  library(shiny)
  #sapply(file.path("R",list.files("R")), source)
  #devtools::load_all()
  ui <- fluidPage(
    tagList(
      Covid19Mirai:::golem_add_external_resources(),
      Covid19Mirai:::mod_barplot_ui("plot")
    )
  )
  server <- function(input, output, session) {

    # Data ----
    # Data ----
    orig_data <- readRDS(system.file("datahub/DATA.rds", package = "Covid19Mirai"))$orig_data

    pop_data = get_pop_datahub()
    orig_data_aggregate = build_data_aggr(orig_data, pop_data)

    lw_orig_data_aggregate =  lw_vars_calc(orig_data_aggregate)
    pw_orig_data_aggregate =  lw_vars_calc(orig_data_aggregate,14)

    orig_data_aggregate_today = orig_data_aggregate %>%
      add_growth_death_rate()
    orig_data_aggregate_today = orig_data_aggregate_today  %>%
      left_join(lw_orig_data_aggregate %>% select(-population))  %>%
      left_join(pw_orig_data_aggregate %>% select(-population))

    #inputcountries = c("Italy","Germany", "Switzerland", "Sweden", "UK", "France", "Spain", "Russia") # example with countries


    callModule(mod_barplot_server, "plot", orig_data_aggregate_today, n_highlight = 10, istop = TRUE)
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

#################### Stringency

if (interactive()) {
  library(shiny)
  #sapply(file.path("R",list.files("R")), source)
  #devtools::load_all()
  ui <- fluidPage(
    tagList(
      Covid19Mirai:::golem_add_external_resources(),
      mod_barplot_ui("plot", plot1 = "ui_stringency", plot2 = "ui_stringency")
    )
  )
  server <- function(input, output, session) {

    # Data ----
    # Data ----
    orig_data <- readRDS(system.file("datahub/DATA.rds", package = "Covid19Mirai"))$orig_data

    pop_data = get_pop_datahub()
    orig_data_aggregate = build_data_aggr(orig_data, pop_data)

    lw_orig_data_aggregate =  lw_vars_calc(orig_data_aggregate)
    pw_orig_data_aggregate =  lw_vars_calc(orig_data_aggregate,14)

    orig_data_aggregate_today = orig_data_aggregate %>%
      add_growth_death_rate()
    orig_data_aggregate_today = orig_data_aggregate_today  %>%
      left_join(lw_orig_data_aggregate %>% select(-population))  %>%
      left_join(pw_orig_data_aggregate %>% select(-population))

    inputcountries = c("Italy","Germany", "Switzerland", "Sweden", "UK", "France", "Spain", "Russia") # example with countries

    str_palette = c(col = "Greys", rev = TRUE, skip = 3)

    callModule(mod_barplot_server, "plot", orig_data_aggregate_today, plottitle = c("Highest Stringency Index", "Stringency Index"),
               g_palette = list("plot_1" = str_palette,
                                "plot_2" = str_palette,
                                calc = TRUE),
               pickvariable = list("plot_1" = character(), "plot_2" = "confirmed"))
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}


if (interactive()) {
  library(shiny)
  #sapply(file.path("R",list.files("R")), source)
  #devtools::load_all()
  ui <- fluidPage(
    tagList(
      Covid19Mirai:::golem_add_external_resources(),
      mod_barplot_ui("plot", plot1 = "ui_stringency", plot2  = NULL)
    )
  )
  server <- function(input, output, session) {

    # Data ----
    # Data ----
    orig_data <- readRDS(system.file("datahub/DATA.rds", package = "Covid19Mirai"))$orig_data

    pop_data = get_pop_datahub()
    orig_data_aggregate = build_data_aggr(orig_data, pop_data)

    lw_orig_data_aggregate =  lw_vars_calc(orig_data_aggregate)
    pw_orig_data_aggregate =  lw_vars_calc(orig_data_aggregate,14)

    orig_data_aggregate_today = orig_data_aggregate %>%
      add_growth_death_rate()
    orig_data_aggregate_today = orig_data_aggregate_today  %>%
      left_join(lw_orig_data_aggregate %>% select(-population))  %>%
      left_join(pw_orig_data_aggregate %>% select(-population))

    inputcountries = c("Italy","Germany", "Switzerland", "Sweden", "UK", "France", "Spain", "Russia") # example with countries

    str_palette = c(col = "Greys", rev = TRUE, skip = 3)

    callModule(mod_barplot_server, "plot", orig_data_aggregate_today, plottitle = c("Stringency Index"),
               g_palette = list("plot_1" = str_palette,
                                calc = TRUE),
               pickvariable = list("plot_1" = "confirmed"))
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}



if (interactive()) {
  library(shiny)
  #sapply(file.path("R",list.files("R")), source)
  #devtools::load_all()
  ui <- fluidPage(
    tagList(
      Covid19Mirai:::golem_add_external_resources(),
      mod_barplot_ui("plot", plot1 = "ui_vaccines", plot2  = NULL)
    )
  )
  server <- function(input, output, session) {

    # Data ----
    # Data ----
    orig_data <- readRDS(system.file("datahub/DATA.rds", package = "Covid19Mirai"))$orig_data

    pop_data = get_pop_datahub()
    orig_data_aggregate = build_data_aggr(orig_data, pop_data)

    lw_orig_data_aggregate =  lw_vars_calc(orig_data_aggregate)
    pw_orig_data_aggregate =  lw_vars_calc(orig_data_aggregate,14)

    orig_data_aggregate_today = orig_data_aggregate %>%
      add_growth_death_rate()
    orig_data_aggregate_today = orig_data_aggregate_today  %>%
      left_join(lw_orig_data_aggregate %>% select(-population))  %>%
      left_join(pw_orig_data_aggregate %>% select(-population))

    inputcountries = c("Italy","Germany", "Switzerland", "Sweden", "UK", "France", "Spain", "Russia") # example with countries

    orig_data_aggregate_today = orig_data_aggregate_today %>% filter(Country.Region %in% inputcountries)

    vax_palette = c(col = "Blues", rev = TRUE, skip = 3)

    callModule(mod_barplot_server, "plot", orig_data_aggregate_today, plottitle = c("Vaccination status"),
               g_palette = list("plot_1" = vax_palette,
                                calc = TRUE),
               pickvariable = list("plot_1" = "confirmed"))
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}



if (interactive()) {
  library(shiny)
  #sapply(file.path("R",list.files("R")), source)
  #devtools::load_all()
  ui <- fluidPage(
    tagList(
      Covid19Mirai:::golem_add_external_resources(),
      mod_barplot_ui("plot", plot1 = "ui_confirmed", plot2  = "ui_hosp")
    )
  )
  server <- function(input, output, session) {

    # Data ----
    # Data ----
    orig_data <- get_datahub(country = "India", lev = 2) %>%
      get_timeseries_by_contagion_day_data()


    #pop_data = get_pop_datahub()
    orig_data_aggregate = build_data_aggr(orig_data)

    lw_orig_data_aggregate =  lw_vars_calc(orig_data_aggregate)
    pw_orig_data_aggregate =  lw_vars_calc(orig_data_aggregate,14)

    orig_data_aggregate_today = orig_data_aggregate %>%
      add_growth_death_rate()
    orig_data_aggregate_today = orig_data_aggregate_today  %>%
      left_join(lw_orig_data_aggregate %>% select(-population))  %>%
      left_join(pw_orig_data_aggregate %>% select(-population))


    #inputcountries = c("Italy","Germany", "Switzerland", "Sweden", "UK", "France", "Spain", "Russia") # example with countries

    #orig_data_aggregate_today = orig_data_aggregate_today %>% filter(Country.Region %in% inputcountries)

    com_palette = c(col = "Reds", rev = TRUE, skip = 3)

    callModule(mod_barplot_server, "plot", orig_data_aggregate_today, #plottitle = c("Confirmed status"),
               istop = FALSE, n_highlight = length(orig_data_aggregate_today$Country.Region),
               # g_palette = list("plot_1" = barplots_colors$confirmed,
               #                  calc = FALSE),
               g_palette = list("plot_1" = barplots_colors$confirmed$uniform,
                                "plot_2" = barplots_colors$hosp$uniform,
                                calc = FALSE),
               #pickvariable = list("plot_1" = "confirmed_rate_1M_pop","plot_2" = "hosp_rate_1M_pop"),
               plottitle =  c("Confirmed positive cases per area", "Hospitalised and Intensive Care per area")

               #pickvariable = list("plot_1" = "confirmed")
               )
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}


