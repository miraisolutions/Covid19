#' country_comparison UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom shinycssloaders withSpinner
mod_country_comparison_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(
      hr(),
      div(
        uiOutput(ns("from_nth_case"))
      ),
      hr(),
      selectInput(label = "Countries", inputId = ns("select_countries"), choices = NULL, selected = NULL, multiple = TRUE)
    ),
    #withSpinner(uiOutput(ns("barplots"))),
    withSpinner(uiOutput(ns("lineplots"))),
    fluidRow(
      column(5,
             withSpinner(uiOutput(ns("rateplots")))
      ),
      column(7,
             withSpinner(uiOutput(ns("lines_points_plots")))
             )
    ),
    fluidRow(
      column(6,
             withSpinner(mod_scatterplot_ui(ns("scatterplot_plots")))
      ),
      column(6,
             withSpinner(uiOutput(ns("status_stackedbarplot")))
      )
    ),
    fluidRow(
      column(6,
             withSpinner(uiOutput(ns("scatterplot_stringency")))
      ),
      column(6,
             mod_barplot_ui(ns("barplot_stringency_index"), plot1 = "ui_stringency", plot2 = NULL)
      )
    ),
    fluidRow(
      column(6,
             withSpinner(mod_scatterplot_ui(ns("scatterplot_vax_vars"), growth = FALSE))
      ),
      column(6,
             withSpinner( mod_barplot_ui(ns("barplot_vax_index"), plot1 = "ui_vaccines", plot2 = NULL))
      )
    ),
    mod_add_table_ui(ns("add_table_countries"))
  )
}

#' country_comparison Server Function
#'
#' @param data data.frame with countries
#' @param nn min number of cases for used to filter country data
#' @param w number of days of outbreak. Default 7
#' @param n.select min number of cases for a country to be considered in selectInput.
#'
#' @example ex-country-comparison.R
#'
#' @import dplyr
#'
#' @noRd
mod_country_comparison_server <- function(input, output, session, data, countries, nn = 1000, w = 7, n.select){
  ns <- session$ns

  # Data ----

  observe(
    updateSelectInput(session, "select_countries", choices = sort(countries()$Country.Region), selected = c("Switzerland", "Italy"))
  )
  output$from_nth_case<- renderUI({
    HTML(paste(
               message_conf_case("Areas", nn),
      #paste0("Only Countries with more than ", n.select, " confirmed cases can be chosen."),
               #paste0("Some countries are not providing Recovered data."),
               message_missing_data("Recovered and Tests", "some countries"),
               #paste0("1st day is the day when ", nn ," confirmed cases are reached.")
                message_firstday(nn),
      sep = "<br/>"))
  })

  all_countries_data <- data %>%
    filter(contagion_day > 0) %>%
    arrange(desc(date))

  all_countries_data_today = all_countries_data %>%
    add_growth_death_rate()

  lw_all_countries_data =  lw_vars_calc(all_countries_data)

  all_countries_data_today = all_countries_data_today  %>%
    left_join(lw_all_countries_data %>% select(-population))


  observeEvent(input$select_countries,{
    if (input$select_countries != "") {
      # Data ----

      countries_data <- all_countries_data %>%
          filter(Country.Region %in% input$select_countries) %>%
          arrange(desc(date))

      countries_data_today = all_countries_data_today %>%
        filter(Country.Region %in% input$select_countries)

      # align contagion day for comparisons
      # data_filtered <-
      #   countries_data %>%
      #   rescale_df_contagion(n = nn, w = w)
      # output$from_nth_case <- renderText({
      #   paste0("Only countries with more than ", n, " confirmed cases, and outbreaks longer than ", w, " days considered. Contagion day 0 is the first day with more than ", n ," cases.")
      # })

      # Bar plots ----
      if (FALSE) {
        output$barplots <- renderUI({
          lapply(input$select_countries, function(country){
            tagList(
              h2(country),
              mod_bar_plot_day_contagion_ui(ns(paste0(country,"_bar_plot_day_contagion")))
            )
          })
        })

        lapply(input$select_countries, function(country){
          country_data <- countries_data %>%
            filter(Country.Region %in% country)
          callModule(mod_bar_plot_day_contagion_server, paste0(country,"_bar_plot_day_contagion"), country_data, nn = nn)
        })
      }

    # Line plots ----
    output$lineplots <- renderUI({
      tagList(
        h2("Countries Comparison"),
        mod_lineplots_day_contagion_ui(ns("lineplots_day_contagion"))
      )
    })
    vaxflag = sum(countries_data$vaccines, na.rm = TRUE) > 0

    statuseslineplot = c("confirmed", "deaths", "recovered", "active")
    if (vaxflag)
      statuseslineplot = c("confirmed", "deaths", "vaccines", "active")

    callModule(mod_lineplots_day_contagion_server, "lineplots_day_contagion", countries_data, nn = nn, statuses = statuseslineplot)

    # Rate plots ----
    output$rateplots <- renderUI({
      mod_barplot_ui(ns("rate_plots"))
    })

    callModule(mod_barplot_server, "rate_plots", countries_data_today, n_highligth = length(input$select_countries), istop = FALSE,
               g_palette = list("plot_1" = graph_palette[1:length(input$select_countries)], #barplots_colors$stringency,
                                "plot_2" = graph_palette[1:length(input$select_countries)],
                                calc = FALSE),
               sortbyvar = FALSE)


    # Line with bullet plot


    output$lines_points_plots <- renderUI({
      mod_compare_nth_cases_plot_ui(ns("lines_points_plots"), tests = TRUE, hosp = TRUE, strindx = TRUE, selectvar = "new_confirmed", oneMpop = TRUE, vax = vaxflag)
    })

    callModule(mod_compare_nth_cases_plot_server, "lines_points_plots", countries_data, nn = nn, w = w, n_highligth = length(input$select_countries),
               istop = FALSE, tests = TRUE, hosp = TRUE, strindx = TRUE,  oneMpop = TRUE, vax = vaxflag)


    inputcountries = reactive({input$select_countries}) # pass countries to plot below
    # output$scatterplot_plots <- renderUI({
    #   mod_scatterplot_ui(ns("scatterplot_plots"))
    # })
    # set nmed to 10000 like in global page, istop == FALSE
    callModule(mod_scatterplot_server, "scatterplot_plots", all_countries_data_today, nmed = 10000, n_highligth = length(input$select_countries), istop = FALSE, countries = inputcountries())


    output$status_stackedbarplot <- renderUI({
      mod_stackedbarplot_ui(ns("status_stackedbarplot"))
    })
    callModule(mod_stackedbarplot_status_server, "status_stackedbarplot", countries_data_today, n_highligth = length(input$select_countries), istop = FALSE)

    output$scatterplot_stringency <- renderUI({
      mod_scatterplot_ui(ns("scatterplot_stringency"), growth = FALSE)
    })
    # set nmed to 10000 like in global page, istop == FALSE
    callModule(mod_scatterplot_server, "scatterplot_stringency", all_countries_data_today, nmed = 10000, n_highligth = length(input$select_countries), istop = FALSE, countries = inputcountries(), xvar = "stringency_index", growth = FALSE, fitted = FALSE)

    # > barplot stringency
    callModule(mod_barplot_server, "barplot_stringency_index", countries_data_today, n_highligth = length(input$select_countries), istop = FALSE,
               plottitle = c("Stringency Index"),
               g_palette = list("plot_1" = graph_palette[1:length(input$select_countries)], #barplots_colors$stringency,
                                calc = FALSE),
               sortbyvar = FALSE)

    #scatterplot vax versus vars
    callModule(mod_scatterplot_server, "scatterplot_vax_vars",
               all_countries_data_today, nmed = nn, n_highligth = length(input$select_countries),
               istop = FALSE, countries = inputcountries(), xvar = "vaccines_rate_pop", growth = FALSE, fitted = FALSE)

    #barplot vax

    callModule(mod_barplot_server, "barplot_vax_index", countries_data_today,
               n_highligth = length(input$select_countries), istop = FALSE,
               plottitle = c("Vaccination Status"),
               g_palette = list("plot_1" = graph_palette[1:length(input$select_countries)],#barplots_colors[["vaccines"]],
                                calc = FALSE),
               sortbyvar = FALSE)


    # tables ----
    callModule(mod_add_table_server, "add_table_countries", countries_data, maxrowsperpage = 10)
    }
  })

}
