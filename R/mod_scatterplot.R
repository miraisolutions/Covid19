#' plot_scatterplot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList selectInput
#' @importFrom shinycssloaders withSpinner
mod_scatterplot_ui <- function(id, n_highligth = 5){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6,
             uiOutput(ns("title_scatterplot")),
             selectInput(inputId = ns("growth_factor"), label = "Select growth factor",
                          choices = list("Over 3 days" = "growth_factor_3",
                                         "Over 5 days" = "growth_factor_5",
                                         "Over one week" = "growth_factor_7"),
                          selected = "growth_factor_3"),
             #withSpinner(uiOutput(ns("plot_scatterplot")))
             withSpinner(uiOutput(ns("plot_scatterplot"), height = 400)),
      )
    )
  )
}
#' Scatterplot prevalence vs growth
#'
#' @param df reactive data.frame for multiple countries
#' @param countries reactive character vector of country names

#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @importFrom plotly ggplotly layout
#'
#' @noRd
mod_scatterplot_server <- function(input, output, session, df, n = 1000, w = 7, n_highligth = 5, istop = T, countries){
  ns <- session$ns
  # titles
  if (istop) {
    output$title_scatterplot <- renderUI(div(h4(paste0("Current top ", n_highligth, " Growth vs Prevalence")), align = "center", style = "margin-top:20px; margin-bottom:20px;"))
  } else {
    output$title_scatterplot <- renderUI(div(h4("Growth vs Prevalence"), align = "center", style = "margin-top:20px; margin-bottom:20px;"))
  }

  # select growth factor
  addgrowth = function(df, grate) {
    df$growthfact = df[[grate]]
    df = df[, !(grepl("^growth_factor", names(df)))]
    df
  }

  select1000 = function(orig_data_aggregate, n, w){
    orig_data_aggregate %>%
      Covid19:::select_countries_n_cases_w_days(n = n, w = w) %>%
      filter( date == max(date)) %>%
      align_country_names_pop() %>%
      mutate(country_name = Country.Region) %>%
      get_pop_data() %>%
      filter(population > 10^6) %>% # dropping countries with less than 1 M pop, needed?
      mutate(mortality_rate_1M_pop = round(10^6*deaths/population, digits = 3),
             prevalence_rate_1M_pop = round(10^6*confirmed/population, digits = 3)) %>%
      select(-country_name) %>%
      align_country_names_pop_reverse() %>%
      filter(confirmed > 10000)  %>%
      select(Country.Region,date,confirmed,starts_with("growth"),prevalence_rate_1M_pop)
  }
  pick_rate <- function(df, rate){
    df <-  df  %>%
      bind_cols(df[, rate] %>% setNames("Value"))
    df
  }

  # prepare data select those with more than 10000
  world1000 = reactive({
    select1000(df(), n,w)})

  # compute stats for all growth factors
  med_growth = reactive({apply(world1000()[, grepl("growth", names(world1000())), drop = F],2,  median)})
  med_prevalence = reactive({median(world1000()$prevalence_rate_1M_pop)})

  medgr = reactive({med_growth()[input$growth_factor]})

  df_top = reactive({pick_rate(world1000(), "confirmed") %>%
      arrange(desc(Value)) })
  if (istop)  { # choose top n_highligth
    df_top_new = reactive({df_top() %>%
      top_n(n_highligth, wt = Value)})
  } else {
    df_top_new = reactive({df_top() %>%
        filter(Country.Region %in% countries())})
  }

  dfnew = reactive({addgrowth(df_top_new(),input$growth_factor)})

  caption_growth_factor <- reactive({paste0("growth factor: Computed as total confirmed cases today / total confirmed cases ", gsub("growth_factor_", "", input$growth_factor) ," days ago.")})
  caption_prevalence <- "Prevalence: confirmed cases over 1 M people."
  caption_median <- "Dotted lines show median values among countries with more than 10k cases."

  output$plot_scatterplot <- renderUI({
    tagList(
      plotlyOutput(ns("plot_scatterplot_xy"), height = 400),
      div(p(caption_growth_factor()), align = "center"),
      div(p(caption_prevalence), align = "center"),
      div(p(caption_median), align = "center"),
    )
  })

  output$plot_scatterplot_xy <- renderPlotly({

    df <- dfnew() %>%
      mutate(Country.Region = as.factor(Country.Region))

    p = df %>%
        Covid19:::scatter_plot(list(x = med_prevalence(),
                          y = medgr()))
    p <- p %>%
      ggplotly(tooltip = c("x", "y", "text")) %>%
      layout(legend = list(orientation = "h", y = 1.1, yanchor = "bottom"))
    p
  })
}
