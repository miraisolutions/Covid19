#' growth_death_rate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinycssloaders withSpinner
mod_growth_death_rate_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      column(6,
             div(h4("Current top 5 countries growth factor"), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
             radioButtons(inputId = ns("growth_factor"), label = "",
                          choices = list("Over 3 days" = "growth_factor_3",
                                         "Over 5 days" = "growth_factor_5",
                                         "Over one week" = "growth_factor_7"),
                          selected = "growth_factor_3", inline = TRUE),
             withSpinner(uiOutput(ns("plot_growth_factor")))
      ),
      column(6,
             div(h4("Current top 5 countries death toll"), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
             radioButtons(inputId = ns("radio_pop"), label = "",
                          choices = list("lethality rate" = "lethality_rate",
                                         "mortality rate 1M pop" = "mortality_rate_1M_pop"),
                          selected = "lethality_rate", inline = TRUE),
             withSpinner(uiOutput(ns("plot_death_rate")))
      )
    )
  )
}

#' growth_death_rate Server Function
#'
#' @param df reactive data.frame
#'
#' @import dplyr
#' @import tidyr
#'
#' @example ex-mod_growth_death_rate.R
#'
#' @noRd
mod_growth_death_rate_server <- function(input, output, session, df){
  ns <- session$ns

  # Params ----
  n <- 1000 #min number of cases for a country to be considered
  w <- 7 #min lenght of outbreak
  n_highligth <- 5 # top countries

  # Help funcs ----

  scale_mortality_rate <- function(orig_data_aggregate){
    df1 <- orig_data_aggregate %>%
      select_countries_n_cases_w_days(n = n, w = w) %>%
      filter( date == max(date)) %>%
      align_country_names_pop() %>%
      mutate(country_name = Country.Region) %>%
      get_pop_data() %>%
      filter(population > 10^6) %>% # dropping countries with less than 1 M pop
      mutate(mortality_rate_1M_pop = round(10^6*deaths/population, digits = 3)) %>%
      align_country_names_pop_reverse()
    df1
  }

  pick_rate <- function(df, rate){
    df <-  df  %>%
      bind_cols(df[, rate] %>% setNames("Value"))
    df
  }

  pick_rate_hist <- function(df1, rate){
    df_plot <- df1 %>%
      pick_rate(rate) %>%
      arrange(desc(Value)) %>%
      top_n(n_highligth, wt = Value) %>%
      mutate(Country = factor(Country.Region, levels = .$Country.Region)) %>%
      select(Country, Value)

    df_plot
  }

  # Dataset ----

  df_pop <- reactive(scale_mortality_rate(df()))

  df_base_plot1 <- reactive({pick_rate_hist( req(df_pop()), input$growth_factor)})
  df_base_plot2 <- reactive({pick_rate_hist( req(df_pop()), input$radio_pop)})

  # Plots ----
  caption_growth_factor <- reactive({paste0("Computed as total confirmed cases today / total confirmed cases ", gsub("growth_factor_", "", input$growth_factor) ," days ago.")})
  caption_death_rate_radio <- reactive({
    if (input$radio_pop == "lethality_rate") {
      p <- "/ total confirmed cases today."
    } else {
      p <- "per 1 M population "
    }
    p
  })
  caption_death_rate <- reactive({paste0("Computed as total deaths today ",caption_death_rate_radio())})
  caption_countries <- paste0("Only countries with more than ", n, " confirmed cases, a population of over 1 M and outbreaks longer than ", w, " days considered.")

  output$plot_growth_factor <- renderUI({
    tagList(
      plotlyOutput(ns("plot_growth_factor_hist"), height = 400),
      div(p(caption_growth_factor()), align = "center"),
      div(p(caption_countries), align = "center"),
    )
  })

  output$plot_death_rate <- renderUI({
    tagList(
      plotlyOutput(ns("plot_death_rate_hist"), height = 400),
      div(p(caption_death_rate()), align = "center"),
      div(p(caption_countries), align = "center"),
    )
  })

  output$plot_growth_factor_hist <- renderPlotly({
    p <- plot_rate_hist(df_base_plot1(), color =  "growth_factor", y_min = 1)
    p <- p %>%
      plotly::ggplotly() %>%
      plotly::layout(legend = list(orientation = "h", y = 1.1, yanchor = "bottom"),
                                   xaxis = list(tickfont = list(size = 14)))
    p
  })

  is_percent <- reactive({ifelse(input$radio_pop == "lethality_rate", T, F)})

  output$plot_death_rate_hist <- renderPlotly({
    p <- plot_rate_hist(df_base_plot2(), color =  "death_rate", percent = is_percent())
    p <- p %>%
      plotly::ggplotly() %>%
      plotly::layout(legend = list(orientation = "h", y = 1.1, yanchor = "bottom"),
                     xaxis = list(tickfont = list(size = 14)))
    p
  })

}
