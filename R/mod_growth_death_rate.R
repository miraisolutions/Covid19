#' growth_death_rate UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_growth_death_rate_ui <- function(id){
  ns <- NS(id)
  tagList(
    # fluidRow(
    #   column(7,
    #          offset = 1,
    #          radioButtons(inputId = ns("radio_indicator"), label = "",
    #                       choices = names(case_colors), selected = names(case_colors)[1], inline = TRUE)
    #   ),
    #   column(4,
    #          radioButtons(inputId = ns("radio_log_linear"), label = "",
    #                       choices = c("Log Scale" = "log", "Linear Scale" = "linear"), selected = "linear", inline = TRUE)
    #   )
    # ),
    fluidRow(
      column(6,
             div(h4("Current top 5 countries growth rate"), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
             div(style = "visibility: hidden;", radioButtons(inputId = ns("radio_log_linear_dummy1"), label = "",
             choices = c("Log Scale" = "log", "Linear Scale" = "linear"), selected = "linear", inline = TRUE)),
             plotlyOutput(ns("plot_growth_rate"), height = 400),
             div(p("Computed as new reported cases today / new reported cases yesterday"), align = "center")
             ),
      column(6,
             div(h4("Current top 5 countries death rate"), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
             div(style = "visibility: hidden;", radioButtons(inputId = ns("radio_log_linear_dummy2"), label = "",
             choices = c("Log Scale" = "log", "Linear Scale" = "linear"), selected = "linear", inline = TRUE)),
             plotlyOutput(ns("plot_death_rate"), height = 400),
             div(p("Computed as new reported deaths today / new reported deaths yesterday"), align = "center")
             )
    )
  )
}

#' growth_death_rate Server Function
#'
#' @param world_top_5 reactive data.frame
#'
#' @import dplyr
#'
#' @example ex-mod_growth_death_rate.R
#'
#' @noRd
mod_growth_death_rate_server <- function(input, output, session, world_top_5){
  ns <- session$ns

  pick_rate <- function(df_plot, rate){
    df_plot <-  df_plot  %>%
      bind_cols(df_plot[, rate] %>% setNames("Value")) %>%
      mutate(Country = as.factor(Country.Region)) %>%
      select(Country, Value)
    df_plot
  }

  output$plot_growth_rate <- renderPlotly({

    df_plot <- pick_rate( req(world_top_5()), "growth_rate")

    p <- plot_rate_hist(df_plot, color =  "growth_rate")

    p <- p %>%
      plotly::ggplotly() %>%
      plotly::layout(legend = list(orientation = "h", y = 1.1, yanchor = "bottom"))
    p
  })

  output$plot_death_rate <- renderPlotly({

    df_plot <- pick_rate( req(world_top_5()), "death_rate")

    p <- plot_rate_hist(df_plot, color =  "death_rate" )

    p <- p %>%
      plotly::ggplotly() %>%
      plotly::layout(legend = list(orientation = "h", y = 1.1, yanchor = "bottom"))
    p
  })
}
