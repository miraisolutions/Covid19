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
#' lineplots_day_contagion Server Function
#'
#' @param countries_data reactive data.frame for multiple countries
#'
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#'
#' @noRd
mod_scatterplot_server <- function(input, output, session, countries_data, n_highligth, med, istop = T){
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
  selectmed = function(med, grate) {
    med$y = med$y[grate]
    med
  }
  medgr = reactive({selectmed(med,input$growth_factor)})
  dfnew = reactive({addgrowth(countries_data,input$growth_factor)})

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
        scatter_plot(medgr())
    p <- p %>%
      ggplotly(tooltip = c("x", "y", "text")) %>%
      layout(legend = list(orientation = "h", y = 1.1, yanchor = "bottom"))
    p
  })
}
