#' plot_log_linear UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom plotly plotlyOutput
#' @importFrom shinycssloaders withSpinner
mod_plot_log_linear_ui <- function(id){
  ns <- NS(id)
  tagList(
    radioButtons(inputId = ns("radio_log_linear"), label = "",
                 choices = c("Log Scale" = "log", "Linear Scale" = "linear"), selected = "linear", inline = TRUE),
    withSpinner(plotlyOutput(ns("plot_log_linear"), height = 400))#,
    #tags$div(id = "caption")
  )
}

#' plot_log_linear Server Function
#'
#' @param df data.frame
#' @param type character string. Either area or line. Used to select plot type.
#' @param g_palette character vector of colors for the graph and legend
#'
#' @example man-roxygen/ex-plot_log_linear.R
#'
#' @importFrom plotly renderPlotly
#' @importFrom plotly ggplotly
#' @importFrom plotly layout
#' @importFrom scales label_number
#'
#' @noRd
mod_plot_log_linear_server <- function(input, output, session, df, type, g_palette = graph_palette){
  ns <- session$ns

  log <- reactive({
    input$radio_log_linear != "linear"
  })

  observeEvent(input$radio_log_linear, {
    output$plot_log_linear <- renderPlotly({


      if (type == "area") {
        p <- df %>%
          time_evol_area_plot(stack = T, log = log(), text = "Status") #%>%
          #fix_colors()
      } else {
        p <- df %>%
          time_evol_line_plot(log = log(), text = "Area" , g_palette = graph_palette)
      }
      p <- p + scale_y_continuous(labels = label_number(big.mark = "'")) # add label

      p <- p %>%
        ggplotly(tooltip = c("x", "y", "text")) %>%
        layout(legend = list(orientation = "h", y = 1.1, yanchor = "bottom", itemsizing = "constant"))

      p
    })
    # if (hospcaption && type == "area" && (!("hosp" %in% levels(df$Status)))){
    #   insertUI(
    #     selector = "#plot_log_linear",
    #     where = "afterEnd",
    #     #ui= tags$div(id = "caption", p("Hospitalised data unavailable")),
    #     ui= textInput(inputId = ns("caption"), p("Hospitalised data unavailable")),
    #
    #     immediate = TRUE
    #   )
    # }
  })

}
