#' plot_log_linear UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param select logical, if TRUE selectInput UI added as uiOutput, default FALSE
#' @param area logical, if TRUE area plot UI is done, no radiobutton, default TRUE
#' @noRd
#'
#' @import shiny
#' @importFrom plotly plotlyOutput
#' @importFrom shinycssloaders withSpinner
mod_plot_log_linear_ui <- function(id, select = FALSE, area = TRUE){
  ns <- NS(id)
  if (!select && (!area)) {
    # linear plot
    tagList(
      radioButtons(inputId = ns("radio_log_linear"), label = "",
                   choices = c("Log Scale" = "log", "Linear Scale" = "linear"), selected = "linear", inline = TRUE),
      withSpinner(plotlyOutput(ns("plot_log_linear"), height = 500))#,
    )
  } else if (!select && (area)){
    # global page
    tagList(
      withSpinner(plotlyOutput(ns("plot_area"), height = 500))#,
    )
  } else if (select && (area)) {
    # country page
    tagList(
      uiOutput(ns("select_area_ui")),
      withSpinner(plotlyOutput(ns("plot_area_select"), height = 450))#,
    )
  } else {
    stop("wrong selection in mod_plot_log_linear_ui")
  }

}

#' plot_log_linear Server Function
#'
#' @param df data.frame
#' @param type character string. Either area or line. Used to select plot type.
#' @param g_palette character vector of colors for the graph and legend
#' @param countries character vector of countries considered, NULL if only one
#' @param hosp logical, if TRUE hosp variables are in status. Default FALSE
#' @param active_hosp logical, if TRUE hosp and active are in status, active to be adjusted. Default FALSE
#'
#' @example man-roxygen/ex-plot_log_linear.R
#'
#' @import dplyr
#' @importFrom plotly renderPlotly
#' @importFrom plotly ggplotly
#' @importFrom plotly layout
#' @importFrom scales label_number
#'
#' @noRd
mod_plot_log_linear_server <- function(input, output, session, df, type, g_palette = graph_palette, countries = NULL, hosp = FALSE, active_hosp = FALSE){
  ns <- session$ns
  legend.y = 1.1

  if (!is.null(countries) && type == "area") {
    message("mod_plot_log_linear_server area with select country")
    # select the one with more confirmed cases today
    selectedcountry = df %>% filter(Date == max(Date)) %>%
      group_by(Country.Region)%>% summarise(conf = sum(Value, na.rm = TRUE)) %>%
      arrange(desc(conf)) %>% .$Country.Region %>% .[1]
    log = reactive(FALSE)
    message("selected first country: ", selectedcountry)

    output[["select_area_ui"]] <- renderUI({
      selectInput(label = "Area", inputId = ns("select_area"), choices = sort(countries()$Country.Region), selected = selectedcountry)
    })
    # area plot with selection of countries
    observeEvent(input$select_area,  {    #
      if (input$select_area == "" || (!(input$select_area %in% df$Country.Region))) {
        return()
      }
      message("process area ", req(input$select_area))
      # Data ----
      area_data <-  df %>%
        filter(Country.Region %in% req(input$select_area)) %>%
        select(-Country.Region)

      output$plot_area_select <- renderPlotly({

        p <- area_data %>%
          time_evol_area_plot(stack = TRUE, log = log(), text = "Status", hosp = hosp,active_hosp = active_hosp)
        #p <- p + scale_y_continuous(labels = label_number(big.mark = "'")) # add label

        p <- p %>%
          ggplotly(tooltip = c("x", "y", "text")) %>%
          #ggplotly(tooltip = c("text")) %>%
          layout(legend = list(orientation = "h", y = legend.y, yanchor = "bottom", itemsizing = "constant"))

        p
      })
    })
  } else if (is.null(countries) && type != "area") {
    message("mod_plot_log_linear_server linear with Log")

    log <- reactive({
      req(input$radio_log_linear) != "linear"
    })
    observeEvent(input$radio_log_linear, {
      output$plot_log_linear <- renderPlotly({
        p <- df %>%
            time_evol_line_plot(log = log(), text = "Area" , g_palette = graph_palette)
       # p <- p + scale_y_continuous(labels = label_number(big.mark = "'")) # add label

        p <- p %>%
          ggplotly(tooltip = c("x", "y", "text")) %>%
          #ggplotly(tooltip = c("text")) %>%

          layout(legend = list(orientation = "h", y = legend.y, yanchor = "bottom", itemsizing = "constant"))

        p
      })

    })
  } else if (is.null(countries) && type == "area"){
    message("mod_plot_log_linear_server area plot")

    log = reactive(FALSE)

    output$plot_area <- renderPlotly({
      p <- df %>%
          time_evol_area_plot(stack = TRUE, log = log(), text = "Status", hosp = hosp, active_hosp = active_hosp) #%>%

     # p <- p + scale_y_continuous(labels = label_number(big.mark = "'")) # add label
      p <- p %>%
        ggplotly(tooltip = c("x", "y", "text")) %>%
        #ggplotly(tooltip = c("text")) %>%

        layout(legend = list(orientation = "h", y = legend.y, yanchor = "bottom", itemsizing = "constant"))
      p
    })
  } else {
      stop("mod_plot_log_linear_server unsupported selction")
  }
}
