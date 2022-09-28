#' plot_log_linear UI Function
#'
#' @description A shiny Module.
#'
#' @param id, Internal parameters for {shiny}.
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
      fluidRow(
        div(class = "plottext", align = "center",
            radioButtons(inputId = ns("radio_log_linear"), label = "",
                          choices = c("Log Scale" = "log", "Linear Scale" = "linear"), selected = "linear", inline = TRUE))
      ),
      fluidRow(
        withSpinner(plotlyOutput(ns("plot_log_linear"), height = 500))#,
      )
    )
  } else if (!select && (area)){
    # global page
    fluidRow(
      withSpinner(plotlyOutput(ns("plot_area"), height = 500))#,
    )
  } else if (select && (area)) {
    # country page
    tagList(
      fluidRow(
        uiOutput(ns("select_area_ui"))
      ),
      fluidRow(
        withSpinner(plotlyOutput(ns("plot_area_select"), height = 450))#,
      )
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
#' @param process_data logical, if TRUE apply tsdata_areplot function to df
#' @param fun.args list, names lev and nn, arguments of tsdata_areplot function
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
mod_plot_log_linear_server <- function(input, output, session, df, type, g_palette = graph_palette, countries = NULL, hosp = FALSE, active_hosp = FALSE, process_data = FALSE, fun.args = NULL){
  ns <- session$ns
  legend.y = 1.1

  if (process_data){

    if (is.null(fun.args)) {
      stop("argument fun.args must be provided")
    }
    if(!identical(names(fun.args), c("levs","nn")))
      stop("wrong argument funargs")
    df = do.call("tsdata_areplot", c(list(data = df), fun.args))
      #tsdata_areplot(continent_data,levs, nn = nn)
  }

  if (!is.null(countries) && type == "area") {

    message("mod_plot_log_linear_server area with select country")
    # select the one with more confirmed cases today

    selectedcountry = df %>% filter(Date == max(Date)) %>% #maxdate not available here
      group_by(Country.Region)%>% summarise(conf = sum(Value, na.rm = TRUE)) %>%
      arrange(desc(conf)) %>% .$Country.Region %>% .[1]
    if (is.na(selectedcountry)) # if there are no data
      selectedcountry = ""
    log = reactive(FALSE)
    message("selected first country: ", selectedcountry)

    output[["select_area_ui"]] <- renderUI({
      selectInput(label = "Area", inputId = ns("select_area"), choices = sort(countries()$Country.Region), selected = selectedcountry)
    })
    # area plot with selection of countries
    #observeEvent(input$select_area,  {    #
    plot_output <- "plot_area_select"
    pp <- eventReactive(input$select_area,  {    #

      if (input$select_area == "" || (!(input$select_area %in% df$Country.Region))) {

        # output$plot_area_select <- renderPlotly({

          p_out <- blank_plot(where = input$select_area, add = " All data missing")
        # })
      } else {
        message("process area ", req(input$select_area))
        # Data ----
        area_data <-  df %>%
          filter(Country.Region %in% req(input$select_area)) %>%
          select(-Country.Region)

        # output$plot_area_select <- renderPlotly({
          p <- area_data %>%
            time_evol_area_plot(stack = TRUE, log = log(), text = "Status", hosp = hosp,active_hosp = active_hosp)
          #p <- p + scale_y_continuous(labels = label_number(big.mark = "'")) # add label

          p_out <- p %>%
            #ggplotly(tooltip = c("x", "y", "text")) %>%
            ggplotly(tooltip = c("text")) %>%
            plotly::layout(legend = list(orientation = "h", y = 1.05, yanchor = "bottom", font = list(family = "Arial, sans-serif", size = 10),
                                         itemwidth = 10, itemsizing = "constant", hovermode = 'closest', clickmode = "event", title = ""),
                           yaxis = list(autorange = TRUE, fixedrange = TRUE))

        #})
      }
      p_out
    })
  } else if (is.null(countries) && type != "area") {
    message("mod_plot_log_linear_server linear with Log")

    log <- reactive({
      req(input$radio_log_linear) != "linear"
    })
    plot_output <- "plot_log_linear"

    #observeEvent(input$radio_log_linear, {
    pp <- eventReactive(input$radio_log_linear, {
      #output$plot_log_linear <- renderPlotly({
        p <- df %>%
            time_evol_line_plot(log = log(), text = "Area" , g_palette = graph_palette)

        p <- p %>%
          #ggplotly(tooltip = c("x", "y", "text")) %>%
          ggplotly(tooltip = c("text")) %>%
          plotly::layout(legend = list(orientation = "h", y = 1.05, yanchor = "bottom", font = list(family = "Arial, sans-serif", size = 10),
                               itemwidth = 10, itemsizing = "constant", hovermode = 'closest', clickmode = "event", title = ""),
                         yaxis = list(autorange = TRUE, fixedrange = TRUE))

        p
      #})

    })
  } else if (is.null(countries) && type == "area"){
      message("mod_plot_log_linear_server area plot")

      log = reactive(FALSE)
      plot_output <- "plot_area"

      #output$plot_area <- renderPlotly({
      pp <- reactive( {
        if (nrow(df) > 0)  {
          p <- df %>%
            time_evol_area_plot(stack = TRUE, log = log(), text = "Status", hosp = hosp, active_hosp = active_hosp) #%>%
          # p <- p + scale_y_continuous(labels = label_number(big.mark = "'")) # add label
          p_out <- p %>%
            #ggplotly(tooltip = c("x", "y", "text")) %>%
            ggplotly(tooltip = c("text")) %>%
            plotly::layout(legend = list(orientation = "h", y = 1.05, yanchor = "bottom", font = list(family = "Arial, sans-serif", size = 10),
                                         itemwidth = 10, itemsizing = "constant", hovermode = 'closest', clickmode = "event", title = ""),
                           yaxis = list(autorange = TRUE, fixedrange = TRUE))
        } else {
          p_out <- blank_plot(where = "the area plot", add = " All data missing.")
        }
        p_out
      })

      #})
  } else {
      stop("mod_plot_log_linear_server unsupported selction")
  }

  output[[plot_output]] <- renderPlotly({

    pp()
  })
}
