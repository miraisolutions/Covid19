#' caseBoxes UI Function
#'
#' @description A shiny Module for displaying stats about vaccination
#'
#' @param id, Internal parameters for {shiny}.
#'
#' @example man-roxygen/ex-mod_vaccines_text.R
#'
#' @name mod_vaccines_text
#' @keywords internal

#' @rdname mod_vaccines_text
#' @import shiny
#' @importFrom shinyWidgets useShinydashboard
#' @importFrom shinydashboard box
mod_vaccines_text_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shinyWidgets::useShinydashboard(),
        div(h4("Vaccination Pace"), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
        fluidRow(
          column(3, numericInput(inputId = ns("target"), label = "% Target coverage",
                                value = 70,
                                min = 0,
                                max = 100,
                                step = 1)),
          column(3, numericInput(inputId = ns("doses"), label = "Number of doses",
                                 value = 2,
                                 min = 1,
                                 max = 2,
                                 step = 1)),
          column(3, dateInput(inputId = ns("tdate"), label = "Target date",
                                 value = "2021-09-01",
                                 min = Sys.Date()
                                 ))
      ),
    verticalLayout(
        fluidRow(
          #tags$head(tags$style("#TxtOut {white-space: nowrap;}")),
         # div(verbatimTextOutput(ns("vax_text")), height = "400")
          #div(uiOutput(ns("vax_text")), height = "400")

          #div(htmlOutput(ns("vax_text"), inline = FALSE), height = "400")
          #div(textOutput(ns("vax_text"),inline = TRUE), height = "400")
          #div(

          # tags$head(tags$style("#vax_text{color: red;
          #                        white-space: nowrap;
          #                        font-size: 20px;
          #                        font-style: italic;
          #                        }"
          #     )
          # ),
          shinydashboard::box(
            #title = "Status summary",
            background = "light-blue",
            height = 200, width = 12,
            solidHeader = TRUE, status = "primary", #"info",
            collapsed = TRUE,
            div(style = 'overflow-y: scroll',  htmlOutput(ns("vax_text")))
          ),
          #)
        ),
        withSpinner(plotOutput(ns("vax_line"), height = 200))
    )

  )
    #)
  #tg
}

#' Vaccines text Server Function
#'
#' @param df dataset whole time series.
#' @param dftoday dataset with today and weekly data.
#'
#' @rdname mod_vaccines_text
mod_vaccines_text_server <- function(input, output, session, df, dftoday) {

  df = df %>% select(date, Country.Region,vaccines, new_vaccines, population) %>%
    filter(vaccines > 0)


  dftday = dftoday %>% select(date, Country.Region,vaccines, lw_vaccines, vaccines_rate_pop, population, deaths) %>%
    mutate(population = population - deaths,
           lw_vaccines_per_day = lw_vaccines/7,
           ) %>%
    select(-deaths)
  dftday$start_vaccines_date = min(df$date)
  dftday$vaccines_days = as.numeric(dftday$date-dftday$start_vaccines_date )
  dftday$vaccines_per_day = dftday$vaccines/dftday$vaccines_days

  data = reactive({
    dftday %>%
      # achieved_date = today + (target% * targetdose * (population- vaccines)) / (lw_vaccines/7)
      mutate(lw_achieved_date = date + ceiling(input$target/100 * input$doses * (population- vaccines) / (lw_vaccines_per_day)),
     # achieved_date = today + (target% * targetdose * (population- vaccines)) / (vaccines/(today-startdate))
            achieved_date = date + ceiling(input$target/100 * input$doses* (population- vaccines) / (vaccines/vaccines_days))) %>%
      #mutate() # add number of daily vaccines required to achieve target
      mutate(days_to_target = as.numeric(input$tdate - date)) %>%
      mutate(target_vaccines_per_day = (population- vaccines) / days_to_target )
      })
  .format_num = function(x) {
    formatC(x,digits = 0, big.mark = "'",  format ="d")
  }

  output$vax_text =  renderPrint({
    div(HTML(
      paste0("  ",strong(dftday$Country.Region), ":\n",
          "  Target Date: <b>", input$tdate,"</b>, ", .format_num(data()$days_to_target), " days remaining.<br/>",
          "  Target Coverage: <b>", input$target,"%</b>.<br/>",
          "  Required vaccines per day to cover ",input$target," % of the population by <b>", input$tdate,"</b>: <b>",
                        .format_num(data()$target_vaccines_per_day),"</b>.<br/>",
          "<br/>",
          "  Number of vaccines done as of today: <b>", .format_num(dftday$vaccines),"</b>.<br/>",
          "  Average vaccines per day: <b>", .format_num(dftday$vaccines_per_day),"</b>.<br/>",
          "  With this average pace ", input$target,"% of the population will be covered with <b>",input$doses," doses</b> by <b>",data()$achieved_date,"</b>.<br/>",
          "<br/>",
          "  Number of vaccines done last week: <b>", .format_num(dftday$lw_vaccines),"</b>.<br/>",
          "  Average vaccines per day during last week: <b>", .format_num(dftday$lw_vaccines_per_day),"</b>.<br/>",
          "  With this average pace ", input$target,"% of the population will be covered with <b>",input$doses, " doses</b> by <b>",data()$lw_achieved_date,"</b>.<br/>"
        )
    ), align = "left", style = "margin-top:1px; margin-bottom:1px;
                                 white-space: nowrap;
                                 word-wrap: break-word;
                                 font-size: 12px;
                                 font-style: italic;")
#font-size: 1vh;
  })
  plotdata = df %>%
    select(date, new_vaccines) %>%
    rename(Date = date, Value = new_vaccines) %>%
    mutate(Status = "New Vaccinated")

    #make plot
  output$vax_line <- renderPlot({
    #secondline = NULL
    p <- plot_all_highlight(plotdata, log = FALSE, text = "Area", percent =FALSE,
                            date_x =  TRUE, g_palette = graph_palette[1],  secondline = FALSE, rollw = TRUE, keeporder = TRUE)
    if (data()$target_vaccines_per_day > max(plotdata$Value, na.rm = TRUE))
      p = p + expand_limits(y = data()$target_vaccines_per_day*1.05)

    # add line with target vaccination
    p <- p + #theme(legend.position = "none") +
      #theme(plot.caption = element_text(hjust=0.5, size=rel(1))) +
      geom_hline(yintercept = data()$target_vaccines_per_day, colour = "darkblue", linetype="dotted", size = 0.3) +
      annotate("text", x = min(plotdata$Date), y = data()$target_vaccines_per_day*1.05,
               label = "Target Average Vaccines per Day", size = 2.5,
               hjust = 0) +
      labs(caption = caption_vaccines(), hjust = 0.5, size = 2.5)

    # no legend no interactivity

    # p <- p %>%
    #   plotly::ggplotly(tooltip = c("text", "x_tooltip", "y_tooltip")) %>%
    #   plotly::layout(annotations = list(x = min(plotdata$Date), y = data()$target_vaccines_per_day,
    #                                     align = "left",
    #                                     text = "Target Average Vaccines per Day", size = 1))

     # plotly::layout(legend = list(orientation = "h", y = 1.1, yanchor = "bottom", itemsizing = "constant"))

    p

  })

}

