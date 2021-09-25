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
mod_vaccines_text_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(h4("Vaccination Pace"), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
    fluidRow(
      column(3, numericInput(inputId = ns("target"), label = div(style = "font-size:10px","% Target coverage"),
                             value = 70,
                             min = 0,
                             max = 100,
                             step = 1)),
      column(3, selectInput(inputId = ns("doses"), label = div(style = "font-size:10px","Number of doses"),
                            choices = c(1,2),
                            selected = 2)),
      column(3, selectInput(inputId = ns("confdoses"), label = div(style = "font-size:10px","Doses for already infected"),
                            choices = c(0,1,2),
                            selected = 1)),
      column(3, dateInput(inputId = ns("tdate"), label = div(style = "font-size:10px","Target date"),
                          value = "2021-10-01",
                          min = Sys.Date()
      ))
    ),
    verticalLayout(
      fluidRow(
        # shinydashboard::box(
        #   #title = "Status summary",
        #   background = "light-blue",
        #   height = 200, width = 12,
        #   solidHeader = TRUE, status = "primary", #"info",
        #   collapsed = TRUE,
        #   div(style = 'overflow-y: scroll',  htmlOutput(ns("vax_text")))
        # )
        div(uiOutput(ns("vax_text")), height = 200)
      ),
      withSpinner(plotOutput(ns("vax_line"), height = 200))
    )

  )
  #)
  #tg
}

#' No Vaccines text Server Function
#'
#'
#' @rdname mod_vaccines_text
mod_novaccines_text_server <- function(input, output, session, country) {

  output$vax_text = renderUI({
    div( class = "count-box",
         style = "color: white; max-width: 100%; background-color: #3c8dbc; margin-left: 20px; margin-right: 20px; font-style: italic; white-space: nowrap; word-wrap: break-word",
         HTML(
           paste0("  No Vaccines data for ",strong(country), ".")
         ), align = "left")
  })
  output$vax_line <- renderPlot({
    plot.new()
  })
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


  dftday = dftoday %>% select(date, Country.Region,vaccines, lw_vaccines, vaccines_rate_pop, confirmed, population, deaths) %>%
    mutate(population = population - deaths,
           lw_vaccines_per_day = lw_vaccines/7,
    ) #%>%
  #select(-deaths)
  dftday$start_vaccines_date = min(df$date) # vaccination start dare
  # number of days of vaccination campaign
  dftday$vaccines_days = as.numeric(dftday$date-dftday$start_vaccines_date )
  #vaccines done so far per day
  dftday$vaccines_per_day = dftday$vaccines/dftday$vaccines_days

  data = reactive({

    dftday %>% # if target already reached then 0
      mutate(vaccines_left_to_target = max(0, input$target/100 * (as.integer(req(input$doses)) * (population- round(vaccines/as.integer(req(input$doses))) - (confirmed-deaths)) +
                                                             as.integer(req(input$confdoses)) * (confirmed-deaths)))) %>%
      # achieved_date = today + (target% * (targetdose * (population- round(vaccines/targetdose) - (confirmed-deaths)) + targetdoseconf * (confirmed-deaths))) / (lw_vaccines/7)
      mutate(lw_achieved_date = date + ceiling((vaccines_left_to_target) / (lw_vaccines_per_day)),
             # achieved_date = today + (target% * targetdose * (population- vaccines- (confirmed-deaths))) + targetdoseconf * (confirmed-deaths)) / (vaccines/(today-startdate))
             achieved_date = date + ceiling((vaccines_left_to_target) / (vaccines/vaccines_days))) %>%
      #days_to_target = number of days between today and target date
      mutate(days_to_target = as.numeric(input$tdate - date)) %>%
      # add number of daily vaccines required to achieve target
      mutate(target_vaccines_per_day = (vaccines_left_to_target) / days_to_target )
  })

  .format_num = function(x) {
    formatC(x,digits = 0, big.mark = "'",  format ="d")
  }
  # style = "margin-top:1px; margin-bottom:1px;
  #                                white-space: nowrap;
  #                                word-wrap: break-word;
  #                                font-size: 10px;
  #                                font-style: italic;"
  # width = "100%"
  output$vax_text = renderUI({
    div( class = "count-box",
         style = "color: white; max-width: 100%; background-color: #3c8dbc; overflow-x: scroll; margin-left: 20px; margin-right: 20px; font-style: italic; white-space: nowrap; word-wrap: break-word",
         HTML(
           paste0("  ",strong(dftday$Country.Region), ". Population: ", .format_num(dftday$population),". Doses per population: ",paste(round(dftday$vaccines_rate_pop*100,1), "%"),"<br/>",
                  "  Target Date: <b>", input$tdate,"</b>. <b>", .format_num(data()$days_to_target), "</b> days remaining. Vaccines left to target: <b>", .format_num(data()$vaccines_left_to_target), "</b>.<br/>",
                  "  Target Coverage: <b>", input$target,"%</b>. Target Doses: <b>", req(input$doses),"</b>. Doses for already infected: <b>", req(input$confdoses),"</b>.<br/>",
                  "  Required vaccines per day to cover ",input$target," % of the population by <b>", input$tdate,"</b>: <b>",
                  .format_num(data()$target_vaccines_per_day),"</b>.<br/>",
                  #"  Target Doses: <b>", req(input$doses),"</b>. Doses for already infected: <b>", req(input$doses),"</b>:<br/>",
                  "<br/>",
                  "  Number of vaccines done as of today: <b>", .format_num(dftday$vaccines),"</b>.<br/>",
                  "  Average vaccines per day: <b>", .format_num(dftday$vaccines_per_day),"</b>.<br/>",
                  "  With this average pace ", input$target,"% of the population will be covered with <b>",req(input$doses)," dose",ifelse(req(input$doses)==1, "","s"),"</b> by <b>",data()$achieved_date,"</b>.<br/>",
                  "<br/>",
                  "  Number of vaccines done last week: <b>", .format_num(dftday$lw_vaccines),"</b>.<br/>",
                  "  Average vaccines per day during last week: <b>", .format_num(dftday$lw_vaccines_per_day),"</b>.<br/>",
                  "  With this average pace ", input$target,"% of the population will be covered with <b>",req(input$doses), " dose",ifelse(req(input$doses)==1, "","s"),"</b> by <b>",data()$lw_achieved_date,"</b>.<br/>"
           )
         ), align = "left")
  })

  plotdata = df %>%
    select(date, new_vaccines) %>%
    rename(Date = date, Points = new_vaccines) %>%
    mutate(Status = "New Vaccinated")

  if (sum(plotdata$Points)>0) {
    #make plot if there are vaccine data
    output$vax_line <- renderPlot({
      #secondline = NULL
      message("compute rolling average")
      plotdata = plotdata %>%
        #mutate(WeeklyAvg = zoo::rollapplyr(Points, 7, mean, partial=TRUE, align = "right")) %>%
        mutate(WeeklyAvgVal := rollAvg(Points,Date))
      p <- plot_all_highlight(plotdata, log = FALSE, text = "Area", percent =FALSE,
                              date_x =  TRUE, g_palette = graph_palette[1],  secondline = FALSE, rollw = TRUE, keeporder = TRUE, barplot = FALSE)
      if (data()$target_vaccines_per_day > max(plotdata$Points, na.rm = TRUE)) {
        p = p + expand_limits(y = data()$target_vaccines_per_day*1.05) +
               scale_y_continuous(labels = lab_num, breaks = breaks_lab(c(plotdata$Points, data()$target_vaccines_per_day), .breaks.yaxis)) # add label

      }
      p = p + geom_line(size = 1.35)
      #p$theme$line$size = p$theme$line$size * 3
      # add line with target vaccination
      p <- p + #theme(legend.position = "none") +
        #theme(plot.caption = element_text(hjust=0.5, size=rel(1))) +
        geom_hline(yintercept = data()$target_vaccines_per_day, colour = "red3", linetype="dashed", size = 0.9) +
        annotate("text", x = min(plotdata$Date), y = data()$target_vaccines_per_day*1.07,
                 label = "Target Average Vaccines per Day", size = 2.5,
                 hjust = 0) +
        labs(caption = caption_vaccines(), hjust = 0.5#, #size = 2.5
        )
      # no legend no interactivity
      p

    })
  }
}
