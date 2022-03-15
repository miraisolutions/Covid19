#' caseBoxes UI Function
#'
#' @description A shiny Module for displaying stats about vaccination
#'
#' @param id, Internal parameters for {shiny}.
#'
#' @import shiny
#' @importFrom lubridate day
#'
#' @rdname mod_vaccines_text
mod_vaccines_text_ui <- function(id) {
  targetdate = Sys.Date()+50
  targetdate_day = lubridate::day(targetdate)
  targetdate = targetdate - targetdate_day + 1

  ns <- NS(id)
  tagList(
    div("Vaccination Pace", align = "center", class = "plottitle"),
    fluidRow(
      column(3, div(class = "plottext",
                    numericInput(inputId = ns("target"), label = "% Target coverage",
                             value = 90,
                             min = 0,
                             max = 100,
                             step = 1))),
      # column(3, div(class = "plottext",
      #               selectInput(inputId = ns("doses"), label = "Number of doses",
      #                       choices = c(1,2),
      #                       selected = 2))),
      # column(3, div(class = "plottext",
      #               selectInput(inputId = ns("confdoses"), label = "Doses for already infected",
      #                       choices = c(0,1,2),
      #                       selected = 1))),
      column(3, div(class = "plottext",
                    dateInput(inputId = ns("tdate"), label = "Target date",
                          value = targetdate,
                          min = Sys.Date()
      )))
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

#' No data in variable text Server Function
#' @param input,output,session Internal parameters for {shiny}.
#' @param country character country name
#' @param what variable name
#'
#' @importFrom graphics plot.new
#' @rdname mod_vaccines_text
mod_novaccines_text_server <- function(input, output, session, country, what = "Vaccines") {

  output$vax_text = renderUI({
    div( class = "count-box",
         style = "color: white; max-width: 100%; background-color: #3c8dbc; margin-left: 20px; margin-right: 20px; font-style: italic; white-space: nowrap; word-wrap: break-word",
         HTML(
           paste0("  No ",what," data for ",strong(country), ".")
         ), align = "left")
  })
  output$vax_line <- renderPlot({
    plot.new()
  })
}
#' Vaccines text Server Function
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @param df dataset whole time series.
#' @param dftoday dataset with today and weekly data.
#'
#' @rdname mod_vaccines_text
mod_vaccines_text_server <- function(input, output, session, df, dftoday) {

  df = df %>% select(date, Country.Region, vaccines, new_people_fully_vaccinated, fully_vaccinated_rate, population) %>%
    filter(vaccines > 0) %>% select(-vaccines)

  dftday = dftoday %>% select(date, Country.Region, people_fully_vaccinated,
                              lw_people_fully_vaccinated, fully_vaccinated_rate,
                              confirmed, population, deaths) %>%
    mutate(population = population - deaths,
           #lw_vaccines_per_day = lw_vaccines/7,
           lw_people_fully_vaccinated_per_day = lw_people_fully_vaccinated/7,
    ) #%>%
  #select(-deaths)
  dftday$start_vaccines_date = min(df$date) # vaccination start dare
  # number of days of vaccination campaign
  dftday$vaccines_days = as.numeric(dftday$date-dftday$start_vaccines_date )
  #vaccines done so far per day
  dftday$people_fully_vaccinated_per_day = dftday$people_fully_vaccinated/dftday$vaccines_days

  # target_inp <- reactiveValues(target = input$target)

  data = reactive({

    idx_target <- which(df$fully_vaccinated_rate >= input$target/100)
    day_target_r <- NA
    if (length(idx_target) > 0)
      day_target_r <-  df$date[max(idx_target)]
    dftday %>% # if target already reached then 0
      # mutate(vaccinated_people = (round(vaccines/as.integer(req(input$doses))) - (confirmed-deaths)) +
      #           as.integer(req(input$confdoses)) * (confirmed-deaths)) %>%
      mutate(target_pop = input$target/100* population) %>%
      #  target*population -   (doses * ((population - vaccines/doses - (confirmed - deaths)) +
      mutate(people_left_to_target = max(0, target_pop - people_fully_vaccinated)) %>%
      # # achieved_date = today + (target% * (targetdose * (population- round(vaccines/targetdose) - (confirmed-deaths)) + targetdoseconf * (confirmed-deaths))) / (lw_vaccines/7)
      mutate(lw_achieved_date = date + ceiling((people_left_to_target) / (lw_people_fully_vaccinated_per_day)),
             # achieved_date = today + (target% * targetdose * (population- vaccines- (confirmed-deaths))) + targetdoseconf * (confirmed-deaths)) / (vaccines/(today-startdate))
             achieved_date = date + ceiling((people_left_to_target) / (people_fully_vaccinated/ vaccines_days))) %>%
      #days_to_target = number of days between today and target date
      mutate(days_to_target = as.numeric(input$tdate - date)) %>%
      # add number of daily vaccines required to achieve target
      mutate(target_people_per_day = (people_left_to_target) / days_to_target ) %>%
      # day target was reached
      mutate(day_target_reached = day_target_r)

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
           paste0("  ",strong(dftday$Country.Region), ". Population: ", .format_num(dftday$population),". Fully Vaccinated: ",paste0(round(dftday$fully_vaccinated_rate*100,1), "%"),"<br/>",
                  "  Target Date: <b>", input$tdate,"</b>. <b>", .format_num(data()$days_to_target), "</b> days remaining. People left to target: <b>", .format_num(data()$people_left_to_target), "</b>.<br/>",
                  "  Target Coverage: <b>", input$target,"%</b>.<br/>",
                  "  Required Vaccinations per day to cover ",input$target," % of the population by <b>", input$tdate,"</b>: <b>",
                  .format_num(data()$target_people_per_day),"</b>.<br/>",
                  #"  Target Doses: <b>", req(input$doses),"</b>. Doses for already infected: <b>", req(input$doses),"</b>:<br/>",
                  "<br/>",
                  "  Number of Vaccinations done as of today: <b>", .format_num(dftday$people_fully_vaccinated),"</b>.<br/>",
                  "  Average Vaccinations per day: <b>", .format_num(dftday$people_fully_vaccinated_per_day),"</b>.<br/>",
                  ifelse(data()$people_left_to_target > 0, "  With this average pace ", ""),
                  input$target,"% of the population ",
                  ifelse(data()$people_left_to_target > 0,
                    paste0("will be covered with <b>2 doses</b> by <b>",data()$achieved_date,"</b>"),
                    paste0("was covered with <b>2 doses</b> on <b>",data()$day_target_reached,"</b>")
                    ),
                  "<br/>",
                  "  Number of Vaccinations done last week: <b>", .format_num(dftday$lw_people_fully_vaccinated),"</b>.<br/>",
                  "  Average Vaccinations per day during last week: <b>", .format_num(dftday$lw_people_fully_vaccinated_per_day),"</b>.<br/>",
                  ifelse(data()$people_left_to_target > 0, "  With this average pace ", ""),
                  input$target,"% of the population ",
                  ifelse(data()$people_left_to_target > 0,
                         paste0("will be covered with <b>2 doses</b> by <b>",data()$lw_achieved_date,"</b>"),
                         paste0("was covered with <b>2 doses</b> on <b>",data()$day_target_reached,"</b>")
                  ),
                  "<br/>")
         ), align = "left")
  })

  plotdata = df %>%
    select(date, new_people_fully_vaccinated) %>%
    rename(Date = date, Points = new_people_fully_vaccinated) %>%
    mutate(Status = "New Vaccinations")

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
      if (data()$target_people_per_day > max(plotdata$Points, na.rm = TRUE)) {
        p = p + expand_limits(y = data()$target_people_per_day*1.05) +
               scale_y_continuous(labels = lab_num, breaks = breaks_lab(c(plotdata$Points, data()$target_people_per_day), .breaks.yaxis)) # add label

      }
      p = p + geom_line(size = 1.35)
      #p$theme$line$size = p$theme$line$size * 3
      # add line with target vaccination
      p <- p + #theme(legend.position = "none") +
        #theme(plot.caption = element_text(hjust=0.5, size=rel(1))) +
        geom_hline(yintercept = data()$target_people_per_day, colour = "red3", linetype="dashed", size = 0.9) +
        annotate("text", x = min(plotdata$Date), y = data()$target_people_per_day*1.07,
                 label = "Target Average Vaccinations per Day", size = 2.5,
                 hjust = 0) +
        labs(caption = caption_vaccines(), hjust = 0.5#, #size = 2.5
        )
      # no legend no interactivity
      p

    })
  }
}
