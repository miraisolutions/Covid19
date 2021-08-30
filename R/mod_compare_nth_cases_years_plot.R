#' mod_compare_timeline_plot_ui UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param istop logical to choose title, if top n_highlight countries are selected
#' @param n_highlight number of countries to highlight
#' @param nn min number of cases for used
#' @param vars variable names in the drop down option.
#' @param actives if TRUE then add new_active and active variables to vars.
#' @param tests if TRUE then add new_test and test variables to vars.
#' @param hosp if TRUE then add new_hosp and hosp variables to vars.
#' @param strindx if TRUE then add stringency_index variables to vars.
#' @param vax if TRUE then add new_vaccines and vaccines variables to vars.
#' @param oneMpop if TRUE then rescaled vars over 1M pop are available.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom shinycssloaders withSpinner
mod_compare_timeline_plot_ui <- function(id, titles = 1:3,
                                         istop = TRUE, n_highlight = 10, nn = 1000,
                                         actives = TRUE, tests = FALSE, hosp = FALSE, strindx = FALSE, oneMpop = TRUE, vax = FALSE){
  ns <- NS(id)#
  #plottitle = paste0("Select view")

  plottitleTLSE = "Timeline per variable"
  plottitleTLCY = "Timeline per calendar year"
  plottitleTLTE = "Pandemic time evolution"
  alltitles = c(plottitleTLSE, plottitleTLCY, plottitleTLTE)[titles]

  plot_tabs <- tabsetPanel(
    id = ns("title"),
    type = "hidden",
    tabPanel("Timeline per variable",
             mod_compare_nth_cases_plot_ui(ns("timelines_plot"),
                                        nn = nn, n_highlight = n_highlight,
                                        istop = istop, tests = tests, hosp = hosp, strindx = strindx, oneMpop = oneMpop, vax = vax,
                                        selectvar = "new_confirmed", writetitle = FALSE)
    ),
    tabPanel("Timeline per calendar year",
             mod_compare_nth_cases_years_plot_ui(ns("lines_plot"), vars = .vars_nthcases_plot,
                                                 n_highlight = n_highlight,
                                                 istop = istop, tests = tests, hosp = hosp, strindx = strindx, vax = vax,
                                                 selectvar = "new_deaths", writetitle = FALSE)
    ),
    tabPanel("Pandemic time evolution",
             mod_plot_log_linear_ui(ns("timelinearea_plot"))
    )
  )

  # UI ----
  tagList(
    #uiOutput(ns("title")),
    #div(h4(plottitle), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
    fluidRow(
      column(7,
             offset = 4,
             selectInput(inputId = ns("plot_indicator"), label = "Select view",
                         choices = alltitles, selected = alltitles[1])
      )#,
    ),
    tabPanel("Panel plot",
             plot_tabs
    )
  )

}
#' mod_compare_timeline_plot_server Server Function, chooses between TimeLine plot per variable or Calendar year, or AreaPlot
#'
#' @param df data.frame
#' @param nn minimum date derived from first day with more than nn cases. Default 1000
#' @param n_highlight number of countries to highlight if istop == TRUE
#' @param istop logical to choose title, if top n_highlight countries are selected
#' @param actives if TRUE then add new_active and active variables to vars.
#' @param tests if TRUE then add new_test and test variables to vars.
#' @param hosp if TRUE then add new_hosp and hosp variables to vars.
#' @param strindx if TRUE then add stringency_index variables to vars.
#' @param vax if TRUE then add new_vaccines and vaccines variables to vars.
#'
#' @example ex-mod_compare_nth_cases_years_plot.R
#'
#' @importFrom plotly renderPlotly
#' @importFrom plotly ggplotly
#' @importFrom plotly layout
#' @import dplyr
#' @import tidyr
#' @import lubridate
#' @import ggplot2
#'
#' @noRd
mod_compare_timeline_plot_server <- function(input, output, session, df,
                                             nn = 1000,
                                             n_highlight = min(5,length(unique(df$Country.Region))), istop = TRUE, g_palette = graph_palette, datevar = "date",
                                             actives = TRUE, tests = FALSE, hosp = FALSE, strindx = FALSE, vax = FALSE, oneMpop = TRUE, secondline = NULL, areasearch = FALSE){
  ns <- session$ns

  observeEvent(req(input$plot_indicator), {
    updateTabsetPanel(session = session, inputId = "title", selected = input$plot_indicator)

    message("Process TimeLine Plot ", req(input$plot_indicator))

    switch(req(input$plot_indicator),
           "Timeline per calendar year" = {
             callModule(mod_compare_nth_cases_years_plot_server, "lines_plot",df,
                        nn = nn,
                        n_highlight = n_highlight, istop = istop, g_palette = graph_palette, datevar = datevar,
                        secondline = NULL)#, secondline = "stringency_index")

           }, "Timeline per variable" = {
             callModule(mod_compare_nth_cases_plot_server, "timelines_plot", df ,  nn = nn,
                        n_highlight = n_highlight, istop = istop, g_palette = graph_palette, datevar = "date",
                        actives = actives, tests = tests, hosp = hosp, strindx = strindx, vax = vax, oneMpop = oneMpop, secondline = secondline, areasearch = areasearch)#, secondline = "stringency_index")

           }, "Pandemic time evolution" ={
             levs <- areaplot_vars()
             callModule(mod_plot_log_linear_server, "timelinearea_plot", df = df, type = "area", process_data = TRUE, fun.args = list(levs = levs, nn = nn))

           },
           stop("Wrong title selected")
    )


  })
  #, priority = 10)

}

#' compare_nth_cases_years_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param vars variable names in the drop down option.
#' @param istop logical to choose title, if top n_highlight countries are selected
#' @param n_highlight number of countries to highlight
#' @param actives if TRUE then add new_active and active variables to vars.
#' @param tests if TRUE then add new_test and test variables to vars.
#' @param hosp if TRUE then add new_hosp and hosp variables to vars.
#' @param strindx if TRUE then add stringency_index variables to vars.
#' @param vax if TRUE then add new_vaccines and vaccines variables to vars.
#' @param oneMpop if TRUE then rescaled vars over 1M pop are available.
#' @param selectvar character variable selected in ui.
#' @param writetitle logical if TRUE writes title in input
#'
#' @noRd
#'
#' @import shiny
#' @importFrom plotly plotlyOutput
#' @importFrom shinycssloaders withSpinner
mod_compare_nth_cases_years_plot_ui <- function(id, vars = .vars_nthcases_plot,
                                                istop = TRUE, n_highlight = 10,
                                                actives = TRUE, tests = TRUE, hosp = TRUE, strindx = TRUE, vax = TRUE, selectvar = "new_deaths", writetitle = TRUE){
  ns <- NS(id)

  choices_plot = choice_nthcases_plot(vars, actives = actives, tests = tests, hosp = hosp, strindx = strindx, vax = vax) # do not add stringency_index in possible choices


  plottitle = paste0("Timeline per calendar year")

  divtitle =  switch(writetitle,div(h4(plottitle), align = "center", style = "margin-top:20px; margin-bottom:20px;"),NULL)

  # UI ----
  tagList(
    #uiOutput(ns("title")),
    #div(h4(plottitle), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
    divtitle,
    fluidRow(
      column(7,
             offset = 1,
             selectInput(inputId = ns("radio_indicator"), label = "",
                         choices = choices_plot, selected = selectvar)
      )#,
      # column(4,
      #        selectInput(inputId = ns("radio_log_linear"), label = "",
      #                    choices = c("Log Scale" = "log", "Linear Scale" = "linear"), selected = "linear")
      # )
    ),
    withSpinner(plotlyOutput(ns("plot"), height = 400)),
    #div(uiOutput(ns("caption")), align = "center")
    div(htmlOutput(ns("caption")), align = "center", height = 10)
  )



}

#' mod_compare_nth_cases_years_plot_server Server Function, drows line plot of each calendar year
#'
#' @param df data.frame
#' @param nn minimum date derived from first day with more than nn cases. Default 1000
#' @param n_highlight number of countries to highlight if istop == TRUE
#' @param istop logical to choose title, if top n_highlight countries are selected
#' @param g_palette character vector of colors for the graph and legend
#' @param datevar character variable used for X axis, date or contagion_day
#' @param actives if TRUE then add new_active and active variables to vars.
#' @param tests if TRUE then add new_test and test variables to vars.
#' @param hosp if TRUE then add new_hosp and hosp variables to vars.
#' @param strindx if TRUE then add stringency_index variables to vars.
#' @param vax if TRUE then add new_vaccines and vaccines variables to vars.
#' @param secondline second variable to be plotted for all vars
#' @param areasearch logical if TRUE Country.Region selectInput is used
#'
#' @example ex-mod_compare_nth_cases_years_plot.R
#'
#' @importFrom plotly renderPlotly
#' @importFrom plotly ggplotly
#' @importFrom plotly layout
#' @import dplyr
#' @import tidyr
#' @import lubridate
#' @import ggplot2
#'
#' @noRd
mod_compare_nth_cases_years_plot_server <- function(input, output, session, df,
                                                    nn = 1000,
                                                    n_highlight = min(5,length(unique(df$Country.Region))), istop = TRUE, g_palette = graph_palette, datevar = "date",
                                                    secondline = NULL){
  ns <- session$ns
  df$Date = df[[datevar]]

  rollw = reactiveVal(TRUE)

  df = df %>% .[, c("Country.Region", "date","Date", "population", intersect(.vars_nthcases_plot, names(df)))] %>% mutate(year = format(df$Date, format = "%Y"))

  cum_vars = intersect(get_cumvars(), names(df))

  # put all to the same year
  df$year = as.integer(format(df$date, "%Y"))

  days_year = ifelse(lubridate::leap_year(df$date), 366, 365)

  df$d.year = (df$year - min(df$year)) * days_year
  df$Date = df$date - df$d.year # (to consider leap year)
  # make factor

  # df$Date = format.Date(df$Date, "%m-%d")
  # df$Date = factor(df$Date, ordered = TRUE)

  # rescale aggr vars removing data on 1st available date
  .rescale_year_start = function(var, years) {
    # depends on whether data are sorted asc or desc
    if (years[1] == 0) { # if desc
      id_last_day_of_year = which(!duplicated(years)) -1
      last_day_of_prev_year = c(rep(0, length = table(years)[as.character(unique(years))][1]),
                                rep(var[id_last_day_of_year[-1]], times = table(years)[as.character(unique(years))][-1]))

    } else{
      id_last_day_of_year = which(!duplicated(years))

      last_day_of_prev_year = c(rep(var[id_last_day_of_year[-1]], times = table(years)[as.character(unique(years))][-length(id_last_day_of_year)]),
                                rep(0, length = table(years)[as.character(unique(years))][length(id_last_day_of_year)]))

    }
    var =  var - last_day_of_prev_year
    var
  }

  # if (any(years>0)) {# if there are 2 years at least
  #   df = df %>%
  #     mutate( # add aggregated vars
  #       across(all_of(as.vector(cum_vars)), ~.rescale_year_start(dat = .x, years = years)) # use all_of
  #     )
  # }

  # year column
  # stop if multiple countries are passed

  .calc_line_plot = function(dat, .vars_nthcases_plot, cum_vars = cum_vars) {

    # select only needed variables
    #dat = df %>% .[, c("Country.Region", "date","Date", "population", intersect(.vars_nthcases_plot, names(dat)))]
    # Give dat standard structure; reacts to input$radio_indicator
    df_data <- reactive({
      # filter off x before nn
      date_first_var = min(dat$date[dat[[input$radio_indicator]] > 0], na.rm = TRUE)-1 # remove one day
      dat = dat[dat$date >= date_first_var, , drop = FALSE]

      varsfinal = c("Country.Region", "year", input$radio_indicator, "Date")

      if (any(dat$d.year>0) && (input$radio_indicator %in% cum_vars)) {
        # if there are more than 2 years and if the variable is cumulative
        dat = dat %>%
          mutate( # add aggregated vars
            #across(all_of(as.vector(cum_vars)), ~.rescale_year_start(dat = .x, years = years)) # use all_of
            across(all_of(as.vector(input$radio_indicator)), ~.rescale_year_start(var = .x, years = d.year)) # use all_of
          )
      }
      # if (strindx)
      #   varsfinal = unique(c(varsfinal, "stringency_index"))
      # if (!is.null(secondline))
      #   varsfinal = unique(c(varsfinal, "stringency_index", secondline))
      df_out <- dat %>% .[,varsfinal] %>%
        bind_cols(dat[,input$radio_indicator] %>% setNames("Value")) %>% #arrange(Date) %>%
        rename(Status = year ) %>%
        #rename(Date = contagion_day ) %>%
        select(-input$radio_indicator)
      df_out
    })


    #rollw = TRUE
    # Plot -----
    output$plot <- renderPlotly({
      #secondline = NULL
      # select roll depending on variable
      if (!(input$radio_indicator %in% get_aggrvars()))
        rollw = reactiveVal(FALSE)

      p <- plot_all_highlight(df_data(), log = FALSE, text = "Year", percent = ifelse(input$radio_indicator %in% .rate_vars, TRUE, FALSE),
                              date_x = TRUE, g_palette,  secondline = FALSE, rollw = rollw(), keeporder = TRUE)
      p <- p %>%
        plotly::ggplotly(tooltip = c("text", "x_tooltip", "y_tooltip"))
      # change date format
      p$x$data = lapply(p$x$data, function(dat)  {
        dat$text = gsub("Date: [0-9]+-", "Date: ", dat$text)
        dat
      })

      if (length(unique(df_data()$Status)) == 1)
        p <- p %>%
        plotly::layout(legend = list(orientation = "h", y = 1.1, yanchor = "bottom", itemsizing = "constant"))

      p

    })

  }

  .calc_line_plot(df, .vars_nthcases_plot, cum_vars)


  output$caption <- renderText({
    if (!(input$radio_indicator %in% get_aggrvars()))
      rollw = reactiveVal(FALSE)
    caption_explain = paste0(ifelse(rollw(), "Computed as rolling weekly average. ", ""), "Calendar year comparison.")

    paste0("<p>", caption_explain, sep = '</p>')

  })

}

