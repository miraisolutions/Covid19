
#' given arguments select choices for nth plot
#' @param vars variable names in the drop down option.
#' @param actives if TRUE then add new_active and active variables to vars.
#' @param tests if TRUE then add new_test and test variables to vars.
#' @param hosp if TRUE then add new_hosp and hosp variables to vars.
#' @param strindx if TRUE then add stringency_index variable to vars.
#' @param log if FALSE then negative variables and new variables are removed
#' @param vax if TRUE then add new_vaccines and vaccines variables to vars.
#'
#' @noRd
choice_nthcases_plot = function(vars = .vars_nthcases_plot, actives = TRUE, tests = FALSE, hosp = FALSE, strindx = FALSE, vax = FALSE, log = TRUE){
  if (!actives && any(grepl("active",vars))) {
    vars = vars[!grepl("active", vars)]
  }
  if (!tests && any(grepl("test",vars))) {
    vars = vars[!grepl("test", vars)]
  }
  if (!hosp && any(vars %in% .hosp_vars)) {
    vars =setdiff(vars, .hosp_vars)
    vars =setdiff(vars, prefix_var(.hosp_vars, "new"))
  }
  if (hosp) {
    # TODO not yet adding other hosp vars and 1M pop
    vars = vars[!grepl("hosp", vars)]
    vars = append(vars, as.vector(.hosp_vars), after = min(grep("^new", vars))-1)
    #TODO review
    vars = append(vars, paste0("new_",as.vector(.hosp_vars)), after = min(grep("hosp", vars)))
  }
  if (!strindx) {
    vars = vars[!grepl("stringency", vars)]
  }
  if (!vax) {
    vars = vars[!grepl("vaccin", vars)]
  }
  if (!log) {
    vars = vars[!grepl("^new", vars) & !(vars %in% .neg_vars)]
  }
  choices_plot = varsNames(vars)
  choices_plot
}



#' compare_nth_cases_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id, Internal parameters for {shiny}.
#' @param vars variable names in the drop down option.
#' @param n_highlight number of countries to highlight
#' @param nn min number of cases for used
#' @param istop logical to choose title, if top n_highlight countries are selected
#' @param actives if TRUE then add new_active and active variables to vars.
#' @param tests if TRUE then add new_test and test variables to vars.
#' @param hosp if TRUE then add new_hosp and hosp variables to vars.
#' @param strindx if TRUE then add stringency_index variables to vars.
#' @param vax if TRUE then add new_vaccines and vaccines variables to vars.
#' @param oneMpop if TRUE then rescaled vars over 1M pop are available.
#' @param selectvar character variable selected in ui.
#' @param areasearch logical if TRUE replace with Country.Region selectInput
#' @param writetitle logical if TRUE title id is set
#'
#' @noRd
#'
#' @import shiny
#' @importFrom plotly plotlyOutput
#' @importFrom shinycssloaders withSpinner
mod_compare_nth_cases_plot_ui <- function(id, vars = .vars_nthcases_plot,
                                          istop = TRUE, n_highlight = 10, nn = 1000,
                                          actives = TRUE, tests = FALSE, hosp = FALSE, strindx = FALSE, oneMpop = TRUE, vax = FALSE, selectvar = "new_confirmed", areasearch = FALSE,
                                          writetitle = TRUE){
  ns <- NS(id)
  # TODO: fix height depending on numbe of widgets
  # if(!oneMpop && grepl("1M_pop$", selectvar))
  #   stop("oneMpop is F but selectvar is ", selectvar)
  choices_plot = choice_nthcases_plot(vars, actives, tests, hosp, strindx = strindx, vax = vax) # do not add stringency_index in possible choices
  if (istop) {
    plottitle = paste0("Top ",n_highlight," countries for the chosen variable")
  } else {
    plottitle = paste0("Timeline per variable",
                       ifelse(areasearch, paste0(" area", ifelse(oneMpop, " & Pop. size", "")), "")
                       )
  }
  # UI ----
  divtitle =  switch(writetitle ,div(class = "plottitle", plottitle, align = "center"),NULL)
  if (!oneMpop) {
    if (!areasearch) {
      tagList(
        #uiOutput(ns("title")),
        #div(h4(plottitle), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
        divtitle,
        fluidRow(
          column(7,
                 offset = 1,
                 div(class = "plottext",selectInput(inputId = ns("radio_indicator"), label = "Choose Variable",
                             choices = choices_plot, selected = selectvar))
          ),
          # column(4,
          #        selectInput(inputId = ns("radio_log_linear"), label = div(style = "font-size:10px","Log or Linear"),
          #                    choices = c("Log Scale" = "log", "Linear Scale" = "linear"), selected = "linear")
          # )
          column(4,
                 div(class = "plottext", selectInput(inputId = ns("time_frame"), label = "Choose time-frame",
                             choices = c("Last Month" = "lstmonth", "Last 6 Months" = "lst6month","Since Start" = "sincestart"),
                             selected = "lstmonth"))
          )
        ),
        fluidRow(
          withSpinner(plotlyOutput(ns("plot"), height = 400))
        ),
        #div(uiOutput(ns("caption")), align = "center")
        div(htmlOutput(ns("caption")), align = "center", class = "plottext")

      )
    } else {
      tagList(
        #uiOutput(ns("title")),
        #div(h4(plottitle), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
        divtitle,
        fluidRow(
          column(3,
                 offset = 1,
                 div(class = "plottext", selectInput(inputId = ns("radio_indicator"), label = ,"Choose Variable",
                             choices = choices_plot, selected = selectvar))
          ),
          # column(3, offset = 1,
          #        selectInput(inputId = ns("radio_log_linear"), label = div(style = "font-size:10px","Log or Linear"),
          #                    choices = c("Log Scale" = "log", "Linear Scale" = "linear"), selected = "linear")
          # ),
          column(3, offset = 1,
                 div(class = "plottext", selectInput(inputId = ns("time_frame"), label = "Choose time-frame",
                             choices = c("Last Month" = "lstmonth", "Last 6 Months" = "lst6month","Since Start" = "sincestart"),
                             selected = "lstmonth"))
          ),
          column(3, offset = 1,
                 div(class = "plottext", selectInput(label = "Select Countries",
                             inputId = ns("select_areas"), choices = NULL, selected = NULL,
                             multiple = TRUE))

          )
        ),
        fluidRow(
          withSpinner(plotlyOutput(ns("plot"), height = 400))
        ),
        #div(uiOutput(ns("caption")), align = "center")
        div(htmlOutput(ns("caption")), align = "center", class = "plottext", height = 10) # TODO: check why height = 10

      )

    }


  } else {
    if (!areasearch) {
      tagList(
        #uiOutput(ns("title")),
        #div(h4(plottitle), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
        divtitle,
        fluidRow(
          column(3,
                 offset = 1,
                 div(class = "plottext", selectInput(inputId = ns("radio_indicator"), label = "Choose Variable",
                             choices = choices_plot, selected = selectvar))
          ),
          column(3, offset = 1,
                 div(class = "plottext", selectInput(inputId = ns("radio_1Mpop"), label = "Total or Over 1M People",
                             choices = c("Total" = "tot", "Over 1M people" = "oneMpop"), selected = "oneMpop"))
          ),
          # column(3, offset = 1,
          #        selectInput(inputId = ns("radio_log_linear"), label = div(style = "font-size:10px","Log or Linear"),
          #                    choices = c("Log Scale" = "log", "Linear Scale" = "linear"), selected = "linear")
          # )
          column(3, offset = 1,
                 div(class = "plottext", selectInput(inputId = ns("time_frame"), label = "Choose time-frame",
                             choices = c("Last Month" = "lstmonth", "Last 6 Months" = "lst6month","Since Start" = "sincestart"), selected = "lstmonth"))
          ),
        ),
        fluidRow(
          withSpinner(plotlyOutput(ns("plot"), height = 400))
        ),        #div(uiOutput(ns("caption")), align = "center")
        div(htmlOutput(ns("caption")), align = "center", class = "plottext", height = 10)

      )
    } else {

      tagList(
        #uiOutput(ns("title")),
        #div(h4(plottitle), align = "center", style = "margin-top:20px; margin-bottom:20px;"),
        divtitle,
        fluidRow(
          column(3,
                 #offset = 1,
                 div(class = "plottext", selectInput(inputId = ns("radio_indicator"), label = "Choose Variable",
                             choices = choices_plot, selected = selectvar))
          ),
          column(3, #offset = 1,
                 div(class = "plottext", selectInput(inputId = ns("radio_1Mpop"), label = "Total or Over 1M People",
                             choices = c("Total" = "tot", "Over 1M people" = "oneMpop"), selected = "oneMpop"))
          ),
          column(3, #offset = 1,
                 div(class = "plottext", selectInput(inputId = ns("time_frame"), label = "Choose time-frame",
                             choices = c("Last Month" = "lstmonth", "Last 6 Months" = "lst6month","Since Start" = "sincestart"),
                             selected = "lstmonth"))
          ),
          column(3, #offset = 1,
                 div(class = "plottext", selectInput(label = "Select Countries", inputId = ns("select_areas"), choices = NULL, selected = NULL,
                             multiple = TRUE))
          )
        ),
        fluidRow(
          withSpinner(plotlyOutput(ns("plot"), height = 400))
        ),        #div(uiOutput(ns("caption")), align = "center")
        div(htmlOutput(ns("caption")), align = "center", class = "plottext", height = 10)

      )
    }

  }


}

#' compare_nth_cases_plot Server Function
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
#' @example ex-mod_compare_nth_cases_plot.R
#'
#' @importFrom plotly renderPlotly
#' @importFrom plotly ggplotly
#' @importFrom plotly layout
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#'
#' @noRd
mod_compare_nth_cases_plot_server <- function(input, output, session, df,
                                              nn = 1000,
                                              n_highlight = min(5,length(unique(df$Country.Region))), istop = TRUE, g_palette = graph_palette, datevar = "date",
                                              actives = TRUE, tests = FALSE, hosp = FALSE, strindx = FALSE, vax = FALSE, oneMpop = TRUE, secondline = NULL, areasearch = FALSE,
                                              vars = .vars_nthcases_plot){
  ns <- session$ns
  df$Date = df[[datevar]]
  message("mod_compare_nth_cases_plot_server")
  barp = reactive(length(unique(df$Country.Region)) ==1 &&  req(input$time_frame) != "sincestart")

  if (oneMpop || areasearch ) {
    if (areasearch) {
      countries =   df %>%
        select(date,AsOfDate, Country.Region,confirmed) %>%
        filter(date == AsOfDate) %>%
        arrange(desc(confirmed)) %>% .[,"Country.Region"]
      selected_countries = head(countries$Country.Region,3)
    }
    # Update radio_indicator, if oneMpop then some variables must be excluded
    observe({

      stridxvars = ifelse(strindx && is.null(secondline), TRUE, FALSE)
      choices_plot = choice_nthcases_plot(vars, actives, tests, hosp, strindx = stridxvars, vax = vax) # do not add stringency_index in possible choices
      #varselect = input$radio_indicator

      if (oneMpop) {
        varselect = req(input$radio_indicator)
        if (req(input$radio_1Mpop) == "oneMpop")  {
          if (all(is.na(df$population))) {
            stop("Missing population data")
          }
          choices_plot = intersect(get_aggrvars(), choices_plot)
          names(choices_plot) = names(varsNames(unlist(choices_plot)))
          message("update radio_indicator ", paste(unlist(choices_plot), collapse = ","))
          if (!(varselect) %in% unlist(choices_plot)) {
            message("update selected to new_confirmed")
            varselect = "new_confirmed"
          }
        }
        updateSelectInput(
          session,
          inputId = "radio_indicator",
          label = "Choose Variable",
          choices = choices_plot, selected = varselect
        )
      }

      if ( areasearch) {
        # Update select_areas
        areaselect = if (is.null(input$select_areas) || (!input$select_areas %in% df$Country.Region))
          selected_countries
        else
          input$select_areas
        message("observing select_areas: ", paste(areaselect, collapse = ","))
        updateSelectInput(session, "select_areas", choices = reactive(countries)()$Country.Region, selected = areaselect)

      }

    })
  }

  cum_vars = intersect(get_cumvars(), names(df))
  rollw = reactive(!req(input$radio_indicator) %in% cum_vars) # do not roll if cumulative var

  calc_line_plot = function(dat, vars, cum_vars) {

    .date_first_var = function(d, var, datevar = "date") {
      min(d[[datevar]][d[[var]] > 0], na.rm = TRUE)-1 # remove one day
    }

    reactSelectVar = reactive({

      if (grepl("rate_1M_pop$", req(input$radio_indicator)) && (!(req(input$radio_indicator) %in% names(dat)))) {
        varname = gsub("_rate_1M_pop$","",input$radio_indicator)
      } else
        varname = input$radio_indicator
      varname
    })
    # select only needed variables
    dat = dat %>% .[, c("Country.Region", "date","AsOfDate","Date", "population",intersect(c(vars,"confirmed"), names(dat)))]

    # filter off x before nn
    date_first_contagion = min(dat$date[dat$confirmed >= nn], na.rm = TRUE)
    dat = dat[dat$date >= date_first_contagion, , drop = FALSE]
    # Give dat standard structure; reacts to input$radio_indicator
    df_data_1Mpop <- reactive({
      data = dat
      if (oneMpop && !is.null(input$radio_1Mpop) && input$radio_1Mpop == "oneMpop")  {
        if (all(is.na(data$population))) {
          stop("Missing population data")
        }
        #if (!(paste(req(input$radio_indicator),"rate_1M_pop", sep = "_") %in% names(data))) {
        #varname = gsub("rate_1M_pop$","",reactSelectVar$radio_indicator)
        #reactSelectVar$radio_indicator = gsub("rate_1M_pop$","",reactSelectVar())
        data[, reactSelectVar()] = round(10^6*data[, reactSelectVar()] / data$population, 3)
        #}
      }
      data
    })

    date_first_var = reactive(
      .date_first_var(dat, reactSelectVar())
        # min(dat$date[dat[[reactSelectVar()]] > 0], na.rm = TRUE)-1 # remove one day
      )

    df_data_roll <- reactive({

      if (rollw()) {
        message("compare_nth_cases_plot: compute rolling average")
        data = df_data_1Mpop() %>%
          group_by(Country.Region) %>%
          #mutate(WeeklyAvg = zoo::rollapplyr(Value, 7, mean, partial=TRUE, align = "right")) %>%
          mutate(WeeklyAvgVal := rollAvg(!!sym(reactSelectVar()),date)) %>%
          ungroup()
        message("compare_nth_cases_plot: compute rolling average done")

      } else
        data = df_data_1Mpop()
      if (FALSE) #TODO
        data = data %>%
                  mutate(CasesPerWeekVal = WeeklyAvgVal * 7 / 10) # cases per 100 k

      # date_first_var = min(data$date[data[[reactSelectVar()]] > 0], na.rm = TRUE)-1 # remove one day
      # data = data[data$date >= date_first_var, , drop = FALSE]
      #date_first_var = min(data$date[data[[reactSelectVar()]] > 0], na.rm = TRUE)-1 # remove one day
      data = data[data$date >= date_first_var(), , drop = FALSE]

      data
    })
    df_data_timeframe <- reactive({

      data = df_data_roll()
      if (!is.null(input$time_frame)) {
        if (input$time_frame == "sincestart") {
          #data = data[data$date >= date_first_var, , drop = FALSE]
          #data = df_data_roll() # is it needed?
        } else {
          # df_year = as.integer(format(data$date, "%Y"))

          if (input$time_frame == "lst6month") {
            # n_days <- max()
            # dates_keep <- lmonth_dates_calc(df_year, data$date, nmonth = 30*6+1)
            # data = data[data$Date %in% dates_keep, , drop = FALSE]

            date_lst_6month = max(max(data$date) - 30*6+1,date_first_var()) # TODO: to be changed
            data = data[data$date >= date_lst_6month, , drop = FALSE]

          } else if (input$time_frame == "lstmonth") {
            # dates_keep <- lmonth_dates_calc(df_year, data$date)
            # data = data[data$Date %in% dates_keep, , drop = FALSE]

            date_lst_month = max(max(data$date) - 31,date_first_var()) # TODO: to be changed
            data = data[data$date >= date_lst_month, , drop = FALSE]
          }
          # cum_vars = intersect(get_cumvars(), names(data))
          if ((length(unique(as.character(data$Country.Region)))>0) && (reactSelectVar() %in% cum_vars)) {
            # if there are more than 1 country and if the variable is cumulative
            desc = ifelse(tail(data$date,1) > head(data$date,1), FALSE, TRUE )
            # varsscale = c(reactSelectVar())
            # if ((barp() || rollw() ))
            #   varsscale = c(varsscale, "WeeklyAvgVal")
            # non need to rescale WeeklyAvgVal, it is not being used if cumulative
            data = data %>%
              mutate( # add aggregated vars
                across(all_of(as.vector(reactSelectVar())), ~rescale_from_start(var = .x, category = Country.Region, lstmonth = TRUE, desc = desc)) # use all_of
              )
          }
        }
      }
      data
    })
    df_istop <- reactive({
      data = df_data_timeframe()
      if(istop) {
        # countries_order =  data %>% filter(date == max(date)) %>%
        #   arrange(desc(!!as.symbol(reactSelectVar()))) %>%
        #   #arrange(!!as.symbol(input$radio_indicator)) %>%
        #   top_n(n_highlight, wt = !!as.symbol(reactSelectVar())) %>% .[1:n_highlight,"Country.Region"] %>% as.vector()
        countries_order = data %>% filter(date == AsOfDate) %>%
          slice_max(!!as.symbol(reactSelectVar()), n = n_highlight, with_ties = FALSE) %>% .[,"Country.Region", drop = FALSE]
        data = data %>% right_join(countries_order)  %>%  # reordering according to variable if istop
          mutate(Country.Region = factor(Country.Region, levels = countries_order[,1, drop = TRUE]))
      }
      data
    })


    df_out <- reactive({
      data = df_istop()
      varsfinal = c("Country.Region", reactSelectVar(), "Date")
      if (strindx)
        varsfinal = unique(c(varsfinal, "stringency_index"))
      if (!is.null(secondline))
        varsfinal = unique(c(varsfinal, "stringency_index", secondline))
      if (rollw())
        varsfinal = c(varsfinal, "WeeklyAvgVal")

      out <- data %>% .[,varsfinal] %>%
        bind_cols(data[,reactSelectVar()] %>% setNames("Points")) %>%
        rename(Status = Country.Region ) %>%
        #rename(Date = contagion_day ) %>%
        select(-reactSelectVar())
      out
      # filter dates with 0 contagions
      #  df_out = df_tmp
      #}
    })

    log <- reactive({
      if (is.null(input$radio_log_linear))
        FALSE
      else
        req(input$radio_log_linear) != "linear"
    })

    #rollw = TRUE
    # Plot -----
    #output$plot <- renderPlotly({
      if (length(reactSelectVar()) == 0) {
        p <- blank_plot(where = "selected area", add = " All data missing")
      # } else if (length(reactSelectVar()) == 0){
      #   p <- blank_plot(where = "Variables have", add = " All data missing", what = reactSelectVar())
      } else {
        #secondline = NULL
        #if (!(input$radio_indicator %in% get_aggrvars()) || (input$time_frame != "sincestart"))
        # if  (!(input$radio_indicator %in% get_aggrvars()) )
        #   rollw = reactiveVal(FALSE)
        p <- plot_all_highlight(df_out(), log = log(), text = "Area", percent = ifelse(reactSelectVar() %in% .rate_vars, TRUE, FALSE),
                                date_x = ifelse(datevar == "date", TRUE,FALSE), g_palette,  secondline = secondline, rollw = rollw(), keeporder = keeporder, barplot = barp())
        tooltips = "text"
        p <- p %>%
          plotly::ggplotly(tooltip = tooltips)

        p <- p %>%
          plotly::layout(legend = list(orientation = "h", y = 1.1, yanchor = "bottom", font = list(family = "Arial, sans-serif", size = 10),
                                       itemwidth = 10, itemsizing = "constant",
                                       hovermode = 'closest', clickmode = "event", title = ""),
                         yaxis = list(autorange = TRUE, fixedrange = TRUE))
      }
      p

    #})
  } # end calc_line_plot
  if (!areasearch) {
    keeporder = FALSE
    pp <- reactive(calc_line_plot(df, vars, cum_vars))
  } else {
    keeporder = TRUE

    #observeEvent(!is.null(req(input$select_areas)),{
    pp <- eventReactive(!is.null(req(input$select_areas)),{

      #if (input$select_areas != "") {
        message("Observe event select_areas")
        df_select = df %>% filter(Country.Region %in% input$select_areas)
        idx = order(match(df_select$Country.Region, input$select_areas))
        df_select = df_select[idx,]
        calc_line_plot(df_select, vars, cum_vars)
     # }
    })
  }

  output$plot <- renderPlotly({
    pp()
  })

  output$caption <- renderText({
    #if  (!(input$radio_indicator %in% get_aggrvars()) || (input$time_frame != "sincestart"))
    # if  (!(input$radio_indicator %in% get_aggrvars()) )
    #   rollw = reactiveVal(FALSE)
    # message("barp = ", barp())
    # message("rollw = ", rollw())

    caption_explain = paste0(ifelse(rollw(),
                                    "Line computed as rolling weekly average. ", ""))
    if (strindx && (!is.null(secondline)) && secondline == "stringency_index") {
      caption_explain = c(caption_explain, paste("Dashed lines represent", caption_stringency()))
    }
    paste0("<p>", caption_explain, sep = '</p>')

  })

}

