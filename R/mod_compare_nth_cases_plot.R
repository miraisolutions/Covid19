#' given arguments select choices for nth plot
#' @param vars variable names in the drop down option.
#' @param actives if TRUE then add new_active and active variables to vars.
#' @param tests if TRUE then add new_test and test variables to vars.
#' @param hosp if TRUE then add new_hosp and hosp variables to vars.
#'
choice_nthcases_plot = function(vars = vars_nthcases_plot, actives = TRUE, tests = FALSE, hosp = FALSE){
  if (!actives && any(grepl("active",vars))) {
    #message("remove active var for ", id)
    vars = vars[!grepl("active", vars)]
  }
  if (!tests && any(grepl("test",vars))) {
    #message("remove tests vars for ", id)
    vars = vars[!grepl("test", vars)]
  }
  if (!hosp && any(vars %in% .hosp_vars)) {
    #message("remove hosp vars for ", id)
    for(hospvar in .hosp_vars) {
      vars = vars[!grepl(hospvar, vars)]
    }
  }
  if (hosp) {
    #message("add hosp vars for ", id)
    # TODO not yet adding other hosp vars and 1M pop
    vars = vars[!grepl("hosp", vars)]
    vars = append(vars, as.vector(.hosp_vars), after = min(grep("^new", vars))-1)
    #TODO review
    vars = append(vars, paste0("new_",as.vector(.hosp_vars)), after = min(grep("hosp", vars)))
  }
  # if (oneMpop) {
  #   #message("remove 1M pop for ", id)
  #   #vars = vars[!grepl("1M_pop$", vars)]
  #   # add one min pop vars
  #   aggrvars = intersect(get_aggrvars(), vars)
  #   vars = c(vars, paste(aggrvars, "rate_1M_pop", sep = "_"))
  # }
  choices_plot = varsNames(vars)
  choices_plot
}



#' compare_nth_cases_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param vars variable names in the drop down option.
#' @param actives if TRUE then add new_active and active variables to vars.
#' @param tests if TRUE then add new_test and test variables to vars.
#' @param hosp if TRUE then add new_hosp and hosp variables to vars.
#' @param oneMpop if TRUE then rescaled vars over 1M pop are available.
#' @param selectvar character variable selected in ui.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom plotly plotlyOutput
#' @importFrom shinycssloaders withSpinner
mod_compare_nth_cases_plot_ui <- function(id, vars = vars_nthcases_plot,
                                          # vars = c(#"confirmed", "deaths", "recovered", "active", "hosp",
                                          #              names(.case_colors),
                                          #              prefix_var(names(.case_colors), "new"),
                                          #              #"new_confirmed", "new_deaths", "new_active",
                                          #              #"new_prevalence_rate_1M_pop",
                                          #              #"new_tests", #"new_tests_rate_1M_pop","new_positive_tests_rate",
                                          #               "growth_factor_3", "lethality_rate" ),
                                          actives = TRUE, tests = FALSE, hosp = FALSE, selectvar = "new_confirmed", oneMpop = TRUE){
  ns <- NS(id)
  # if(!oneMpop && grepl("1M_pop$", selectvar))
  #   stop("oneMpop is F but selectvar is ", selectvar)

  choices_plot = choice_nthcases_plot(vars, actives, tests, hosp )

  # UI ----
  if (!oneMpop) {
    tagList(
      uiOutput(ns("title")),
      fluidRow(
        column(7,
               offset = 1,
               selectInput(inputId = ns("radio_indicator"), label = "",
                           choices = choices_plot, selected = selectvar)
        ),
        column(4,
               selectInput(inputId = ns("radio_log_linear"), label = "",
                           choices = c("Log Scale" = "log", "Linear Scale" = "linear"), selected = "linear")
        )
      ),
      withSpinner(plotlyOutput(ns("plot"), height = 400)),
      div(uiOutput(ns("caption")), align = "center")
    )
  } else {
    tagList(
      uiOutput(ns("title")),
      fluidRow(
        column(4,
               offset = 0,
               selectInput(inputId = ns("radio_indicator"), label = "",
                           choices = choices_plot, selected = selectvar)
        ),
        column(2, offset = 1,
               selectInput(inputId = ns("radio_1Mpop"), label = "",
                           choices = c("Total" = "tot", "Over 1M people" = "oneMpop"), selected = "tot")
        ),
        column(4, offset = 1,
               selectInput(inputId = ns("radio_log_linear"), label = "",
                           choices = c("Log Scale" = "log", "Linear Scale" = "linear"), selected = "linear")
        )
      ),
      withSpinner(plotlyOutput(ns("plot"), height = 400)),
      div(uiOutput(ns("caption")), align = "center")
    )
  }


}

#' compare_nth_cases_plot Server Function
#'
#' @param df data.frame
#' @param nn minimum date derived from first day with more than nn cases. Default 1000
#' @param w number of days of outbreak. Default 7
#' @param n_highligth number of countries to highlight if istop == TRUE
#' @param istop logical to choose title, if top n_highligth countries are selected
#' @param g_palette character vector of colors for the graph and legend
#' @param datevar character variable used for X axis, date or contagion_day
#' @param actives if TRUE then add new_active and active variables to vars.
#' @param tests if TRUE then add new_test and test variables to vars.
#' @param hosp if TRUE then add new_hosp and hosp variables to vars.
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
                                              nn = 1000, w = 7,
                                              n_highligth = min(5,length(unique(df$Country.Region))), istop = TRUE, g_palette = graph_palette, datevar = "date",
                                              actives = TRUE, tests = FALSE, hosp = FALSE, oneMpop = TRUE){
  ns <- session$ns
  df$Date = df[[datevar]]

  reactSelectVar = reactive({
    if (grepl("rate_1M_pop$", input$radio_indicator) && (!(input$radio_indicator %in% names(df)))) {
      varname = gsub("_rate_1M_pop$","",input$radio_indicator)
    } else
      varname = input$radio_indicator
    varname
  })

  if (oneMpop) {
    # Update radio_indicator, if oneMpop then some variables must be excluded
    observe({
      choices_plot = choice_nthcases_plot(vars_nthcases_plot, actives, tests, hosp)
      varselect = input$radio_indicator

      if (req(input$radio_1Mpop) == "oneMpop")  {
        if (all(is.na(df$population)))
          stop("Missing population data")
        choices_plot = intersect(get_aggrvars(), choices_plot)
        names(choices_plot) = names(varsNames(unlist(choices_plot)))
        message("update radio_indicator ", paste(unlist(choices_plot), collapse = ","))
        if (!(varselect) %in% unlist(choices_plot)) {
          message("update selected")
          varselect = "new_confirmed"
        }
      }
      updateSelectInput(
        session,
        inputId = "radio_indicator",
        label = "",
        choices = choices_plot, selected = varselect
      )
    })
  }

  # Give DF standard structure; reacts to input$radio_indicator
  df_data <- reactive({
    if (FALSE){ # old without indicator oneMpop
      if (grepl("rate_1M_pop$", req(input$radio_indicator)))  {
        if (all(is.na(df$population)))
          stop("Missing population data")
        if (!(req(input$radio_indicator) %in% names(df))) {
          #varname = gsub("rate_1M_pop$","",reactSelectVar$radio_indicator)
          #reactSelectVar$radio_indicator = gsub("rate_1M_pop$","",reactSelectVar())
          message("divide by pop size")
          df[, reactSelectVar()] = round(10^6*df[, reactSelectVar()] / df$population, 3)
        }
      }
    }
    if (!is.null(input$radio_1Mpop) && req(input$radio_1Mpop) == "oneMpop")  {
      if (all(is.na(df$population)))
        stop("Missing population data")
      #if (!(paste(req(input$radio_indicator),"rate_1M_pop", sep = "_") %in% names(df))) {
        #varname = gsub("rate_1M_pop$","",reactSelectVar$radio_indicator)
        #reactSelectVar$radio_indicator = gsub("rate_1M_pop$","",reactSelectVar())
        message("divide by pop size")
        df[, reactSelectVar()] = round(10^6*df[, reactSelectVar()] / df$population, 3)
      #}
    }


    if(istop) {
      countries_order =  df %>% filter(date == max(date)) %>%
        arrange(desc(!!as.symbol(reactSelectVar()))) %>%
        #arrange(!!as.symbol(input$radio_indicator)) %>%
        top_n(n_highligth, wt = !!as.symbol(reactSelectVar())) %>% .[1:n_highligth,"Country.Region"] %>% as.vector()
      data = df %>% right_join(countries_order)  %>%  # reordering according to variable if istop
                mutate(Country.Region = factor(Country.Region, levels = countries_order[, "Country.Region", drop = T]))
    } else {
      data = df
    }

    # filter off x before nn
    date_first_contagion = min(data$date[data$confirmed >= nn], na.rm = TRUE)
    data = data[data$date >= date_first_contagion, ]

    df_tmp <- data %>% .[,c("Country.Region", reactSelectVar(), "Date")] %>%
      bind_cols(data[,reactSelectVar()] %>% setNames("Value")) %>%
      rename(Status = Country.Region ) %>%
      #rename(Date = contagion_day ) %>%
      select(-reactSelectVar())

    # filter dates with 0 contagions

    if (istop && ("China" %in% df_tmp$Status) && datevar == "contagion_day") {
      # Day of the country with max contagions after china
      max_contagion_no_china <- df_tmp %>%
        filter(Status != "China") %>%
        filter(Date == max(Date)) %>%
        select(Date) %>% unique() %>%
        as.numeric()
      df_out <- df_tmp %>%
        #filter(Status %in% as.vector(countries$Status)) %>% #pick only filtered countries, not needed, now done before
        filter(Date <= max_contagion_no_china) #%>% #cut china
    } else {
      df_out = df_tmp
    }

    df_out
  })

  log <- reactive({
    req(input$radio_log_linear) != "linear"
  })

  # Plot -----
  output$plot <- renderPlotly({
    p <- plot_all_highlight(df_data(), log = log(), text = "Area", percent = ifelse(reactSelectVar() %in% .rate_vars, TRUE, FALSE),
                            date_x = ifelse(datevar == "date", TRUE,FALSE), g_palette)
    p <- p %>%
      plotly::ggplotly(tooltip = c("text", "x_tooltip", "y_tooltip")) %>%
      plotly::layout(legend = list(orientation = "h", y = 1.1, yanchor = "bottom", itemsizing = "constant"))
    p

  })

  if (istop) {
    output$title <- renderUI({
      div(h4(paste0("Top ",n_highligth," countries from day of ", nn ," contagion")), align = "center", style = "margin-top:20px; margin-bottom:20px;")
    })
  } else {
    output$title <- renderUI({
      div(h4(paste0("Timeline from day with ", nn ," contagions")), align = "center", style = "margin-top:20px; margin-bottom:20px;")
    })
  }

  output$caption <- renderUI({

      p(paste0("Computed as rolling weekly average. ",ifelse(datevar == "date", "First day", "Contagion day 0"),
               " is the day when ", nn," confirmed cases are reached."))
  })
}

