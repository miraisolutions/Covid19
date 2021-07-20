vars_vs_stringency = function(ax) {
  list(label = div(style = "font-size:10px",paste0("Select (",ax,") variable")),
       choices = varsNames(c("lw_confirmed_rate_1M_pop", "growth_factor_7", "lw_positive_tests_rate", #"lw_hosp", "lw_icuvent",
                             "lw_deaths_rate_1M_pop")),
       selected = "lw_confirmed_rate_1M_pop")
  }

vars_growth = function(ax = "y") {
  list(label = div(style = "font-size:10px",paste0("Select (",ax,") growth factor")),
                   choices = list("Over 3 days" = "growth_factor_3",
                                  "Over one week" = "growth_factor_7",
                                  "Over 2 weeks" = "growth_factor_14"),
                   selected = "growth_factor_7")
}

vars_vs_growth = function(ax = "x") {
  list(label = div(style = "font-size:10px",paste0("Select (",ax,")  Prevalence over 1M people")),
                      choices = list("Over one week" = "lw_confirmed_rate_1M_pop",
                                     "Over 1 month" = "lm_confirmed_rate_1M_pop",
                                     "Total" = "confirmed_rate_1M_pop"),
                      selected = "lm_confirmed_rate_1M_pop")
}

#' plot_scatterplot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param growth TRUE if 3 growth variables are the y axes.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList selectInput
#' @importFrom shinycssloaders withSpinner
mod_scatterplot_ui <- function(id, growth = TRUE, varsx = NULL, varsy = NULL){
  ns <- NS(id)

  if (growth) {
    if (is.null(varsy)) {
      varsy = vars_growth()
    } else
      varsy = varsy

    if (is.null(varsx)) {
      varsx = vars_vs_growth()
    } else
      varsx = varsx
    tagList(
      uiOutput(ns("title_scatterplot")),
      fluidRow(
        column(6,
               selectInput(inputId = ns("yvar"), label = div(style = "font-size:10px",varsy$label),
                           choices = varsy$choices,
                           selected = varsy$selected)),
        column(6,
               selectInput(inputId = ns("xvar"), label = div(style = "font-size:10px",varsx$label),
                           choices = varsx$choices,
                           selected = varsx$selected)        )
      ),

      withSpinner(plotlyOutput(ns("plot_scatterplot_xy"), height = 400)),
      div(htmlOutput(ns("caption")), align = "center", height = 10)
    )

  } else {
    if (is.null(varsy)) {
      varsy = vars_vs_stringency("y")
    } else
      varsy = varsy
    tagList(
      uiOutput(ns("title_scatterplot")),
      fluidRow(
        column(6, #offset = 6,
               selectInput(inputId = ns("yvar"), label = div(style = "font-size:10px",varsy$label),
                           choices = varsy$choices,
                           selected = varsy$selected))
      ),
      withSpinner(plotlyOutput(ns("plot_scatterplot_xy"), height = 400)),
      div(htmlOutput(ns("caption")), align = "center", height = 10)
    )
  }

}
#' Scatterplot prevalence vs growth
#'
#' @param df data.frame for multiple countries
#' @param nmed number of cases of countries to be used for median computation
#' @param wmed days of outbreak of countries to be used for median computation
#' @param n_highligth number of countries to highlight if istop == TRUE
#' @param istop logical to choose title, if top n_highligth countries are selected
#' @param countries countries selected
#' @param xvar character variable name for x axis
#' @param yvar character variable name for y axis
#' @param growth logical, if TRUE then old growth vs prevalence graph (could be removed)
#' @param fitted logical, if TRUE fitted values are plotted
#'
#' @note if there are no countries with confirmed cases > nmed then all are taken to compute medians
#'
#' @import dplyr
#' @import tidyr
#' @import ggplot2
#' @importFrom plotly ggplotly layout plotly_build
#'
#' @noRd
mod_scatterplot_server <- function(input, output, session, df, nmed = 10000, wmed = 7, n_highligth = 5, istop = TRUE, countries, xvar = "confirmed_rate_1M_pop", yvar = "growth_factor_3", growth = TRUE, fitted = FALSE){
  ns <- session$ns
  # titles
  istoptitle = if(istop) {
      paste("Top", n_highligth, "Confirmed cases: ")
    } else {
    character(0)
    }

  reactList <- reactiveValues(
    yvar = yvar,
    xvar = xvar
  )

  observe({
    if (!is.null(input$yvar))
      reactList$yvar = req(input$yvar)
    if (!is.null(input$xvar))
      reactList$xvar = req(input$xvar)
  })
  xTitle = reactive({
    xt = gsub("^lw_", "", reactList$xvar)
    xt = gsub("^lm_", "", xt)
    xt = names(varsNames(xt))
    xt
    }
  )
  if (growth) {
    #reactList$yvar = input$yvar
    output$title_scatterplot <- renderUI(div(h4(paste(istoptitle, "Growth vs", xTitle())), align = "center", style = "margin-top:20px; margin-bottom:20px;"))
  } else {
    #reactList$yvar = xvar
    #output$title_scatterplot <- renderUI(div(h4(paste0(istoptitle, names(varsNames(req(input$yvar))), " vs ", names(varsNames(xvar)))), align = "center", style = "margin-top:20px; margin-bottom:20px;"))
    output$title_scatterplot <- renderUI(div(h4(paste(istoptitle, names(varsNames(reactList$yvar)), "vs", xTitle())), align = "center", style = "margin-top:20px; margin-bottom:20px;"))
  }


  #world = function(dat, n, w){
  world = function(dat, yvar){
    dat %>% #TODO select_countries_n_cases_w_days can be removed if data_filtered is the input
      #select_countries_n_cases_w_days(n = n, w = w) %>%
      filter( date == maxdate & !!sym(reactList$xvar) != 0 & !!sym(reactList$yvar) != 0) %>%
      #select(Country.Region,date,confirmed,starts_with("growth"), !!xvar)
      select(Country.Region,date,confirmed, !!reactList$xvar, !!reactList$yvar)

  }
  pick_rate <- function(df, rate){
    df <-  df  %>%
      bind_cols(df[, rate] %>% setNames("Value"))
    df
  }

  # prepare data select those with more than 10000
  #world_data = world(df, n,w)
  world_data = reactive({world(df, req(input$yvar))})

  confirmed1000 =  reactive({any(world_data()$confirmed > nmed)})

  world_10000 = reactive({
      if (confirmed1000()) {
        wrld_10000 = world_data() %>% filter(confirmed > nmed)
      }    else {
        wrld_10000 = world_data()
      }
    wrld_10000
  })


  # compute stats for all growth factors

  med_calc = function(dat, var) {
    y = dat[[var]]
    y = y[y!=0]
    median(y, na.rm = TRUE)
  }
  med_y = reactive({
    med_calc(world_10000(), reactList$yvar)
    })
  med_x = reactive({
    med_calc(world_10000(), reactList$xvar)
    })

  if (istop)  { # choose top n_highligth
    df_top = pick_rate(df, "confirmed") %>%
        arrange(desc(Value))
    df_top = df_top %>%
      #top_n(n_highligth, wt = Value) %>% .[1:n_highligth,, drop = FALSE]
      slice_max(Value, n = n_highligth, with_ties = FALSE)

  } else {
    df_top = df %>%
        filter(Country.Region %in% countries)
  }
  df_top_new = reactive({
    df_top %>%
    filter( date == maxdate & !!sym(reactList$xvar) != 0 & !!sym(reactList$yvar) != 0) %>%
      #select(Country.Region,date,confirmed,starts_with("growth"), !!xvar)
      select(Country.Region,date,confirmed, !!reactList$xvar, !!reactList$yvar)
  })


  caption_var = function(var, axis = "x") {
    if (grepl("^growth_factor",var)) {
      paste0("(", axis,") " , caption_growth_factor_fun(var))
    } else if (grepl("stringency", var)) {
      paste0("(", axis,") " , caption_stringency())
    } else if (var %in% c("lethality_rate", "deaths_rate_1M_pop")) {
      paste0("(", axis,") " , caption_death_fun(var))
    } else if(grepl("confirmed_rate_1M_pop", reactList$xvar)) {
      paste0("(", axis,") " , caption_prevalence())
    } else {
      paste0("(", axis,") " , names(varsNames(var)))
    }
  }
  caption_yvar <- reactive({
    caption_var( reactList$yvar, "y")
    })

  caption_xvar <- reactive({
    caption_var( reactList$xvar, "x")
  })


  caption_median <- reactive({
        paste("Dotted lines show median values",
          ifelse(confirmed1000(), paste0("among countries with more than ", nmed," cases."), ""))
      })


  output$caption <- renderText({
    msg = c(caption_yvar(), caption_xvar(), caption_median())
    if (fitted)
      msg = c(msg, paste(caption_fitted(), "among all countries of this page with data"))
    paste0("<p>", msg, sep = '</p>')
  })

  output$plot_scatterplot_xy <- renderPlotly({
    coefflm = NULL
    if (fitted) {
      fitlm = lm(world_10000()[[reactList$yvar]] ~ world_10000()[[reactList$xvar]])
      coefflm = coefficients(fitlm)
    }
    df_plot <- df_top_new() %>%
      mutate(Country.Region = as.factor(Country.Region))
    p = df_plot %>%
        scatter_plot(list(x = med_x(),
                          y = med_y()), xvar = reactList$xvar, yvar = reactList$yvar, coefflm = coefflm)
    p <- p %>%
      #ggplotly(tooltip = c("text", "y"))# %>%
      ggplotly(tooltip = c("text"))# %>%

      # layout(legend = list(orientation = "h",
      #                      #y = 1.1,
      #                      yanchor = "bottom"))

    p <- plotly_build(p)

    length<-length(p$x$data)
    invisible(lapply(1:length, function(x) p$x$data[[x]]<<-c(p$x$data[[x]], textposition ='top center')))

    p
  })
}
