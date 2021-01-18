
#' barplot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param plot1 character: ui_growth ui_death ui_stringency, choose UI element from uichoices for plot1 graph.
#' @param plot2 character: ui_growth ui_death ui_stringency, choose UI element from uichoices for plot2 graph.
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom shinycssloaders withSpinner
mod_barplot_ui <- function(id, plot1 = "ui_growth", plot2  = "ui_death"){
  ns <- NS(id)
  uichoices = list(ui_growth = list(label = "",
                                     choices = list("Over 3 days" = "growth_factor_3",
                                                    # "Over 5 days" = "growth_factor_5",
                                                    "Over one week" = "growth_factor_7",
                                                    "Over 2 weeks" = "growth_factor_14"),
                                     selected = "growth_factor_7"),
                   ui_death =  list(label = "",
                                     choices = varsNames(c("lethality_rate","deaths_rate_1M_pop",
                                                           c("lw_lethality_rate","lw_deaths_rate_1M_pop"))),
                                     selected = "lethality_rate"),

                   ui_stringency =  list(label = "",
                                          choices = varsNames(c("stringency_index")),
                                          selected = "stringency_index"),
                   ui_vaccines =  list(label = "",
                                         choices = varsNames(c("vaccines","vaccines_rate_pop",
                                                               c("lw_vaccines","lw_vaccines_rate_pop"))),
                                         selected = "vaccines")
                   )

  if (!is.null(plot1))
      uichoice1 = uichoices[[plot1]]
  else
    uichoice1 = NULL
  if (!is.null(plot2))
    uichoice2 = uichoices[[plot2]]
  else
    uichoice2 = NULL

  if (!is.null(uichoice1) && !is.null(uichoice2)) {
    tagList(
      div(id = id,

      fluidRow(
        column(6,
               tagList(
                 uiOutput(ns("title_plot_1")),
                 selectInput(inputId = ns("plot_1"), label = uichoice1$label,
                             choices = uichoice1$choices ,
                             selected = uichoice1$selected),
                 withSpinner(plotlyOutput(ns("plot_plot_1_hist"), height = 400)),
                 div(htmlOutput(ns("caption1")), align = "center")
               )
        ),
        column(6,
               tagList(
                 uiOutput(ns("title_plot_2")),
                 selectInput(inputId = ns("plot_2"), label = uichoice2$label,
                             choices = uichoice2$choices ,
                             selected = uichoice2$selected),
                 withSpinner(plotlyOutput(ns("plot_plot_2_hist"), height = 400)),
                 div(htmlOutput(ns("caption2")), align = "center")
               )
        )
      )
      )
    )
  } else if (!is.null(uichoice1)){
    # if (length(uichoice1$choices) == 1) {
    #   tagList(
    #     fluidRow(
    #       uiOutput(ns("title_plot_1")),
    #       textInput(inputId = ns("plot_1"), value =  uichoice1$selected, label = NULL),
    #       withSpinner(plotlyOutput(ns("plot_plot_1_hist"), height = 400)),
    #       div(htmlOutput(ns("caption1")), align = "center")
    #     )
    #   )
    # } else {
      tagList(
        div(id = id,

        fluidRow(
          uiOutput(ns("title_plot_1")),
          selectInput(inputId = ns("plot_1"), label = uichoice1$label,
                      choices = uichoice1$choices ,
                      selected = uichoice1$selected),
          withSpinner(plotlyOutput(ns("plot_plot_1_hist"), height = 400)),
          div(htmlOutput(ns("caption1")), align = "center")
        )
        )
      )
    #}

  } else {
    stop("If only one plot to be done then use plot1")
  }

}

#' growth_death_rate Server Function
#'
#' @param df reactive data.frame
#' @param n_highligth number of countries to highlight
#' @param istop logical to choose title
#' @param g_palette list of character vector of colors for the graph and legend of plot_1 and death_rate
#' @param plottitle character vector giving title to plot1 and plot2
#' @param pickvariable list of character vector of 1 variable name to use for sorting bars from lef, default is none
#'
#' @import dplyr
#' @import tidyr
#'
#' @example ex-mod_growth_death_rate.R
#'
#' @noRd
mod_barplot_server <- function(input, output, session, df,
                                         n_highligth = 5, istop = TRUE,
                                         g_palette = list("plot_1" = barplots_colors[["growth_factor"]],
                                                        "plot_2" = barplots_colors[["death_rate"]],
                                                        "calc" = FALSE),
                                         plottitle = c("Growth factor", "Death toll"),
                                         pickvariable = list("plot_1" = character(0), "plot_2" = character(0)),
                                         sortbyvar = TRUE){
  ns <- session$ns


  # Help funcs ----
  pick_rate_hist <- function(df1, rate, pickvar) {
    if (length(pickvar) == 0)
      pick_var = rate
    else
      pick_var = pickvar
    df_plot <- df %>%
      #arrange(desc(rate)) %>%
      filter( date == max(date) & #!!sym(rate) != 0 &
              population >= 100000) # %>% # filter out those with rate = 0 and small countries

    if (sortbyvar) {
      df_plot <- df_plot %>%
        slice_max(!!sym(pick_var), n = n_highligth, with_ties = FALSE) #%>%
    }
    if (n_highligth < length(unique(df_plot$Country.Region))) {
      df_plot <- df_plot %>%
        filter(Country.Region %in% head(unique(df_plot$Country.Region),5))
    }

    df_plot <- df_plot %>%
      mutate(Country = factor(Country.Region, levels = .$Country.Region)) %>%
      select(Country, rate) %>% setNames(c("Country","Value"))

    df_plot
  }

  isPlot2 = ifelse(length(plottitle) == 2, TRUE, FALSE)
  # Dataset ----
  #df_pop <- reactive(scale_mortality_rate(df))

  df_base_plot1 <- reactive({pick_rate_hist(df, req(input$plot_1), pickvariable[["plot_1"]])})
  if (isPlot2)
    df_base_plot2 <- reactive({pick_rate_hist(df, req(input$plot_2), pickvariable[["plot_2"]])})

  # Plots ----
  # titles
  if (istop) {
    varsort1 = if(length(pickvariable[["plot_1"]]) == 0) pickvariable[["plot_1"]] else  paste("by <", names(varsNames(pickvariable[["plot_1"]])), ">")
    output$title_plot_1 <- renderUI(div(h4(HTML(paste0("Current top ", n_highligth, " countries ", varsort1, '<br/>',plottitle[1], '<br/>'))), align = "center", style = "margin-top:20px; margin-bottom:20px;"))
    if (isPlot2) {
      varsort2 = if(length(pickvariable[["plot_2"]]) == 0) pickvariable[["plot_2"]] else  paste("by<", names(varsNames(pickvariable[["plot_2"]])), ">")
      output$title_plot_2 <- renderUI(div(h4(HTML(paste0("Current top ", n_highligth, " countries ", varsort2, '<br/>',plottitle[2], '<br/>'))), align = "center", style = "margin-top:20px; margin-bottom:20px;"))
    }

  } else {
    output$title_plot_1 <- renderUI(div(h4(plottitle[1]), align = "center", style = "margin-top:20px; margin-bottom:20px;"))
    if (isPlot2)
      output$title_plot_2 <- renderUI(div(h4(plottitle[2]), align = "center", style = "margin-top:20px; margin-bottom:20px;"))
  }

  # captions

  captionbarplot = function(var) {
    if (grepl("^growth_factor",var)) {
      caption_growth_factor_fun(var)
    } else if (var %in% c("lethality_rate", "deaths_rate_1M_pop")) {
      caption_death_fun(var)
    }  else if (grepl("^stringency",var)) {
      caption_stringency()
    } else
      names(varsNames(var))
  }
  caption_plot_1 <- reactive({
    cap = captionbarplot(req(input$plot_1))
    if (length(pickvariable[[1]])>0 && (pickvariable[[1]] != req(input$plot_1))) {
      cap = c(cap, paste("Order from left to right by <", names(varsNames(pickvariable[[1]])), ">"))
    }
    cap
  })
  if (isPlot2) {
    caption_plot_2 <- reactive({
      cap = captionbarplot(req(input$plot_2))
      if (length(pickvariable[[2]])>0 && (pickvariable[[2]] != req(input$plot_2))) {
        cap = c(cap, paste("Order from left to right by <", names(varsNames(pickvariable[[2]])), ">"))
      }
      cap
    })
  }

  # plots

  output$caption1 <- renderText({
    paste0("<p>", caption_plot_1(), sep = '</p>')
      })

  if (isPlot2) {
      output$caption2 <- renderText({
        paste0("<p>", caption_plot_2(), sep = '</p>')
      })
  }

  is_percent_1 <- reactive({ifelse(req(input$plot_1) %in% .rate_vars, TRUE, FALSE)})

  output$plot_plot_1_hist <- renderPlotly({
    g_palette_1 = g_palette[["plot_1"]]

    if (g_palette$calc)
      g_palette_1 = palette_calc(g_palette_1, as.character(unique(df_base_plot1()$Country[order(df_base_plot1()$Value)])))

    df_base_plot1_filtered = df_base_plot1() %>%
      filter(Value !=0)
    if (length(g_palette_1) > 1)
      g_palette_1 = g_palette_1[df_base_plot1()$Country %in% df_base_plot1_filtered$Country]
    p <- plot_rate_hist(df_base_plot1_filtered, y_min = 1,
                        percent = is_percent_1(),
                        g_palette =  g_palette_1)
    p <- p %>%
      plotly::ggplotly(tooltip = c("x","text")) %>%
      plotly::layout(legend = list(orientation = "h", y = 1.1, yanchor = "bottom")
                      #             xaxis = list(tickfont = list(size = 10))
                     )
    p
  })
  if (isPlot2) {
    is_percent_2 <- reactive({ifelse(req(input$plot_2) %in% .rate_vars, TRUE, FALSE)})
    output$plot_plot_2_hist <- renderPlotly({

      g_palette_2 = g_palette[["plot_2"]]

      if (g_palette$calc)
        g_palette_2 =  palette_calc(g_palette_2, as.character(unique(df_base_plot2()$Country[order(df_base_plot2()$Value)])))

      df_base_plot2_filtered = df_base_plot2() %>%
        filter(Value !=0)
      if (length(g_palette_2) > 1)
        g_palette_2 = g_palette_2[df_base_plot2()$Country %in% df_base_plot2_filtered$Country]

      p <- plot_rate_hist(df_base_plot2(), percent = is_percent_2(), g_palette =  g_palette_2)
      p <- p %>%
        plotly::ggplotly(tooltip = c("x","text")) %>%
        plotly::layout(legend = list(orientation = "h", y = 1.1, yanchor = "bottom")
                       #xaxis = list(tickfont = list(size = 10))
        )
      p
    })
  }

}
