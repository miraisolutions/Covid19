#' caseBoxes UI Function
#'
#' @description A shiny Module for displaying the number of cases by type as
#'   colored boxes.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @example man-roxygen/ex-mod_caseBoxes.R
#'
#' @name mod_caseBoxes
#' @keywords internal

#' @rdname mod_caseBoxes
#' @import shiny
mod_caseBoxes_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3, shiny::uiOutput(ns("confirmed"))),
      column(3, shiny::uiOutput(ns("death"))),
      column(3, shiny::uiOutput(ns("recovered"))),
      column(3, shiny::uiOutput(ns("active")))
    ),
    fluidRow(
      column(3, shiny::uiOutput(ns("new_confirmed"))),
      column(3, shiny::uiOutput(ns("new_death"))),
      column(3, shiny::uiOutput(ns("new_recovered"))),
      column(3, shiny::uiOutput(ns("new_active")))
    )
  )
}

#' caseBoxes Server Function
#'
#' @param counts Reactive expression yielding the named vector of cases by type.
#'
#' @rdname mod_caseBoxes
mod_caseBoxes_server <- function(input, output, session, counts) {

  output$confirmed <- renderUI({
    countBox(title1 = "Confirmed: ",
             subtitle1 = counts()[["confirmed"]],
             title2 = "New: ",
             subtitle2 =  counts()[["new_confirmed"]],
             color = "white",
             background = case_colors[["confirmed"]])
  })
  output$death <- renderUI({
    countBox(title1 = "Deaths: ",
             subtitle1 = counts()[["deaths"]],
             title2 = "New: ",
             subtitle2 =  counts()[["new_deaths"]],
             color = "white",
             background = case_colors[["deaths"]])
  })
  output$recovered <- renderUI({
    countBox(title1 = "Recovered: ",
             subtitle1 = counts()[["recovered"]],
             title2 = "New: ",
             subtitle2 =  counts()[["new_recovered"]],
             color = "white",
             background = case_colors[["recovered"]])
  })
  output$active <- renderUI({
    countBox(title1 = "Active: ",
             subtitle1 = counts()[["active"]],
             title2 = "New: ",
             subtitle2 =  counts()[["new_active"]],
             color = "white",
             background = case_colors[["active"]])
  })
}

countBox <- function(title1, subtitle1, title2, subtitle2, color, background, width = "100%") {
  div(
    class = "count-box",
    shiny::h3(title1),
    shiny::p(formatC(subtitle1, format = "f", big.mark = ",", digits  = 0)),
    br(),
    shiny::h3(title2),
    shiny::p(formatC(subtitle2, format = "f", big.mark = ",", digits  = 0)),
    style = sprintf(
      "color: %s; background-color: %s; width: %s;",
      color, background, width
    )
  )
}
