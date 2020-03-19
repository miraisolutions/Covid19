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
#' @importFrom shiny NS tagList
mod_caseBoxes_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(3, shiny::uiOutput(ns("confirmed"))),
      column(3, shiny::uiOutput(ns("death"))),
      column(3, shiny::uiOutput(ns("recovered"))),
      column(3, shiny::uiOutput(ns("active")))
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
    countBox("Confirmed",
             counts()[["confirmed"]],
             color = "white",
             background = case_colors[["confirmed"]])
  })
  output$death <- renderUI({
    countBox("Deaths",
             counts()[["deaths"]],
             color = "white",
             background = case_colors[["deaths"]])
  })
  output$recovered <- renderUI({
    countBox("Recovered",
             counts()[["recovered"]],
             color = "white",
             background = case_colors[["recovered"]])
  })
  output$active <- renderUI({
    countBox("Active",
             counts()[["active"]],
             color = "white",
             background = case_colors[["active"]])
  })
}

case_colors <- c(
  "confirmed" = "#dd4b39",
  "deaths" = "black",
  "recovered" = "#00a65a",
  "active" = "#3c8dbc"
)

countBox <- function(title, subtitle, color, background, width = "100%") {
  div(
    class = "count-box",
    shiny::h3(title),
    shiny::p(subtitle),
    style = sprintf(
      "color: %s; background-color: %s; width: %s;",
      color, background, width
    )
  )
}
