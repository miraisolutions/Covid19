#' caseBoxes UI Function
#'
#' @description A shiny Module for displaying the number of cases by type as
#'   colored boxes.
#'
#' @param id, Internal parameters for {shiny}.
#' @param hosp logical, if TRUE hospitalised box variables is added.
#'
#' @example man-roxygen/ex-mod_caseBoxes.R
#'
#' @name mod_caseBoxes
#' @keywords internal

#' @rdname mod_caseBoxes
#' @import shiny
mod_caseBoxes_ui <- function(id, hosp = FALSE) {
  ns <- NS(id)
  tg = tagList(
      div(id = id,
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
  )
  if (hosp) {
    tg = tagList(
      div(id = id,
          tg,
        fluidRow(
          column(3, shiny::uiOutput(ns("hosp"))),
          column(3, shiny::uiOutput(ns("icuvent"))),
          column(3, shiny::uiOutput(ns("tests"))),
          column(3, shiny::uiOutput(ns("testratepositive")))
        ),
        fluidRow(
          column(3, shiny::uiOutput(ns("new_hosp"))),
          column(3, shiny::uiOutput(ns("new_icuvent"))),
          column(3, shiny::uiOutput(ns("new_tests"))),
          column(3, shiny::uiOutput(ns("new_test_rate_positive")))
        )
      )
    )
  }
  tg
}

#' caseBoxes Server Function
#'
#' @param counts Reactive expression yielding the named vector of cases by type.
#' @param hosp logical, if TRUE hospitalised box variables is added.
#'
#' @rdname mod_caseBoxes
mod_caseBoxes_server <- function(input, output, session, counts, hosp = FALSE, vax = NULL) {

  if (!hosp) {
    vaxflag = !is.null(vax)

    if (!vaxflag || vax != "confirmed") {
      output$confirmed <- renderUI({
        countBox3(title1 = "Confirmed: ",
                 subtitle1 = counts[["confirmed"]],
                 title2 = "Last Week: ",
                 subtitle2 =  counts[["lw_confirmed"]],
                 title3 = "Prev. Week: ",
                 subtitle3 =  counts[["pw_confirmed"]],
                 color = "white",
                 background = .case_colors[["confirmed"]])
      })
    }
    if (!vaxflag || vax != "deaths") {
      output$death <- renderUI({
        countBox3(title1 = "Deaths: ",
                 subtitle1 = counts[["deaths"]],
                 title2 = "Last Week: ",
                 subtitle2 =  counts[["lw_deaths"]],
                 title3 = "Prev. Week: ",
                 subtitle3 =  counts[["pw_deaths"]],
                 color = "white",
                 background = .case_colors[["deaths"]])
      })
    }
    if (!vaxflag || vax != "recovered") {
      output$recovered <- renderUI({
        countBox3(title1 = "Recovered: ",
                 subtitle1 = counts[["recovered"]],
                 title2 = "Last Week: ",
                 subtitle2 =  counts[["lw_recovered"]],
                 title3 = "Prev. Week: ",
                 subtitle3 =  counts[["pw_recovered"]],
                 color = "white",
                 background = .case_colors[["recovered"]])
      })
    }
    if (!is.null(vax)) {
      message("Vaccinated box")
      output[[vax]]  <- renderUI({
        countBox3(title1 = "Vaccinated: ",
                 subtitle1 = counts[["vaccines"]],
                 title2 = "Last Week: ",
                 subtitle2 =  counts[["lw_vaccines"]],
                 title3 = "Prev. Week: ",
                 subtitle3 =  counts[["pw_vaccines"]],
                 color = "white",
                 background = .case_colors[["vaccines"]])
      })
    }
    if (!vaxflag || vax != "active") {
      output$active <- renderUI({
        countBox3(title1 = "Active: ",
                 subtitle1 = counts[["active"]],
                 title2 = "Last Week: ",
                 subtitle2 =  counts[["lw_active"]],
                 title3 = "Prev. Week: ",
                 subtitle3 =  counts[["pw_active"]],
                 color = "white",
                 background = .case_colors[["active"]],
                 diffcalc = FALSE)
      })
    }

  } else {
    output$hosp <- renderUI({
      countBox3(title1 = "Hospitalised: ",
               subtitle1 = counts[["hosp"]],
               title2 = "Last Week: ",
               subtitle2 =  counts[["lw_hosp"]],
               title3 = "Prev. Week: ",
               subtitle3 =  counts[["pw_hosp"]],
               color = "white",
               background = .hosp_colors[["hosp"]],
               diffcalc = FALSE)
    })
    output$icuvent <- renderUI({
      countBox3(title1 = "Int. Care: ",
               subtitle1 = counts[["icuvent"]],
               title2 = "Last Week: ",
               subtitle2 =  counts[["lw_icuvent"]],
               title3 = "Prev. Week: ",
               subtitle3 =  counts[["pw_icuvent"]],
               color = "white",
               background = .hosp_colors[["icuvent"]],
               diffcalc = FALSE)
    })
    output$tests <- renderUI({
      countBox3(title1 = "Tests: ",
               subtitle1 = counts[["tests"]],
               title2 = "Last Week: ",
               subtitle2 =  counts[["lw_tests"]],
               title3 = "Prev Week: ",
               subtitle3 =  counts[["pw_tests"]],
               color = "white",
               background = .tests_colors[["tests"]])
    })
    output$testratepositive <- renderUI({
      countBox3(title1 = "Positive Tests: ",
               subtitle1 = counts[["positive_tests_rate"]],
               title2 = "Last Week: ",
               subtitle2 =  counts[["lw_positive_tests_rate"]],
               title3 = "Prev. Week: ",
               subtitle3 =  counts[["pw_positive_tests_rate"]],
               color = "white",
               background = .tests_colors[["positive_tests_rate"]],
               perc = TRUE,
               diffcalc = FALSE)
    })
  }
}

countBox <- function(title1, subtitle1, title2, subtitle2, color, background, width = "100%", perc = FALSE) {

  format_thousands = function(x)
    formatC(x, format = "f", big.mark = "'", digits  = 0)
  format_perc = function(x)
    paste(x*100, "%")
  format_fun = ifelse(perc, format_perc, format_thousands)
  pp =  div(
    class = "count-box",
    shiny::h3(title1),
    shiny::p(format_fun(subtitle1)),
    br(),
    shiny::h3(title2),
    shiny::p(format_fun(subtitle2)),
    style = sprintf(
      "color: %s; background-color: %s; width: %s;",
      color, background, width
    )
  )
}
#' CSS boxes Function
#'
#' @param title1 character first string, variable
#' @param subtitle1 numeric value of title1
#' @param title2 character 2nd string, variable
#' @param subtitle2 numeric value of title2
#' @param title3 character 3rd string, variable
#' @param subtitle3 numeric value of title3
#' @param color character text's colour
#' @param background character box's colour
#' @param width with of box
#' @param perc logical if TRUE then values are %
#' @param diffcalc logical if TRUE diffeerence in % is also displayed
#'
#' @import tidyr
#' @import shiny
#'
#' @noRd
countBox3 <- function(title1, subtitle1, title2, subtitle2, title3, subtitle3, color, background, width = "100%", perc = FALSE, diffcalc = TRUE) {

  format_thousands = function(x)
    formatC(x, format = "f", big.mark = "'", digits  = 0)
  format_perc = function(x)
    paste(x*100, "%")
  format_fun = ifelse(perc, format_perc, format_thousands)

  format_change = function(x) {
    sign.perc = "+"
    if (x < 0)
      sign.perc = ""
    paste0(sign.perc,x*100, "%")

  }
  change.perc = round(subtitle2 / subtitle3 -1, 3)
  change.perc[!is.finite(change.perc)] = 0
  if (diffcalc) {
    titlestringdiff = "Diff.:"
    stringgdiff = format_change(change.perc)
  } else {
    stringgdiff = titlestringdiff = ""
  }


  div(
    class = "count-box",
    shiny::h3(title1),
    shiny::p(format_fun(subtitle1)),
    br(),
    shiny::h3(title2),
    shiny::p(format_fun(subtitle2)),
    br(),
    shiny::h3(title3),
    shiny::p(format_fun(subtitle3)),
    br(),
    shiny::h3(titlestringdiff),
    shiny::p(stringgdiff),
    style = sprintf(
      "color: %s; background-color: %s; width: %s;",
      color, background, width
    )
  )
}

