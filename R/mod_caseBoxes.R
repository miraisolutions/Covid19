#' caseBoxes UI Function
#'
#' @description A shiny Module for displaying the number of cases by type as
#'   colored boxes.
#'
#' @param id, Internal parameters for {shiny}.
#' @param hosp logical, if TRUE hospitalised box variables is added.
#' @param outputui logical, if TRUE shiny::uiOutput is used
#'
#'
#' @keywords internal
#' @import shiny
mod_caseBoxes_ui <- function(id, hosp = FALSE, outputui = FALSE) {
  ns <- NS(id)

  if (!outputui) {
    tg = tagList(
      div(id = id,
          fluidRow(
            column(3,
                   tags$div(id = ns("confirmed"))),
            column(3,
                   tags$div(id = ns("death"))),
            column(3,
                   tags$div(id = ns("recovered"))),
            column(3,
                   tags$div(id = ns("active")))
          )
      )
    )
    if (hosp) {
      tg = tagList(
        div(id = id,
            tg,
            fluidRow(
              column(3,
                     tags$div(id = ns("hosp"))),
              column(3,
                     tags$div(id = ns("icuvent"))),
              column(3,
                     tags$div(id = ns("tests"))),
              column(3,
                     tags$div(id = ns("testratepositive")))
            )#,
        )
      )
    }
  } else {
    tg = tagList(
      div(id = id,
          fluidRow(
            column(3, shiny::uiOutput(ns("confirmed"))),
            column(3, shiny::uiOutput(ns("death"))),
            column(3, shiny::uiOutput(ns("recovered"))),
            column(3, shiny::uiOutput(ns("active")))

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
            )
        )
      )
    }
  }

  tg
}

#' caseBoxes Server Function
#'
#' @param input,output,session Internal parameters for {shiny}.
#'
#' @param counts Reactive expression yielding the named vector of cases by type.
#' @param hosp logical, if TRUE hospitalised box variables is added.
#' @param vax character, output id for the vaccination box
#' @param renderui logical, if TRUE shiny::renderUI is used
mod_caseBoxes_server <- function(input, output, session, counts, hosp = FALSE, vax = NULL, renderui = FALSE) {

  ns <- session$ns

  if (!hosp) {
    vaxflag = !is.null(vax)

    if (!vaxflag || vax != "confirmed") {
      if(!renderui) {
        insertUI(paste0("#",ns("confirmed")),
                 ui = countBox3(title1 = "Confirmed: ",
                                subtitle1 = counts[["confirmed"]],
                                title2 = "Last Week: ",
                                subtitle2 =  counts[["lw_confirmed"]],
                                title3 = "Prev. Week: ",
                                subtitle3 =  counts[["pw_confirmed"]],
                                color = "white",
                                background = .case_colors[["confirmed"]]),
                 session = session,
                 immediate = TRUE
        )
      }  else {
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

    }
    if (!vaxflag || vax != "deaths") {
      if(!renderui)
        insertUI(paste0("#",ns("death")),
                 ui = countBox3(title1 = "Deaths: ",
                              subtitle1 = counts[["deaths"]],
                              title2 = "Last Week: ",
                              subtitle2 =  counts[["lw_deaths"]],
                              title3 = "Prev. Week: ",
                              subtitle3 =  counts[["pw_deaths"]],
                              color = "white",
                              background = .case_colors[["deaths"]]),
                 session = session,
                 immediate = TRUE
        )
      else {
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

    }
    if (!vaxflag || vax != "recovered") {
      if(!renderui)
        insertUI(paste0("#",ns("recovered")),
                 ui = countBox3(title1 = "Recovered: ",
                                subtitle1 = counts[["recovered"]],
                                title2 = "Last Week: ",
                                subtitle2 =  counts[["lw_recovered"]],
                                title3 = "Prev. Week: ",
                                subtitle3 =  counts[["pw_recovered"]],
                                color = "white",
                                background = .case_colors[["recovered"]]),
                 session = session,
                 immediate = TRUE
        )
      else {
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

    }
    if (!is.null(vax)) {
      message("Vaccine doses box")
      if(!renderui)
        insertUI(paste0("#",ns(vax)),
                 ui = countBox3(title1 = "Fully Vaccinated: ",
                                subtitle1 = counts[["fully_vaccinated_rate"]],
                                title2 = "L. Week Doses: ",
                                subtitle2 =  counts[["lw_vaccines"]],
                                title3 = "P. Week Doses: ",
                                subtitle3 =  counts[["pw_vaccines"]],
                                color = "white",
                                background = .case_colors[["vaccines"]],
                                type = c("perc","thousands","thousands")),
                 session = session,
                 immediate = TRUE
        )
      else {
        output[[vax]]  <- renderUI({
          countBox3(title1 = "Fully Vaccinated: ",
                    subtitle1 = counts[["fully_vaccinated_rate"]],
                    title2 = "L. Week Doses: ",
                    subtitle2 =  counts[["lw_vaccines"]],
                    title3 = "P. Week Doses: ",
                    subtitle3 =  counts[["pw_vaccines"]],
                    color = "white",
                    background = .case_colors[["vaccines"]],
                    type = c("perc","thousands","thousands"))
        })
      }

    }
    if (!vaxflag || vax != "active") {
      if(!renderui)
        insertUI(paste0("#",ns("active")),
               ui = countBox3(title1 = "Active: ",
                          subtitle1 = counts[["active"]],
                          title2 = "Last Week: ",
                          subtitle2 =  counts[["lw_active"]],
                          title3 = "Prev. Week: ",
                          subtitle3 =  counts[["pw_active"]],
                          color = "white",
                          background = .case_colors[["active"]],
                          diffcalc = TRUE),
               session = session,
               immediate = TRUE
      )
      else {
        output$active <- renderUI({
          countBox3(title1 = "Active: ",
                   subtitle1 = counts[["active"]],
                   title2 = "Last Week: ",
                   subtitle2 =  counts[["lw_active"]],
                   title3 = "Prev. Week: ",
                   subtitle3 =  counts[["pw_active"]],
                   color = "white",
                   background = .case_colors[["active"]],
                   diffcalc = TRUE)
        })
      }

    }

  } else {
    if(!renderui) {
      insertUI(paste0("#",ns("hosp")),
               ui = countBox3(title1 = "Hospitalized: ",
                              subtitle1 = counts[["hosp"]],
                              title2 = "Last Week: ",
                              subtitle2 =  counts[["lw_hosp"]],
                              title3 = "Prev. Week: ",
                              subtitle3 =  counts[["pw_hosp"]],
                              color = "white",
                              background = .hosp_colors[["hosp"]],
                              type = c("thousands", "sign", "sign"),
                              diffcalc = FALSE),
               session = session,
               immediate = TRUE
      )
      insertUI(paste0("#",ns("icuvent")),
               ui = countBox3(title1 = "Int. Care: ",
                              subtitle1 = counts[["icuvent"]],
                              title2 = "Last Week: ",
                              subtitle2 =  counts[["lw_icuvent"]],
                              title3 = "Prev. Week: ",
                              subtitle3 =  counts[["pw_icuvent"]],
                              color = "white",
                              background = .hosp_colors[["icuvent"]],
                              type = c("thousands", "sign", "sign"),
                              diffcalc = FALSE),
               session = session,
               immediate = TRUE
      )
      insertUI(paste0("#",ns("tests")),
               ui = countBox3(title1 = "Tests: ",
                              subtitle1 = counts[["tests"]],
                              title2 = "Last Week: ",
                              subtitle2 =  counts[["lw_tests"]],
                              title3 = "Prev Week: ",
                              subtitle3 =  counts[["pw_tests"]],
                              color = "white",
                              background = .tests_colors[["tests"]]),
               session = session,
               immediate = TRUE
      )
      insertUI(paste0("#",ns("testratepositive")),
               ui = countBox3(title1 = "Positive Tests: ",
                              subtitle1 = counts[["positive_tests_rate"]],
                              title2 = "Last Week: ",
                              subtitle2 =  counts[["lw_positive_tests_rate"]],
                              title3 = "Prev. Week: ",
                              subtitle3 =  counts[["pw_positive_tests_rate"]],
                              color = "white",
                              background = .tests_colors[["positive_tests_rate"]],
                              type = "perc",
                              diffcalc = FALSE),
               session = session,
               immediate = TRUE
      )
    } else {
      output$hosp <- renderUI({
        countBox3(title1 = "Hospitalized: ",
                 subtitle1 = counts[["hosp"]],
                 title2 = "Last Week: ",
                 subtitle2 =  counts[["lw_hosp"]],
                 title3 = "Prev. Week: ",
                 subtitle3 =  counts[["pw_hosp"]],
                 color = "white",
                 background = .hosp_colors[["hosp"]],
                 type = c("thousands", "sign", "sign"),
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
                 type = c("thousands", "sign", "sign"),
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
                 type = "perc",
                 diffcalc = FALSE)
      })
    }



  }
}

countBox <- function(title1, subtitle1, title2, subtitle2, color, background, width = "100%", perc = FALSE) {

  format_thousands = function(x)
    formatC(x, format = "f", big.mark = "'", digits  = 0)
  format_perc = function(x)
    paste0(x*100, "%")
  fun_sign = function(x) {
    paste(ifelse(x>0, "+",""),format_thousands(x))
  }
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
#' @param type character `perc`, `thousands`, `sign`, default perc
#' @param diffcalc logical if TRUE difference in % is also displayed
#'
#' @import tidyr
#' @import shiny
#'
#' @noRd
countBox3 <- function(title1, subtitle1, title2, subtitle2, title3, subtitle3, color, background, width = "100%", type = c("thousands"), diffcalc = TRUE) {

  format_thousands = function(x) {
    if (is.na(x))
      return(x)
    formatC(x, format = "f", big.mark = "'", digits  = 0)
  }
  format_perc = function(x) {
    ifelse(is.na(x),"",paste0(x*100,"%"))
  }
  format_sign = function(x) {
    paste0(ifelse(x>0, "+",""),format_thousands(x))
  }

  if (!all(type %in% c("perc", "thousands", "sign")))
    stop("Wrong type arg ", type)

  format_choice = list(format_thousands = format_thousands,
                       format_perc = format_perc,
                       format_sign = format_sign)
  if (length(type) == 1)
    type = rep(type, 3)
  format_fun = format_choice[paste("format", type, sep = "_")]

  # ifelse(perc, format_perc, format_thousands)

  format_change = function(x) {
    sign.perc = "+"
    if (is.na(x) || x < 0)
      sign.perc = ""
    textx = ifelse(is.na(x),"",format_perc(x))
    paste0(sign.perc,textx)

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
    shiny::p(format_fun[[1]](subtitle1)),
    br(),
    shiny::h3(title2),
    shiny::p(format_fun[[2]](subtitle2)),
    br(),
    shiny::h3(title3),
    shiny::p(format_fun[[3]](subtitle3)),
    br(),
    shiny::h3(titlestringdiff),
    shiny::p(stringgdiff),
    style = sprintf(
      "color: %s; background-color: %s; width: %s;",
      color, background, width
    )
  )
}

