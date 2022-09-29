#' add_table UI Function
#'
#' @description A shiny Module.
#'
#' @param id Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
mod_add_table_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(DTOutput(ns("table")), style = "margin-left: 20px;margin-right: 20px;"),
    # Add button to download report
    downloadButton(ns("download_table"), label = "Download")
  )
}

#' add_table Server Function
#' @param df data.frame reactive input to table
#' @param maxrowsperpage number of rows. Integer
#'
#'
#' @noRd
mod_add_table_server <- function(input, output, session, df, maxrowsperpage = 5){
  ns <- session$ns
  # Add Labels
  df_out = df %>% .[,c("date","AsOfDate",setdiff(names(df), c("date","AsOfDate")))] # place as of date in first place

  perccol = intersect(names(df_out), .rate_vars)

  numcol = names(df_out)[(sapply(df_out, is.numeric))]

  numcol_small <- sapply(df_out[, numcol], max, na.rm = TRUE) < 1000
  numcol_small <- names(numcol_small)[numcol_small]
  numcol_small <- setdiff(numcol_small, perccol)

  numcol_thous <- names(df_out) %in% setdiff(setdiff(numcol, perccol), numcol_small)
  numcol_small <- names(df_out) %in%  numcol_small
  perccol       <- names(df_out) %in%  perccol

#
#   numcol_thous <- cols
#   numcol_thous[numcol][!numcol_small_val] <- TRUE
#
#   cols <- rep(FALSE, ncol(df_out)) %>%
#     setNames(names(df_out))
#   numcol_small <- cols
#   numcol_small[names(numcol_small_val)] <- TRUE
#   numcol_thous = numcol
#
#   numcol[numcol_thous] = numcol_small
#
#   numcol[numcol_thous] = numcol_small
#   numcol_thous[numcol_thous] = !numcol_small

  vars = intersect(names(df_out), unlist(varsNames()))
  names(df_out)[names(df_out) %in% vars] = names(varsNames(vars))


  output$table <- renderDT(
    datatable(df_out %>% capitalize_names_df(),
              rownames = FALSE,
              selection = "single",
              filter = 'top',
              escape = FALSE,
              plugins = 'natural',
              options = getTableOptions(maxrowsperpage = maxrowsperpage)) %>%
      DT::formatRound(numcol_small, 2, mark = "'") %>%
      DT::formatRound(numcol_thous, 0, mark = "'") %>%
      DT::formatPercentage(perccol, 2, mark = "'")
  )

  # generate output report
  output$download_table <- downloadHandler(filename = function() {
    paste("data-", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    write.csv(df_out, file)
  }
  )
}
