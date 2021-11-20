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
  df = df %>% .[,c("date","AsOfDate",setdiff(names(df), c("date","AsOfDate")))] # place as of date in first place


  vars = intersect(names(df), unlist(varsNames()))
  names(df)[names(df) %in% vars] = names(varsNames(vars))

  output$table <- renderDT(
    datatable(df %>% capitalize_names_df(),
              rownames = FALSE,
              selection = "single",
              #filter = 'bottom',
              escape = FALSE,
              plugins = 'natural',
              options = getTableOptions(maxrowsperpage = maxrowsperpage))
  )

  # generate output report
  output$download_table <- downloadHandler(filename = function() {
    paste("data-", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    write.csv(df, file)
  }
  )
}
