if (interactive()) {
  devtools::load_all()
  long_title <- "Lorem ipsum dolor sit amet, consectetur adipisicing elit."
  ui <- fluidPage(
    tagList(
      Covid19Mirai:::golem_add_external_resources(),
      Covid19Mirai:::mod_caseBoxes_ui("boxes")
    )
  )
  server <- function(input, output, session) {
    statuses <- c("confirmed", "deaths", "recovered", "active")
    # select all variables
    allstatuses = c(statuses, paste0("new_", statuses))
    counts <- reactive({
      invalidateLater(2000)
      as.list(stats::setNames(sample.int(1000, length(allstatuses)), allstatuses))
    })
    callModule(Covid19Mirai:::mod_caseBoxes_server, "boxes", counts = counts)
  }
  runApp(shinyApp(ui = ui, server = server), launch.browser = TRUE)
}

