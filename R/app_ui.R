#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    fluidPage(
      # Header ----
      fluidRow(
        id = "header",
        div(style = "margin-left:3%; margin-bottom:10px;",
            a(
              div(h1("Covid19"), style = "color:white!important;"),
              href = "https://github.com/miraisolutions/Covid19", target="_blank"
            ),
            a(
              div(
                style = "color:black!important;",
                p("Data Repository by Johns Hopkins CSSE: ",
                  textOutput("last_update", inline = TRUE))),
                href = "https://github.com/CSSEGISandData/COVID-19", target="_blank"
            )
        )
      ),
      # body ----
      tabsetPanel(
        id = "main_ui",
        tabPanel("Global",
                 id = "tab_global",
                 mod_global_ui("global")),
        tabPanel("Country",
                 id = "tab_global",
                 mod_country_ui("country")),
        tabPanel("Country Comparison",
                 id = "tab_global",
                 mod_country_comparison_ui("country_comparison"))
      ),
      # Footer ----
      fluidRow(
        id = "footer",
        a(
          id = "git-footer",
          href = "https://github.com/miraisolutions/Covid19.git", target="_blank",
          target = "_blank",
          icon("github-square", "fa-2x")
        ),
        a(
          href = "http://www.mirai-solutions.com", target="_blank",
          target = "_blank",
          img(
            id = "mirai-footer",
            src = "www/mirai.png",
            align = "right",
            height = "100px"
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){

  add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'Covid19'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

