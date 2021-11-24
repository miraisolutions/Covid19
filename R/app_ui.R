#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bsplus use_bs_tooltip bs_embed_tooltip
#' @noRd
app_ui <- function(request) {

  # Params ----
  n <- 1000 #  min number of cases for a country to be considered. Default 1000
  # to be used in Global and Comparison
  w <- 7 # number of days of outbreak. Default 7
  now = as.POSIXct(Sys.time()) # given time zone
  AsOfDate =  as.character(as.Date(now - delay_date()))

  message("app_ui")
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    fluidPage(
      # Enable tooltips
      use_bs_tooltip(),
      # Header  ----
       fluidRow(
        id = "header",
        div(
          id = "header-left",
          div(
            id = "title",
            a(
              href = "https://github.com/miraisolutions/Covid19",
              target = "_blank",
              span(id = "app-name", "Covid19")
            ),
            a(
              href = "https://github.com/miraisolutions/Covid19/blob/master/NEWS.md",
              target = "_blank",
              span(id = "version", get_Covid19_version())
            ),
            actionButton(
              inputId = "btn_whatsnew",
              label = "",
              icon = icon("exclamation-circle")
              # style = "color: white; background-color: transparent; border-color: transparent; padding: 0px 10px !important; margin: -10px!important; vertical-align: baseline !important;"
            )
          ),
          a(
            # href = "https://github.com/CSSEGISandData/COVID-19",
            #href = "https://github.com/bumbeishvili/covid19-daily-data",
            href =  "https://covid19datahub.io/",
            target = "_blank",
            span(
              id = "subtitle",
              #"Data source: worldometers from 26.03.2020, JHU CSSE before.",
              tags$p(paste("Data source: COVID-19 Data Hub, latest update on", AsOfDate)) %>%
              #textOutput("last_update", inline = TRUE) %>%
                bs_embed_tooltip(title = "Data Repository by COVID-19 Data Hub. More information in the README on our github page.", placement = "right")

            )
          )
        ), # end header-left

        div(
          id = "header-right",
          a(
            href = "https://mirai-solutions.ch",
            target = "_blank",
            img(
              id = "mirai-logo-header",
              src = "www/mirai_logo_inverted_manual.png",
              align = "right"
            )
          )
        ) # end header-left

      ), # end Header fluidRow
      modalDialog(title = "Covid19Mirai loading message",
                  tags$p("Data is growing. Allow 30 seconds for the first page to load."),
                  tags$p("Load first page fully before navigating to others."),
                  tags$p("Dashboard designed for  desktop view.")),

      # body ----
      #' tags$head(
      #'   # Note the wrapping of the string in HTML()
      #'   tags$style(HTML("
                #@import url('https://fonts.googleapis.com/css2?family=Yusei+Magic&display=swap');
      #'           body {
      #'             background-color: #FFEBCD;
      #'             color: white;
      #'           }"
      #'   ))
      #' ),
      #tabsetPanel(
      navbarPage(
        "",
        #"Continents and Countries insight",
        id = "main_ui",
        tabPanel("Global",
                 id = "tab_global",
                 mod_global_ui("global")),
        tabPanel("Continents",
                 #tabsetPanel(
                 navbarPage(
                   "",
                   id = "continents_ui",
                   tabPanel("Summary",
                          id = "tab_global",
                           mod_continent_comparison_ui("continent_comparison")),
                   tabPanel("Europe",
                            id = "tab_global",
                            mod_continent_ui("Europe_comparison", "europe", nn = n)),
                   tabPanel("Asia",
                            id = "tab_global",
                            mod_continent_ui("Asia_comparison", "asia", nn = n)),
                   tabPanel("Africa",
                            id = "tab_global",
                            mod_continent_ui("Africa_comparison", "africa", nn = n)),
                   tabPanel("Lat. America & Carib.",
                            id = "tab_global",
                            mod_continent_ui("LatAm_comparison", "latam", nn = n)),
                   tabPanel("Northern America",
                            id = "tab_global",
                            mod_continent_ui("NorthernAmerica_comparison","northernamerica", nn = n)),
                   tabPanel("Oceania",
                            id = "tab_global",
                            mod_continent_ui("Oceania_comparison","oceania", nn = n))
                 )
                ),
        tabPanel("Switzerland",
                 id = "tab_global",
                 mod_ind_country_ui("swiss")),
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
          href = "https://github.com/miraisolutions/Covid19.git",
          target = "_blank",
          icon("github-square", "fa-2x")
        ),
        a(
          href = "http://www.mirai-solutions.com",
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

