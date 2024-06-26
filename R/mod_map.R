#' map UI Function
#'
#' @description A shiny Module.
#'
#' @param id, Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom leaflet leafletOutput
#' @importFrom shinycssloaders withSpinner
mod_map_ui <- function(id){
  ns <- NS(id)
  vars = setdiff(names(.case_colors), c("hosp","recovered", "vaccines")) # remove hosp and vaccines for now
  vars = c(vars, "fully_vaccinated_rate")
  choices_map <- c(vars, paste0("lw_",vars), "stringency_index")# %>%
    #setNames(gsub("_", " ",c(vars, paste0("lw_",vars), "stringency_index"))) %>% as.list()

  choices_map = varsNames(choices_map)


  fluidPage(
    # fixedRow(
    #   column(12,
    #          offset = 5,
    #          actionButton(ns("goButton"), "Show World heat map for the main indicators", class = "button-style", style="color: #fff; background-color: #337ab7; border-color: #2e6da4;")
    #   )
    #
    # ),
    div(
      id = "map_ui",
      style = "position: relative;",

      # Height needs to be in pixels. Ref https://stackoverflow.com/questions/39085719/shiny-leaflet-map-not-rendering
      leafletOutput(ns("map"), width = "100%", height = "800")
      ,
      tags$head(tags$style(
        HTML('
               #input_date_control {background-color: rgba(192,192,192,0.6);}
               #sel_date {background-color: rgba(0,0,255,1);}
               #help-block a {color: #ff0000 !important;}'
        )
      )),
      absolutePanel(
        #id = ns("input_date_control"), class = "panel panel-default",
        id = "input_date_control", class = "panel panel-default",
        width = "27.5vw",
        #height = "20vh",
        top = 10, left = 10, draggable = FALSE,
        #fixed = TRUE,
        div(style = "margin:10px;",
            uiOutput(ns("title_map")),
            radioButtons(inputId = ns("radio_choices"), label = "", choices = choices_map, selected = "confirmed", inline = T),
            radioButtons(inputId = ns("radio_pop"), label = "", choices = c("total", "per 1M pop"), selected = "total", inline = T),
            #uiOutput(ns("slider_ui")),
            helpText("Click on the country to obtain its details."),
            div(uiOutput(ns("caption")), align = "center")

        )
      )
    )
  )
}

#' map Server Function
#'
#' @param orig_data_aggregate data.frame
#' @param countries_data_map data.frame sp for mapping
#' @example man-roxygen/ex-mod_map.R
#'
#' @import dplyr
#' @import tidyr
#' @import leaflet
#' @import leaflet.extras
#' @importFrom sp merge
#' @noRd
mod_map_server <- function(input, output, session, orig_data_aggregate, countries_data_map){
  ns <- session$ns

  # Data ----
  #load data

  #data <- orig_data_aggregate #%>% align_country_names()

  orig_data_aggregate$country_name <- as.character(unique(as.character(countries_data_map$NAME))[charmatch(orig_data_aggregate$Country.Region, unique(as.character(countries_data_map$NAME)))])

  data_clean <- orig_data_aggregate %>%
    filter(!is.na(country_name))
  keepcols = c("country_name","Country.Region","date","AsOfDate",
               names(data_clean)[sapply(data_clean, is.numeric)])
  data_clean = data_clean[, keepcols] # remove, not used

  # UI controls ----
  # output$slider_ui <- renderUI({
  #
  #   sliderInput(inputId = ns("slider_day"), label = div(style = "font-size:10px","Day"), min = min(orig_data_aggregate$date, na.rm = TRUE), max = max(orig_data_aggregate$date), value = max(orig_data_aggregate$date),
  #               dragRange = FALSE, animate = animationOptions(interval = 4000, loop = FALSE), step = 7)
  # })

  # Map ----

  # Data for a given date

  data_date <- data_clean %>%
      filter(date == AsOfDate) %>%
      select(-c(Country.Region, contagion_day)) #%>%
 # })
  # observeEvent(input$goButton, {

    data_plot <- reactive({
      data_selected <- data_date %>%
        bind_cols(data_date[,input$radio_choices] %>%
                    setNames("indicator"))

      if (req(input$radio_pop) == "per 1M pop") {
        max.pop = 100000
        data_selected <- data_selected %>%
          filter(population > max.pop) %>% # filter out very small countries
          # percentage of indicator per 1M population
          mutate(indicator = round(1000000 * .$indicator / .$population))
      }
      data_selected <- data_selected %>%
        select(country_name, indicator, update_ui()$textvar)

      data <-  sp::merge(countries_data_map,
                         data_selected,
                         by.x = "NAME",
                         by.y = "country_name",
                         sort = FALSE)
      # GM: To be removed with new legend NAs can be shown
      #data[["indicator"]] <- replace_na(data[["indicator"]], 0)
      data
    })
    update_ui <- reactive(update_radio(input$radio_choices, global = TRUE))

    if (isolate(!is.null(update_ui()$caption))) {
      output$caption <- renderUI(
        div(p(update_ui()$caption), align = "left",
            style = "margin-top:5px; margin-bottom:5px;")
      )
    }
    # add Title to output
    output$title_map <- renderUI(div(h4(update_ui()$graph_title), align = "center",
                                     style = "margin-top:10px; margin-bottom:0px;"))

    max_value <- reactive({
      max(data_plot()[["indicator"]])
    })

    domain <- reactive({
      c(0,log(roundUp(max_value())))
    })


    output$map <- renderLeaflet({
      # Using leaflet() to include non dynamic aspects of the map
      leaflet(elementId = 'map',
              data = countries_data_map,
              options = leafletOptions(zoomControl = FALSE) # not needed, clashes with slider
      ) %>%
        setView(0, 30, zoom = 3)
    })

    # # update map with reactive part
    observe({
      #observeEvent(data_plot(), {
      #output$map:poly <- renderPlot({
      #if("per 1M pop" %in% req(input$radio_pop)) {
      if(req(input$radio_pop) == "per 1M pop") {
        var1M =   "per 1M pop"
        if (!req(input$radio_choices) %in% get_aggrvars())
          return()
      }
      else {
        var1M = NULL
      }
      leg_par <- legend_fun(data_plot()$indicator, input$radio_choices)
      #message("leg_par$bins 1:", paste(leg_par$bins, collapse = ","))
      #mapdata = leafletProxy("map", data = data_plot())  %>%
      #mapdata = leafletProxy("map")  %>%

      mapdata = leafletProxy("map", data = countries_data_map)  %>%
        clearMarkers() %>%
        #clearShapes() %>% removes everything
        clearControls() %>% addLegend(position = "bottomright",
                                      #layerId="colorLegend",
                                      pal = leg_par$pal,
                                      opacity = leg_par$opacity,
                                      bins = leg_par$bins,
                                      values = leg_par$values,
                                      data = leg_par$data,
                                      labFormat = leg_par$labFormat
        )
      mapdata = mapdata %>%
        addPolygons(layerId = ~NAME,
                    # GM line for new colors
                    fillColor = pal_fun(input$radio_choices, data_plot()$indicator)(pal_fun_calc(data_plot()$indicator, input$radio_choices)),
                    fillOpacity = 1,
                    color = "#BDBDC3",
                    group = "mapdata",
                    label = ~NAME,
                    weight = 1,
                    popup = map_popup_data(data_plot(), "NAME", "indicator", input$radio_choices, update_ui()$textvar, namvarsfx = var1M),
                    popupOptions = popupOptions(keepInView = T, autoPan = F
                                                #autoPanPadding = c(100, 100)
                                                #offset = c(100,0)
                    )
        ) #%>%  addLegend(position = "bottomright",
      #                  #layerId="colorLegend",
      #                  pal = leg_par$pal,
      #                  opacity = leg_par$opacity,
      #                  bins = leg_par$bins,
      #                  values = leg_par$values,
      #                  data = leg_par$data,
      #                  labFormat = leg_par$labFormat
      # )

      mapdata =  addSearchFeatures(mapdata, targetGroups  = "mapdata",
                                   options = searchFeaturesOptions(zoom=0, openPopup=TRUE, firstTipSubmit = TRUE,
                                                                   position = "topright",hideMarkerOnCollapse = T,
                                                                   moveToLocation = FALSE))

      mapdata
      # }


    })
 #})



}
