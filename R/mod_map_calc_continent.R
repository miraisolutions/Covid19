#' Continent map calc UI Function
#'
#' @description A shiny Module.
#'
#' @param id Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
#' @importFrom leaflet leafletOutput
#' @importFrom shinycssloaders withSpinner
mod_map_area_calc_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$style(type = "text/css", " .leaflet .legend {font-size: 10px; line-height: 15px;font-family: 'Arial', sans-serif;}"),

    div(class = "plotitle",uiOutput(ns("title_map")), align = "center"),
    div(class = "plottext",uiOutput(ns("controls"))), # radio buttons to be updated in server
    # Height needs to be in pixels. Ref https://stackoverflow.com/questions/39085719/shiny-leaflet-map-not-rendering
    withSpinner(leafletOutput(ns("map_area_calc"), width = "100%", height = "500")),
    div(htmlOutput(ns("caption")), align = "center",class = "plottext")
)
 # )
}

#' map calc Server Function
#'
#' @param df data.frame as of today
#' @param countries_data_map data.frame sp for mapping
#' @param area character continent or subcontinent or area name
#' @param variable character variable name
#' @param max.pop integer cut off Country.Region with lower population
#'
#' @example man-roxygen/ex-mod_map_area_calc.R
#'
#' @import dplyr
#' @import tidyr
#' @import leaflet
#' @import leaflet.extras
#'
#' @noRd
mod_map_area_calc_server <- function(input, output, session, df, countries_data_map, area, variable = "confirmed", max.pop = 100000, countrymap = FALSE){
  ns <- session$ns
  update_ui <- update_radio(variable)
  if (!is.null(update_ui$new_buttons)){

    n.but = length(update_ui$new_buttons$name)
    if (n.but == 1) {
      observe({

        output$controls <- renderUI({
          ns <- session$ns
          tagList(
            radioButtons(inputId = ns(update_ui$new_buttons$name[[1]]), label = "",
                         choices = update_ui$new_buttons$choices[[1]],
                         selected = update_ui$new_buttons$choices[[1]][names(update_ui$new_buttons$selected[[1]])],
                         inline = T)
          )
        })
      })

      button = reactiveValues("radio" = update_ui$new_buttons$choices[[1]][[names(update_ui$new_buttons$selected[[1]])]])

    } else if (n.but == 2) {
      observe({

        output$controls <- renderUI({
          ns <- session$ns
          tagList(
            fluidRow(
              column(6,
                radioButtons(inputId = ns(update_ui$new_buttons$name[[1]]), label = "",
                           choices = update_ui$new_buttons$choices[[1]],
                           selected = update_ui$new_buttons$choices[[1]][names(update_ui$new_buttons$selected[[1]])],
                           inline = T)
            ),
            column(6,
              radioButtons(inputId = ns(update_ui$new_buttons$name[[2]]), label = "",
                         choices = update_ui$new_buttons$choices[[2]],
                         selected = update_ui$new_buttons$choices[[2]][names(update_ui$new_buttons$selected[[2]])],
                         inline = T)
            )
           )
          )
        })
      })
      button = reactiveValues("radio" = update_ui$new_buttons$choices[[1]][[names(update_ui$new_buttons$selected[[1]])]])
      button$oneMpop = update_ui$new_buttons$selected[[2]]
    }
    observe({
      req(input[[update_ui$new_buttons$name[[1]]]])
      #message("observe button 1 ", paste( req(input[[update_ui$new_buttons$name[[1]]]]), collapse = ","))
      button[[update_ui$new_buttons$name[[1]]]] =  input[[update_ui$new_buttons$name[[1]]]]
    #})
    if (n.but == 2){
      #observe({
        #message("observe button 2 ", paste( req(input[[update_ui$new_buttons$name[[2]]]]), collapse = ","))

        req(input[[update_ui$new_buttons$name[[2]]]])
        button[[update_ui$new_buttons$name[[2]]]] =  input[[update_ui$new_buttons$name[[2]]]]
    }
    })
    #}
  } else
    button = NULL

  data_clean <- reactive({
    # data <- orig_data_aggregate %>%
    #   filter(date %in% head(date,day()))  #%>%# select data last 7 days or 1
     # remove for all variables, otherwise some countries appear in a map and not in another one
      message("remove very small countries not to mess up map")
      data = df %>%
        filter(population > max.pop)
      # TODO: it can be moved outside, in the data preparation
      if ((grepl("growth",variable) && grepl("prev",variable))) {
        data = data %>%
          mutate(growth_vs_prev_3 = y_vs_x_calc(data, yvar = "growth_factor_3", xvar = "confirmed_rate_1M_pop"),
                 growth_vs_prev_7 = y_vs_x_calc(data, yvar = "growth_factor_7", xvar = "confirmed_rate_1M_pop"),
                 growth_vs_prev_14 = y_vs_x_calc(data, yvar = "growth_factor_14", xvar = "confirmed_rate_1M_pop"))
      }
      if ((grepl("growth",variable) && grepl("stringency",variable))) {
        data = data %>%
          mutate(growth_vs_stringency_3 = y_vs_x_calc(data, yvar = "growth_factor_3", xvar = "stringency_index", xLab = "Stringency"),
                 growth_vs_stringency_7 = y_vs_x_calc(data, yvar = "growth_factor_7", xvar = "stringency_index", xLab = "Stringency"),
                 growth_vs_stringency_14 = y_vs_x_calc(data, yvar = "growth_factor_14", xvar = "stringency_index", xLab = "Stringency"))

      }
     data$country_name <- as.character(unique(as.character(countries_data_map$NAME))[charmatch(data$Country.Region, unique(as.character(countries_data_map$NAME)))])

    data_clean <- data %>%
      filter(!is.na(country_name))

    data_clean
  })

  # update variable name

  new_var_calc = function(var, but) {
    if (grepl("growth",var) && grepl("prev",var)){
      newvar =  paste("growth_vs_prev", tail(strsplit(but$radio,"_")[[1]], 1), sep = "_")
    } else if (grepl("growth",var) && grepl("stringency",var)){
      newvar =  paste("growth_vs_stringency", tail(strsplit(but$radio,"_")[[1]], 1), sep = "_")
    } else if (!is.null(but) && !is.na(but$radio)) {
      newvar = ifelse(var == but$radio, var, but$radio)
      if (!is.null( but$oneMpop) && but$oneMpop == "oneMpop") {
        newvar = paste0(newvar, "_rate_1M_pop")
      }
    } else
      newvar = var
    message("new var = ",newvar)

    newvar
  }
  new_var =  reactive({
    new_var_calc(variable, button)
  })
  # Map ----
  data_plot <- reactive({
    data_selected <- data_clean() %>%
      bind_cols(data_clean()[,new_var()] %>%
                  setNames("indicator"))
    data_selected <- data_selected %>%
      select(country_name, indicator, update_ui$textvar) %>% # textvar contains additional columns for popup
      mutate(country_name = as.factor(country_name))

    # if (!is.null(button$oneMpop) && req(button$oneMpop) == "oneMpop") {
    #   data_selected <- data_selected %>%
    #     # percentage of indicator per 1M population
    #     mutate(indicator = round(1000000 * .$indicator / .$population))
    # }

    data_plot <-  sp::merge(countries_data_map,
                            data_selected,
                            by.x = "NAME",
                            by.y = "country_name",
                            all.x = TRUE, # set to T, it was F
                            sort = FALSE)
    data_plot
  })

  # add Title to output
  output$title_map <- renderUI(div(h4(update_ui$graph_title), align = "center",
                                   style = "margin-top:20px; margin-bottom:20px;"))

  # add caption info depending on plot
  if (!is.null(update_ui$caption)) {
    output$caption <- renderText(
      update_ui$caption
    )
  }

  output[["map_area_calc"]] <- renderLeaflet({
    if (!countrymap) {
      map = leaflet(
        data = data_plot(), # TODO: inside renderLeaflet the call should not depend on the data
        options = leafletOptions(zoomControl = FALSE,
                                 minZoom = area_map_spec(area, "zoom")*0.95, maxZoom = area_map_spec(area, "zoom")*1.05,
                                 dragging = TRUE,
                                 centerFixed = TRUE,
                                 maxBounds = list(
                                   c(area_map_spec(area, "lat")[1], area_map_spec(area, "lat")[2]),
                                   c(area_map_spec(area, "lat")[3], area_map_spec(area, "lat")[4])
                                 ),
                                 #sizingPolicy =leafletSizingPolicy(
                                   browser.defaultWidth = "80%",
                                   browser.fill = F
                                   # browser.padding = 100,
                                   # viewer.suppress = TRUE, knitr.figure = FALSE,
                                   # knitr.defaultWidth = "100%"
                                   #)
        )) %>%
        setView(
          lng = mean(area_map_spec(area, "lat")[c(1,3)]), lat = mean(area_map_spec(area, "lat")[c(2,4)]),
                zoom = area_map_spec(area, "zoom"))
    } else {
      map = leaflet(
        data = data_plot(),
        options = leafletOptions(zoomControl = FALSE,
                                 minZoom = area_map_spec(area, "zoom")*0.975, maxZoom = area_map_spec(area, "zoom")*1.025,
                                 maxBounds = list(
                                   c(data_plot()@bbox["y","min"], data_plot()@bbox["x","min"]),
                                   c(data_plot()@bbox["y","max"], data_plot()@bbox["x","max"])
                                 ),
                                 dragging = TRUE,
                                 centerFixed = TRUE,
                                 #sizingPolicy =leafletSizingPolicy(
                                 browser.defaultWidth = "80%",
                                 browser.fill = F
        ))  %>% clearBounds() %>%
        setView(
          lng = mean(data_plot()@bbox["y",]), lat = mean(data_plot()@bbox["x",]),
          zoom = area_map_spec(area, "zoom"))
    }
    leg_par <- legend_fun(data_plot()$indicator, new_var())
    map = map %>%
      addPolygons(layerId = ~NAME,
                  fillColor = pal_fun(new_var(), data_plot()$indicator)(pal_fun_calc(data_plot()$indicator, new_var())),
                  fillOpacity = 1,
                  color = "#BDBDC3",
                  group = "polygonsmap",
                  label = ~NAME,
                  weight = 1,
                  popup = map_popup_data(data_plot(), "NAME", "indicator", new_var(), update_ui$textvar),
                  popupOptions = popupOptions(keepInView = T, autoPan = F
                                              #autoPanPadding = c(100, 100)
                                              #offset = c(100,0)
                                              )

                  )
     map =  addSearchFeatures(map, targetGroups  = "polygonsmap",
                     options = searchFeaturesOptions(zoom=0, openPopup=TRUE, firstTipSubmit = TRUE,
                                                     hideMarkerOnCollapse = T,
                                                     moveToLocation = FALSE)
                      )
      do.call(what = "addLegend", args = c(list(map = map), leg_par, list(position = area_map_spec(area, "legend"))))

  })

}

#' Updates UI radiobuttons depending to variable chosen for the map
#' @param var variable name
#' @param growthvar integer, 3 5 7 depending on choice
#' @param global logical, TRUE when used in global map
#'
#' @return list list(new_buttons = new_buttons, graph_title = graph_title, caption = caption, textvar= textvar)
#' new_buttons: UI radiobuttons
#' graph_title: graph title
#' caption: vaption
#' textvar: variables to add in popup
update_radio <- function(var, growthvar = 7, global = FALSE){

  graph_title = var
  textvar = NULL

  oneMpopbut = c("Total" = "tot", "Over 1 M people" = "oneMpop")


  if ((grepl("growth",var) && grepl("fact",var))) { # growth factor
    new_buttons = list(name = list("radio"),
                       choices = list(unlist(varsNames()[grep("(growth)*fact", varsNames())])),
                       selected = list(unlist(varsNames(paste0("growth_factor_", growthvar)))))
    #caption <- paste0("Growth Factor: total confirmed cases today / total confirmed cases (3 5 7) days ago.")
    caption <- caption_growth_factor_fun("(3 7 14)")

    graph_title = "Growth Factor"
    textvar = c("new_confirmed","lw_confirmed","confirmed","lw_active")

  } else if (FALSE && grepl("(prevalence|rate)(?:.+)(prevalence|rate)",var)) {
    #mapvar = grep("(confirmed|rate)(?:.+)(confirmed|rate)", varsNames(), value = T)
    mapvar = unlist(varsNames(
        prefix_var("confirmed_rate_1M_pop", c("","lw","new"))
    ))
    #mapvar = c(grep("^confirmed", mapvar, value = TRUE), grep("^lm", mapvar, value = TRUE),grep("^lw", mapvar, value = TRUE),grep("^new", mapvar, value = TRUE))
    #mapvar = mapvar[!grepl("^lm", mapvar)]
    namesmapvar = c("Total",# "Last Month",
                    "Last Week",
                    "Last Day")
    #TODO: order is not stable
    names(mapvar) = namesmapvar
    new_buttons = list(name = "radio",
                       choices = mapvar, selected = mapvar["Last Week"])
    caption <- paste(caption_prevalence(), names(varsNames("confirmed_rate_1M_pop")))
    graph_title = "Prevalence of contagion over 1M"
    textvar = c("new_confirmed","lw_confirmed","confirmed","population", "new_confirmed_rate_1M_pop", "lw_confirmed_rate_1M_pop", "pw_confirmed_rate_1M_pop", "lm_confirmed_rate_1M_pop", "confirmed_rate_1M_pop")

  } else if (grepl("death", var) || grepl("mortality", var)) {
    #mapvar = unlist(varsNames(c("lethality_rate","lw_lethality_rate","deaths_rate_1M_pop","lw_deaths_rate_1M_pop")))
    mapvar = unlist(varsNames(c(
      prefix_var("lethality_rate", c("","lw")),
      prefix_var("deaths_rate_1M_pop", c("","lw"))
    )))

    #names(mapvar) = c("Lethality Rate", "Mortality over 1M pop")
    new_buttons = list(name = list("radio"),
                       choices = list(mapvar), selected = list(mapvar[grepl("^Mortality",names(mapvar))]))

    caption_leth_rate <- paste("Lethality Rate:", caption_death_fun("lethality_rate"))
    caption_mrt_rate <- paste("Mortality Rate:", caption_death_fun("deaths_rate_1M_pop"))
    caption = HTML(paste(c(caption_leth_rate,caption_mrt_rate), collapse = '<br/>'))
    graph_title = "Death Rate"
    textvar = c("new_deaths", "lw_deaths", "deaths", "lw_lethality_rate","pw_lethality_rate","population")
    if(global) {
      textvar = c(textvar, c("lethality_rate","deaths_rate_1M_pop"))
      #textvar = textvar[!grepl("deaths",textvar)]
    }
  } else if ((grepl("growth",var) && grepl("prev",var))) {
    #new_buttons = NULL
    new_buttons = list(name = list("radio"),
                       choices = list(unlist(varsNames()[grep("(growth)*fact", varsNames())])),
                       selected = list(unlist(varsNames(paste0("growth_factor_", growthvar)))))

    caption_growth_factor <- caption_growth_factor_fun("(3 7 14)")

    caption_prevalence <- paste(caption_prevalence(), names(varsNames("confirmed_rate_1M_pop")))
    caption =HTML(paste(c(caption_growth_factor,caption_prevalence), collapse = '<br/>'))
    graph_title = "Growth versus Prevalence"
    textvar = c(as.vector(grep("^growth_factor", unlist(varsNames()), value = TRUE)), "new_confirmed_rate_1M_pop", "lw_confirmed_rate_1M_pop", "confirmed_rate_1M_pop")

  } else if (grepl("active", var)) {
    #mapvar = paste0(c("", "lw_","new_"),"active")
    mapvar = unlist(varsNames(c(
        prefix_var("active", c("","lw","new"))
      )))

    #mapvar = varsNames()[mapvar]
    names(mapvar) = c("Total", "Last Week",
                      "Last Day")
    new_buttons = list(name = list("radio"),
                       choices = list(mapvar), selected = list(mapvar["Last Week"]))
    # new_buttons = list(name = list("radio","oneMpop"),
    #                    choices = list(mapvar, oneMpopbut), selected = list(mapvar["Last Week"], oneMpopbut["Over 1 M people"]))

    caption =HTML(paste(c(caption_active()), collapse = '<br/>'))

    graph_title = "Active cases"
    textvar = c("new_active","lw_active","pw_active","active", "new_confirmed", "confirmed","new_recovered","recovered")

  } else if (grepl("confirmed", var)) {
    #mapvar = grep("confirmed$", varsNames(), value = T)
    mapvar = unlist(varsNames(c(
        prefix_var("confirmed", c("","lw","new"))
      )))
    names(mapvar) = c("Total", "Last Week",
                      "Last Day")
    # new_buttons = list(name = "radio",
    #                    choices = mapvar, selected = mapvar["Last Week"])
    new_buttons = list(name = list("radio","oneMpop"),
                       choices = list(mapvar, oneMpopbut), selected = list(mapvar["Last Week"], oneMpopbut["Over 1 M people"]))

    caption <- "Total, Last Week, New and Over 1 Million peope Confirmed Positive cases"
    graph_title = "Confirmed positive cases"
    textvar = c(c(rev(prefix_var("confirmed")), "confirmed"),c(rev(prefix_var("confirmed_rate_1M_pop")),"confirmed_rate_1M_pop"),
                "growth_factor_3","fully_vaccinated_rate",
                "tests", "population")
    if (global) {
      caption <- "Total and Over a million Confirmed positive cases"
      textvar = setdiff(textvar, "growth_factor_3")
    }
  }   else if (grepl("stringency", var) && grepl("index$", var)) {
    new_buttons = NULL
    graph_title = "Current Stringency Index"
    caption <- caption_stringency()
    textvar = c(as.vector(grep("^growth_factor", unlist(varsNames()), value = TRUE)), "new_confirmed_rate_1M_pop", "lw_confirmed_rate_1M_pop", "confirmed_rate_1M_pop")
    if(global) {
      textvar = textvar[(!grepl("growth", textvar)) & (!grepl("^lw", textvar))]
      textvar = c(textvar, "new_confirmed", "new_deaths","hosp")
    }
  } else if ((grepl("growth",var) && grepl("stringency",var))) {
    #new_buttons = NULL
    new_buttons = list(name = list("radio"),
                       choices = list(unlist(varsNames()[grep("(growth)*fact", varsNames())])),
                       selected = list(unlist(varsNames(paste0("growth_factor_", growthvar)))))

    caption_growth_factor <- caption_growth_factor_fun("(3 7 14)")

    caption_stringency <- caption_stringency()
    caption =HTML(paste(c(caption_growth_factor,caption_stringency), collapse = '<br/>'))
    graph_title = "Growth vs Current Stringency Lock-Down Index"
    textvar = c(as.vector(grep("^growth_factor", unlist(varsNames()), value = TRUE)), "stringency_index", "new_confirmed_rate_1M_pop", "lw_confirmed_rate_1M_pop","pw_confirmed_rate_1M_pop")

  } else if (var == "tests") {

    #mapvar = grep("tests$", varsNames(), value = T)
    mapvar = unlist(varsNames(c(
          prefix_var("tests", c("","lw","new"))
        )))
    names(mapvar) = c("Total", "Last Week",
                      "Last Day")
    # new_buttons = list(name = "radio",
    #                    choices = mapvar, selected = mapvar["Last Week"])
    new_buttons = list(name = list("radio","oneMpop"),
                       choices = list(mapvar, oneMpopbut), selected = list(mapvar["Last Week"], oneMpopbut["Over 1 M people"]))

    caption <- paste("Total, Last Week, New and", caption_tests()[1])
    caption = c(caption, caption_tests()[2])
    caption =HTML(paste(caption, collapse = '<br/>'))

    graph_title = "Tests per Population size"
    textvar = c(c(rev(prefix_var("tests")), "tests"),c(rev(prefix_var("tests_rate_1M_pop")), "tests_rate_1M_pop"),
                "population", "lw_positive_tests_rate", "pw_positive_tests_rate","positive_tests_rate")

  } else if (grepl("positive", var)) {
    #mapvar = grep("positive_tests_rate", varsNames(), value = T)
    mapvar = unlist(varsNames(c(
          prefix_var("positive_tests_rate", c("","lw","new"))
        )))
    names(mapvar) = c("Total", "Last Week",
                      "Last Day")
    new_buttons = list(name = list("radio"),
                       choices = list(mapvar), selected = list(mapvar["Last Week"]))
    caption <- paste("Total, Last Week and New", caption_positive_tests())
    caption_tests <- caption_tests()[2]
    caption =HTML(paste(c(caption,caption_tests), collapse = '<br/>'))

    graph_title = "Positive Tests rate"
    textvar = c("lw_tests","tests", "lw_confirmed","confirmed", "lw_confirmed_rate_1M_pop", "pw_confirmed_rate_1M_pop","confirmed_rate_1M_pop")
  }  else if (FALSE && grepl("hospitalized", var) && grepl("1M", var)) {
    hospvars_1M_pop = paste(.hosp_vars,"rate_1M_pop",  sep = "_" )
    mapvar = unlist(varsNames(hospvars_1M_pop))
    #mapvar = prefix_var("hosp_rate_1M_pop", c("","lw","new"))

    new_buttons = list(name = "radio",
                       choices = mapvar, selected = mapvar[mapvar == "hosp_rate_1M_pop" ])
    caption <- "Current status of Hospitalization over 1 M people."
    caption_hosp <- paste("Data may not be available for all areas and", length(mapvar), "statuses")
    caption =HTML(paste(c(caption,caption_hosp), collapse = '<br/>'))

    graph_title = "Hospitalization over 1 M people"
    #textvar = c("active",c(as.vector(t(sapply(c("new_","lw_"), paste0, .hosp_vars))), "icuvent_rate_hosp", "population"))
    textvar = c("lw_confirmed", c(paste0("new_",as.vector(.hosp_vars)), as.vector(.hosp_vars), "icuvent_rate_hosp", "population"))

  } else if (grepl("hospitalized", var)) {
      mapvar = unlist(varsNames(.hosp_vars))
      # new_buttons = list(name = "radio",
      #                    choices = mapvar, selected = mapvar["Hospitalized"])
      new_buttons = list(name = list("radio","oneMpop"),
                         choices = list(mapvar, oneMpopbut), selected = list(mapvar["Hospitalized"], oneMpopbut["Over 1 M people"]))

      caption <- "Current status of Hospitalization."
      caption_hosp <- paste("Data may not be available for all areas and", length(mapvar), "statuses")
      caption =HTML(paste(c(caption,caption_hosp), collapse = '<br/>'))

      graph_title = "Hospitalization"
      #textvar = c("active",c(as.vector(t(sapply(c("new_","lw_"), paste0, .hosp_vars))), "icuvent_rate_hosp"))
      textvar = c("lw_confirmed", c(paste0("new_",as.vector(.hosp_vars)), "lw_hosp", "pw_hosp","icuvent_rate_hosp", "fully_vaccinated_rate"))

  } else if (grepl("vaccin", var)) {
    #mapvar = grep("vaccines$", varsNames(), value = T)
      mapvar = unlist(varsNames(c(
            prefix_var("fully_vaccinated_rate", c("")),
            prefix_var("vaccines_rate_1M_pop", c("","lw"))
          )))
      # names(mapvar) = c("Total", "Last Week",
      #                   "Last Day")
      new_buttons = list(name = list("radio"),
                         choices = list( mapvar), selected = list(mapvar[mapvar == "fully_vaccinated_rate" ]))

      # caption <- paste("Total, Last Week and New", caption_tests("Vaccines")[1])
      # caption = c(caption, caption_tests()[2])
      caption <- paste("Fully Vaccinated and Administred doses")
      caption_vax <- caption_tests("Vaccines")[2]

      caption =HTML(paste(c(caption,caption_vax,caption_vaccines()), collapse = '<br/>'))

      graph_title = "Vaccination"
      textvar = c("new_vaccines","lw_vaccines", "vaccines", "fully_vaccinated_rate","vaccinated_rate", "population", "lw_vaccines_rate_pop", "vaccines_rate_pop")

  } else  {
      new_buttons = NULL
      caption = NULL
  }
  textvar = c("AsOfDate", textvar)
  if (global) {
    textvar = textvar[!grepl("^new",textvar )]
    textvar = textvar[!grepl("^pw",textvar )]

    textvar = c(textvar, "population")
    textvar = unique(textvar)
  }

  list(new_buttons = new_buttons, graph_title = graph_title, caption = caption, textvar= textvar)

}

#' Utility for popup message in map
#' @param data map data
#' @param nam character: component of country names from data, NAME
#' @param ind character: component of values from data, indicator
#' @param namvar character: vector, additional variable names
#' @param textvar character: vector, text for the additional variables
#' @param namvarsfx character: vector, text suffix for namevar. Default NULL: no suffix.
#'
#' @return vector pop up messages, html
map_popup_data <- function(data, nam, ind, namvar, textvar, namvarsfx = NULL){
  x = data[[ind]]
  NAME = data[[nam]]
  textvars = NULL
  varName = names(varsNames(namvar))
  if (!is.null(namvarsfx)) {
    varName = paste(varName, namvarsfx)
  }
  if (!is.null(textvar)) {
    textvar = setdiff(textvar, namvar) # remove namvar if textvar is present
    #TODO: we could taje all lw variables if namvar is lw, same with new
    if (length(namvar)>0) {
      textvars = list(data = as.list(data@data[c(textvar)]),
                      NAME = names(varsNames(textvar)))
      names(textvars$data) = textvar
    } else
      textvars = NULL
  }

  text.pop.x = gen_text(x, namvar)

  .paste_text = function(nam, txt, col = NULL) {

    if (!is.null(col)){
      coltext = c(paste0("<style='color:", col,";'>"))
    } else
      coltext = NULL
    .pastecol = function(char = NULL,ptxt){
      if (!is.null(char)) {
        if (!is.null(ptxt))
          char = gsub(">"," ",char)
        ptxt = gsub("<","",ptxt)
      } #else
        #ptext = paste0(ptxt, "</style>")
      paste0(char, ptxt)
    }
    paste0(
      #"<style> div.leaflet-popup-content {width:auto !important;}</style>",
      .pastecol(char = "<strong>",ptxt = coltext ), nam,":",
      " </strong>",
      .pastecol(ptxt = coltext ),txt, ifelse(is.null(col), "", "</style>"),
      "<br>")

  }

  name_text = .paste_text("Country", NAME, .case_colors["confirmed"]) # to get a blue
  val_text = .paste_text(varName, text.pop.x, "darkblue")

  text = paste0(name_text, val_text)

  if (!is.null(textvars)) {

    values.pop.vars = NULL

    for (tvar in textvar) {
      values.pop.vars[[tvar]] = gen_text(textvars$data[[tvar]], tvar)
    }
    #values.pop.vars = lapply(textvars$data,gen_text, namvar =  textvar)
    names(values.pop.vars) = textvars$NAME
    tex.pop.vars =  lapply(names(values.pop.vars), function(nn)
      .paste_text(nn, values.pop.vars[[nn]] ))
    # add text
    tex.pop.vars = c(list(text), tex.pop.vars)
    text = Reduce(paste0, tex.pop.vars)
  }
  text
}
#' Utility legend building
#' @param x numeric vector of map data
#' @param var character: variable name
#' @return list legend parameters
legend_fun <- function(x, var){
  if (is.numeric(x)) { # if variable is numeric
    maxv = max(x, na.rm =T)
    minxv = min(x, na.rm =T)
    dg = nchar(as.character(round(max(abs(minxv),maxv, na.rm =T))))
    #dg = nchar(as.character(round(maxv)))
    domain = choose_domain(x, var)
    cvx = sd(x,na.rm = T) / mean(x,na.rm = T)
    #if (dg < 6 || (var %in% domainlin_vars()) || cvx<1){
    bin = domain(x)

    if ((grepl("growth",var) && grepl("fact",var)) || (var %in% .rate_vars) || (var %in% domainlin_vars()) || cvx<1){

      bin = seq(bin[1],bin[2], length = 4)
      val = seq(min(bin),max(bin), length = 5000)
      dat = val
      suf = ""
      if (grepl("1M", var))
        suf = " over 1M"
      #if (dg==1 && maxv<=1 && minxv >= 0)
      if (var %in% .rate_vars)
        suf = " %"
      transf = function(x,dg){
        #if (dg==1 && maxv<=1 && minxv >= 0)
        if (var %in% .rate_vars)
          x = x * 100
        x
      }

      form = labelFormat(transform = function(x) x,
                         suffix = suf, digits = getdg_lab(dg, maxv, minxv), big.mark = "'")
    }  else { # high values, like total

      bin = seq(bin[1],bin[2], length = 5)
      if (F & any(x<0, na.rm = TRUE)) {
        #add 0, not possible
        bin = sort(c(bin, 0))
      }

      if(any(x<0, na.rm = TRUE))
        #val = c(-log(1:exp(-min(bin))),log(1:exp(max(bin))))
        val = c(-log(seq(1,exp(-min(bin)), length = 5000)),log(seq(1,exp(max(bin)), length = 5000)))
      else
        val = log(seq(1,exp(max(bin)), length = 5000))
      dat = val
      suf = ifelse(grepl("1M", var)," over 1M", "")
      .round_val = function(x){
       if (any(x<0, na.rm = TRUE)){
         y = rep(NA,length(x))
         y[!is.na(x) & x<0] = -round_up(exp(-x[!is.na(x) & x<0]))
         y[!is.na(x) & x>=0] = round_up(exp(x[!is.na(x) & x>=0]))
         y[is.infinite(y)] = 0 # perhaps to be moved also in the other case
       } else
           y = round_up(exp(x))
         y
      }
      form = labelFormat(transform = function(x) .round_val(x), suffix = suf, big.mark = "'")
    }
    res =     list(
      bins = bin,
      values = val,
      data = dat,
      labFormat = form)

  } else {
    label =  unique(sort(x))
    form = labelFormat(transform = function(x) unique(sort(x)))
    res =     list(
      labels = label,
      values = unique(sort(x)),
      labFormat = form)

  }
  #res$position = "bottomright"
  #res$className= "panel-panel-default legend"
  res$opacity = 1
  res$pal = pal_fun(var, x)
  res
}
domainlog <- function(x) {
  maxv = max(x, na.rm = TRUE)
  c(0,log(round_up(maxv)))
}
domainlog_neg <- function(x) {
  maxv = max(x, na.rm = TRUE)
  minv = min(x, na.rm = TRUE)
  #bound = c(-log(round_up(-minv)),log(round_up(maxv)))
  maxlimit = max(abs(x), na.rm = T)
  bound = c(-log(round_up(maxlimit)),log(round_up(maxlimit)))
  bound
}

domainlin <- function(x) {
  c(max(0,round_up(min(x, na.rm = TRUE), down = TRUE)),round_up(max(x, na.rm = TRUE)))
}
domainlin_neg <- function(x) {
  maxlimit = max(abs(x), na.rm = TRUE)
  c(-round_up(maxlimit),round_up(maxlimit))
}
domainfact <- function(x) {
  unique(sort(x))
}
domaingrowth <- function(x) {
  c(1,round_up(max(x, na.rm = TRUE)))
}
domainrate <- function(x) {
  c(floor(pmax(0,min(x, na.rm = TRUE))*100),round_up(max(x, na.rm = TRUE)*100))
}
domainindex <- function(x) {
  c(0,100)
}

# to control variables that do not go from 0 to many, where many countries have close values
domainlin_vars <- function(x) {
  vars = unlist(varsNames())
  vars = grep("vaccines", vars, value = TRUE)
  #vars[(grepl("growth",var) && grepl("fact",var))]
  vars = setdiff(vars, grep("rate_pop", vars, value = TRUE))
  "none" # switched off for the moment
}

#' Utility to choose domain for legend
#' @param x numeric vector of map data
#' @param var character vector of variable name
#' @return numeric vector of range
choose_domain <- function(x, var) {
  if (is.numeric(x)) {
    maxy = max(x, na.rm = T)
    minxy = min(x, na.rm = T)
    dg = nchar(as.character(round(max(abs(minxy),maxy))))
    cvx = sd(x,na.rm = T) / mean(x,na.rm = T)
    if (var %in% .rate_vars){ # if rate
      domain = domainrate
    } else if (var %in% .index_vars){ # if rate
      domain = domainindex
    } else if ((grepl("growth",var) && grepl("fact",var))){
      # growth factors variables
      domain = domaingrowth
    # } else if ((var %in% domainlin_vars()) || cvx<1) {
    #   domain = domainlin
    #} else if (dg < 6) {
    } else if ((var %in% domainlin_vars()) || cvx<1) {
      if (var %in% .neg_vars && any(x<0, na.rm = TRUE))
        domain = domainlin_neg
      else
        domain = domainlin
    } else {
      if (var %in% .neg_vars && any(x<0, na.rm = TRUE))
        domain = domainlog_neg
      else
        domain = domainlog
    }
  } else
    domain = domainfact
  domain
}
#' Utility derive palette given datadisplay.brewer.all()

#' @param x numeric vector of map data
#' @param var character: variable name
#'
#' @importFrom grDevices colorRampPalette
#' @return palette
pal_fun = function(var,x){
  domain = choose_domain(x, var)
  hospvars_1M_pop = paste(.hosp_vars,"rate_1M_pop",  sep = "_" )

  if (grepl("confirmed", var)  || grepl("(prevalence|rate)(?:.+)(prevalence|rate)", var)) {
    colorNumeric(palette = "Reds", domain = domain(x), na.color = "lightgray")
  } else if (grepl("death", var) || grepl("mortality", var) || grepl("lethal", var)) {
    colorNumeric(palette = "Greys", domain = domain(x), na.color = "lightgray")
  } else if (grepl("active", var)) {
    # totale active to be excluded because of bad data that could be negative
    if ((grepl("new", var) || grepl("lw", var)) && any(x<0, na.rm = TRUE)) {
      # colorRampPalette to customize and mix 2 palettes
       colorNumeric(palette = colorRampPalette(c("yellow", "#3c8dbc"), interpolate = "linear" )(length(x)),
                   domain = domain(x), na.color = "lightgray")
        # colorNumeric(palette = "GnBu",
        #            domain = domain(x), na.color = "lightgray")

      } else
      colorNumeric(palette = "Blues", domain = domain(x), na.color = "lightgray")

  }  else if (grepl("recovered", var)) {
    colorNumeric(palette = "Greens", domain = domain(x), na.color = "lightgray")
  }  else if (grepl("stringency", var) && grepl("index$", var)) {
    colorNumeric(palette = "Greys", domain = domain(x), na.color = "lightgray")
  }  else if (grepl("growth",var) && grepl("fact",var)) {
    colorNumeric(palette = "Oranges", domain = domain(x), na.color = "lightgray")
  }  else if (grepl("tests", var) && (!grepl("positive", var))) {
    colorNumeric(palette = "BuGn", domain = domain(x), na.color = "lightgray")
  }  else if (grepl("positive", var)) {
    colorNumeric(palette = "YlOrRd", domain = domain(x), na.color = "lightgray")
  }  else if ((grepl("growth",var) && grepl("prev",var))) {
    colorFactor(palette = c("darkgreen", "yellow3", "#E69F00","#dd4b39"), domain = domain(x), ordered = TRUE, na.color = "lightgray")
  } else if ((grepl("growth",var) && grepl("stringency",var))) {
    colorFactor(palette = c("darkgreen", "#3c8dbc", "#dd4b39","gray3"), domain = domain(x), ordered = TRUE, na.color = "lightgray")
  } else if (var %in% c(hospvars_1M_pop, .hosp_vars, prefix_var(.hosp_vars), prefix_var(hospvars_1M_pop))) {
    if (grepl("hosp", var)) {
      colorNumeric(palette = "Blues", domain = domain(x), na.color = "lightgray")
    } else
      colorNumeric(palette = "Purples", domain = domain(x), na.color = "lightgray")
  } else  if (grepl("vaccin", var)) {
    colorNumeric(palette = "Blues", domain = domain(x), na.color = "lightgray")
  } else
    stop("non existing color palette for ", var)
}
#' Utility calculate colors given palette
#' @param x numeric vector of map data
#' @param var character vector of variable name
#' @return rescaled x
pal_fun_calc <- function(x, var){
  if (is.numeric(x)){
    # maxv = max(x)
    # dg = nchar(as.character(round(maxv)))
    maxv = max(x, na.rm = T)
    minxv = min(x, na.rm = T)
    dg = nchar(as.character(round(max(abs(minxv),maxv))))
    cvx = sd(x,na.rm = T) / mean(x,na.rm = T)

    #if (dg == 1 && maxv<=1 && minxv>=0) {
    if (var %in% .rate_vars) {
      y = x *100 # rate
    #} else if (dg < 6 || (var %in% domainlin_vars()) || cvx<1) {
    } else if ((grepl("growth",var) && grepl("fact",var)) || (var %in% domainlin_vars()) || cvx<1) {
      # linear scale
      y = x
    } else {
      if (any(x<0, na.rm = TRUE)){
        y = rep(NA,length(x))
        y[!is.na(x) & x<0] = -log(-x[!is.na(x) & x<0])
        y[!is.na(x) & x>=0] = log(x[!is.na(x) & x>=0])
        #y[is.infinite(y)] = 0 # perhaps to be moved also in the other case
        #y = log(x-min(x)+1) - log(-min(x))
      }
      else{
        y = log(x)
      }
    }
    y[is.infinite(y)] = 0 # perhaps to be moved also in the other case

  } else
    y = x


  y
}
#' Utility round labels in domain and map
#' @param y numeric vector of map data
#' @return numeric rounded x value
roundlab = function(y) {
  # maxy = max(y)
  # dg = nchar(as.character(round(maxy)))
  #
  maxy = max(y, na.rm = T)
  minxy = min(y, na.rm = T)
  dg = nchar(as.character(round(max(abs(minxy),maxy))))

  dglab = getdg_lab(dg, maxy, minxy)
  round(y, dglab)
}

