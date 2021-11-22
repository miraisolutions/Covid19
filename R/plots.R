#' length breaks of x axis
.breaks.xaxis = 6
#' length breaks of y axis
.breaks.yaxis = 6

#' Utility for Y labels in plots
#'
#' @param x vector of data
#'
#' @importFrom scales label_number
#'
#' @return x labelled values
lab_num = function(x) {

  mx = median(x, na.rm = TRUE)
  thausands = ifelse(mx > 7500 & mx <= 750000, TRUE, FALSE)
  millions = ifelse(mx > 750000, TRUE, FALSE)

  if (millions) {
    x = round(x / 1000000,2)
    suffix = "M"
  } else if (thausands) {
    x = round(x / 1000,2)
    suffix = "K"
  }

  accy = ifelse(diff(range(x, na.rm = TRUE))<0.05, 0.001,
                ifelse(diff(range(x, na.rm = TRUE))<1, 0.01,
                       ifelse(max(x, na.rm = TRUE) <= 10, 0.1,
                              ifelse(max(x, na.rm = TRUE) <= 100, 1,
                                     ifelse(max(x, na.rm = TRUE) <= 1000, 10,
                                            ifelse(max(x, na.rm = TRUE) <= 10000, 100, 1000))))))

  .labnumfun = label_number(accuracy = accy, big.mark = "'")

  #scales::label_number_si()(x) # in the end it is better to customize
  if (thausands || millions) {
    #x = round(x / 1000,2)
    x = paste(.labnumfun(x),suffix)
  } else {
    x = .labnumfun(x)
  }
  x
}
#' Utility for labels breaks in plots
#'
#' @param x vector of data
#' @param breaks integer number of breaks in label, default = .breaks.xaxis
#'
#' @return x breaks values
breaks_lab = function(x, breaks = .breaks.xaxis) {
  x.d.lim = range(x, na.rm = TRUE)
  # if (is.numeric(x))
  #   x.d.breaks = seq(x.d.lim[1],x.d.lim[2], length.out = breaks)
  # else
  #   x.d.breaks = seq(x.d.lim[1],x.d.lim[2], length.out = breaks)
  x.d.breaks = seq(x.d.lim[1],x.d.lim[2], length.out = breaks)

  x.d.breaks
}


#' Utility for Y labels in plots percentage
#'
#' @param x vector of data
#'
#' @importFrom scales label_number
#'
#' @return x labelled values
#' @export
lab_percent = function(x) {
  maxx = max(x, na.rm = TRUE)
  dg = nchar(as.character(round(maxx)))
  digit = 1
  if (dg == 1)
     digit = 2
  if (diff(range(x, na.rm = TRUE)) > 20)
    digit = 0
   paste0(round(x, digit), "%")
}

#' stacked barplot status
#'
#' @param df data.frame
#' @param percent logical to make the y axis in percent
#' @param labsize numeric, x axis label size
#' @param labangle numeric, x axis label angle
#'
#' @import ggplot2
#'
#' @return ggplot plot
#' @export
stackedbarplot_plot <- function(df, percent = TRUE, labsize = 8.5, labangle = 30) {
  if (nrow(df) == 0) {
    p = ggplot()
    return(p)
  }
  suffix = NULL
  if (percent) {
    df$ratio.over.cases <- 100*df$ratio.over.cases
    suffix = "%"
  }
  if (length(unique(df$Country.Region)) > 25) {
    labsize = labsize - min(length(unique(df$Country.Region))/20-1,3.2)
    labangle = labangle + min(length(unique(df$Country.Region))-25,30)
  }
  p <- df %>%
    ggplot(aes(x = Country.Region, y = ratio.over.cases, fill = status,
               text = paste0("Percentage: ", round(ratio.over.cases, 1), suffix,"</br>",
               label = paste("Count: ",
                             formatC(countstatus, format = "f", big.mark = "'", digits  = 0))))) +
    basic_plot_theme() +
    geom_col(position = position_stack(reverse = TRUE)) +
    theme(
      axis.text.x = element_text(angle = labangle, size = labsize)
    )

  .ylabfun = ifelse(percent, lab_percent, lab_num)
  p <- p + scale_y_continuous(labels = .ylabfun, n.breaks = .breaks.yaxis) # add label

  # p = p %>%
  #   fix_colors()
  p
}

#' Time evolution as line plot
#'
#' @rdname time_evol_line_plot
#'
#' @param df data.frame with column called Date and x column to plot
#' @param log logical for applying log scale
#' @param text element for tooltip
#' @param g_palette character vector of colors for the graph and legend
#' @param formatdate character a valid date format, used to remove years, default dd-mm-yyyy
#'
#' @return line plot of given variable by date
#'
#' @import ggplot2
#' @importFrom  dplyr mutate
#' @importFrom  dplyr case_when
#'
#' @examples
#' \dontrun{
#' df <- get_timeseries_full_data() %>%
#' get_timeseries_by_contagion_day_data %>%
#' get_timeseries_global_data()
#'
#' df <- df %>%
#' pivot_longer(cols = -date, names_to = "status", values_to = "value") %>%
#'  mutate(status = as.factor(status)) %>%
#'  capitalize_names_df()
#'
#' df %>% time_evol_line_plot()
#' }
#'
#' \dontrun{
#' country_tot <- get_timeseries_full_data() %>%
#'  get_timeseries_by_contagion_day_data %>%
#'  aggregate_country_data() %>%
#'  top_n(10)
#'
#' df <- get_timeseries_full_data() %>%
#'  get_timeseries_by_contagion_day_data() %>%
#'  aggregate_province_timeseries_data() %>%
#'  filter(Country.Region %in% country_tot$Country.Region) %>%
#'  select(Country.Region, date, confirmed)
#'
#'  df <- df %>%
#'  mutate(status = as.factor(Country.Region)) %>%
#'  mutate(value = confirmed) %>%
#'  capitalize_names_df()
#'
#' df %>% time_evol_line_plot()
#' }
#'
#' @export
time_evol_line_plot <- function(df, log = FALSE, text = "", g_palette = graph_palette, formatdate = "%d-%m-%y") {
  if (log) {
    df <- df %>%
      mutate(Value = case_when(
        Value == 0 ~ 1,
        TRUE ~ Value
      ))
  }

  #df$statuslabel = factor(names(varsNames(df$Status)), levels = names(varsNames(levels(df$Status))))

  .popuptext = function(Date, Value, status, txt = text){
    paste(
      paste("Date",": ",Date,"<br>"),
      paste("Value",": ",funformat(Value,FALSE),"<br>"),
      paste(txt,": ",status,"<br>"),
      sep = ""
    )
  }
  p <- ggplot(df, aes(x = Date, y = Value, colour = Status, group = Status,
                      #text = paste0(text, ": ", Status)#, label = funformat(Value,FALSE))
                      text= .popuptext(Date, Value, Status)
                      )) +
    geom_line() +
    basic_plot_theme() +
    scale_color_manual(values = g_palette) +
    scale_x_date(breaks = breaks_lab(df$Date, .breaks.xaxis),
                 date_minor_breaks = "1 week",
                 #limits = range(df$Date, na.rm = TRUE),
                 date_labels = formatdate) +
    scale_y_continuous(labels = lab_num, breaks = breaks_lab(df$Value, .breaks.yaxis)) +

    # theme(
    #   axis.text.x = element_text(angle = 45, hjust = 1)
    # )

  if (log) {
    p <- p %>%
      add_log_scale()
  }

  p
}


#' Time evolution as area plot
#'
#' @rdname time_evol_area_plot
#'
#' @param df data.frame with column called Date and x column to plot
#' @param stack logical for producing a stacked plot
#' @param log logical for applying log scale
#' @param text element for tooltip
#' @param hosp logical, if TRUE hosp variables are in status. Default FALSE
#' @param active_hosp logical, if TRUE hosp and active are in status, active to be adjusted. Default FALSE
#' @param formatdate character a valid date format, used to remove years, default dd-mm-yyyy
#'
#' @return area plot of given variable by date
#'
#' @import ggplot2
#' @importFrom  dplyr group_by
#' @importFrom  dplyr ungroup
#' @importFrom  dplyr arrange
#' @importFrom  dplyr desc
#' @importFrom  dplyr mutate
#' @importFrom  dplyr case_when
#'
#' @examples
#' \dontrun{
#' df <- get_timeseries_full_data() %>%
#' get_timeseries_by_contagion_day_data %>%
#' get_timeseries_global_data()
#'
#' df <- df %>%
#' pivot_longer(cols = -date, names_to = "status", values_to = "value") %>%
#'  mutate(status = as.factor(status)) %>%
#'  capitalize_names_df()
#'
#' df %>% time_evol_line_plot()
#' }
#'
#' \dontrun{
#' country_tot <- get_timeseries_full_data() %>%
#'  get_timeseries_by_contagion_day_data %>%
#'  aggregate_country_data() %>%
#'  top_n(10)
#'
#' df <- get_timeseries_full_data() %>%
#'  get_timeseries_by_contagion_day_data() %>%
#'  aggregate_province_timeseries_data() %>%
#'  filter(Country.Region %in% country_tot$Country.Region) %>%
#'  select(Country.Region, date, confirmed)
#'
#'  df <- df %>%
#'  mutate(status = as.factor(Country.Region)) %>%
#'  mutate(value = confirmed) %>%
#'  capitalize_names_df()
#'
#' df %>% time_evol_line_plot()
#' }
#'
#' @export
time_evol_area_plot <- function(df, stack = F, log = F, text = "", hosp = FALSE, active_hosp, formatdate = "%d-%m-%y") {

  # remove days with all 0s

  if (stack) {
    if (hosp) {
      if (sum(df$Status == "icuvent") > 0)
        df$Value[df$Status == "hosp"] = pmax(df$Value[df$Status == "hosp"] -  df$Value[df$Status == "icuvent"], 0, na.rm = TRUE)
    }
    if (active_hosp) {
      if (sum(df$Status == "hosp") > 0)
        df$Value[df$Status == "active"] = pmax(df$Value[df$Status == "active"] -  df$Value[df$Status == "hosp"], 0, na.rm = TRUE)
    }

    df <- df %>%
      arrange(desc(Status)) %>%
      group_by(Date) %>%
      mutate(ValueMax = cumsum(Value), ValueMin = dplyr::lag(ValueMax, default = 0)) %>%
      ungroup()
  } else {
    df <- df %>%
      mutate(ValueMax = Value, ValueMin = 0)
  }

  if (log) {
    # fix 0s and use lead/lag to make sure there are no gaps
    df <- df %>%
      arrange(Date) %>%
      group_by(Status) %>%
      mutate(
        ValueMax = case_when(
          ValueMax == 0 & dplyr::lag(ValueMax, 1, 0) > 1 ~ 1,
          ValueMax == 0 & dplyr::lead(ValueMax, 1, 0) > 1 ~ 1,
          ValueMax == 0 ~ NA_real_,
          TRUE ~ ValueMax
        ),
        ValueMin = case_when(
          ValueMin = 0 & dplyr::lead(ValueMin, 1, 0) > 1 ~ 1,
          ValueMin = 0 & dplyr::lag(ValueMin, 1, 0) > 1 ~ 1,
          is.na(ValueMax) ~ NA_real_,
          TRUE ~ pmax(1L, ValueMin)
        )
      ) %>%
      ungroup()
  }
  ylim = range(df[, c("ValueMin", "ValueMax")], na.rm = TRUE)
  df$statuslabel = factor(names(varsNames(df$Status)), levels = names(varsNames(levels(df$Status))))

  .popuptext = function(Date, Value, status, txt = text){
    paste(
      paste("Date",": ",Date,"<br>"),
      paste("Value",": ",funformat(Value,FALSE),"<br>"),
      paste(txt,": ", as.character(status),"<br>"),
      sep = ""
    )
  }

  # x.d.lim = range(df$Date)
  # x.d.breaks = seq(x.d.lim[1],x.d.lim[2], length.out = .breaks.xaxis) # changed to 6 like th other timeline plots
  p <- ggplot(df, aes(x = Date, y = Value, group = statuslabel, #label = paste("Value:",funformat(Value,FALSE)),
                      #text = paste0(text, ": ", statuslabel)
                      text= .popuptext(Date, Value, statuslabel)
              )) +
    geom_ribbon(aes(ymin = ValueMin, ymax = ValueMax, colour = statuslabel, fill = statuslabel), alpha = 0.5, position = 'identity') +

    # shall we instead go for a step-area done with a (wide) barplot? This would reflect the integer nature of the data
    # geom_crossbar(aes(ymin = ValueMin, ymax = ValueMax, colour = Status, fill = Status, width = 1.1), size = 0, alpha = 1, position = 'identity') +
    basic_plot_theme() +
    #scale_x_date(date_breaks = "2 weeks", date_minor_breaks = "1 day", date_labels = "%d-%m") +
    scale_x_date(breaks = breaks_lab(df$Date, .breaks.xaxis),
                 date_minor_breaks = "1 week", #limits = range(df$Date, na.rm = TRUE),
                  date_labels = formatdate) +
    scale_y_continuous(labels = lab_num, breaks = breaks_lab(ylim, .breaks.yaxis)) #+ # add label

    # theme(
    #   axis.text.x = element_text(angle = 45, hjust = 1)
    # )
  p <- p %>%
    fix_colors(labs = TRUE, hosp = hosp)

  if (log) {
    p <- p %>%
      add_log_scale()
  }
  if (stack) {
    if (hosp) {
      # adjust hosp data here
      if (sum(p$data$Status == "icuvent")>1)
        p$data$Value[p$data$Status == "hosp"] =
                    p$data$Value[p$data$Status == "hosp"] +  p$data$Value[p$data$Status == "icuvent"]

    }
    if (active_hosp) {
      if (sum(p$data$Status == "hosp")>1)
        p$data$Value[p$data$Status == "active"] =
                    p$data$Value[p$data$Status == "active"] +  p$data$Value[p$data$Status == "hosp"]
    }
  }
  p
}

#' Time evolution as area plot facet
#'
#' @rdname time_evol_line_facet_plot
#'
#' @param df data.frame with column called Date and x column to plot
#' @param log character string log or linear
#' @param g_palette character vector of colors for the graph and legend
#' @param formatdate character a valid date format, used to remove years, default dd-mm-yyyy
#' @return area plot by date
#'
#' @import ggplot2
#' @importFrom scales label_number
#'
#' @export
time_evol_line_facet_plot <- function(df, log, g_palette = graph_palette, formatdate = "%d-%m-%y") {

  if (log == "log") {
    df <- df %>%
      mutate(value = ifelse(value == 0, NA, value))
  }
  dfmindate = df %>% #filter(bool_new == "new") %>%
    filter(value >0) %>%
    ungroup() %>%
    group_by(status) %>%
    summarize(mindate = min(date)-1) %>% # remove one day
    ungroup()

  dfnames = names(df)
  df = df %>% left_join(select(dfmindate,status, mindate), by = "status") %>%
    filter(date >= mindate) %>%
    select(-mindate)

  # x.d.lim = range(df$date)
  # x.d.breaks = seq(x.d.lim[1],x.d.lim[2], length.out = .breaks.xaxis)
  p <-  ggplot(df, aes(x = date, y = value)) +
    geom_line(aes(colour = Country.Region), size = 1.7) + # size must be specified again being facet it is smaller
    #geom_line(aes(colour = Country.Region)) +

    # geom_area(aes(colour = Country.Region, fill = Country.Region), size = 1, alpha = 0.5, position = 'dodge') +
    basic_plot_theme() +
    #theme(panel.background = element_rect(fill = backgroud_map_col))+ # set grey background
    scale_color_manual(values = g_palette) #+# add label
    #scale_x_date(date_breaks = "1 week", date_minor_breaks = "1 day", date_labels = "%d-%m") +
    # scale_x_date(#breaks = x.d.breaks,
    #              date_minor_breaks = "1 week", #limits = x.d.lim,
    #              date_labels = "%d-%m") +
    # theme(
    #   axis.text.x = element_text(angle = 45, hjust = 1)
    # )

  p <- p %>%
    fix_legend_position()

  if (log == "log") {
    p <- p %>%
      add_log_scale()
  }

  p <- p +
    facet_wrap( ~ status, scales = "free", nrow = 1, ncol = 4) +
    theme(strip.text = element_text(colour = 'white'))  +
    scale_y_continuous(labels = lab_num, breaks = breaks_lab)

  p <- p + scale_x_date(breaks = breaks_lab,
    date_minor_breaks = "1 week", #limits = x.d.lim,
    date_labels = formatdate) +
    # theme(
    #   axis.text.x = element_text(angle = 45, hjust = 1)
    # )
    basic_plot_theme()

  # color top strip based on status
  # reference: https://github.com/tidyverse/ggplot2/issues/2096
  g <- ggplot_gtable(ggplot_build(p))
  strip_both <- which(grepl('strip-', g$layout$name))
  fills <- .case_colors[levels(p$data$status)]
  k <- 1
  for (i in strip_both) {
    j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
    g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
    k <- k + 1
  }
  grid.draw(g)

}

#' Transform y to log scale
#' @rdname add_log_scale
#'
#' @param p ggplot object
#'
#' @import ggplot2
#'
#' @return p ggplot object
#'
#' @export
add_log_scale <- function(p){
  p <- p + scale_y_log10()  #scale_y_continuous(trans = 'log10')
  p
}

#' Remove legend
#' @rdname remove_legend
#'
#' @param p ggplot object
#'
#' @import ggplot2
#'
#' @return p ggplot object
#'
#' @export
remove_legend <- function(p){
  p <- p + theme(legend.position = "none")
  p
}

#' Evolution from contagion day
#' @rdname from_contagion_day_bar_plot
#'
#' @param df data.frame with column called Contagion_day and x column to plot
#'
#' @return barplot of given variable by contagion
#'
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' df <- get_timeseries_full_data() %>%
#'  get_timeseries_by_contagion_day_data() %>%
#'  aggregate_province_timeseries_data() %>%
#'  filter(Country.Region == "Italy")
#'  select(Country.Region, contagion_day, confirmed)
#'
#'  df <- df %>%
#'  mutate(status = as.factor(Country.Region)) %>%
#'  mutate(value = confirmed) %>%
#'  capitalize_names_df()
#'
#' df %>% from_contagion_day_bar_plot()
#' }
#'
#' @return p ggplot object
#'
#' @export
from_contagion_day_bar_plot <- function(df){
  p <- ggplot(df, aes(x = `Contagion Day`, y = Value, fill = Status)) +
    geom_bar(stat = "identity") +
    basic_plot_theme()
  p
}

#' Evolution from contagion day facet
#'
#' @param df data.frame to plot
#' @param xdate character variable for x axis
#' @param formatdate character a valid date format, used to remove years, default dd-mm-yyyy
#'
#' @return barplot facet
#'
#' @import ggplot2
#' @importFrom grid grid.draw
#'
#' @export
from_contagion_day_bar_facet_plot <- function(df, xdate = "date", formatdate = "%d-%m-%y"){
  df$Date = df[[xdate]]
  # dfmindate = df %>% filter(bool_new == "new") %>%
  #   filter(value >0) %>%
  #   group_by(status) %>%
  #   mutate(mindate = min(date)-1) %>% # remove one day
  #   filter(date == mindate) %>%
  #   ungroup()

  dfmindate = df %>% filter(bool_new == "new") %>%
    filter(value >0) %>%
    ungroup() %>%
    group_by(status) %>%
    summarize(mindate = min(date)-1) %>% # remove one day
    ungroup()

  dfnames = names(df)
  df = df %>% left_join(select(dfmindate,status, mindate), by = "status") %>%
    filter(date >= mindate) %>%
    select(-mindate)
  p <- ggplot(df, aes(x = Date, y = value, fill = bool_new)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = new_total_colors) + #c("total" = "#C8C8C8", "new" = "#ea8b5b")) +
    basic_plot_theme() +
    facet_wrap( ~ status, scales = "free", nrow = 1, ncol = 4) +
    theme(strip.text = element_text(colour = 'white')) +
    scale_y_continuous(labels = lab_num, breaks = breaks_lab) # add label

  if (xdate == "date") {
    # date x axis
    # x.d.lim = range(df$Date)
    # x.d.breaks = seq(x.d.lim[1],x.d.lim[2], length.out = .breaks.xaxis)
    p <- p + scale_x_date(breaks = breaks_lab,
                          date_minor_breaks = "1 week", #limits = x.d.lim,
                          date_labels = formatdate) #+
      # theme(
      #   axis.text.x = element_text(angle = 45, hjust = 1)
      # )
  } else {
    p <- p + scale_x_discrete(breaks = breaks_lab) # add label
  }


  p <- p %>%
    fix_legend_position()
  # color top strip based on status
  # reference: https://github.com/tidyverse/ggplot2/issues/2096
  g <- ggplot_gtable(ggplot_build(p))
  strip_both <- which(grepl('strip-', g$layout$name))
  fills <- .case_colors[levels(p$data$status)]
  k <- 1
  for (i in strip_both) {
    j <- which(grepl('rect', g$grobs[[i]]$grobs[[1]]$childrenOrder))
    g$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
    k <- k + 1
  }
  grid.draw(g)
}

#' Evolution by_date
#' @rdname date_bar_plot
#'
#' @param df data.frame with column called Date and x column to plot
#'
#' @return barplot of given variable by contagion
#'
#' @import ggplot2
#'
#' @examples
#' \dontrun{
#' df <- get_timeseries_full_data() %>%
#'  get_timeseries_by_contagion_day_data() %>%
#'  aggregate_province_timeseries_data() %>%
#'  filter(Country.Region == "Italy")
#'  select(Country.Region, date, confirmed)
#'
#'  df <- df %>%
#'  mutate(status = as.factor(Country.Region)) %>%
#'  mutate(value = confirmed) %>%
#'  capitalize_names_df()
#'
#' df %>% date_bar_plot()
#' }
#'
#' @export
date_bar_plot <- function(df){
  p <- ggplot(df, aes(x = Date, y = Value, fill = Status)) +
    geom_bar(stat = "identity") +
    basic_plot_theme()
  p
}

#' Fix colors in plot
#' @rdname fix_colors
#'
#' @param p ggplot object
#' @param labs logical, if TRUE then variables labels are used
#' @param hosp logical, if TRUE then hosp colors are used
#'
#' @import ggplot2
#'
#' @return p ggplot object
#'
#' @export
fix_colors <- function(p, labs = FALSE, hosp = FALSE) {

  if (hosp) {
    varcolors = .hosp_colors[.hosp_vars]
  } else {
    varcolors = .case_colors
  }
  varlabs = c(names(varcolors),prefix_var(names(varcolors), c("lw","new")))
  cc_vect  = rep(varcolors, times = 3)
  names(cc_vect) = varlabs
  if (labs) {
    names(cc_vect) = names(varsNames(names(cc_vect)))
  }

  p <- p +
    suppressWarnings(scale_color_manual(values = c(cc_vect))) +
    suppressWarnings(scale_fill_manual(values = c(cc_vect)))
  p
}

#' Fix legend position
#' @rdname fix_legend_position
#'
#' @param p ggplot object
#'
#' @import ggplot2
#'
#' @return p ggplot object
#'
#' @export
fix_legend_position <- function(p){
  p <- p +
    # adjust legend position ref: https://stackoverflow.com/questions/7270900/position-legend-in-first-plot-of-facet
    theme(legend.position = "bottom",
          legend.box = "horizontal",
          legend.background = element_rect(fill = "white", colour = NA))
  p
}

#' Format in ggplotly
#' @param x numeric vector of data
#' @param perc logical data are percentage if TRUE
#' @param digits numeric, digits for the case where perc = TRUE
#'
#' @return x formatted x
funformat <- function(x, perc, digits = NULL) {

  if (!perc) {
    maxy = max(x, na.rm = T)
    minxy = min(x, na.rm = T)
    dg = nchar(as.character(round(max(abs(minxy),maxy))))
    dglab = getdg_lab(dg, maxy, minxy)
    formatC(roundlab(x), format = "f", big.mark = "'", digits = dglab
            #ifelse(is.null(digits), dglab, digits)
            )
    # use label_number
  }
  else
    paste0( round(x,ifelse(is.null(digits), 3, digits)), "%")
}

#' plot all countries but highlight first 10
#'
#' @param df data.frame with column called Date and Value column to plot
#' @param log logical for applying log scale
#' @param text element for tooltip
#' @param percent logical to make the y axis in percent
#' @param date_x logical to convert x-axis labels to dates
#' @param g_palette character vector of colors for the graph and legend
#' @param rollw logical, if TRUE then rolling Weekly averages are computed
#' @param barplot logical, if TRUE barplot of Points is done
#' @param secondline character, variable name to be added with new line and axis
#' @param keeporder logical, if TRUE then lines order is kept according to df
#' @param formatdate character a valid date format, used to remove years, default dd-mm-yyyy
#'
#' @note secondline argument not working, ggplotly removes secons axis. Not being used at the moment.
#'
#' @import ggplot2
#' @import zoo
#' @importFrom scales label_number
#'
#' @export
plot_all_highlight <- function(df, log = FALSE, text = "", percent =  FALSE, date_x = FALSE, g_palette = graph_palette, rollw = TRUE , barplot = FALSE, secondline = NULL, keeporder = FALSE, formatdate = "%d-%m-%y") {
  if (nrow(df) == 0) {
    p = ggplot()
    return(p)
  }

  #clean df for log case
  if (log) {
    df <- df %>%
      mutate(Value = case_when(
        Value == 0 ~ 1,
        TRUE ~ Value
      ))
  }
  # if percentage, multiply by 100
  if (percent) {
    #df$Points <- 100*df$Points
    df[, names(df) %in% c("WeeklyAvgVal", "Points")] <- 100 * df[, names(df) %in% c("WeeklyAvgVal", "Points")]
  }
  #df_highlight = df

  #TODO y_tooltip should be wrapped with gentext(Value), not so nice below, it does not seem to work
  # df = df %>% rename(Variable = Value)
  # df_highlight = df_highlight %>% rename(Variable = Value)
  #
  # df$Value = gen_text(df$Variable)
  # df_highlight$Value = gen_text(df_highlight$Variable)
  varChoice = ifelse(rollw, "WeeklyAvgVal", "Points")
  #df$ValueVar = funformat(df[[varChoice]], percent) # not working

  if (keeporder)
    df$Status = factor(df$Status , levels = unique(df$Status ))

  .ylabfun = ifelse(percent, lab_percent, lab_num)

  if (rollw)
    df$WeeklyAvg = funformat(df$WeeklyAvgVal, percent)
  #df$Points= df$Value
  df$Value= funformat(df$Points, percent)

  .popuptext = function(Status, Date, Value, WeeklyAvg, txt = text){
    txt = paste(
      paste("Date",": ",Date,"<br>"),
      paste(txt,": ",Status,"<br>"),
      paste("Value",": ",Value,"<br>"),
      sep = ""
    )
    if (!missing(WeeklyAvg))
      txt = paste(txt, paste("WeeklyAvg",": ",WeeklyAvg,"<br>"), sep = "")
    txt
  }
  #df$labs =  .popuptext(df$Status, df$WeeklyAvg, df$Value)
  # p <- ggplot(df, aes(x = Date, y = !!sym(varChoice), group = Status,
  #                     colour = Status, text = .popuptext(Status, Value, WeeklyAvg)
  #                     ))
  # p +
  #   geom_line() +
  #   basic_plot_theme() +
  #   #noaxislab_theme() +
  #   theme(panel.background = element_rect(fill = backgroud_map_col))+ # set grey background
  #   scale_color_manual(values = g_palette) +
  #   scale_y_continuous(labels = .ylabfun, breaks = breaks_lab(df[[varChoice]], .breaks.yaxis))


  if (barplot) {
    varChoice = "Points"
    if (rollw)
      p <- ggplot(df, aes(x = Date, y = Points, colour = Status, group = Status, text = .popuptext(Status, Date, Value, WeeklyAvg)))
    else
      p <- ggplot(df, aes(x = Date, y = Points, colour = Status, group = Status, text = .popuptext(Status, Date, Value)))


    p = p + geom_bar(aes(x = Date, y = Points, colour = Status), stat = "identity")
    if (rollw)
      p = p +
            geom_line(aes(x = Date, y = WeeklyAvgVal, colour = Status) , size = 1.25)
    p = p+
      basic_plot_theme() +
      #noaxislab_theme() +
      theme(panel.background = element_rect(fill = backgroud_map_col))+ # set grey background
      scale_color_manual(values = g_palette) +
      scale_y_continuous(labels = .ylabfun, breaks = breaks_lab(c(0,df[[varChoice]]), .breaks.yaxis))

  } else{
    if (rollw)
      p <- ggplot(df, aes(x = Date, y = !!sym(varChoice), colour = Status, group = Status, text = .popuptext(Status, Date, Value, WeeklyAvg)))
    else
      p <- ggplot(df, aes(x = Date, y = !!sym(varChoice), colour = Status, group = Status, text = .popuptext(Status, Date, Value)))

   p = p +
      geom_line() +
      basic_plot_theme() +
      #noaxislab_theme() +
      theme(panel.background = element_rect(fill = backgroud_map_col))+ # set grey background
      scale_color_manual(values = g_palette) +
      scale_y_continuous(labels = .ylabfun, breaks = breaks_lab(df[[varChoice]], .breaks.yaxis))
  }

  # add label

  if (log) {
    p <- p %>%
      add_log_scale()
  }

  if (FALSE && !is.null(secondline)) {
    message("add  second line ", secondline)
    # rescale by value
    # review, should be rescaled by range values, 0-100
    ratiorescale = max(p$data[[varChoice]], na.rm = TRUE) / max(p$data[[secondline]], na.rm = TRUE)
    #p$data[[secondline]] = p$data[[secondline]] * ratiorescale

    p <- p +
      geom_line(aes(y = !!sym(secondline)), linetype = "dashed") +
      #basic_plot_theme() +
      scale_y_continuous(
        n.breaks = .breaks.yaxis,
        # Add a second axis and specify its features
        #label_number(big.mark = "'"),
        sec.axis = sec_axis( trans= function(x){ x / ratiorescale},
                             name = names(varsNames(secondline)))
      ) +
      secondline_theme()
    #p$data[[secondline]] = p$data[[secondline]] / ratiorescale

  }
  if (date_x) { # mutate x axis to a date format

    p <- p +
      scale_x_date(breaks = breaks_lab(df$Date, .breaks.xaxis),
                   #limits = range(df$Date, na.rm = TRUE),
                   date_minor_breaks = "1 week",
                   date_labels = formatdate) #+
  }
  if (length(unique(df$Status)) == 1) {
    p = p+
      theme(legend.position = "none")
  }

  p
}


#' plot rate as hist
#'
#' @param df data.frame
#' @param percent logical to make the y axis in percent
#' @param y_min min value on y axis
#' @param g_palette character vector of colors for the graph and legend
#' @param labsize numeric, x axis label size
#' @param labangle numeric, x axis label angle
#'
#' @import ggplot2
#' @importFrom plotly style ggplotly layout
#'
#' @return ggplot plot
#' @export
plot_rate_hist <- function(df, percent =  FALSE, y_min = 0, g_palette, labsize = 8.5, labangle = 30) {
  if (nrow(df) == 0 || all(is.na(df$Value))) {
    p = ggplot()
    return(p)
  }
  if (percent) {
    df$Value <- 100*df$Value
  }
  if(!any(df$Country %in% names(g_palette))){
    pal = as.character(g_palette) # if normal case
  } else {
    pal = g_palette[as.character(df$Country)] # if palette given per country
  }

  if (length(unique(df$Country)) > 16) {
    labsize = labsize - min(length(unique(df$Country))/14-1,3.2)
    labangle = labangle + min(length(unique(df$Country))-16,30)
  }
  minv = min(df$Value, na.rm = TRUE)
  rangemin = y_min + diff(range(c(y_min,minv)))/2
  ylim_bottom = ifelse(minv < 0 , minv*1.05, rangemin)
  y_min = c(ifelse(percent, rangemin, ylim_bottom))

  ylim = c(y_min, max(df$Value, na.rm = TRUE)*1.05)

  .popuptext = function(asofdate,  xvarexpr, percent, digits = NULL){
    paste(
      paste("AsOfDate: ",asofdate,"<br>"),
      paste0("Value: ",funformat(xvarexpr, percent, digits), "<br>"),
      sep = ""
    )
  }

  avgVal = mean(df$Value)
  # df$text = .popuptext(df$AsOfDate, df$Value, percent)
  # df$ValText = funformat(df$Value, percent, switch(percent, NULL, 2))
  #
  p <- ggplot(df, aes(x = Country, y = Value, group = 1,
                        text = .popuptext(AsOfDate,Value,percent, switch(percent, NULL, 2)))) +
    geom_bar(stat = "identity", fill = pal) +
    basic_plot_theme() +
    theme(panel.background = element_rect(fill = backgroud_map_col))+ # set grey background
    coord_cartesian(ylim = ylim) +
    theme(
      axis.text.x = element_text(angle = labangle, size = labsize)
    )
  labfun = ifelse(percent, lab_percent, lab_num)
  p <- p + scale_y_continuous(labels = labfun, breaks = breaks_lab(ylim, .breaks.yaxis)) #scale_y_continuous(labels = scales::label_percent(accuracy = 1))#scale_y_continuous(labels = scales::percent_format(accuracy = 1))

  deltaIncr = diff(ylim) / 40
  p = p +
    annotate("segment", x = 0.5, xend= nrow(df)+0.5, y = avgVal, yend = avgVal, linetype = "dotted", size = 0.3) +
    annotate("text", x = nrow(df), y = avgVal + deltaIncr, label = "Avg", size = 1.5,group = 3, hjust = 1)  +
    annotate("segment", x = 0.5, xend= nrow(df)+0.5, y = 0, yend = 0, size = 0.15)

  if(any(df$Value != 0)) {
    traces = 2:4
  } else {
    traces = 2:3
  }

  if (length(unique(df$Country)) < 15 && any(df$Value != 0)) {# add text if there is space, not working due to plotly
    p = p +
        annotate("text", x = df$Country, y = df$Value + deltaIncr, label = funformat(df$Value, percent, switch(percent, NULL, 2)), size = 2.2, vjust = -1, group = 2)
    traces = c(2,traces+1)
  }
  if(length(pal)>1)
    traces = seq(length(pal)+1, length(pal)+max(traces)-1)
  # p = p +
  #   geom_text(label = funformat(df$Value, percent, switch(percent, NULL, 2)), size = 2.25, position = position_dodge(width = 0.8), vjust = -2)
  pply = p %>% plotly::ggplotly(tooltip = c("x","text"),
                                layerData = 1,
                               #textposition = 'outside'
                               originalData = FALSE
                               ) %>%
    plotly::style(hoverinfo = "skip", traces = traces) %>%
    plotly::layout(title = "",
                        hovermode = 'closest', clickmode = "event", showlegend = FALSE,
                   yaxis = list(fixedrange = TRUE),
                   xaxis = list(fixedrange = TRUE))
  pply
}

#' scatter plot between prevalence and growth rate
#'
#' @param df data.frame
#' @param med list with median values for x and y
#' @param x.min numeric adjustment for Cartesian x axis
#' @param y.min numeric adjustment for Cartesian y axis
#' @param xvar character variable name for x axis
#' @param yvar character variable name for y axis
#' @param coefflm numeric, intercept and slop of simple lm model
#' @param addLabels logical,if TRUE then labels are added to points
#'
#' @import ggplot2
#' @importFrom scales label_number
#'
#' @return ggplot plot
scatter_plot <- function(df, med, x.min = c(0.9, 1.1), y.min = c(0.99,1.01), xvar = "confirmed_rate_1M_pop", yvar = "growth_factor_3", coefflm = NULL, addLabels = TRUE) {

  if (nrow(df) == 0) {
    p = ggplot()
    return(p)
  }

  if (grepl("confirmed", xvar) && grepl("^growth", yvar)) {
    color_cases = c("yellow3", "darkgreen", "#dd4b39", "#E69F00")
  } else if (grepl("stringency", xvar)) {
    color_cases = c("#3c8dbc","darkgreen", "gray3","#dd4b39")
  } else if (grepl("vaccines", xvar)) {
    color_cases = c("darkgreen","deepskyblue1", "deepskyblue4","gray3")
  } else if (grepl("confirmed", xvar) && grepl("^hosp", yvar)) {
    color_cases = c("deepskyblue1","darkgreen", "gray3","deepskyblue4")
  } else
    color_cases = c("#dd4b39","darkgreen", "gray3","#3c8dbc")

  percenty = ifelse(yvar %in% .rate_vars, TRUE, FALSE)
  percentx = ifelse(xvar %in% .rate_vars, TRUE, FALSE)

  if(percentx) {
    df[[xvar]] = 100 * df[[xvar]]
    med$x = 100* med$x
  }
  if(percenty) {
    df[[yvar]] = 100 * df[[yvar]]
    med$y = 100* med$y
  }

  color_cntry = rep(color_cases[1], nrow(df))
  color_cntry[df[[xvar]] < med$x & df[[yvar]] < med$y ] = color_cases[2]
  color_cntry[df[[xvar]] >= med$x & df[[yvar]] >= med$y ] = color_cases[3]
  color_cntry[df[[xvar]] < med$x & df[[yvar]] >= med$y ] = color_cases[4]

  xlim =  c(min(df[[xvar]],med$x, na.rm = TRUE)- diff(range(df[[xvar]],med$x, na.rm = TRUE))*(1-x.min[1]),
            max(df[[xvar]],med$x, na.rm = TRUE)*x.min[2])
  ylimtop = max(df[[yvar]], med$y, na.rm = TRUE)

 # ylimbot = min(1, df[[yvar]],med$y)
  ylimbot = min(df[[yvar]],med$y, na.rm = TRUE)- diff(range(df[[yvar]],med$y, na.rm = TRUE))*(1-y.min[1])

  ylim = c(ylimbot-diff(c(ylimbot,ylimtop))*(1-y.min[1]), ylimtop + diff(c(ylimbot,ylimtop))*(y.min[2]-1))

  #accy = ifelse(diff(ylim)<0.05, 0.001, 0.01)


  .popuptext = function(area, asofdate, yvarexpr, xvarexpr){
    paste(
      paste("Area: ",area,"<br>"),
      paste("AsOfDate: ",asofdate,"<br>"),
      paste(names(varsNames(yvar)) ,funformat(yvarexpr, percenty),"<br>"),
      paste(names(varsNames(xvar)), funformat(xvarexpr, percentx), "<br>"),
      sep = ""
    )
  }
  p <- ggplot(df, aes(x = !! sym(xvar), y = !! sym(yvar),
                      text = .popuptext(Country.Region, AsOfDate, !! sym(yvar), !! sym(xvar)),
                      group = 1
                      #text = .popuptext,
                      #text = paste(names(varsNames("confirmed_rate_1M_pop")), formatC(confirmed_rate_1M_pop, format = "f", big.mark = "'", digits  = 1), "</br>")
  )) +
    labs(x= paste("(x)",names(varsNames(xvar))), y = paste("(y)",names(varsNames(xvar)))) +
    geom_point(color = color_cntry, size = 1.3)   +
    geom_vline(xintercept = med$x, colour = "darkblue", linetype="dotted", size = 0.3) +
    geom_hline(yintercept = med$y, colour = "darkblue", linetype="dotted", size = 0.3)

  if (!is.null(coefflm)) {
    p <- p +   geom_abline(intercept = coefflm[1], slope = coefflm[2], color = "grey", linetype="dashed", size = 0.8)
    #p <- p +   geom_line(aes(y = predlm, x = !! sym(xvar)), size = 1)
    #p <- p +   geom_smooth(method = "lm", se = FALSE)
  }
  if (addLabels) {
    p <- p +
      geom_text(aes(x = !! sym(xvar), y = !! sym(yvar), label= Country.Region),
                check_overlap = TRUE, color = color_cntry, size = 2.8)
  }
  p <- p +
    coord_cartesian(ylim = ylim,
                    xlim = xlim) +
    basic_plot_theme()


  labfuny = ifelse(percenty, lab_percent, lab_num)
  labfunx = ifelse(percentx, lab_percent, lab_num)
  p <- p + scale_y_continuous(labels = labfuny, breaks = breaks_lab(ylim, .breaks.yaxis)) +
            scale_x_continuous(labels = labfunx, breaks = breaks_lab(xlim, .breaks.xaxis)) # add label

  p
}

#' draw a blank plot with message
#'
#' @param where character where data are missing
#' @param what character variable name, "" if unknown
#' @param add character additional text message
#'
#' @import ggplot2
#'
#' @return ggplot plot
blank_plot <- function(where = "", what = "", add = "") {
  msg = paste0("  No ", what ,"data for ",where, ".", add)
  ggplot() +
    annotate("text", x = 0, y = 0.5, label = msg, size = 4, group = 1, hjust = 0.5)  +
    basic_plot_theme() +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.line.x = element_blank(),
      axis.line.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank()
    )
}

#' default palette for graph pages
#' @import RColorBrewer
graph_palette = c(brewer.pal(12, "Paired"), brewer.pal(8, "Set2"), brewer.pal(8, "Dark2"))

#' background color of maps
#'
backgroud_map_col = "grey90"

#' caption growth factor
#' @param growthvar character growth factor variable
#' @return character text for caption
caption_growth_factor_fun <- function(growthvar)
  #paste0("Growth Factor: total confirmed cases since ", gsub("growth_factor_", "", growthvar)  ," days ago. / total confirmed cases in previous 30 days")
  paste0("Growth Factor: total confirmed cases today / total confirmed cases ", gsub("growth_factor_", "", growthvar) ," days ago. (within last 2 months)")

#' caption death
#' @param var character growth factor variable
#' @return character text for caption
caption_death_fun <- function(var) {
  if (var %in% .rate_vars) {
    caption_radio <- "/ total confirmed cases as of today."
  } else {
    caption_radio <- "per 1 M population."
  }
  caption_death <- paste0("Computed as total deaths as of today ",caption_radio)
  caption_death
}

#' caption stringency index
#' @return character text for caption
caption_stringency <- function()
  "Stringency Lock-Down Index as of today: none (0-100) max"

#' caption prevalence
#' @return character text for caption
caption_prevalence <- function()
  "Prevalence:"

#' caption vaccination source
#' @return character text for caption
caption_source_vaccines <- function() {
  c("Source ourworldindata.org.")
}

#' caption vaccination
#' @return character text for caption
caption_vaccines <- function() {
  paste("Simple count of number of vaccines.", caption_source_vaccines(), collapse = " ")
}

#' caption tests
#' @param text character text of variable
#' @param area character text of area
#' @return character text for caption
caption_tests <- function(text = "Tests", area = "countries") {
  caption_tests1 <- paste("Over 1 Million people", text)
  caption_tests2 <- paste("Updated",text,"figures could be unavailable for some", area)
  c(caption_tests1, caption_tests2)
}

#' caption positive tests
#' @return character text for caption
caption_positive_tests <- function()
  "% of positive tests."

#' caption active
#' @return character text for caption
caption_active <- function() {
  caption_active <- "Active values can be biased by non reported recovered cases"
  caption_color <- "Yellow scale to represent negative active."
  c(caption_active, caption_color)
}

#' caption fitted values
#' @param col character color name
#' @param type character linetype
#' @return character text for caption
caption_fitted <- function(col = "Grey", type = "dashed")
  paste(col, type, "line: predicted values from Linear Model (y vs x)")

#' compute rolling Weekly average
#' @param x numeric vector of values
#' @param date Date vector of dates
#' @param days number of days over which average to be computed
#' @return x as partial rolling average
#'
rollAvg = function(x, date, days = 7) {
  # check if desc or asc
  if (date[1] > tail(date, 1)) {
    align = "left"
    x[cumsum(x) == 0] = NA # set to NA if it starts with 0s so that the average does not get computed
  } else {
    align = "right"
    x[rev(cumsum(rev(x)) == 0)] = NA # set to NA if it starts with 0s so that the average does not get computed
  }
  round(zoo::rollapplyr(x, days, mean, partial=TRUE, align = align),3)
}

