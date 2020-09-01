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
stackedbarplot_plot <- function(df, percent =  TRUE, labsize = 10, labangle = 30) {
  suffix = NULL
  if (percent) {
    df$ratio.over.cases <- 100*df$ratio.over.cases
    suffix = "%"
  }

  if (length(unique(df$Country)) > 30) {
    labsize = labsize - min(length(unique(df$Country))/30-1,2.8)
    labangle = labangle + min(length(unique(df$Country))-30,30)
  }

  p <- df %>%
    ggplot(aes(x = Country.Region, y = ratio.over.cases, fill = status,
               text = paste0("percentage: ", round(ratio.over.cases, 1), suffix,"</br>",
               label = paste("count: ",
                             formatC(countstatus, format = "f", big.mark = "'", digits  = 0)))))+
    basic_plot_theme() +
    geom_col(position = position_stack(reverse = TRUE)) +
    theme(
      axis.text.x = element_text(angle = labangle, size = labsize)
    )
  if (percent) {
    p <- p + scale_y_continuous(labels = function(x) paste0(x, "%"))
  }
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
time_evol_line_plot <- function(df, log = FALSE, text = "", g_palette = graph_palette) {
  if (log) {
    df <- df %>%
      mutate(Value = case_when(
        Value == 0 ~ 1,
        TRUE ~ Value
      ))
  }
  x.d.lim = range(df$Date)
  x.d.breaks = seq(x.d.lim[1],x.d.lim[2], length.out = 10)
  p <- ggplot(df, aes(x = Date, y = Value, colour = Status, text = paste0(text, ": ", Status))) +
    geom_line() +
    basic_plot_theme() +
    scale_color_manual(values = g_palette) +
    scale_x_date(breaks = x.d.breaks,
                 date_minor_breaks = "1 week",
                 limits = x.d.lim,
                 date_labels = "%d-%m") +
    scale_y_continuous(labels = label_number(big.mark = "'")) +
    # scale_x_date(date_breaks = "2 weeks", date_minor_breaks = "1 week", limits = range(df$Date),
    #              date_labels = "%d-%m") +
    theme(
      axis.text.x = element_text(angle = 45),
    )

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
time_evol_area_plot <- function(df, stack = F, log = F, text = "") {

  if (stack) {
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

  df$statuslabel = factor(names(varsNames(df$Status)), levels = names(varsNames(levels(df$Status))))

  x.d.lim = range(df$Date)
  x.d.breaks = seq(x.d.lim[1],x.d.lim[2], length.out = 10)

  p <- ggplot(df, aes(x = Date, y = Value,
                      text = paste0(text, ": ", statuslabel)
              )) +
    geom_ribbon(aes(ymin = ValueMin, ymax = ValueMax, colour = statuslabel, fill = statuslabel), alpha = 0.5, position = 'identity') +

    # shall we instead go for a step-area done with a (wide) barplot? This would reflect the integer nature of the data
    # geom_crossbar(aes(ymin = ValueMin, ymax = ValueMax, colour = Status, fill = Status, width = 1.1), size = 0, alpha = 1, position = 'identity') +
    basic_plot_theme() +
    #scale_x_date(date_breaks = "2 weeks", date_minor_breaks = "1 day", date_labels = "%d-%m") +
    scale_x_date(breaks = x.d.breaks,
                 date_minor_breaks = "1 week", limits = x.d.lim,
                  date_labels = "%d-%m") +
    scale_y_continuous(labels = label_number(big.mark = "'")) + # add label

    theme(
      axis.text.x = element_text(angle = 45)
    )

  p <- p %>%
    fix_colors(labs = TRUE)

  if (log) {
    p <- p %>%
      add_log_scale()
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
#'
#' @return area plot by date
#'
#' @import ggplot2
#' @importFrom scales label_number
#'
#' @export
time_evol_line_facet_plot <- function(df, log, g_palette = graph_palette) {

  if (log == "log") {
    df <- df %>%
      mutate(value = ifelse(value == 0, NA, value))
  }
  x.d.lim = range(df$date)
  x.d.breaks = seq(x.d.lim[1],x.d.lim[2], length.out = 10)
  p <-  ggplot(df, aes(x = date, y = value)) +
    geom_line(aes(colour = Country.Region), size = 1.7) + # size must be specified again being facet it is smaller
    #geom_line(aes(colour = Country.Region)) +

    # geom_area(aes(colour = Country.Region, fill = Country.Region), size = 1, alpha = 0.5, position = 'dodge') +
    basic_plot_theme() +
    theme(panel.background = element_rect(fill = backgroud_map_col))+ # set grey background
    scale_color_manual(values = g_palette) +
    scale_y_continuous(labels = label_number(big.mark = "'")) +# add label
    #scale_x_date(date_breaks = "1 week", date_minor_breaks = "1 day", date_labels = "%d-%m") +
    scale_x_date(breaks = x.d.breaks,
                 date_minor_breaks = "1 week", limits = x.d.lim,
                 date_labels = "%d-%m") +

    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
    )

  p <- p %>%
    fix_legend_position()

  if (log == "log") {
    p <- p %>%
      add_log_scale()
  }

  p <- p +
    facet_wrap( ~ status, scales = "free_y", nrow = 1, ncol = 4) +
    theme(strip.text = element_text(colour = 'white'))

  # color top strip based on status
  # reference: https://github.com/tidyverse/ggplot2/issues/2096
  g <- ggplot_gtable(ggplot_build(p))
  strip_both <- which(grepl('strip-', g$layout$name))
  fills <- case_colors
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
#'
#' @return barplot facet
#'
#' @import ggplot2
#' @importFrom grid grid.draw
#'
#' @export
from_contagion_day_bar_facet_plot <- function(df){
  p <- ggplot(df, aes(x = contagion_day, y = value, fill = bool_new)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = new_total_colors) + #c("total" = "#C8C8C8", "new" = "#ea8b5b")) +
    basic_plot_theme() +
    facet_wrap( ~ status, scales = "free_y", nrow = 1, ncol = 4) +
    theme(strip.text = element_text(colour = 'white'))

  p <- p %>%
    fix_legend_position()

  # color top strip based on status
  # reference: https://github.com/tidyverse/ggplot2/issues/2096
  g <- ggplot_gtable(ggplot_build(p))
  strip_both <- which(grepl('strip-', g$layout$name))
  fills <- case_colors
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

#' Fix colors
#' @rdname fix_colors
#'
#' @param p ggplot object
#' @param labs logical, if TRUE then variables labels arte used
#'
#' @import ggplot2
#'
#' @return p ggplot object
#'
#' @export
fix_colors <- function(p, labs = FALSE) {
  if (labs) {
    cc_vect = c(case_colors_labs(), case_colors_labs(prefix_case_colors(prefix = "lw")),
                case_colors_labs(prefix_case_colors(prefix = "new")))
  } else {
    cc_vect = c(case_colors, prefix_case_colors(prefix = "lw"), prefix_case_colors(prefix = "new"))
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


#' plot all countries but highlight first 10
#'
#' @param df data.frame with column called Date and Value column to plot
#' @param log logical for applying log scale
#' @param text element for tooltip
#' @param n_highligth number of elements to highlight
#' @param percent logical to make the y axis in percent
#' @param date_x logical to convert x-axis labels to dates
#' @param g_palette character vector of colors for the graph and legend
#'
#' @import ggplot2
#' @import zoo
#' @importFrom scales label_number
#'
#' @export
plot_all_highlight <- function(df, log = FALSE, text = "", n_highligth = 10, percent =  F, date_x = F, g_palette = graph_palette) {

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
    df$Value <- 100*df$Value
  }
  # df_highlight <- df %>%
  #   filter(as.integer(Status) < n_highligth + 1) #pick top n_highligth countries, using factor level (factor is ordered by decreasing Value)

  # rolling weekly average (#80), changed alignment to right
  df_highlight <- df %>% group_by(Status) %>%
    arrange(Date)  %>%
    mutate(Value = zoo::rollapplyr(Value, 7, mean, partial=TRUE, align = "right"))

  if (F) { # not used, legacy
    df_highlight_max <- df_highlight %>%
      group_by(Status) %>%
      filter(Value == max(Value)) %>%
      ungroup()
  }
  #TODO y_tooltip should be wrapped with gentext(Value), not so nice below, it does not seem to work
  # df = df %>% rename(Variable = Value)
  # df_highlight = df_highlight %>% rename(Variable = Value)
  #
  # df$Value = gen_text(df$Variable)
  # df_highlight$Value = gen_text(df_highlight$Variable)

  p <- ggplot(df, aes(x = Date, y = Value, colour = Status, text = paste0(text, ": ", Status), x_tooltip = Date, y_tooltip = Value)) +
    geom_line(data = df_highlight, aes(x = Date, y = Value, colour = Status)) +
    basic_plot_theme() +
    theme(panel.background = element_rect(fill = backgroud_map_col))+ # set grey background
    scale_color_manual(values = g_palette)

  if (percent) {
    p <- p + scale_y_continuous(labels = function(x) paste0(x, "%"))
  } else
    p <- p + scale_y_continuous(labels = label_number(big.mark = "'")) # add label

  if (log) {
    p <- p %>%
      add_log_scale()
  }

  if (date_x) { # mutate x axis to a date format
    x.d.lim = range(df$Date)
    x.d.breaks = seq(x.d.lim[1],x.d.lim[2], length.out = 10)

    p <- p +
      scale_x_date(breaks = x.d.breaks,
                   limits = x.d.lim,
                   date_minor_breaks = "1 week", date_labels = "%d-%m") +
      theme(
        axis.text.x = element_text(angle = 45)
      )
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
#'
#' @return ggplot plot
#' @export
plot_rate_hist <- function(df, percent =  FALSE, y_min = 0, g_palette, labsize = 10, labangle = 30) {
  if (percent) {
    df$Value <- 100*df$Value
  }
  if(!any(df$Country %in% names(g_palette))){
    pal = as.character(g_palette) # if normal case
  } else {
    pal = g_palette[as.character(df$Country)] # if palette given per country
  }

  if (length(unique(df$Country)) > 18) {
    labsize = labsize - min(length(unique(df$Country))/18-1,2.8)
    labangle = labangle + min(length(unique(df$Country))-18,30)
  }
  p <- ggplot(df, aes(x = Country, y = Value)) +
    geom_bar(stat = "identity", fill = pal) +
    basic_plot_theme() +
    theme(panel.background = element_rect(fill = backgroud_map_col))+ # set grey background
    coord_cartesian(ylim = c(y_min, max(df$Value))) +
    theme(
      axis.text.x = element_text(angle = labangle, size = labsize)
    )

  if (percent) {
    p <- p + scale_y_continuous(labels = function(x) paste0(x, "%")) #scale_y_continuous(labels = scales::label_percent(accuracy = 1))#scale_y_continuous(labels = scales::percent_format(accuracy = 1))
  }
  p
}

#' scatterplot between prevalence and growth rate
#'
#' @param df data.frame
#' @param med list with median values for x and y
#' @param x.min numeric adjustment for cartesian x axis
#' @param y.min numeric adjustment for cartesian y axis
#'
#' @import ggplot2
#' @importFrom scales label_number
#'
#' @return ggplot plot
#' @export
scatter_plot <- function(df, med, x.min = c(0.875, 1.125), y.min = c(0.99,1.01)) {

  df = df %>% rename(
    growthfactor = starts_with("growth")
  )
  # mean.x = mean(df$prevalence_rate_1M_pop)
  # mean.y = mean(df$growthfactor)
  color_cntry = rep("#E69F00", nrow(df))
  color_cntry[df$prevalence_rate_1M_pop < med$x & df$growthfactor < med$y ] = "darkgreen"
  color_cntry[df$prevalence_rate_1M_pop > med$x & df$growthfactor > med$y ] = "#dd4b39"
  color_cntry[df$prevalence_rate_1M_pop < med$x & df$growthfactor > med$y ] = "yellow3"

  xlim =  c(min(df$prevalence_rate_1M_pop,med$x)- diff(range(df$prevalence_rate_1M_pop,med$x))*(1-x.min[1]),
            max(df$prevalence_rate_1M_pop,med$x)*x.min[2])
  ylimtop = max(df$growthfactor, med$y)

 # ylimbot = min(1, df$growthfactor,med$y)
  ylimbot = min(df$growthfactor,med$y)- diff(range(df$growthfactor,med$y))*(1-y.min[1])

  ylim = c(ylimbot-diff(c(ylimbot,ylimtop))*(1-y.min[1]), ylimtop + diff(c(ylimbot,ylimtop))*(y.min[2]-1))

  # ylim =  c(min(df$growthfactor,med$y)- diff(range(df$growthfactor,med$y))*(1-y.min[1]),
  #           max(df$growthfactor,med$y)*y.min[2])

  accy = ifelse(diff(ylim)<0.05, 0.001, 0.01)
  p <- ggplot(df) +
    basic_plot_theme() +
    scale_x_continuous(labels = label_number(
                                             big.mark = "'"
                                             #suffix = "K"
                                             )) +
    scale_y_continuous(#limits = c(1, NA), # removed because growthrates can be even <1
                       labels = label_number(accuracy = accy),
                       n.break = 5
                       #labels = function(x) paste0(x, "%")
                       ) +

    # theme(
    #   axis.text.x = element_text()
    # ) +
    #labs(x="prevalence over 1M", y = "growth factor") +
    geom_point(aes(x = prevalence_rate_1M_pop, y = growthfactor,
                   text = paste("prevalence 1M: ", formatC(prevalence_rate_1M_pop, format = "f", big.mark = "'", digits  = 1), "</br>")),
               color = color_cntry, size = 1.3) +
    geom_vline(xintercept = med$x, colour = "darkblue", linetype="dotted", size = 0.3) +
    geom_hline(yintercept = med$y, colour = "darkblue", linetype="dotted", size = 0.3) +
    geom_text(aes(x = prevalence_rate_1M_pop, y = growthfactor, label= Country.Region),
              check_overlap = TRUE, color = color_cntry, size = 3.3) +
    coord_cartesian(ylim = ylim,
                    xlim = xlim)

  p
}

#' default palette for graph pages
#' @import RColorBrewer
graph_palette = c(brewer.pal(12, "Paired"), brewer.pal(8, "Set2"), brewer.pal(8, "Dark2"))

#' background color of maps
#'
backgroud_map_col = "grey90"

#' caption growth factor
#' @param growthvar character growth factor variable
#' @return character gtext for caption
caption_growth_factor_fun <- function(growthvar)
  #paste0("Growth Factor: total confirmed cases since ", gsub("growth_factor_", "", growthvar)  ," days ago. / total confirmed cases in previous 30 days")
  paste0("Growth Factor: total confirmed cases today / total confirmed cases ", gsub("growth_factor_", "", growthvar) ," days ago. (within last 2 months)")

