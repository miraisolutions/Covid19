#' Time evolution as line plot
#'
#' @rdname time_evol_line_plot
#'
#' @param df data.frame with column called Date and x column to plot
#'
#' @return line plot of given variable by date
#'
#' @import ggplot2
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
time_evol_line_plot <- function(df) {
  p <- ggplot(df, aes(x = Date, y = Value, colour = Status)) +
    geom_line(size = 2) +
    basic_plot_theme()
  p
}


#' Time evolution as area plot
#'
#' @rdname time_evol_area_plot
#'
#' @param df data.frame with column called Date and x column to plot
#'
#' @return line plot of given variable by date
#'
#' @import ggplot2
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
time_evol_area_plot <- function(df) {
  p <- ggplot(df, aes(x = Date, y = Value)) +
    geom_area(aes(colour = Status, fill = Status), size = 2, alpha = 0.5, position = position_dodge(0.8)) +
    basic_plot_theme()
  p <- p %>%
    fix_colors()
  p
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
  p <- p + scale_y_continuous(trans = 'log10')
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
from_contagion_day_bar_facet_plot <- function(df){
  p <- ggplot(df, aes(x = contagion_day, y = value, fill = bool_new)) +
    geom_bar(stat = "identity") +
    basic_plot_theme() +
    facet_wrap( ~ status, scales = "free_y", nrow = 1, ncol = 4)
  p
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
#'
#' @import ggplot2
#' @import RColorBrewer
#'
#' @return p ggplot object
#'
#' @export
fix_colors <- function(p){
  p <- p +
    scale_color_manual(values = c("confirmed" = "red", "deaths" = "black","recovered" = "green" , "active" = "blue",
                                  "new_confirmed" = "red", "new_deaths" = "black","new_recovered" = "green" , "new_active" = "blue")) +
    scale_fill_manual(values = c("confirmed" = "red", "deaths" = "black","recovered" = "green" , "active" = "blue",
                                  "new_confirmed" = "red", "new_deaths" = "black","new_recovered" = "green" , "new_active" = "blue"))

  p
}
