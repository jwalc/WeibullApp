weibull_y_axis_ <- list(scale = scales::trans_new(name = "weibull_y",
                                                  trans = function(x) log(log(1 / (1 - x))),
                                                  inverse = function(x) 1 - 1 / exp(exp(x))),
                        breaks_major = c(0.001, 0.002, 0.003, 0.005, 0.01, 0.02, 0.03, 0.05, seq(0.1, 0.9, by = 0.1), 0.95, 0.99, 0.999),
                        breaks_minor = c(seq(0.001, 0.01, by = 0.001), seq(0.01, 0.1, by = 0.01), seq(0.1, 0.9, by = 0.1)),
                        labels = c(0.001, 0.002, 0.003, 0.005, 0.01, 0.02, 0.03, 0.05, seq(0.1, 0.9, by = 0.1), 0.95, 0.99, 0.999) * 100
)

weibull_x_limits <- function (x_vals) {
  x_vals <- x_vals[x_vals > 0]
  
  x_min <- base::floor(base::min(base::log10(x_vals)))
  x_max <- base::ceiling(base::max(base::log10(x_vals)))
  return(c(x_min, x_max))
}

weibull_x_axis <- function (x_vals) {
  x_limits <- weibull_x_limits(x_vals)
  
  x_breaks <- c(c(1:3, 5) %o% 10^(x_limits[1]:x_limits[2]-1))
  x_breaks_minor <- c(c(1:10) %o% 10^(x_limits[1]:x_limits[2]-1))
  
  return(list(
    limits = x_limits,
    breaks_major = x_breaks,
    breaks_minor = x_breaks_minor,
    labels = x_breaks
  ))
}

weibull_q_plot <- function (in_data, time = "time", q = "F_i", method = "method", regr_line = NULL) {
  #' @title Weibull Quantile Plot
  #' 
  #' Creates a Weibull quantile plot. Scales are transformed such that dots are linear.
  #' 
  #' @param in_data tibble, containing time and quantile data and optional estimation method identificator
  #' @param time character, name of column containing time data
  #' @param q character, name of column containing quantile estimations
  #' @param method character, name of column containing method identificator
  #' @return ggplot object

  time_ <- as.symbol(time)
  q_ <- as.symbol(q)
  method_ <- as.symbol(method)
  
  # compute breaks, limits and labels for x-axis
  weibull_x_axis_ <- weibull_x_axis(in_data[time])
  
  # if method column exists, color dots by method
  if (!method %in% names(in_data)) {
    w_plot <- ggplot2::ggplot(data = in_data, mapping = aes(x = !!time_, y = !!q_))
  } else {
    # set colors for methods for being able to plot regression lines in the same colors
    color_values <- c("Median Rank" = "#999933", "Sudden Death" = "#33CC00",
                      "Kaplan-Meier" = "#FF9900", "Nelson" = "#CC0033",
                      "Johnson" = "#6666FF")
    w_plot <- ggplot2::ggplot(data = in_data, mapping = aes(x = !!time_, y = !!q_, color = !!method_)) +
      scale_color_manual(values = color_values)
  }
  
  # create scatterplot
  w_plot <- w_plot +
    geom_point() +
    coord_trans(x = "log10", y = weibull_y_axis_$scale) +
    scale_y_continuous(breaks = weibull_y_axis_$breaks_major,
                       minor_breaks = weibull_y_axis_$breaks_minor,
                       labels = weibull_y_axis_$labels,
                       limits = c(0.001, 0.999)) +
    scale_x_continuous(limits = 10^weibull_x_axis_$limits,
                       breaks = weibull_x_axis_$breaks_major,
                       minor_breaks = weibull_x_axis_$breaks_minor,
                       labels = weibull_x_axis_$labels) +
    geom_hline(yintercept = 1 - 1/exp(1), color = "blue", linetype = "dotted") +
    labs(title = "Weibull Plot", y = "Quantile in %")
    
  # add regression lines
  if(!is.null(regr_line)) {
    
    line_list <- regr_line %>%
      group_by(method) %>%
      group_split()
    
    for (line in line_list) {
      m_name <- select(line, method) %>% distinct() %>% pull()
      w_plot <- w_plot +
        geom_line(data = line, aes(x = x, y = y), color = color_values[m_name])
    }
  }

  return(w_plot)
}
