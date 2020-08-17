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

predict_path <- function (x_min, x_max, b, T, graining = 1000) {
  #' @title Predict Quantiles for Weibull Plot
  #'
  #' Computes value pairs for the regression line.
  #' 
  #' @param x_min minimal x value
  #' @param x_max maximal x value
  #' @param b Weibull paramter
  #' @param T Weibull parameter
  #' @param graining sets the number of values to predict. Higher graining should lead to a smoother line.
  #' @return tibble containing the x, y value pairs for the Weibull Plot
  pred_data <- tibble(x = seq(x_min, x_max, length.out = graining)) %>%
    mutate(y_hat = b * log(x) - b * log(T)) %>%
    mutate(y = 1 - 1 / exp(exp(y_hat))) %>%
    select(x, y)
  return(pred_data)
}


weibull_q_plot <- function (in_data, time = "time", q = "F_i", method = "method", regr_line = TRUE) {
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
  
  # add regression line
  if (regr_line) {
    if (!method %in% names(in_data)) {
      # if it works, why change it?
      df <- in_data %>%
        dplyr::filter(!is.na(!!q_)) %>%
        dplyr::mutate(y_transform = log(log(1 / (1 - !!q_))), x_transform = log(!!time_))
      res <- linear_regression(dplyr::pull(df, y_transform), dplyr::pull(df, x_transform))
      pred <- predict_path(x_min = min(df[time]),
                           x_max = max(df[time]),
                           b = res$slope,
                           T = exp(- res$intercept / res$slope))
      w_plot <- w_plot +
        geom_line(data = pred, aes(x = x, y = y), color = "red")
    } else {
      df_list <- in_data %>%
        dplyr::filter(!is.na(!!q_)) %>%
        dplyr::mutate(y_transform = log(log(1 / (1 - !!q_))), x_transform = log(!!time_)) %>%
        dplyr::group_by(!!method_) %>%
        dplyr::group_split()
      x_min <- min(in_data[time])
      x_max <- max(in_data[time])
      
      for (df in df_list) {
        m_name <- select(df, !!method_) %>% distinct() %>% pull()
        res <- linear_regression(dplyr::pull(df, y_transform), dplyr::pull(df, x_transform))
        
        # workaround for plotting a straight line with nonlinearly transformed axis
        pred <- predict_path(x_min = x_min,
                             x_max = x_max,
                             b = res$slope,
                             T = exp(- res$intercept / res$slope))
        w_plot <- w_plot +
          geom_line(data = pred, aes(x = x, y = y), color = color_values[m_name])
      }
    }
  }

  return(w_plot)
}
