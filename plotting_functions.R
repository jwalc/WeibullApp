weibull_y_axis_ <- list(scale = scales::trans_new(name = "weibull_y",
                                                  trans = function(x) log(log(1 / (1 - x))),
                                                  inverse = function(x) 1 - 1 / exp(exp(x))),
                        breaks_major = c(0.001, 0.002, 0.003, 0.005, 0.01, 0.02, 0.03, 0.05, seq(0.1, 0.9, by = 0.1), 0.95, 0.99, 0.999),
                        breaks_minor = c(seq(0.001, 0.01, by = 0.001), seq(0.01, 0.1, by = 0.01), seq(0.1, 0.9, by = 0.1)),
                        labels = c(0.001, 0.002, 0.003, 0.005, 0.01, 0.02, 0.03, 0.05, seq(0.1, 0.9, by = 0.1), 0.95, 0.99, 0.999) * 100
)

weibull_x_limits <- function (x_vals) {
  
}

weibull_q_plot <- function (in_data, time = "time", q = "F_i") {
  time_ <- as.symbol(time)
  q_ <- as.symbol(q)

  w_plot <- ggplot2::ggplot(data = in_data, mapping = aes(x = !!time_, y = !!q_)) +
    geom_point() +
    coord_trans(x = "log10", y = weibull_y_axis_$scale) +
    scale_y_continuous(breaks = weibull_y_axis_$breaks_major,
                       minor_breaks = weibull_y_axis_$breaks_minor,
                       labels = weibull_y_axis_$labels,
                       limits = c(0.001, 0.999)) +
    # scale_x_continuous(limits = c(10^4, 10^5 * 1.5)) +
    geom_hline(yintercept = 1 - 1/exp(1), color = "blue", linetype = "dotted") +
    labs(title = "Weibull Plot", y = "Quantile in %")
  
  return(w_plot)
}
