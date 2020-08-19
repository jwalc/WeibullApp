
linear_regression <- function (y, x, alpha = 0.05) {
  #' @title Linear Regression of y on x
  #' 
  #' Computes intercept and slope parameters for the linear regression
  #' of y on x.
  #' 
  #' @param y vector
  #' @param x vector
  #' @return tibble containing intercept and slope parameters
  
  if (length(x) != length(y)) {
    warning("Vectors x and y must be of same length!")
    return(NULL)
  }
  
  # Estimating slope and intercept
  x_bar <- mean(x)
  y_bar <- mean(y)
  n <- length(x)
  
  S_xy <- sum(x * y) - n * x_bar * y_bar
  S2_x <- sum(x^2) - n * x_bar^2
  
  beta_1 <- S_xy / S2_x
  beta_0 <- y_bar - beta_1 * x_bar
  
  # Estimating R^2
  residuals <- y - (beta_0 + beta_1 * x)
  
  SST <- sum((y - y_bar)^2)
  SSR <- sum(residuals^2)
  R_squared <- 1 - (SSR / SST)
  
  # Estimating confidence intervals
  sigma_hat <- (1 / (n-2)) * SSR
  sigma_beta_0 <- sqrt((sigma_hat * (1/n) * sum(x^2)) / (n * S2_x))
  sigma_beta_1 <- sqrt(sigma_hat / (n * S2_x))
  
  beta_0_min <- beta_0 - qt(p = 1 - (alpha/2), df = n - 2) * sigma_beta_0
  beta_0_max <- beta_0 + qt(p = 1 - (alpha/2), df = n - 2) * sigma_beta_0
  beta_1_min <- beta_1 - qt(p = 1 - (alpha/2), df = n - 2) * sigma_beta_1
  beta_1_max <- beta_1 + qt(p = 1 - (alpha/2), df = n - 2) * sigma_beta_1
  
  result  <- tibble(intercept = beta_0,
                    slope = beta_1,
                    R_squared = R_squared,
                    intercept_conf_min = beta_0_min, 
                    intercept_conf_max = beta_0_max,
                    slope_conf_min = beta_1_min,
                    slope_conf_max = beta_1_max)
  
  return(result)
}

weibull_model <- function (in_data, time = "time", q = "F_i", method = "method") {
  #' @title Linear Regression on Weibull Data
  #' 
  #' Performs a linear regression on time and estimated quantile data to compute a linear model.
  #' This in turn can be used to compute the Weibull parameters b and T from the model's
  #' slope and intercept.
  #' 
  #' @param in_data tibble, containing time and estimated quantiles
  #' @param time character, name of column containing time data
  #' @param q character, name of column containing estimated quantiles
  #' @param method character, name of column containing estimation method identificator
  #' @return tibble, containing linear regression output (see linear_regression)

  time_ <- base::as.symbol(time)
  q_ <- base::as.symbol(q)
  method_ <- base::as.symbol(method)
  
  m_names <- in_data %>%
    select(!!method_) %>%
    distinct() %>%
    pull()
  
  res <- tibble(NULL)
  
  df_list <- in_data %>%
    dplyr::filter(!is.na(!!q_)) %>%
    dplyr::mutate(y_transform = log(log(1 / (1 - !!q_))), x_transform = log(!!time_)) %>%
    dplyr::group_by(!!method_) %>%
    dplyr::group_split()
  i <- 1
  for (df in df_list) {
    reg_result <- linear_regression(dplyr::pull(df, y_transform), dplyr::pull(df, x_transform))
    reg_result$method <- df[method][1,] %>% pull()
    res <- dplyr::bind_rows(res, reg_result)
  }
  return(res)
}

weibull_paramters_from_model <- function (slope, intercept, method) {
  #' @title Weibull Parameters from Linear Model
  #' 
  #' Returns Weibull parameters when given slope and intercept of underlying linear model.
  #' 
  #' @param slope numeric, slope estimate for linear model
  #' @param intercept numeric, intercept estimate for linear model
  #' @param method character, name of quantile estimation method
  #' @return tibble, containing b and T parameters

  return(tibble(b = slope,
                T = base::exp(- intercept / slope),
                method = method))
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

predict_paths <- function (weibull_parameters, x_min, x_max) {
  #' @title Predict Regression Line for Weibull Paper Plot
  #' 
  #' Computes regression lines for a set of given weibull_paramters. These should come
  #' from the weibull_parameters_from_model function or resemble there structure and naming.
  #' 
  #' @param weibull_parameters tibble, see output from weibull_parameters_from_model
  #' @param x_min numeric, lower bound for intervall on which to compute the line
  #' @param x_max numeric, upper bound for intervall on which to compute the line
  #' @return tibble, x and y values for Weibull Paper Plot and method identificator

  res <- tibble(NULL)
  for (i in 1:dim(weibull_parameters)[1]) {
    path <- predict_path(x_min = x_min,
                         x_max = x_max,
                         b = weibull_parameters[i,]$b,
                         T = weibull_parameters[i,]$T)
    path$method <- weibull_parameters[i,]$method
    res <- bind_rows(res, path)
  }
  return(res)
}
