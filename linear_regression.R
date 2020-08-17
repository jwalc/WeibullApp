
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
