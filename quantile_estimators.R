input_handler <- function (in_data) {
  return(in_data)
}

mr_regression <- function (in_data, time = "time", event = "event", simplified = FALSE) {
  #' @title Median Rank Regression for Weibull Quantiles
  #' 
  #' Calculates quantile estimates for the Weibull distribution using median rank regression.
  #' Using the estimators:
  #'  - default: F_i = (rank - 0.3) / (n + 0.4)
  #'  - simplified: F_i = rank / (n + 1)
  #' Coding of events:
  #'  - 0: time to failure
  #'  - 1: survival (right censored data)
  #' 
  #' @param in_data numeric vector or tibble containing time to failure
  #' @param time character, name of column containing time data
  #' @param event character, name of column containing event type
  #' @param simplified boolean, specifying use of simplified estimator 
  #' @return tibble, returns a tibble with added columns rank and estimated quantiles
  
  time_ <- base::as.symbol(time)
  event_ <- base::as.symbol(event)
  
  df <- in_data %>%
    dplyr::arrange(!!time_) %>%
    dplyr::filter(!!event_ == 0) %>%
    dplyr::mutate(rank = seq(1:n()))
  
  if (simplified) {
    df <- df %>%
      dplyr::mutate(F_i = rank / (n() + 1))
  } else if (!simplified) {
    df <- df %>%
      dplyr::mutate(F_i = (rank - 0.3) / (n() + 0.4))
  } else {
    warning("Invalid input for parameter 'simplified'!")
    return(in_data)
  }
  
  if (!all(in_data[event] == 0)) {
    warning("mr_regression does not consider right censored data!")
    df <- dplyr::full_join(in_data, df[c(time, event, "rank", "F_i")], by = c(time, event)) %>%
      dplyr::arrange(rank)
  }
  
  return(df)
}
