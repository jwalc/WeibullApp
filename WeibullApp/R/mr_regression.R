#' @title Median Rank Regression for Weibull Quantiles
#' 
#' Calculates quantile estimates for the Weibull distribution using median rank regression.
#' Using the estimators:
#'  - default: F_i = (rank - 0.3) / (n + 0.4)
#'  - simplified: F_i = rank / (n + 1)
#' Coding of events:
#'  - 1: time to failure
#'  - 0: survival (right censored data)
#'  
#' @details Expects a tibble of supported format. For more info check project documentation or
#' data_converter
#' 
#' @param in_data numeric vector or tibble containing time to failure
#' @param time character, name of column containing time data
#' @param event character, name of column containing event type
#' @param n_events character, optional name of column containing number of events for row
#' @param simplified boolean, specifying use of simplified estimator
#' @param append boolean, specifying if quantiles should be appended to the given in_data tibble 
#' @return tibble, returns a tibble with added columns rank and estimated quantiles

mr_regression <- function (in_data, time = "time", event = "event", n_events = NA, simplified = FALSE) {
  
  if (!base::is.logical(simplified)) {
    warning("Invalid input for parameter 'simplified'!")
    return()
  }
  
  if (is.null(in_data)) {
    warning("Input data is NULL!")
    return()
  }
  
  time_ <- base::as.symbol(time)
  event_ <- base::as.symbol(event)
  n_events_ <- base::as.symbol(n_events)
  
  df <- in_data
  if (nrow(df) == 0) {
    return()
  }
  
  if (!is.na(n_events)) {
    # methods needs one event per row
    df <- df %>%
      uncount(!!n_events_)
  }
  
  df <- df %>%
    dplyr::arrange(!!time_) %>%
    dplyr::filter(!!event_ == 1) %>%
    dplyr::mutate(rank = base::seq(1:dplyr::n()))
  
  if (base::isTRUE(simplified)) {
    df <- df %>%
      dplyr::mutate(F_i = rank / (dplyr::n() + 1))
  } else if (!base::isTRUE(simplified)) {
    df <- df %>%
      dplyr::mutate(F_i = (rank - 0.3) / (dplyr::n() + 0.4))
  }
  
  df <- df %>%
    dplyr::select(c(time, "F_i")) %>%
    dplyr::filter(!is.na(F_i)) %>%
    dplyr::mutate(method = "Median Rank")

  
  return(df)
}
