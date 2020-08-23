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
#'  @details This method absolutely needs one(!) event per row. Hence you must specify n_events when
#'  using method with a Kaplan-Meier-style dataframe. Otherwise the estimator is incorrect!
#' 
#' @param in_data numeric vector or tibble containing time to failure
#' @param time character, name of column containing time data
#' @param event character, name of column containing event type
#' @param n_events character, optional name of column containing number of events for row
#' @param simplified boolean, specifying use of simplified estimator
#' @param append boolean, specifying if quantiles should be appended to the given in_data tibble 
#' @return tibble, returns a tibble with added columns rank and estimated quantiles

mr_regression <- function (in_data, time = "time", event = "event", n_events = NA, simplified = FALSE) {

  time_ <- base::as.symbol(time)
  event_ <- base::as.symbol(event)
  n_events_ <- base::as.symbol(n_events)
  
  df <- input_handler(in_data)
  if (nrow(df) == 0) {
    return(in_data)
  }
  
  if (!event %in% names(df)) {
    warning("Assuming all events are failures.")
    df[event] <- base::rep(1,base::dim(df)[1])
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
  
  if (simplified) {
    df <- df %>%
      dplyr::mutate(F_i = rank / (dplyr::n() + 1))
  } else if (!simplified) {
    df <- df %>%
      dplyr::mutate(F_i = (rank - 0.3) / (dplyr::n() + 0.4))
  } else {
    warning("Invalid input for parameter 'simplified'!")
    return(in_data)
  }
  
  df <- df %>%
    dplyr::select(c(time, "F_i")) %>%
    dplyr::filter(!is.na(F_i)) %>%
    dplyr::mutate(method = "Median Rank")

  
  return(df)
}
