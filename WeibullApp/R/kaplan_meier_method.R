#' @title Kaplan-Meier Method for Weibull Quantile Estimation
#' 
#' Computes Weibull quantiles using the Kaplan-Meier Method. Suitable for right censored data.
#' For more details on estimation method, please read the documentation.
#' 
#' @param in_data tibble, containing time to failure and data for number and type of events per timestamp
#' @param time character, name of the column containing time data
#' @param event character, name of the column containing event type
#' @param n_events character, name of the column containing number of events of type at timestamp
#' @return tibble with quantile estimations

kaplan_meier_method <- function (in_data, time = "time", event = "event", n_events = "n_events") {
  
  cols_exist <- c(time, event, n_events) %in% names(in_data)
  if (!all(cols_exist)) {
    warning(paste("The column",
                  c(time, event, n_events)[!cols_exist],
                  "does not exist in the given tibble!\n"))
    return()
  }
  rm(cols_exist)
  
  time_ <- as.symbol(time)
  event_ <- as.symbol(event)
  n_events_ <- as.symbol(n_events)
  
  df <- in_data %>%
    dplyr::group_by(!!time_, !!event_) %>%
    # this makes sure that multiple line with same time and event are combined
    dplyr::summarise(n_ev = sum(!!n_events_)) %>%
    dplyr::ungroup() %>%
    # here the actual method begins
    dplyr::arrange(!!time_) %>%
    dplyr::mutate(n_fail = base::ifelse(!!event_ == 1, n_ev, 0)) %>%
    dplyr::mutate(n_i = base::sum(n_ev) - base::cumsum(n_ev) + n_ev) %>%
    dplyr::group_by(!!time_) %>%
    dplyr::mutate(n_i = base::max(n_i)) %>%
    dplyr::mutate(km2 = all(!!event_ == 1))
  
  
  if (pull(tail(df["km2"], 1))) {
    # Kaplan-Meier 2.0
    # In case of the last event being failures only, we need to adjust the estimator in order to not
    # return a value of F_i = 1.
    df <- df %>%
      dplyr::ungroup() %>%
      dplyr::mutate(k_i = (n_i - n_fail + 1) / (n_i + 1)) %>%
      dplyr::mutate(F_i = base::ifelse(!!event_ == 1, 1-base::cumprod(k_i), NA))
  } else {
    # regular Kaplan-Meier
    df <- df %>%
      dplyr::ungroup() %>%
      dplyr::mutate(k_i = (n_i - n_fail) / n_i) %>%
      dplyr::mutate(F_i = base::ifelse(!!event_ == 1, 1-base::cumprod(k_i), NA))
  }
  
  df <- df %>%
    dplyr::select(c(time, "F_i")) %>%
    filter(!is.na(F_i))
  
  df["method"] <- "Kaplan-Meier"
  
  return(df)
}
