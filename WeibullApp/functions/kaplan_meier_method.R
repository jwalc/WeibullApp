#' @title Kaplan-Meier Method for Weibull Quantile Estimation
#' 
#' Computes Weibull quantiles using the Kaplan-Meier Method. Suitable for right censored data.
#' For more details on estimation method, please read the documentation.
#' 
#' @param in_data tibble, containing time to failure and data for number and type of events per timestamp
#' @param time character, name of the column containing time data
#' @param event character, name of the column containing event type
#' @param n_events character, name of the column containing number of events of type at timestamp
#' @param append boolean, specifying if quantiles should be appended to the given in_data tibble 
#' @return tibble with added quantile estimations

kaplan_meier_method <- function (in_data, time = "time", event = "event", n_events = "n_events", append = FALSE) {
  
  time_ <- as.symbol(time)
  event_ <- as.symbol(event)
  n_events_ <- as.symbol(n_events)
  
  if (!n_events %in% names(in_data)) {
    warning("Invalid 'n_events' argument! Assuming one event per row!")
    in_data["n_events"] <- rep(1, dim(in_data)[1])
  }
  
  df <- in_data %>%
    dplyr::arrange(!!time_) %>%
    dplyr::mutate(n_fail = base::ifelse(!!event_ == 1, !!n_events_, 0)) %>%
    dplyr::mutate(n_i = base::sum(!!n_events_) - base::cumsum(!!n_events_) + !!n_events_) %>%
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
    dplyr::select(-c(n_fail, n_i, k_i, km2))
  
  if (!append) {
    df <- df %>%
      select(c(time, "F_i")) %>%
      filter(!is.na(F_i))
  }
  
  df["method"] <- "Kaplan-Meier"
  
  return(df)
}
