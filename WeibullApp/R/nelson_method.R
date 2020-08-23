#' @title Nelson Method for Weibull Quantile Estimation
#' 
#' Computes Weibull quantiles using the Nelson Method. Suitable for right censored data.
#' 
#' @details This method needs data with one event per row! If data of a
#' Kaplan-Meier-style dataframe with multiple events per row is given, you must(!)
#' pass the n_events column name for correct computation!
#' 
#' @param in_data tibble, containing time to failure and event data
#' @param time character, name of column containing time data
#' @param event character, name of column containing event type
#' @param n_events character, optional name for column with number of events per row
#' @return tibble with added quantile estimations

nelson_method <- function (in_data, time = "time", event = "event", n_events = NA) {

  time_ <- as.symbol(time)
  event_ <- as.symbol(event)
  n_events_ <- as.symbol(n_events)
  
  if (!is.na(n_events)) {
    # methods needs one event per row
    df <- df %>%
      uncount(!!n_events_)
  }
  
  df <- in_data %>%
    dplyr::arrange(!!time_) %>%
    dplyr::mutate(rank = base::seq(dplyr::n(), 1)) %>%
    dplyr::filter(!!event_ == 1) %>%
    dplyr::mutate(lambda_i = 1 / rank) %>%
    dplyr::mutate(H_i = base::cumsum(lambda_i)) %>%
    dplyr::mutate(F_i = 1 - base::exp(- H_i))

  df <- df %>%
    select(c(time, "F_i")) %>%
    filter(!is.na(F_i))

  
  df["method"] <- "Nelson"
  
  return(df)
}
