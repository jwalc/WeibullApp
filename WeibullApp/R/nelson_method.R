#' @title Nelson Method for Weibull Quantile Estimation
#' 
#' Computes Weibull quantiles using the Nelson Method. Suitable for right censored data.
#' 
#' @param in_data tibble, containing time to failure and event data
#' @param time character, name of column containing time data
#' @param event character, name of column containing event type
#' @param append boolean, specifying if quantiles should be appended to the given in_data tibble 
#' @return tibble with added quantile estimations

nelson_method <- function (in_data, time = "time", event = "event") {

  time_ <- as.symbol(time)
  event_ <- as.symbol(event)
  
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
