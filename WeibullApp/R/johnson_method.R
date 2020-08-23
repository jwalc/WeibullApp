#' @title Johnson Method for Weibull Quantile Estimation
#' 
#' Computes Weibull quantiles using Johnson Method. Suitable for right censored data.
#' 
#' @param in_data tibble, containing time to failure and data for number and type of events per timestamp
#' @param time character, name of the column containing time data
#' @param event character, name of the column containing event type
#' @param n_events character, name of the column containing number of events of type at timestamp
#' @param append boolean, specifying if quantiles should be appended to the given in_data tibble 
#' @return tibble with added quantile estimations

johnson_method <- function (in_data, time = "time", event = "event", n_events = "n_events", append = FALSE) {
  
  time_ <- as.symbol(time)
  event_ <- as.symbol(event)
  n_events_ <- as.symbol(n_events)
  
  if (!n_events %in% names(in_data)) {
    warning("Invalid 'n_events' argument! Assuming one event per row!")
    in_data["n_events"] <- rep(1, dim(in_data)[1])
  }
  
  N <- base::sum(in_data[n_events])
  
  df <- in_data %>%
    dplyr::arrange(!!time_) %>%
    dplyr::mutate(n_out = base::cumsum(!!n_events_) - !!n_events_) %>%
    dplyr::group_by(!!time_) %>%
    dplyr::mutate(n_out = base::min(n_out)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!!event_ == 1)
  
  k <- base::dim(df)[1]
  df["j"] <- c(as.numeric((N + 1) / (N + 1 - df["n_out"][1,])), base::rep(0, k-1))
  for (i in 2:k) {
    delta_j <- ((N + 1 - df["j"][i-1,]) / (N + 1 - df["n_out"][i,])) * df["n_events"][i,]
    df["j"][i,] <- df["j"][i-1,] + delta_j
  }
  
  df <- df %>%
    dplyr::mutate(F_i = (j - 0.3) / (N + 0.4))
  
  if (append) {
    df <- dplyr::full_join(in_data, df[c(time, event, "F_i")], by = c(time, event))
  } else {
    df <- df %>%
      select(c(time, "F_i")) %>%
      filter(!is.na(F_i))
  }
  
  df["method"] <- "Johnson"
  
  return(df)
}