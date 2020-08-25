#' @title Johnson Sudden Death Method for Weibull Quantile Estimation
#' 
#' Computes Weibull Quantiles using Johnson's Sudden Death Method. Suitable for right censored data.
#' Ranks are estimated by:
#' rank_i = rank_(i-1) + delta_rank(i, i-1)
#' with delta_rank(i, i-1) = (N + 1 - rank_(i-1)) / (N + 1 - sum_m),
#' where N is the number of datapoints and
#' sum_m is the cummulative sum of sample sizes that are ranked earlier.
#' Quantiles are then estimated by:
#' F_i = (rank_i - 0.3) / (N + 0.4)
#' 
#' @details This method can be used for data where there is one failure per sample.
#' 
#' @param in_data tibble, containing time to failure, event and sample identification
#' @param time character, name of column containing time data
#' @param event character, name of column containing event type
#' @param sample character, name of column containing sample identificator
#' @return tibble with added columns rank and estimated quantiles

johnson_sd_method <- function (in_data, time = "time", event = "event", n_events = NA, sample = "sample") {

  if (is.na(n_events)) {
    cols <- c(time, event, sample)
    cols_exist <- cols %in% names(in_data)
  } else {
    cols <- c(time, event, n_events, sample)
    cols_exist <- cols %in% names(in_data)
  }
  if (!all(cols_exist)) {
    warning(paste("The column",
                  cols[!cols_exist],
                  "does not exist in the given tibble!\n"))
    return()
  }
  rm(cols, cols_exist)
  
  time_ <- as.symbol(time)
  event_ <- as.symbol(event)
  sample_ <- as.symbol(sample)
  n_events_ <- as.symbol(n_events)
  
  df <- in_data
  
  if (!is.na(n_events)) {
    if (any(df[n_events][df[event] == 1] != 1)) {
      warning("Sudden Death method can not be used for more than one failure per sample.")
      return()
    }
    # methods needs one event per row
    df <- df %>%
      uncount(!!n_events_)
  }
  
  df <- df %>%
    dplyr::group_by(!!sample_) %>%
    dplyr::mutate(n_sample = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!!event_ == 1) %>%
    dplyr::arrange(!!time_)
  
  df["rank"] <- c(1, base::rep(0, base::dim(df)[1] - 1))
  
  N <- base::sum(df["n_sample"])
  
  # Compute average ranks following Johnson Method
  for (i in 2:base::dim(df)[1]) {
    delta_j <- (N + 1 - df["rank"][i-1,]) / (N + 1 - base::sum(df["n_sample"][1:i-1,]))
    df["rank"][i,] <- df["rank"][i-1,] + delta_j
  }
  
  df <- df %>%
    dplyr::mutate(F_i = (rank - 0.3) / (N + 0.4)) %>%
    dplyr::select(c(time, "F_i")) %>%
    dplyr::filter(!is.na(F_i)) %>%
    dplyr::mutate(method = "Sudden Death")
  
  return(df)
}
