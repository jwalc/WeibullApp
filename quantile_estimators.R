input_handler <- function (in_data) {
  if (dplyr::is_tibble(in_data)) {
    return(in_data)
  } else {
    # TODO handling of non tibble type
  }
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

johnson_method <- function (in_data, time = "time", event = "event", sample = "sample") {
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
  #' @param in_data tibble, containing time to failure, event and sample identification
  #' @param time character, name of column containing time data
  #' @param event character, name of column containing event type
  #' @param sample character, name of column containing sample identificator
  #' @return tibble with added columns rank and estimated quantiles
  
  time_ <- as.symbol(time)
  event_ <- as.symbol(event)
  sample_ <- as.symbol(sample)
  
  if (!sample %in% names(in_data)) {
    warning("Sample identification not possible! Did you spell the column name correctly?")
    return(in_data)
  }
  
  df <- in_data %>%
    dplyr::group_by(!!sample_) %>%
    dplyr::mutate(n_sample = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!!event_ == 0) %>%
    dplyr::arrange(!!time_)
  
  df["rank"] <- c(1, rep(0, dim(df)[1] - 1))
  
  N <- base::sum(df["n_sample"])
  
  # Compute average ranks following Johnson Method
  for (i in 2:dim(df)[1]) {
    delta_j <- (N + 1 - df["rank"][i-1,]) / (N + 1 - base::sum(df["n_sample"][1:i-1,]))
    df["rank"][i,] <- df["rank"][i-1,] + delta_j
  }
  
  df <- df %>%
    dplyr::mutate(F_i = (rank - 0.3) / (N + 0.4))
  
  df <- dplyr::full_join(in_data, df[c(time, event, sample, "rank", "F_i")], by = c(time, event, sample)) %>%
    dplyr::arrange(rank)
  
  return(df)
}
