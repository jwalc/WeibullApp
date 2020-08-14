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
    dplyr::mutate(rank = base::seq(1:base::n()))
  
  if (simplified) {
    df <- df %>%
      dplyr::mutate(F_i = rank / (base::n() + 1))
  } else if (!simplified) {
    df <- df %>%
      dplyr::mutate(F_i = (rank - 0.3) / (base::n() + 0.4))
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

johnson_sd_method <- function (in_data, time = "time", event = "event", sample = "sample") {
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
  
  df["rank"] <- c(1, base::rep(0, base::dim(df)[1] - 1))
  
  N <- base::sum(df["n_sample"])
  
  # Compute average ranks following Johnson Method
  for (i in 2:base::dim(df)[1]) {
    delta_j <- (N + 1 - df["rank"][i-1,]) / (N + 1 - base::sum(df["n_sample"][1:i-1,]))
    df["rank"][i,] <- df["rank"][i-1,] + delta_j
  }
  
  df <- df %>%
    dplyr::mutate(F_i = (rank - 0.3) / (N + 0.4))
  
  df <- dplyr::full_join(in_data, df[c(time, event, sample, "rank", "F_i")], by = c(time, event, sample)) %>%
    dplyr::arrange(rank)
  
  return(df)
}

nelson_method <- function (in_data, time = "time", event = "event") {
  #' @title Nelson Method for Weibull Quantile Estimation
  #' 
  #' Computes Weibull quantiles using the Nelson Method. Suitable for right censored data.
  #' 
  #' @param in_data tibble, containing time to failure and event data
  #' @param time character, name of column containing time data
  #' @param event character, name of column containing event type
  #' @return tibble with added quantile estimations
  
  time_ <- as.symbol(time)
  event_ <- as.symbol(event)
  
  df <- in_data %>%
    dplyr::arrange(!!time_) %>%
    dplyr::mutate(rank = base::seq(base::n(), 1)) %>%
    dplyr::filter(!!event_ == 0) %>%
    dplyr::mutate(lambda_i = 1 / rank) %>%
    dplyr::mutate(H_i = base::cumsum(lambda_i)) %>%
    dplyr::mutate(F_i = 1 - base::exp(- H_i))
  
  df <- dplyr::full_join(in_data, df[c(time, event, "F_i")], by = c(time, event))

  return(df)
}

kaplan_meier_method <- function (in_data, time = "time", event = "event", n_events = "n_events") {
  #' @title Kaplan-Meier Method for Weibull Quantile Estimation
  #' 
  #' Computes Weibull quantiles using the Kaplan-Meier Method. Suitable for right censored data.
  #' 
  #' @param in_data tibble, containing time to failure and data for number and type of events per timestamp
  #' @param time character, name of the column containing time data
  #' @param event character, name of the column containing event type
  #' @param n_events character, name of the column containing number of events of type at timestamp
  #' @return tibble with added quantile estimations

  time_ <- as.symbol(time)
  event_ <- as.symbol(event)
  n_events_ <- as.symbol(n_events)
  
  if (!n_events %in% names(in_data)) {
    warning("Invalid 'n_events' argument! Assuming one event per row!")
    in_data["n_events"] <- rep(1, dim(in_data)[1])
  }
  
  df <- in_data %>%
    dplyr::arrange(!!time_) %>%
    dplyr::mutate(n_fail = base::ifelse(!!event_ == 0, !!n_events_, 0)) %>%
    dplyr::mutate(n_i = base::sum(!!n_events_) - base::cumsum(!!n_events_) + !!n_events_) %>%
    dplyr::group_by(!!time_) %>%
    dplyr::mutate(n_i = base::max(n_i)) %>%
    dplyr::mutate(k_i = (n_i - n_fail) / n_i) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(F_i = base::ifelse(!!event_ == 0, 1-base::cumprod(k_i), NA)) %>%
    dplyr::select(-c(n_fail, n_i, k_i))
  
  return(df)
}

johnson_method <- function (in_data, time = "time", event = "event", n_events = "n_events") {
  #' @title Johnson Method for Weibull Quantile Estimation
  #' 
  #' Computes Weibull quantiles using Johnson Method. Suitable for right censored data.
  #' 
  #' @param in_data tibble, containing time to failure and data for number and type of events per timestamp
  #' @param time character, name of the column containing time data
  #' @param event character, name of the column containing event type
  #' @param n_events character, name of the column containing number of events of type at timestamp
  #' @return tibble with added quantile estimations
  
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
    dplyr::filter(!!event_ == 0)
  
  k <- base::dim(df)[1]
  df["j"] <- c(as.numeric((N + 1) / (N + 1 - df["n_out"][1,])), base::rep(0, k-1))
  for (i in 2:k) {
    delta_j <- ((N + 1 - df["j"][i-1,]) / (N + 1 - df["n_out"][i,])) * df["n_events"][i,]
    df["j"][i,] <- df["j"][i-1,] + delta_j
  }
  
  df <- df %>%
    dplyr::mutate(F_i = (j - 0.3) / (N + 0.4))
  
  df <- dplyr::full_join(in_data, df[c(time, event, "F_i")], by = c(time, event))
  
  return(df)
}
