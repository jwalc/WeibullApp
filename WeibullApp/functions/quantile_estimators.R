
input_handler <- function (in_data) {
  #' @title Input Data Handler for Quantile Estimators
  #' 
  #' Checks type of input data and tries to convert it to a supported format.
  #' 
  #' @param in_data input data of variable type
  #' @return tibble with at least two columns for time and event data
  
  if (tibble::is_tibble(in_data)) {
    return(in_data)
  
  } else if (base::is.vector(in_data)) {
    if (base::is.numeric(in_data)) {
      # Assume all events are Failures
      df <- tibble::tibble(time = in_data,
                   event = base::rep(1, base::length(in_data)))
    } else {
      warning("Trying to convert values to numerics. Assuming all events are failures.")
      df <- tibble::tibble(time = base::as.numeric(in_data),
                           event = base::rep(1, base::length(in_data)))
      if (any(is.na(df))) {
        df <- tibble(time = NULL, event = NULL)
        warning("Could not convert data successfully.")
      }
    }
    return(df)
    
  } else {
    warning("Can not handle input data. Please try a vector or tibble.")
    return(tibble(time = NULL, event = NULL))
  }
}

mr_regression <- function (in_data, time = "time", event = "event", n_events = NA, simplified = FALSE, append = FALSE) {
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
  
  if (append) {
    if (!all(df[event] == 1)) {
      warning("mr_regression does not consider right censored data!")
      df <- dplyr::full_join(in_data, df[c(time, event, "rank", "F_i")], by = c(time, event)) %>%
        dplyr::arrange(!!time_)
      df <- df %>%
        dplyr::select(-c("rank"))
      df["method"] <- "Median Rank"
    }
  } else {
    df <- df %>%
      dplyr::select(c(time, "F_i")) %>%
      dplyr::filter(!is.na(F_i)) %>%
      dplyr::mutate(method = "Median Rank")
  }
  
  return(df)
}

johnson_sd_method <- function (in_data, time = "time", event = "event", sample = "sample", append = FALSE) {
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
  #' @param append boolean, specifying if quantiles should be appended to the given in_data tibble 
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
    dplyr::mutate(F_i = (rank - 0.3) / (N + 0.4))
  
  if (append) {
    df <- dplyr::full_join(in_data, df[c(time, event, sample, "rank", "F_i")], by = c(time, event, sample)) %>%
      dplyr::arrange(rank)
    df["method"] <- "Sudden Death"
  } else {
    df <- df %>%
      dplyr::select(c(time, "F_i")) %>%
      dplyr::filter(!is.na(F_i)) %>%
      dplyr::mutate(method = "Sudden Death")
  }
  
  return(df)
}

nelson_method <- function (in_data, time = "time", event = "event", append = FALSE) {
  #' @title Nelson Method for Weibull Quantile Estimation
  #' 
  #' Computes Weibull quantiles using the Nelson Method. Suitable for right censored data.
  #' 
  #' @param in_data tibble, containing time to failure and event data
  #' @param time character, name of column containing time data
  #' @param event character, name of column containing event type
  #' @param append boolean, specifying if quantiles should be appended to the given in_data tibble 
  #' @return tibble with added quantile estimations
  
  time_ <- as.symbol(time)
  event_ <- as.symbol(event)
  
  df <- in_data %>%
    dplyr::arrange(!!time_) %>%
    dplyr::mutate(rank = base::seq(dplyr::n(), 1)) %>%
    dplyr::filter(!!event_ == 1) %>%
    dplyr::mutate(lambda_i = 1 / rank) %>%
    dplyr::mutate(H_i = base::cumsum(lambda_i)) %>%
    dplyr::mutate(F_i = 1 - base::exp(- H_i))
  
  if (append) {
    df <- dplyr::full_join(in_data, df[c(time, event, "F_i")], by = c(time, event))
  } else {
    df <- df %>%
      select(c(time, "F_i")) %>%
      filter(!is.na(F_i))
  }
  
  df["method"] <- "Nelson"
  
  return(df)
}

kaplan_meier_method <- function (in_data, time = "time", event = "event", n_events = "n_events", append = FALSE) {
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

johnson_method <- function (in_data, time = "time", event = "event", n_events = "n_events", append = FALSE) {
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

weibull_estimation <- function(in_data, time = "time", event = "event", n_events = "n_events",
                               sample = "sample", estimation_method = "Median Rank") {
  #' @title Weibull Quantile Estimation Wrapper Method
  #' 
  #' Computes Weibull Quantiles using given estimation method. Supports Median Rank Regression,
  #' Johnson's Sudden Death Method, Kaplan-Meier Method, Nelson Method and Johnson Method.
  #' 
  #' @param in_data tibble, containing data for estimation methods
  #' @param time character, name of column containing time data
  #' @param event character, name of column containing event type
  #' @param n_events character, name of column containing number of events at that timestamp
  #' @param sample character, name of column containing sample identification
  #' @param estimation_method character, one or more of the following supported methods:
  #' "mr_regression", "sudden_death", "kaplan_meier", "nelson", "johnson"
  #' @return tibble with three columns: time, F_i and method
  
  if ("Median Rank" %in% estimation_method) {
    df_mr <- mr_regression(in_data = in_data, time = time, event = event, n_events = n_events,
                           simplified = FALSE, append = FALSE)
  } else {
    df_mr <- tibble(time = NULL, F_i = NULL, method = NULL)
  }
  
  if ("Sudden Death" %in% estimation_method) {
    df_sd <- johnson_sd_method(in_data = in_data, time = time, event = event,
                               sample = sample, append = FALSE)
  } else {
    df_sd <- tibble(time = NULL, F_i = NULL, method = NULL)
  }
  
  if ("Kaplan-Meier" %in% estimation_method) {
    df_km <- kaplan_meier_method(in_data = in_data, time = time, event = event,
                                 n_events = n_events, append = FALSE)
  } else {
    df_km <- tibble(time = NULL, F_i = NULL, method = NULL)
  }
  
  if("Nelson" %in% estimation_method) {
    df_ne <- nelson_method(in_data = in_data, time = time, event = event,
                           append = FALSE)
  } else {
    df_ne <- tibble(time = NULL, F_i = NULL, method = NULL)
  }
  
  if ("Johnson" %in% estimation_method) {
    df_jo <- johnson_method(in_data = in_data, time = time, event = event,
                            n_events = n_events, append = FALSE)
  } else {
    df_jo <- tibble(time = NULL, F_i = NULL, method = NULL)
  }
  
  return(rbind(df_mr, df_sd, df_km, df_ne, df_jo))
}
