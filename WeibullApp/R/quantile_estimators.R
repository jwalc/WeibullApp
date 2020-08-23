source("mr_regression.R")
source("johnson_sd_method.R")
source("nelson_method.R")
source("kaplan_meier_method.R")
source("johnson_method.R")

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
                           simplified = FALSE)
  } else {
    df_mr <- tibble(time = NULL, F_i = NULL, method = NULL)
  }
  
  if ("Sudden Death" %in% estimation_method) {
    df_sd <- johnson_sd_method(in_data = in_data, time = time, event = event,
                               sample = sample)
  } else {
    df_sd <- tibble(time = NULL, F_i = NULL, method = NULL)
  }
  
  if ("Kaplan-Meier" %in% estimation_method) {
    df_km <- kaplan_meier_method(in_data = in_data, time = time, event = event,
                                 n_events = n_events)
  } else {
    df_km <- tibble(time = NULL, F_i = NULL, method = NULL)
  }
  
  if("Nelson" %in% estimation_method) {
    df_ne <- nelson_method(in_data = in_data, time = time, event = event)
  } else {
    df_ne <- tibble(time = NULL, F_i = NULL, method = NULL)
  }
  
  if ("Johnson" %in% estimation_method) {
    df_jo <- johnson_method(in_data = in_data, time = time, event = event,
                            n_events = n_events)
  } else {
    df_jo <- tibble(time = NULL, F_i = NULL, method = NULL)
  }
  
  return(rbind(df_mr, df_sd, df_km, df_ne, df_jo))
}
