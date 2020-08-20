convert_data <- function(in_data, time = NULL, event = NULL, n_events = NULL, sample = NULL) {
  #' @title Convert Data to supported format
  #' 
  #' Takes in a DataFrame or tibble and converts it to a tibble of the format currently supported
  #' in the WeibullApp.
  #' 
  #' @param in_data DataFrame or tibble
  #' @param time character, name of column containing time data
  #' @param event character, name of column specifying type of event
  #' @param n_events character, name of column describing number of events in that row
  #' @param sample character, name of column containing sample identificator
  #' @return tibble

  n_rows <- base::dim(in_data)[1]
  
  if (base::is.null(time)) {
    warning("The is no column containing time data!")
    return(NULL)
  } else {
    df <- tibble::tibble(time = dplyr::pull(in_data[time]))
  }
  
  if (base::is.null(event)) {
    warning("No event column given! Assuming all events to be failures!")
    df$event <- base::rep(1, n_rows)
  } else {
    df$event <- in_data[event]
  }
  
  if (base::is.null(n_events)) {
    warning("No column specifying number of events given! Assuming each row consists of one event!")
    df$n_events <- base::rep(1, n_rows)
  } else {
    df$n_events <- in_data[n_events]
  }
  
  if (base::is.null(sample)) {
    warning("No column for sample identification given! Assuming all data comes from one sample!")
    df$sample <- base::rep("A", n_rows)
  } else {
    df$sample <- in_data[sample]
  }
  
  return(df)
}
