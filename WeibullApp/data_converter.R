convert_data <- function(in_data, time = NA, event = NA, n_events = NA, sample = NA) {
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
  
  if (base::is.na(time) | time == "NA") {
    warning("The is no column containing time data!")
    return(NULL)
  } else {
    df <- tibble::tibble(time = dplyr::pull(in_data[time]))
  }
  
  if (base::is.na(event) | event == "NA") {
    warning("No event column given! Assuming all events to be failures!")
    df$event <- base::rep(1, n_rows)
  } else {
    df$event <- in_data[event]
  }
  
  if (base::is.na(n_events) | n_events == "NA") {
    warning("No column specifying number of events given! Assuming each row consists of one event!")
    df$n_events <- base::rep(1, n_rows)
  } else {
    df$n_events <- in_data[n_events]
  }
  
  if (base::is.na(sample) | sample == "NA") {
    warning("No column for sample identification given! Assuming all data comes from one sample!")
    df$sample <- base::rep("A", n_rows)
  } else {
    df$sample <- in_data[sample]
  }
  
  return(df)
}

list_user_data <- function (filename, path = "./data/user_data/") {
  return(base::list.files(path = path))
}

save_user_data <- function (filename, data, path = "./data/user_data/") {
  existing <- list_user_data()
  if (filename %in% existing) {
    print("File already exists!")
    return(FALSE)
  } else {
    print(paste("Saving", filename))
    readr::write_csv(data, path = paste0(path, filename))
    return(TRUE)
  }
}

delete_user_data <- function (filename, path = "./data/user_data/") {
  
}

