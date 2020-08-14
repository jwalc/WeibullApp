input_handler <- function (in_data) {
  return(in_data)
}

mr_regression <- function (in_data, time = "time", event = "event", simplified = FALSE) {
  
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
