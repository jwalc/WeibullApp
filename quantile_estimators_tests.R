test_input_handler <- function () {
  # Basic test for valid input_data
  input_data <- tibble(Laufzeit = c(28000, 12000, 67000, 91000, 54000, 40000, 110000, 74000, 88000, 37000),
                    Ausfall = c(rep(0, 8), 1, 1))
  input_handler(input_data)
  
  # Test for a valid input vector
  input_vector <- c(28000, 12000, 67000, 91000, 54000, 40000, 110000, 74000, 88000, 37000)
  input_handler(input_vector)
  
  # Tests input vectors of incorrect datatype
  input_string <- "Hello"
  input_string_2 <- c("1", "24", "042", "634.1")
  input_handler(input_string)
  input_handler(input_string_2)
}

test_mr_regression <- function () {
  # Basic functionality test
  mr_data <- tibble(time = c(28000, 12000, 67000, 91000, 54000, 40000, 110000, 74000, 88000, 37000),
                    event = c(rep(0, 10)))
  mr_regression(mr_data, "time", "event")
  mr_regression(mr_data, "time", "event", simplified = TRUE)
  
  # Testing existence of survival events and custom column names
  mr_data <- tibble(Laufzeit = c(28000, 12000, 67000, 91000, 54000, 40000, 110000, 74000, 88000, 37000),
                    Ausfall = c(rep(0, 8), 1, 1))
  mr_regression(mr_data, "Laufzeit", "Ausfall")
  mr_regression(mr_data, "Laufzeit", "Ausfall", simplified = TRUE)
  
  # Test for valid vector input
  mr_vector <- c(28000, 12000, 67000, 91000, 54000, 40000, 110000, 74000, 88000, 37000)
  mr_regression(mr_vector)
  
  # Test for valid vector of invalid datatype
  mr_vector_2 <- as.character(mr_vector)
  mr_regression(mr_vector_2)

  # Test for invalid vector
  mr_vector_3 <- "I'm invalid"
  mr_regression(mr_vector_3)
}

