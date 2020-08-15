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

