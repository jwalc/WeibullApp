df_1_in <- tibble::tibble(time = c(100, 200, 300),
                          event = c(1, 1, 1),
                          n_events = c(1, 1, 1))
df_1_exp <- tibble::tibble(F_i_simplified = c(0.25, 0.5, 0.75),
                           F_i_default = c(7/34, 0.5, 27/34))

# --- Testing correct computation --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
testthat::context("Estimator works")

testthat::test_that("Default Estimator", {
  expect_equal(mr_regression(in_data = df_1_in, time = "time", event = "event", n_events = "n_events", simplified = FALSE)$F_i,
               expected = df_1_exp$F_i_default)
})

testthat::test_that("Simplified Estimator", {
  expect_equal(mr_regression(in_data = df_1_in, time = "time", event = "event", n_events = "n_events", simplified = TRUE)$F_i,
               expected = df_1_exp$F_i_simplified)
})

# --- Testing expected output types --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
testthat::context("Expected output types")

testthat::test_that("Output types", {
  expect_warning(
    expect_true(is.null(mr_regression(in_data = NULL)))
  )
  expect_s3_class(mr_regression(in_data = df_1_in), "tbl_df")
  expect_true(is.null(mr_regression(in_data = tibble::tibble(time = NULL))))
})

# --- Testing invalid input parameters --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
testthat::context("Handling unexpected input types")

testthat::test_that("Invalid simplified", {
  expect_warning(
    expect_true(is.null(mr_regression(in_data = df_1_in, simplified = "TRUE")))
  )
  expect_warning(
    expect_true(is.null(mr_regression(in_data = df_1_in, simplified = "FALSE")))
  )
  expect_warning(
    expect_true(is.null(mr_regression(in_data = df_1_in, simplified = "some character")))
  )
  expect_warning(
    expect_true(is.null(mr_regression(in_data = df_1_in, simplified = 0)))
  )
  expect_warning(  
    expect_true(is.null(mr_regression(in_data = df_1_in, simplified = 1)))
  )
  expect_warning(
    expect_true(is.null(mr_regression(in_data = df_1_in, simplified = df_1_exp)))
  )
})


rm(df_1_in, df_1_exp)
