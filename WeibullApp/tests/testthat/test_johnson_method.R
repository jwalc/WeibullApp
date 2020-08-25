df_1_in <- tibble(time = c(1,2,3,3),
                  event = c(0,1,1,0),
                  n_events = c(2,1,2,2))

df_1_exp <- tibble(time = c(2,3),
                   F_i = c(31/222, 1/2),
                   method = rep("Johnson", 2))

df_2_in <- tibble(time = c(1,1,1,2,2,3),
                  event = c(1,1,0,1,0,1),
                  n_events = c(2,1,2,1,2,3))

df_2_exp <- tibble(time = c(1,2,3),
                   F_i = c(9/38, 93/266, 6/7),
                   method = rep("Johnson", 3))


# --- Testing correct computation --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
testthat::context("johnson_method: correct computation")

testthat::test_that("Computation", {
  expect_equal(johnson_method(df_1_in), df_1_exp)
  expect_equal(johnson_method(df_2_in), df_2_exp)
})

# --- Testing expected output types --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
testthat::context("johnson_method: expected output types")

testthat::test_that("Output types", {
  expect_warning(
    expect_true(is.null(johnson_method(in_data = tibble::tibble(time = NULL))))
  )
  expect_warning(
    expect_true(is.null(johnson_method(in_data = NULL)))
  )
  expect_warning(
    expect_true(is.null(johnson_method(in_data = tibble(time = NULL, event = NULL, n_events = NULL))))
  )
  expect_s3_class(johnson_method(in_data = df_1_in), "tbl_df")
  expect_warning(
    expect_true(is.null(johnson_method(in_data = df_1_in, time = "Zeit", event = "Ausfall")))
  )
})


rm(df_1_in, df_1_exp, df_2_in, df_2_exp)
