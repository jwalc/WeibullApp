df_1_in <- tibble(time = c(1,2,3,4),
                  event = c(1,0,0,1),
                  n_events = c(1,1,1,1))

df_1_exp <- tibble(time = c(1,4),
                   F_i = 1 - exp(-c(1/4, 5/4)),
                   method = rep("Nelson", 2))

df_2_in <- tibble(time = c(1,2),
                  event = c(1,0),
                  n_events = c(2,1))

df_2_exp <- tibble(time = c(1,1),
                   F_i = 1 - exp(-c(1/3, 5/6)),
                   method = rep("Nelson", 2))

# --- Testing correct computation --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
testthat::context("nelson_method: correct computation")

testthat::test_that("Computation", {
  expect_equal(nelson_method(df_1_in), df_1_exp)
  expect_equal(nelson_method(df_2_in, n_events = "n_events"), df_2_exp)
})

# --- Testing expected output types --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
testthat::context("nelson_method: expected output types")

testthat::test_that("Output types", {
  expect_warning(
    expect_true(is.null(nelson_method(in_data = tibble::tibble(time = NULL))))
  )
  expect_warning(
    expect_true(is.null(nelson_method(in_data = NULL)))
  )
  expect_s3_class(nelson_method(in_data = df_1_in), "tbl_df")
})

rm(df_1_in, df_1_exp, df_2_in, df_2_exp)
