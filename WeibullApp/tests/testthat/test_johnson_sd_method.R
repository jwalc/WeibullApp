df_1_in <- tibble(time = rep(c(1,2,3,4,5,6), 5),
                  event = c(rep(1,6), rep(0,24)),
                  n_events = rep(1,30),
                  sample = rep(c("A", "B", "C", "D", "E", "F"), 5))

df_1_exp <- tibble(time = seq(1,6),
                   F_i = c(7/304, 241/3952, 2937/27664, 8999/55328,
                           0.2396682265, 0.3680349256),
                   method = rep("Sudden Death", 6))

df_2_in <- tibble(time = c(1,1,2,2,3,3),
                  event = c(1,0,1,0,1,0),
                  n_events = c(1,4,1,2,1,3),
                  sample = c("A", "A", "B", "B", "C", "C"))

df_2_exp <- tibble(time = c(1,2,3),
                   F_i = c(7/124, 11/62, 43/124),
                   method = rep("Sudden Death", 3))

df_3_in <- tibble(time = c(1,2,3),
                  event = c(1,1,1),
                  n_events = c(1,2,1),
                  sample = c("A","B","C"))

# --- Testing correct computation --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
testthat::context("johnson_sd_method: correct computation")

testthat::test_that("Computation", {
  expect_equal(johnson_sd_method(df_1_in), df_1_exp)
  expect_equal(johnson_sd_method(df_1_in, n_events = "n_events"), df_1_exp)
  expect_equal(johnson_sd_method(df_2_in, n_events = "n_events"), df_2_exp)
})

# --- Testing expected output types --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
testthat::context("johnson_sd_method: expected output types")

testthat::test_that("Output types", {
  expect_warning(
    expect_true(is.null(johnson_sd_method(in_data = tibble::tibble(time = NULL))))
  )
  expect_warning(
    expect_true(is.null(johnson_sd_method(in_data = NULL)))
  )
  expect_warning(
    expect_true(is.null(johnson_sd_method(in_data = tibble(time = NULL, event = NULL, n_events = NULL))))
  )
  expect_s3_class(johnson_sd_method(in_data = df_1_in), "tbl_df")
  expect_warning(
    expect_true(is.null(johnson_sd_method(in_data = df_1_in, time = "Zeit", sample = "Versuch")))
  )
  expect_warning(
    expect_true(is.null(johnson_sd_method(in_data = df_3_in, n_events = "n_events")))
  )
})


rm(df_1_in, df_1_exp, df_2_in, df_2_exp)
