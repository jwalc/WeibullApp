df_1_in <- tibble(time = c(1,2,3,4),
                  event = c(1,0,1,0),
                  n_events = c(2,1,1,2))

df_1_exp <- tibble(time = c(1,3),
                   F_i = c(1/3, 5/9),
                   method = rep("Kaplan-Meier", 2))

df_2_in <- tibble(time = c(1,1,1,2,2,3),
                  event = c(1,1,0,1,0,1),
                  n_events = c(1,2,1,1,1,1))

df_2_exp <- tibble(time = c(1,2,3),
                   F_i = c(3/8, 17/32, 49/64),
                   method = rep("Kaplan-Meier", 3))

# --- Testing correct computation --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
testthat::context("kaplan_meier_method: correct computation")

testthat::test_that("Computation", {
  expect_equal(kaplan_meier_method(df_1_in), df_1_exp)
  expect_equal(kaplan_meier_method(df_2_in), df_2_exp)
})

# --- Testing expected output types --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
testthat::context("kaplan_meier_method: expected output types")

testthat::test_that("Output types", {
  expect_warning(
    expect_true(is.null(kaplan_meier_method(in_data = tibble::tibble(time = NULL))))
  )
  expect_warning(
    expect_true(is.null(kaplan_meier_method(in_data = NULL)))
  )
  expect_warning(
    expect_true(is.null(kaplan_meier_method(in_data = tibble(time = NULL, event = NULL, n_events = NULL))))
  )
  expect_s3_class(kaplan_meier_method(in_data = df_1_in), "tbl_df")
  expect_warning(
    expect_true(is.null(kaplan_meier_method(in_data = df_1_in, time = "Zeit", event = "Ausfall")))
  )
})


rm(df_1_in, df_1_exp, df_2_in, df_2_exp)

