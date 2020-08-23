library(testthat)
library(covr)

source("R/mr_regression.R")
source("R/nelson_method.R")
source("R/quantile_estimators.R")

testthat::test_dir("tests/testthat/")

coverage <- covr::file_coverage(
  source_files = c("R/mr_regression.R",
                   "R/nelson_method.R"),
  test_files = c("tests/testthat/test_mr_regression.R",
                 "tests/testthat/test_nelson_method.R")
)

report(coverage)
