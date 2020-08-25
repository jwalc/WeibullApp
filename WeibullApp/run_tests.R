library(testthat)
library(covr)
library(tidyverse)

source("R/mr_regression.R")
source("R/nelson_method.R")
source("R/kaplan_meier_method.R")
source("R/johnson_method.R")
source("R/johnson_sd_method.R")
source("R/quantile_estimators.R")

testthat::test_dir("tests/testthat/", reporter = "summary")

coverage <- covr::file_coverage(
  source_files = c("R/mr_regression.R",
                   "R/nelson_method.R",
                   "R/kaplan_meier_method.R",
                   "R/johnson_method.R",
                   "R/johnson_sd_method.R"),
  test_files = c("tests/testthat/test_mr_regression.R",
                 "tests/testthat/test_nelson_method.R",
                 "tests/testthat/test_kaplan_meier_method.R",
                 "tests/testthat/test_johnson_method.R",
                 "tests/testthat/test_johnson_sd_method.R")
)

report(coverage)
