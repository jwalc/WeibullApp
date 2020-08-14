# Basic Weibull Analysis

## Weibull Quantile Estimation
All estimator functions take in dplyr::tibble objects with at least two columns:
  - one time column,
  - one event column: 0 = Time to failure, 1 = Survival.
If only one column or a vector is given, the estimators assume it to be the time
column and all events to be 0 (so no survival data is given).
All estimator functions will return a tibble object with at least the time,
event, rank and quantile columns. These will be added to the given tibble, if
more columns are given initially.
