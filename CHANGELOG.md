# Changelog

## [Unreleased]

### Added
- New tests for quantile estimators
- Added checks for input parameter and warnings to estimator functions
- Changing x- and y-axis on the plot on "Weibull Paper" panel
- Option to download the plot shown on the "Weibull Paper" panel
- When saving converted data, the app shows a message on the save status
- Change labels on Weibull Paper plot

### Changed
- Start using a projectwide standardised format for datasets
- Change estimator functions to use the standardised format
- Fix disappearing legend on Weibull-Plot
- Changed weibull_q_plot to fit new functionality
- Weibull Paper panel is a module now
- Fix dataConverterModule saving before pressing of "save" button
- Adds a workaround for the failure rate plot displaying weird jags

### Removed
- Remove append parameter from quantile estimator functions

## [0.1] - 2020-08-21

### Added
- methods for quantile estimation
- functions for making a Weibull-Plot using ggplot2
- ShinyApp and modules


[0.1]: https://github.com/jwalc/WeibullApp/releases/tag/v0.1
