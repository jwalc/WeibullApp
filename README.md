# WeibullApp v0.1

## About this project
In this project I try to implement a few methods for Weibull parameter
estimation. At the heart of it all are estimation methods for quantiles of
the Weibull distribution function. These are used to estimate the parameters
of the two-parameter Weibull distribution function. The functionalities are
combined in a ShinyApp. Functionalities will include:
  - Choosing example data sets
  - reading in own data
  - choosing estimation methods for quantiles
  - visualization of a Weibull-paper plot
  - output of linear model to estimate parameters
  - plotting of Weibull distribution, density and failure rate

For a more detailed documentation please refere to the PDFs in the documentation
directory.
You are welcome to try the app using your own data but I can not guarantee the
accuracy of the results (at least for now). Testing is done by hand at the
moment but I plan to implement a testing suite in the future.

Any kind of feedback is highly appreciated!

## Whats next?
High priority:
  - [ ] update documentation
  - [ ] cleanup code
  - [ ] add example datasets
  - [x] tests for estimation functions

Less priority:
  - [ ] Parameter Estimation: Pretty print for model output
  - [x] Weibull Paper: option to download plots
  - [ ] Weibull Explorer: option to show plots with fitted values
  - [ ] Weibull Explorer: option to show data as points
  - [ ] Import Panels: option to delete user files
  - [ ] Import Panels: support input of single vector as time data
  - [ ] options to rename columns
  - [ ] change labels on plot
  - [x] Weibull Paper: user may change x- and y-limits of plot for "zooming"
  - [ ] Weibull Explorer: prettier version of failure rate plots with low values for T and b > 2

## Known Issues
These have no particular order
 - [x] mr_regression can not handle data with n_events != 1 properly
 - [x] failure rate plot sometimes displays weird jags
 - [x] Weibull Paper: legend disappears on plot when all methods are unselected
 - [x] Data Converter Module: can save data without converting it
 - [x] Data Converter Module: saves file after changing textInput and without clicking the save button
 - [ ] Data Picker: shows error message when no files are in data/user_data/

## How to contribute
For now I do not seek to have any code contributions since this project is
intended for (my) learning purposes. I do, however, appreciate any kind of
feedback.
