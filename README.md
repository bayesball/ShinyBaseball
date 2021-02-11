# ShinyBaseball

A collection of functions to illustrate the use of Shiny apps in baseball research.

This package depends on the following packages that should be installed first.

shiny, ggplot2, dplyr, stringr, tidyr

To install, use the install_github() function from the remotes package:

library(remotes)
install_github("bayesball/ShinyBaseball")

The main two Shiny functions:

BrushingZone()

Shiny app to illustrate brushing to explore hit, home run, and launch speed measurements over the zone (2019 data)

PitchOutcome()

Shiny app to illustrate pitch outcomes using 2019 Statcast data for pitchers

