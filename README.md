# ShinyBaseball

A collection of functions to illustrate the use of Shiny apps in baseball research.

A general overview of this package can be found at

https://bayesball.github.io/Introduction_to_ShinyBaseball.html

This package depends on the following packages that should be installed first.

shiny, ggplot2, dplyr, stringr, tidyr

To install the ShinyBaseball package, use the install_github() function from the remotes package:

library(remotes)

install_github("bayesball/ShinyBaseball")

Here are the main two Shiny functions:

BrushingZone()

Shiny app to illustrate brushing to explore hit, home run, and launch speed measurements over the zone (2019 data)

![GitHub Logo](/images/brushingzone.png)

PitchOutcome()

Shiny app to illustrate pitch outcomes using 2019 Statcast data for pitchers

![GitHub Logo](/images/pitchoutcome.png)


