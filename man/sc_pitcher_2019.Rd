\name{sc_pitcher_2019}
\alias{sc_pitcher_2019}

\title{
  2019 Statcast Pitch Data
}
\description{
  Statcast pitch measurements for 2019 season
}
\usage{
  sc_pitcher_2019
}
\format{
   A data frame with 732,473 observations on the following 11 variables.
  \describe{
  \item{pitcher}{pitcher id}
  \item{batter}{batter id}
  \item{pitch_type}{code for pitch type}
  \item{plate_x}{horizontal location of pitch}
  \item{plate_z}{vertical location of pitch}
  \item{p_throws}{arm of pitcher}
  \item{stand}{side of batter}
  \item{type}{indicates if strike, ball, or in-play}
  \item{events}{outcome of the resulting plate appearance}
  \item{description}{description of the pitch result}
  \item{estimated_ba_using_speedangle}{estimated batting average from launch speed and angle}
  }
}
\source{
Basball Savant
}


