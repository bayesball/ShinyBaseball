\name{sc2022ip}
\alias{sc2022ip}

\title{
  2022 Statcast In-Play Data
}
\description{
  Statcast measurements on balls in-play for 2022 season
}
\usage{
  sc2022ip
}
\format{
   A data frame with 101,975 observations on the following 6 variables.
  \describe{
  \item{batter}{Statcast code of batter}
   \item{launch_angle}{launch angle (degrees)}
  \item{launch_speed}{launch speed (mph)}
  \item{events}{description of inplay event}
  \item{estimated_ba_using_speedangle}{estimated batting average from launch speed and angle}
  \item{estimated_woba_using_speedangle}{estimated batting average from launch speed and angle}
  }
}
\source{
Baseball Savant
}


