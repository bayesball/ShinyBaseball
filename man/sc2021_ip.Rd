\name{sc2021_ip}
\alias{sc2021_ip}

\title{
  2021 Statcast In-Play Data
}
\description{
  Statcast measurements on balls in-play for 2021 season
}
\usage{
  sc2019_ip
}
\format{
   A data frame with 121,108 observations on the following 13 variables.
  \describe{
  \item{batter}{Statcast code of batter}
  \item{plate_x}{horizontal location of pitch}
  \item{plate_z}{vertical location of pitch}
  \item{H}{logical variable that is TRUE for a hit}
  \item{HR}{logical variable that is TRUE for a home run}
  \item{launch_speed}{launch speed (mph)}
  \item{launch_angle}{launch angle (degrees)}
  \item{estimated_ba}{estimated batting average from launch speed and angle}
  \item{hc_x}{landing location of ball in play - x}
  \item{hc_y}{landing location of ball in play - y}
  \item{stand}{side of batter}
  \item{if_fielding_alignment}{infield positioning of defense}
  \item{of_fielding_alignment}{outfield positioning of defense}
  }
}
\source{
Basball Savant
}


