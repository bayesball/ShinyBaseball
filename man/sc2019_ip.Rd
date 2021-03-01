\name{sc2019_ip}
\alias{sc2019_ip}

\title{
  2019 Statcast In-Play Data
}
\description{
  Statcast measurements on balls in-play for 2019 season
}
\usage{
  sc2019_ip
}
\format{
   A data frame with 125,751 observations on the following 11 variables.
  \describe{
  \item{player_name}{name of hitter}
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
  }
}
\source{
Basball Savant
}


