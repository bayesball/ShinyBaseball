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
   A data frame with 125,751 observations on the following 7 variables.
  \describe{
  \item{player_name}{name of hitter}
  \item{plate_x}{horizontal location of pitch}
  \item{plate_z}{vertical location of pitch}
  \item{H}{logical variable that is TRUE for a hit}
  \item{HR}{logical variable that is TRUE for a home run}
  \item{launch_speed}{launch speed (mph)}
  \item{estimated_ba}{estimated batting average from launch speed and angle}
  }
}
\source{
Basball Savant
}


