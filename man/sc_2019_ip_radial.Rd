\name{sc_2019_ip_radial}
\alias{sc_2019_ip_radial}

\title{
  2019 Statcast In-Play Data for Radial Chart
}
\description{
  Statcast in-play measurements for 2019 season for construction of a radial chart
}
\usage{
  sc2019_ip_radial
}
\format{
   A data frame with 125,751 observations on the following 8 variables.
  \describe{
  \item{game_date}{game date}
  \item{pitcher}{pitcher id}
  \item{launch_angle}{launch angle}
  \item{launch_speed}{exit velocity}
  \item{estimated_ba}{estimated batting average based on values of launch angle and exit velocity}
 \item{events}{outcome of the resulting plate appearance}
\item{HR}{indicates if a home run was hit (1) or not (0)}
\item{H}{indicates if a hit was recorded (1) or not (0)
}
\source{
Basball Savant
}


