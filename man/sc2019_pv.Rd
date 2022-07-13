\name{sc2019_pv}
\alias{sc2019_pv}

\title{
  2019 Statcast Pitch Value Data
}
\description{
  Pitch value measurements for all pitches in 2019 season
}
\usage{
  sc2019_pv
}
\format{
   A data frame with 350,000 observations on the following 8 variables for the 2019 season.
  \describe{
  \item{pitch_type}{pitch type code}
  \item{plate_x}{horizontal location of pitch}
  \item{plate_z}{vertical location of pitch}
  \item{stand}{batting side of hitter}
  \item{p_throws}{side of pitching arm}
  \item{Count}{balls and strikes}
  \item{new_Count}{new count or end of pa event}
  \item{Pitch Value}{value of the pitch transition}
  }
}
\source{
Basball Savant
}


