\name{bfastts}
\alias{bfastts}
\title{Create a regular time series object by combining data and date information}
\description{
  Work in progress. Also all the rest below is work in progress.
}

\usage{
bfastts(data, dates)
}

\arguments{
  \item{data}{A data vector }
  \item{dates}{Optional input of dates for each measurement in the 'data' variable. In 
    case the data is a irregular time series, a vector with 'dates' for 
  	each measurement can be supplied using this 'dates' variable. The irregular data will be linked
  	with the dates vector to create daily regular time series with a frequency = 365. Extra days
  	in leap years might cause problems. Please be carefull using this option as it is 
  	experimental. Feedback is welcome. 
  	}
}

\details{
  \code{bfastts} provides...
}

\value{
  \code{bfastts} returns an object of class \code{"bfastts"}, i.e., a list with components as follows.
  \item{data}{original \code{"ts"} time series,}
}

\references{
Verbesselt J, Zeileis A, Herold M (2011).
  Near Real-Time Disturbance Detection in Terrestrial Ecosystems Using Satellite
  Image Time Series: Drought Detection in Somalia.
  Working Paper 2011-18. Working Papers in Economics and Statistics,
  Research Platform Empirical and Experimental Economics, Universitaet Innsbruck.
  \url{http://EconPapers.RePEc.org/RePEc:inn:wpaper:2011-18}.
  Submitted to Remote Sensing and Environment.
}

\author{Achim Zeileis, Jan Verbesselt}

\seealso{\code{\link[strucchange]{monitor}}, \code{\link[strucchange]{mefp}}, \code{\link[strucchange]{breakpoints}}}

\examples{
 # 	When dealing with irregular time seris use the na.rm = TRUE for plotting to remove days
 # 	for which no measurements are available
}
\keyword{ts}

