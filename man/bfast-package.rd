\name{bfast}
\Rdversion{1.1}
%%\alias{bfast-package}
\alias{bfast}
\docType{package}
\title{
BFAST (Breaks For Additive Seasonal and Trend)
}
\description{
The BFAST package provides a function that integrate the iterative decomposition of time series into a trend, seasonal, remainder component with significant break detection in the decomposed components of the time series.
}
\details{
\tabular{ll}{
Package: \tab bfast\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2009-09-30\cr
License: \tab \cr
LazyLoad: \tab yes\cr
}

The package contains: \cr
- function that does the iterative decomposition and break detection as described in reference cited below \cr
- plot and print function \cr
- an example data set: Simulated NDVI time series (simts), and a NDVI time series of a P. radiata plantation that is harvested (harvest).  \cr
}
\author{
Jan Verbesselt and Rob Hyndman
}
\references{
Verbesselt, J., R. Hyndman, G. Newnham, and D. Culvenor (In Press). Detecting trend and seasonal changes in satellite image time series. \cr 
Remote Sensing of Environment. DOI: 10.1016/j.rse.2009.08.014.
}
\keyword{ts}
\seealso{
\code{\link[strucchange]{breakpoints}}
}
\examples{
# See \code{\link[bfast]{seasonal.breaks}} for examples.
}
