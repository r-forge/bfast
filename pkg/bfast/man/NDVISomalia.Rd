\name{NDVISomalia}
\Rdversion{1.4}
\alias{NDVISomalia}
\docType{data}
\title{
16-day NDVI time series from the south of Somalia
}
\description{
\code{NDVISomalia} is a dataframe containing time and two NDVI time series to illlustrate how the monitoring approach works.
}

\usage{data(NDVISomalia)}

\source{
Verbesselt, J., R. Hyndman, G. Newnham, and D. Culvenor (2009). 
Detecting trend and seasonal changes in satellite image time series. 
\emph{Remote Sensing of Environment}. \url{http://dx.doi.org/10.1016/j.rse.2009.08.014}. 
}

\examples{
## first define the data as a regular time series (i.e. ts object)
NDVI <- as.ts(zoo(NDVISomalia$NDVI.b,NDVISomalia$Time))
plot(NDVI)

## References
citation("bfast")
}
\keyword{datasets}
