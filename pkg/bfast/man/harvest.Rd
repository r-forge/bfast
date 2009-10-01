\name{harvest}
\Rdversion{1.1}
\alias{harvest}
\docType{data}
\title{
16-day NDVI time series for a Pinus radiata plantation.
}
\description{
Data (simts) is an object of class "ts".
}

\usage{data(harvest)}

\source{
Verbesselt, J., R. Hyndman, G. Newnham, and D. Culvenor (In Press). Detecting trend and seasonal changes in satellite image time series. Remote Sensing of Environment.DOI: 10.1016/j.rse.2009.08.014. Or see \url{http://robjhyndman.com/papers/detecting-trend-and-seasonal-changes-in-satellite-image-time-series}.
}

\examples{
data(harvest)
plot(harvest,ylab='NDVI')
}
\keyword{datasets}
\keyword{ts}
