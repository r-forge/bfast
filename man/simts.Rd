\name{simts}
\Rdversion{1.1}
\alias{simts}
\docType{data}
\title{
Simulated seasonal 16-day NDVI time series
}
\description{
Data (simts) is an object of class "stl" and consists out of a seasonal, trend (equals = 0), and a noise component (simulated noise typical for remotely sensed satellite data).
}

\usage{data(simts)}

\source{
Verbesselt, J., R. Hyndman, G. Newnham, and D. Culvenor (In Press). Detecting trend and seasonal changes in satellite image time series. Remote Sensing of Environment.DOI: 10.1016/j.rse.2009.08.014. Or see \url{http://robjhyndman.com/papers/detecting-trend-and-seasonal-changes-in-satellite-image-time-series}.
}

\examples{
data(simts)
plot(simts)
}
\keyword{datasets}