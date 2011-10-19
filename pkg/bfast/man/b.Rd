\name{b}
\Rdversion{1.4}
\alias{b}
\docType{data}
\title{
16-day NDVI time series from the south of Somalia
}
\description{
\code{b} is a dataframe containing time and two NDVI time series to illlustrate how the monitoring approach works.
}

\usage{data(b)}

\source{
Needs to be added.
}

\examples{
## first define the data as a regular time series (i.e. ts object)
NDVI <- as.ts(zoo(b$NDVI.b,b$Time))
plot(NDVI)

## References
citation("bfast")
}
\keyword{datasets}
