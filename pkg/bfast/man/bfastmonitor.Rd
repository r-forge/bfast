\name{bfastmonitor}
\Rdversion{1.4}
\alias{bfastmonitor}
\title{
Near real-time disturbance detection within time-series data
}
\description{ A generic time series based disturbance detection approach by modelling stable historical behaviour to enable detection of abnormal changes within newly acquired data. Different models can be use to model the stable historical behavior. A harmonic seasonic season trend model is used as a default in the regresssion modelling.
}
\usage{
bfastmonitor(data, start, formula = response ~ trend + harmon, order = 3, lag = NULL, slag = NULL, history = c("ROC", "BP", "all"), h = 0.25, end = 10, level = 0.05, hpc = "none", verbose = FALSE, plot = FALSE)
}
\arguments{
  \item{data}{ A univariate time series to be analyzed. This should be an object of class "ts" with a frequency greater than one.
}
  \item{start}{
}
  \item{formula}{
}
  \item{order}{ The order of the harmonic model
}
  \item{lag}{
}
  \item{slag}{
}
  \item{history}{
}
  \item{h}{
}
  \item{end}{
}
  \item{level}{
}
  \item{hpc}{A character specifying the high performance computing support. Default is "none", can be set to "foreach". Install the "foreach" package for hpc support.
}	
  \item{verbose}{
}
  \item{plot}{
}
}
\details{To be completed.
}

\references{
Please refer to the working paper on \url{http://eeecon.uibk.ac.at/wopec2/repec/inn/wpaper/2011-18.pdf}
}

\author{
Achim Zeileis, Jan Verbesselt
}

\seealso{
\code{\link[strucchange]{breakpoints}} for more examples and background information about estimation of breakpoints in time series.
}
\examples{
require(zoo)
data(b)
## create a time series object from the data and plot
NDVI <- as.ts(zoo(b$NDVI.b,b$Time))
plot(NDVI)
## apply the bfast monitor function on the data 
## start of the monitoring period is c(2010,13) 
## and the ROC method is used as a method to automatically identify a stable history
bfastmonitor(NDVI,start=c(2010,13),verbose=TRUE,plot=TRUE, history='ROC')

## Another example with a different time series object
NDVI <- as.ts(zoo(b$NDVI.a,b$Time))
plot(NDVI)
## apply the bfast monitor function on the data 
## start of the monitoring period is c(2010,13) 
## and the ROC method is used as a method to automatically identify a stable history
#bfastmonitor(NDVI,start=c(2010,13),verbose=TRUE,plot=TRUE, history='ROC')

## More example will follow here
## e.g. manually set the history period

## Using rainfall data

## etc.


## References
citation("bfast")

## For more info
#?bfastmonitor

}
\keyword{ts}

