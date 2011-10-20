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
  \item{start}{ The start data of the monitoring period.
}
  \item{formula}{ The formula of the model to be used to fit the 'stable history period'. The default model is 'trend+harmon' where the order of the harmonic model is defined by the order variable below. Other models that could be used are e.g. response ~ 1 (just an intercept) or response ~ trend (just a linear trend).
}
  \item{order}{ The order of the harmonic model if used in the model formula.
}
  \item{lag}{
}
  \item{slag}{
}
  \item{history}{ The method to be used for automatically defining the stable history period. "ROC" is used our working paper. The start of the stable history period can also be set manually by e.g. history = c(2000, 1). See example below.
}
  \item{h}{ The minimal segment size between potentially detected breaks in the trend model given as fraction relative to the sample size.
}
  \item{end}{ (only used for MOSUM/ME processes). Maximum time (relative to the history period) that will be monitored. Default is 10 times the history period.
}
  \item{level}{ Significance level of the test, i.e., probability of type I error.
}
  \item{hpc}{A character specifying the high performance computing support. Default is "none", can be set to "foreach". Install the "foreach" package for hpc support.
}	
  \item{verbose}{ If TRUE, signal breaks by text output.
}
  \item{plot}{ If TRUE, plot results.
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

## See Fig. 6 a and b in our \url{http://eeecon.uibk.ac.at/wopec2/repec/inn/wpaper/2011-18.pdf}
## for more information about the data time series and acknowledgements

NDVIa <- as.ts(zoo(som$NDVI.a,som$Time))
plot(NDVIa)
## apply the bfast monitor function on the data 
## start of the monitoring period is c(2010,13) 
## and the ROC method is used as a method to automatically identify a stable history
mona <- bfastmonitor(NDVIa,start=c(2010,13),verbose=TRUE,plot=TRUE, history='ROC')

## Summary of the fitted seasonal trend model
summary(mona$model)

## the pattern in the running mean of residuals
## this illustrates the empirical fluctuation process
## and the significance of the detected break.
plot(mona$mefp, functional = NULL)

NDVIb <- as.ts(zoo(som$NDVI.b,som$Time))
plot(NDVIb)
monb <- bfastmonitor(NDVIb,start=c(2010,13),verbose=TRUE,plot=TRUE, history='ROC')
## Summary of the fitted seasonal trend model
summary(monb$model)
## the pattern in the running mean of residuals
## this illustrates the empirical fluctuation process
## and the significance of the detected break.
plot(monb$mefp, functional = NULL)

## set the stable history period manually and use a 4th order harmonic model
bfastmonitor(NDVIb, start = c(2010,13), history = c(2008,7), order= 4, plot=TRUE)

## just use a 6th order harmonic model
mon <- bfastmonitor(NDVIb , formula = response ~ harmon, 
    start = c(2010,13), order= 6, plot=TRUE)
summary(mon$model)

## References
citation("bfast")

## For more info
?bfastmonitor

}
\keyword{ts}

