\name{seasonal.breaks}
\Rdversion{1.1}
\alias{seasonal.breaks}
\title{
Break detection in the seasonal and trend component of time series
}
\description{
Iterative break detection in seasonal and trend component of time series. Seasonal breaks is a function that combines the iterative decomposition of time series into a trend, seasonal, remainder component with significant break detection in the decomposed components of the time series.
}
\usage{
seasonal.breaks(Yt, h = 0.15, max.iter = NULL, breaks = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Yt}{ univariate time series to be analyzed. This should be an object of class "ts" with a frequency greater than one without NA's.
}
  \item{h}{ minimal segment size either given as fraction relative to the sample size or as an integer giving the minimal number of observations in each segment.
}
  \item{max.iter}{ maximum amount of iterations allowed for estimation of breakpoints in seasonal and trend component.
}
  \item{breaks}{ integer specifying the maximal number of breaks to be calculated. By default the maximal number allowed by h is used.
}
}
\details{

}
\value{
    An object of the class "seasonalbreaks" is a list with the following elements:
    \item{Yt}{ equals the Yt used as input.}
    \item{output}{ is a list with the following elements (for each iteration):}
    
        \item{Tt}{ the fitted trend component }
        \item{St}{ the fitted seasonal component }
        \item{Nt}{ the remainder}
        \item{Vt}{ equals Yt - St for each iteration}
        \item{bp.Vt}{ output of the \code{\link[strucchange]{breakpoints}} function for the trend model (see Reference)}
        \item{ci.Vt}{ output of the  \code{\link[strucchange]{confint}} function for the trend model (see Reference)}
        \item{Wt}{ equals Yt - Tt for each iteration}
        \item{bp.Vt}{ output of the \code{\link[strucchange]{breakpoints}} function for the seasonal model (see Reference)}
        \item{ci.Vt}{ output of the  \code{\link[strucchange]{confint}} function for the seasonal model (see Reference)}
    
    \item{nobp}{ is a list with the following elements:}
        \item{nobp.Vt}{ logical, TRUE if there are breakpoints detected}
        \item{nobp.Wt}{ logical, TRUE if there are breakpoints detected}
        
    \item{magnitude}{ magnitude of the biggest change detected in the trend component}
    \item{Time}{ timing of the biggest change detected in the trend component}
}
\references{
Verbesselt, J., R. Hyndman, G. Newnham, and D. Culvenor (In Press). Detecting trend and seasonal changes in satellite image time series. Remote Sensing of Environment.DOI: 10.1016/j.rse.2009.08.014. Or see \url{http://robjhyndman.com/papers/detecting-trend-and-seasonal-changes-in-satellite-image-time-series}.
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link[bfast]{plot.seasonalbreaks}} for plotting of seasonal.break() results. \cr
\code{\link[strucchange]{breakpoints}} for more examples and background information about estimation of breakpoints in time series.
}
\examples{
require(bfast)

# Load Simulated Data
data(simts) # Load simulated NDVI time series
plot(simts) # stl object

datats <- apply(simts$time.series,1,sum) # sum of all the components (season,abrupt,remainder)
tsp(datats) <- tsp(simts$time.series[,1]) # assign time series attributes
datats <- as.ts(datats)  # it is a time series
plot(datats)

fit <- seasonal.breaks(datats,h=0.15,max.iter=1)
plot(fit,realdata=FALSE,sim=simts,type="final",title=FALSE)

fit # prints out whether breakpoints are detected in the seasonal and trend component

# Load Real Data
# The data should be a regular ts() object without NA's
data(harvest)   # MODIS 16-day cleaned and interpolated NDVI time series 
# See Fig. 8 b in reference
plot(harvest, ylab='NDVI')

fit <- seasonal.breaks(harvest,h=0.15,max.iter=1,breaks=2)
plot(fit,realdata=TRUE,type="final",title=FALSE)

pdf("output.pdf") # check for png in getwd()
plot(fit,realdata=TRUE,type="all",title=FALSE) #plots all the intermediate outputs
dev.off()

}
\keyword{ts}

