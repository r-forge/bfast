\name{bfast}
\Rdversion{1.1}
\alias{bfast}
\docType{package}
\title{
BFAST (Breaks For Additive Seasonal and Trend)
}

\description{
The BFAST package models the iterative decomposition of 
time series into trend, seasonal and remainder components 
with break detection in the decomposed components of the 
time series.
}

\details{
The package contains:
\itemize{
\item a function that does the iterative decomposition and break detection as described in Verbesselt et al (in press);
\item functions for plotting and printing the resulting decomposition;
\item \code{simts}: an example data set (simts);
\item \code{harvest}: an NDVI time series of a P. radiata plantation that is harvested (harvest).
}
}

\author{
Jan Verbesselt and Rob Hyndman
}

\references{
Verbesselt, J., R. Hyndman, G. Newnham, and D. Culvenor (In Press). 
Detecting trend and seasonal changes in satellite image time series. 
Remote Sensing of Environment. DOI: 10.1016/j.rse.2009.08.014. 
Or see \url{http://robjhyndman.com/papers/bfast1}.
}

\keyword{ts}
