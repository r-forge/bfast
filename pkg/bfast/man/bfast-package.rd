\name{bfast-package}
\Rdversion{1.1}
\alias{bfast-package}
\docType{package}
\title{
BFAST (Breaks For Additive Seasonal and Trend)
}

\description{BFAST integrates the decomposition of time series into trend, seasonal, and remainder 
components with methods for detecting and characterizing abrupt changes within the trend and seasonal components.
BFAST can be used to analyze different types of satellite image time series and can be applied 
to other disciplines dealing with seasonal or non-seasonal time series,such as hydrology, 
climatology, and econometrics. The algorithm can be extended to label detected changes with
information on the parameters of the fitted piecewise linear models.
}

\details{
The package contains:
\itemize{
\item \code{\link[bfast]{bfast}}: the main function that does the iterative decomposition and break detection as described 
    in Verbesselt et al (in press);
\item functions for plotting and printing the resulting decomposition obtained from \code{bfast};
\item \code{\link[bfast]{simts}}: an example data set (simts);
\item \code{\link[bfast]{harvest}}: an NDVI time series of a P. radiata plantation that is harvested (harvest).
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
