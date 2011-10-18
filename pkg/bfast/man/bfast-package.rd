\name{bfast-package}
\alias{bfast-package}
\Rdversion{1.4}
\docType{package}
\title{
BFAST (Breaks For Additive Seasonal and Trend)
}

\description{BFAST integrates the decomposition of time series into trend, seasonal, and remainder 
components with methods for detecting and characterizing abrupt changes within the trend and seasonal components.
BFAST can be used to analyze different types of satellite image time series and can be applied 
to other disciplines dealing with seasonal or non-seasonal time series,such as hydrology, 
climatology, and econometrics. The algorithm can be extended to label detected changes with
information on the parameters of the fitted piecewise linear models. \cr
BFASTmonitor function is added to the package to monitor for disturbance at the end of time series (i.e. in near real-time) by modelling stable historical behaviour to enable detection of abnormal changes within newly acquired data. Different models can be use to model the stable historical behavior. A harmonic seasonic season trend model is used as a default in the regresssion modelling.
}

\details{The package contains:
\itemize{
\item \code{\link[bfast]{bfast}}: the main function that does the iterative decomposition and break detection as described 
    in Verbesselt et al (2010);
\item \code{\link[bfastmonitor]{bfast}}: the monitoring approach to provides functionality to detect disturbances in near real-time.
    in Verbesselt et al (Review);
\item functions for plotting and printing  \code{\link[bfast]{bfast}};
\item \code{\link[bfast]{simts}}: an example data set;
\item \code{\link[bfast]{harvest}}: an NDVI time series of a P. radiata plantation that is harvested.
\item \code{\link[bfast]{NDVISomalia}}: two NDVI time series of a location in the south of Somalia to illustrate the near real-time disturbance approach
}
}

\author{
Jan Verbesselt, Achim Zeileis and Rob Hyndman
}

\references{
Verbesselt, J., R. Hyndman, G. Newnham, and D. Culvenor (2009). 
Detecting trend and seasonal changes in satellite image time series. 
\emph{Remote Sensing of Environment}. \url{http://dx.doi.org/10.1016/j.rse.2009.08.014}. 
Or see \url{http://robjhyndman.com/papers/bfast1}.
Or see \url{http://eeecon.uibk.ac.at/wopec2/repec/inn/wpaper/2011-18.pdf}.
}

\keyword{ts}
