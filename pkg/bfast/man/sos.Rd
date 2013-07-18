\name{sos}
\alias{sos}
\title{A function to derive the start of season}

\description{
A function to extract the start of the season from time series (e.g. NDVI).
The function is prepared by Eliakim Hamunyela and Wiecher Olthof, Wageningen University.
}

\usage{
sos(x, current_year, compInterval = 15, p = 4, n = 21, 
plot = TRUE, rmdips = TRUE)
}

\arguments{
  \item{x}{ a regular time series object, ts
}
  \item{current_year}{ The year of which you want to derive the start of the season
}
  \item{p}{ Savitsky-golay input variable
}
  \item{n}{ Savitsky-golay input variable
}

  \item{compInterval}{ The interval between measurements, with remote sensing data it is referred to as the compositing interval
}	
  \item{plot}{ Logical, if true visualise the result and output
}
  \item{rmdips}{ Logical (true/false), if TRUE then clouds spikes are removed
}

}

\details{ This is a function created with the framework of an MSc thesis}

\value{
    \item{realstart}{ Estimated start of the season}
}

\author{
Eliakim Hamunyela, Wiecher Olthof, and Jan Verbesselt
}

\examples{
  require(bfast)
  require(signal)  
  plot(ndvi) ## gimms ndvi time series
  sos(ndvi, current_year = 2006, rmdips = TRUE, plot = TRUE)
	
}

\keyword{ts}
