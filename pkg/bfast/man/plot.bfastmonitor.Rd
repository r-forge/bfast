\name{plot.bfast}
\Rdversion{1.4}
\alias{plot.bfastmonitor}

\title{
Methods for objects of class "bfastmonitor".
}
\description{
Plot methods for objects of class "bfastmonitor".
}
\usage{
%%## S3 method for class 'bfast':
\method{plot}{bfastmonitor}(x, main = TRUE, ylab = "Data", ...)
}

\arguments{
  \item{x}{ \code{\link[bfast]{bfast}} object }
  \item{main}{an overall title for the plot.}
  \item{ylab}{Indicates the type of plot. See details.}
  \item{...}{further arguments passed to the \code{\link[graphics]{plot}} function.}
}

\details{
This function creates a standard plots to demonstrate the results of a bfastmonitoring.
}

\author{
Jan Verbesselt, Achim Zeileis
}

\examples{
## See \code{\link[bfast]{bfastmonitor}} for examples.
# References
citation("bfast")
}

\keyword{ts}

