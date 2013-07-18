\name{removedips}
\alias{removedips}
\title{A function to remove dips - spikes from the data}

\description{
A function to extract "dips" from a time series i.e. typical negative spikes within remotely sensed time series representing clouds.
}

\usage{
removedips(x)
}

\arguments{
  \item{x}{ a numeric vector containing data values typical for a NDVI time series.
}
}

\details{To be completed.}

\value{
    \item{y}{ a vector without clouds spikes}
}

\author{
Eliakim Hamunyela, Wiecher Olthof, and Jan Verbesselt
}

\examples{
  require(bfast)
  y <- as.ts(zoo(som$NDVI.a, som$Time))
  test <- removedips(y)
  plot(y, type = "l", col = "grey", lwd = 2)
  lines(test, col = "red")
}

\keyword{ts}
