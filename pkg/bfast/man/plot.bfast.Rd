\name{plot.bfast}
\Rdversion{1.1}
\alias{plot.bfast}

\title{
Methods for objects of class "bfast".
}
\description{
Plot methods for objects of class "bfast".
}
\usage{
%%## S3 method for class 'bfast':
\method{plot}{bfast}(x, realdata = TRUE, sim = NULL, 
  type = c("data", "final", "components", "acf", "largest", "all", "extra"), 
  title = TRUE, ...)
}

\arguments{
  \item{x}{ \code{\link[bfast]{bfast}} object 
  }
  \item{realdata}{TRUE if it is real data (i.e. univariate time series); 
   FALSE if it is a simulated time series. It that case provide and 'stl' object to the sim variable 
  }
  \item{sim}{ \code{\link[stats]{stl}} object if realdata = TRUE 
  }
  \item{type}{ Indicates the type of plot
  }
  \item{title}{
  }
 \item{...}{ further arguments passed to or from other methods.}
}

\details{
This function creates various plots to demonstrate the results of a bfast decomposition. 
The type of plot shown depends on the value of \code{final}.
}

\author{
Jan Verbesselt
}

\examples{
## See \code{\link[bfast]{bfast}} for examples.
}

\keyword{ts}

