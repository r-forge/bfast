\name{plot.seasonalbreaks}
\Rdversion{1.1}
\alias{plot.seasonalbreaks}

\title{
Methods for objects of class "seasonalbreaks".
}
\description{
Plot methods for objects of class "seasonalbreaks".
}
\usage{
%%## S3 method for class 'seasonalbreaks':
\method{plot}{seasonalbreaks}(x, realdata = TRUE, sim = NULL, type = c("data", "final", "components", "acf", "largest", "all", "extra"), title = TRUE, ...)
}

\arguments{
  \item{x}{ \code{\link[bfast]{seasonal.breaks}} object 
  }
  \item{realdata}{ TRUE is it is real data (i.e. univariate time series) FALSE if it is a simulated time series.
  It that case provide and 'stl' object to the sim variable 
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
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
## See \code{\link[bfast]{seasonal.breaks}} for examples.
}

\keyword{ts}

