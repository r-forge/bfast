\name{seasonal}
\Rdversion{1.1}
\alias{seasonal}
\title{
A method 'seasonal' used within plot.seasonalbreaks() to provide 'plot.stl' looking plots with the extra option of overlaying a second 'stl' object.
}
\description{
The function is used within \code{\link[bfast]{plot.seasonalbreaks}} so it is not necessary to use this function for other purposes. \cr
Methods for objects of class stl, typically the result of \code{\link[stats]{plot.stl}}.
}
\usage{
seasonal(x, out, sim = NULL, labels = colnames(X), set.pars = list(mar = c(0, 6, 0, 6), oma = c(6, 0, 4, 0), tck = -0.01, mfrow = c(nplot, 1)), main = NULL, range.bars = FALSE, ..., col.range = "light gray")
}
\arguments{
  
  \item{x}{ \code{\link[stats]{stl}} object.
  }
  
  \item{out}{ \code{\link[bfast]{seasonal.breaks}} object.
  }
  
  \item{sim}{ \code{\link[stats]{stl}} object.
  }
  
  \item{labels}{ character of length 4 giving the names of the component time-series.
  }
  
  \item{set.pars}{ settings for par(.) when setting up the plot.
  }
  
  \item{main}{ plot main title.
  }
  
  \item{range.bars}{ logical indicating if each plot should have a bar at its right side which are of equal heights in user coordinates.
  }
  
  \item{...}{ further arguments passed to or from other methods.
  }
  \item{col.range}{ colour to be used for the range bars, if plotted. Note this appears after ... and so cannot be abbreviated.
  }
}

\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}

\seealso{
\code{\link[stats]{plot.stl}}
}

\keyword{ts}

