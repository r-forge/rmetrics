\name{diff}
\alias{diff}

\alias{diff.timeDate}


\title{Lagged 'timeDate' differences}

\description{
  Returns suitably lagged and iterated differences.
}

\usage{
\method{diff}{timeDate}(x, lag = 1, differences = 1, \dots)  
}

\arguments{
  \item{x}{
    an object of class \code{"timeDate"}.
  }  
  \item{lag}{
    an integer indicating which lag to use.  
  }
  \item{differences}{
    an integer indicating the order of the difference.
  }
  \item{\dots}{
    arguments passed to other methods.
  }
}

\value{
    
  if \code{x} is a vector of length \code{n} and \code{differences=1},
  then the computed result is equal to the successive differences
  \code{x[(1+lag):n] - x[1:(n-lag)]}.  If \code{difference} is larger
  than one this algorithm is applied recursively to \code{x}. Note that
  the returned value is a vector which is shorter than \code{x}.
    
}

\seealso{
  \code{\link{difftimeDate}} for the difference of two \code{"timeDate"} objects.
}

\examples{
## create character vectors
dts <- c("1989-09-28", "2001-01-15", "2004-08-30", "1990-02-09")
tms <- c(  "23:12:55",   "10:34:02",   "08:30:00",   "11:18:23")
## timeDate
GMT <- timeDate(dts, zone = "GMT", FinCenter = "GMT") + 24*3600
GMT

## suitably lagged and iterated differences
diff(GMT)
diff(GMT, lag = 2)
diff(GMT, lag = 1, diff = 2)
}

\keyword{chron}
