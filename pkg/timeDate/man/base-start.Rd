\name{start}
\alias{start}

\alias{end}

\alias{start.timeDate}
\alias{end.timeDate}

\alias{min.timeDate}
\alias{max.timeDate}

\alias{range.timeDate}


\title{Terminal times and range}


\description{
    
  Extracts the time when the first or last observation was taken, or
  computes the range of the dates in a \code{"timeDate"} object.
    
}


\usage{
\method{start}{timeDate}(x, \dots)

\method{end}{timeDate}(x, \dots)

\method{min}{timeDate}(\dots, na.rm = FALSE)

\method{max}{timeDate}(\dots, na.rm = FALSE)

\method{range}{timeDate}(\dots, na.rm = FALSE)
}


\arguments{
  
  \item{x}{
    an object of class \code{"timeDate"}.
  }
  \item{\dots}{
    ignored by \code{start} and \code{end}; 
    a 'timeDate' object for \code{min}, \code{max}, and \code{range}.
  } 
  \item{na.rm}{
    not used.
  }
}


\details{

  Conceptually, the \code{"timeDate"} object is sorted before the
  computations. In particular, \code{start} is not necessarilly the
  first element of the object and similarly for the other functions.

  \code{min} and \code{max} are equivalent to \code{start} end
  \code{end}, respectively.

  \code{range} returns the earlies and the latest times in a
  \code{"timeDate"} object. The remaining functions return only one of
  them, as suggested by their names.
    
}


\value{
  an object of class \code{"timeDate"}
}


\examples{  
## timeCalendar - 
   # Random Calendar Dates:
\dontshow{set.seed(1234)}
   tR = sample(timeCalendar())
   sort(tR)
   tR
    
## start | end  -
   start(tR)
   end(tR)
   
## The First and Last Time Stamp:
   tR[1]
   tR[length(tR)]
   rev(tR)[1]
   
## The Range:
   c(start(tR), end(tR))
   range(tR)
}

\keyword{chron}
