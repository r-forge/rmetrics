\name{window}
\alias{window}

\alias{window.timeDate}
\alias{cut.timeDate}


\title{Time windows}

\description{
    
  Extract the subset of a \code{"timeDate"} object observed between two
  time stamps.
       
}

\usage{
\method{window}{timeDate}(x, start , end, \dots)

\method{cut}{timeDate}(x, from , to, \dots)
}

\arguments{
  
  \item{x}{
    an object of class \code{"timeDate"}.
  }
  \item{start, end}{
    starting date, required, and end date, optional. If supplied 
    \code{to} must be after \code{from}.
  }
  \item{from, to}{
    starting date, required, and end date, optional. If supplied 
    \code{to} must be after \code{from}.
  }
  \item{\dots}{
    arguments passed to other methods.
  }         
}


\value{
  an object of class \code{"timeDate"}
}

\note{

  % :TODO:
  
  The method for \code{cut} has been discouraged in the sources for a
  long time (with a recommendation to use \code{window}). It was
  officially deprecated in v4032.109 and will be removed or replaced by
  a method that is consistent with the methods for \code{cut} in base R,

}

\examples{  
## timeCalendar
# monthly dates in current year
tS = timeCalendar()
tS 
   
## window
# 2nd quarter window:
tS[4:6]
window(tS, tS[4], tS[6])
}

\keyword{chron}
