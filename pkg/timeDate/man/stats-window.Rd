\name{window}
\alias{window}

\alias{window.timeDate}
\alias{cut.timeDate} % the method was removed on 2025-10-20 but keep
		     % this for now.

\title{Time windows}

\description{
    
  Extract the subset of a \code{"timeDate"} object observed between two
  time stamps.
       
}

\usage{
\method{window}{timeDate}(x, start , end, \dots)
}

\arguments{
  \item{x}{
    an object of class \code{"timeDate"}.
  }
  \item{start, end}{
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
