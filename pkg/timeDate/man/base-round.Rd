\name{round}
\alias{round}

\alias{round.timeDate}

\alias{trunc}
\alias{trunc.timeDate}


\title{Rounding and truncating 'timeDate' objects}

\description{
    
  Rounds and truncates objects of class 'timeDate'.  
       
}

\usage{
\method{round}{timeDate}(x, digits = c("days", "hours", "mins")) 

\method{trunc}{timeDate}(x, units = c("days", "hours", "mins"), \dots)     
}

\arguments{
  \item{digits, units}{
    a character string denoting the date/time units in which the 
    results are desired.
  }   
  \item{x}{
    an object of class \code{"timeDate"}.
  }  
  \item{\dots}{
    arguments passed to other methods.
  }
}

\details{
    
  The two functions \code{round} and \code{trunc} allow to round or to
  truncate \code{"timeDate"} objects to the specified unit and return
  them as \code{"timeDate"} objects.

  There is an inconsistency in that \code{round} uses \code{digits} as
  argument and not \code{units}.

}

\value{
  an object of class \code{"timeDate"}
}

\examples{
## round

## truncate
}

\keyword{chron}
