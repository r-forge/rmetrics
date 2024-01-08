\name{length}
\alias{length}

\alias{length.timeDate}


\title{Length of a 'timeDate' object}

\description{
  Returns the length of a \code{"timeDate"} object.
}

\usage{
\method{length}{timeDate}(x)
}

\arguments{
    
  \item{x}{
    an object of class \code{"timeDate"}.
  }
}

\value{
  an integer of length 1
}

\examples{  
## timeCalendar
tC = timeCalendar()
   
## length - 
length(tC)
}

\keyword{chron}
