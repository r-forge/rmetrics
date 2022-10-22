\name{dayOfYear}
\alias{dayOfYear}


\title{Day of the year}

\description{
    
  Returns the day of the year from a \code{"timeDate"} object.
      
}


\usage{
dayOfYear(x)
}

\arguments{
  
  \item{x}{
    an object of class \code{"timeDate"}.
  }  
       
}


\value{

  the day count as integer value starting January, 1st

}


\seealso{
  \code{\link{dayOfWeek}}
}


\examples{  
## timeCalendar -  
   tC = timeCalendar()
       
## The days of the Year:
   dayOfYear(tC)
}

\keyword{chron}
