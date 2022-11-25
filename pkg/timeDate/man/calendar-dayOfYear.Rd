\name{dayOfYear}
\alias{dayOfYear}


\title{Day of the year}

\description{
    
  Returns the days of the year of the data in a \code{"timeDate"}
  object.
      
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

  vector of integers representing the number of days since the beginning
  of the year. For January, 1st it is one.

}


\seealso{
  \code{\link{dayOfWeek}}
}


\examples{  
## timeCalendar -  
   tC = timeCalendar(2022)
       
## The days of the Year:
   dayOfYear(tC)
}

\keyword{chron}
