\name{dayOfWeek}
\alias{dayOfWeek}


\title{Day of the week}

\description{
    
  Returns the days of the week of the data in a \code{"timeDate"}
  object.
      
}


\usage{
dayOfWeek(x)
}


\arguments{
  
  \item{x}{
    an object of class \code{"timeDate"}.
  }  
}


\value{

  a character vector giving the days of the week corresponding to the
  elements of \code{x}. The names are in English, abbreviated to 
  three letters.

}


\seealso{
  \code{\link{dayOfYear}}
}


\examples{  
## timeCalendar -  
   tC = timeCalendar(2022)
       
## The days of the Year:
   dayOfWeek(tC)
}

\keyword{chron}
