\name{difftimeDate}
\alias{difftimeDate}


\title{Difference of two 'timeDate' objects}

\description{
  Returns the difference of two 'timeDate' objects.
}

\usage{
difftimeDate(time1, time2, 
    units = c("auto", "secs", "mins", "hours", "days", "weeks"))  
}

\arguments{
  \item{time1, time2}{
    two objects objects of class \code{"timeDate"}.
  }
  \item{units}{
    a character string denoting the date/time units in which the 
    results are desired.
  }
}

\value{

  \code{difftimeDate}, takes a difference of two \code{"timeDate"}
  objects and returns an object of class \code{"difftime"} with an
  attribute indicating the units.

}

\examples{
## create character vectors
dts = c("1989-09-28", "2001-01-15", "2004-08-30", "1990-02-09")
dts
   
## timeDate
GMT = timeDate(dts, zone = "GMT", FinCenter = "GMT") 
GMT

## diff
# suitably lagged and iterated differences
difftimeDate(GMT[1:2], GMT[-(1:2)])  
}

\keyword{chron}
