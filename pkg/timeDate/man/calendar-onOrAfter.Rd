\name{onOrAfter}
\alias{onOrAfter}

\alias{onOrBefore}

\alias{timeNdayOnOrAfter}
\alias{timeNdayOnOrBefore}


\title{On-or-after/before dates}

\description{
    
  Compute the date that is a "on-or-after" or "on-or-before" n-day.
    
}

\usage{
timeNdayOnOrAfter(charvec, nday = 1, format = "\%Y-\%m-\%d", 
    zone = "", FinCenter = "")
    
timeNdayOnOrBefore(charvec, nday = 1, format = "\%Y-\%m-\%d", 
    zone = "", FinCenter = "")
}

\arguments{
  
  \item{charvec}{
    a character vector of dates and times.
  }
  \item{nday}{
    an integer vector with entries ranging from 
    \code{0} (Sunday) to \code{6} (Saturday).
  }
  \item{format}{
    the format specification of the input character vector.
  }
  \item{zone}{
    the time zone or financial center where the data were recorded.
  }      
  \item{FinCenter}{
    a character with the location of the  
    financial center named as "continent/city". 
  }
         
}

\details{
    
  \code{timeNdayOnOrAfter} returns the date in the specified month that
  is a n-day (e.g. Sunday) on or after the given date. Month and date
  are given through argument \code{charvec}.
    
  The function \code{timeNdayOnOrBefore} returns the date that is a
  n-day on or before the given date.

}

\value{
  an object of class \code{"timeDate"}
}

\examples{
## date as character string
charvec = "2006-04-16"
   
## timeNdayOnOrAfter
# What date has the first Monday on or after March 15, 1986?
timeNdayOnOrAfter("1986-03-15", 1)
    
## timeNdayOnOrBefore
# What date has Friday on or before April 22, 1977?
timeNdayOnOrBefore("1986-03-15", 5)
}

\keyword{chron}
