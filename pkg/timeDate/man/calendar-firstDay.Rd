\name{firstDay}
\alias{firstDay}

\alias{lastDay}

\alias{timeFirstDayInMonth}
\alias{timeLastDayInMonth}

\alias{timeFirstDayInQuarter}
\alias{timeLastDayInQuarter}


\title{First and last days}

\description{
    
  Computes the first/last day in a given month/quarter.
    
}

\usage{
timeFirstDayInMonth(charvec, format = "\%Y-\%m-\%d", zone = "", 
    FinCenter = "")
timeLastDayInMonth(charvec, format = "\%Y-\%m-\%d", zone = "", 
    FinCenter = "")

timeFirstDayInQuarter(charvec, format = "\%Y-\%m-\%d", zone = "", 
    FinCenter = "")
timeLastDayInQuarter(charvec, format = "\%Y-\%m-\%d", zone = "", 
    FinCenter = "")
}

\arguments{
  \item{charvec}{
    a character vector or object from a class representing time,
    such as \code{"timeDate"}, \code{"POSIXlt"}, etc.
  }
  \item{format}{
    the format specification of the input character vector.
  }
  \item{zone}{
    the time zone or financial center where the data were recorded.
  }
  \item{FinCenter}{
    a character with the location of the financial center named as
    "continent/city".
  }
}

\details{

  The functions \code{timeFirstDayInMonth} and \code{timeLastDayInMonth}
  return the first or last day, respectively, in a given month and year.
    
  The same functionality for quarterly time horizons is provided by the
  functions \code{timeFirstDayInQuarter} and
  \code{timeLastDayInQuarter}.

  If argument \code{FinCenter} is missing or the empty string and the
  object is from a time-date class, it is taken from slot
  \code{"FinCenter"} (if charvec is \code{"timeDate"}) or from attribute
  \code{"tzone"} (if from another time-date class and not NULL). If all
  of this fails, \code{FinCenter} is obtained with
  \code{\link{getRmetricsOptions}}.

  If \code{zone} is missing or the empty string, it is set to (the
  deduced value for) \code{FinCenter}.

}

\value{
  an object of class \code{"timeDate"}
}

\seealso{
  \code{\link{trunc.timeDate}},

  \code{\link{timeFirstDayInMonth}},
  \code{\link{timeLastDayInMonth}},
  \code{\link{timeFirstDayInQuarter}},
  \code{\link{timeLastDayInQuarter}},
  
  \code{\link{timeNthNdayInMonth}},
  \code{\link{timeLastNdayInMonth}},

  \code{\link{timeNdayOnOrAfter}},
  \code{\link{timeNdayOnOrBefore}}
}

\examples{
## date as character string
charvec <- "2006-04-16"
myFinCenter <- getRmetricsOptions("myFinCenter")

## What date has the last day in a month for a given date?
timeLastDayInMonth(charvec, format = "\%Y-\%m-\%d", 
  zone = myFinCenter, FinCenter = myFinCenter)
timeLastDayInMonth(charvec)
timeLastDayInMonth(charvec, FinCenter = "Zurich")
   
## What date has the first day in a month for a given date?
timeFirstDayInMonth(charvec)
    
## What date has the last day in a quarter for a given date?
timeLastDayInQuarter(charvec)
    
## What date has the first day in a quarter for a given date?
timeFirstDayInQuarter(charvec)
}

\keyword{chron}
