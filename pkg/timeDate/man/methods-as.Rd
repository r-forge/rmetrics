\name{coerceToOther}
\alias{coerceToOther}

\alias{as.character.timeDate}
\alias{as.double.timeDate}
\alias{as.list.timeDate}
\alias{as.data.frame.timeDate}
\alias{as.POSIXct.timeDate}
\alias{as.POSIXlt.timeDate}
\alias{as.Date.timeDate}

\alias{coerce,timeDate,character-method}
\alias{coerce,timeDate,numeric-method}
\alias{coerce,timeDate,data.frame-method}
\alias{coerce,timeDate,list-method}
\alias{coerce,timeDate,POSIXct-method}
\alias{coerce,timeDate,POSIXlt-method}
\alias{coerce,timeDate,Date-method}


\title{Coercion from 'timeDate' to other classes}

\description{
  Coerce and transform objects of class \code{"timeDate"}.
}


\usage{
\method{as.character}{timeDate}(x, \dots)

\method{as.double}{timeDate}(x, 
    units = c("auto", "secs", "mins", "hours", "days", "weeks"), ...)

\method{as.data.frame}{timeDate}(x, \dots)

\method{as.POSIXct}{timeDate}(x, tz = "", \dots)

\method{as.POSIXlt}{timeDate}(x, tz = "", \dots)

\method{as.Date}{timeDate}(x, method = c("trunc", "round", "next"), \dots)
}

\arguments{
  \item{x}{
    an object of class \code{"timeDate"}.
  }
  \item{units}{
    a character string denoting the date/time units in which the 
    results are desired.
  }
  \item{tz}{
    inputs the time zone to POSIX objects, i.e. the time zone, 
    \code{zone}, or financial center string, \code{FinCenter}, 
    as used by \code{"timeDate"} objects.
  }
  \item{method}{
    a character string denoting the method how to determine the
    dates.
  }    
  \item{\dots}{
    arguments passed to other methods.
  }
}

\value{
  an object from the designated target class
}

\seealso{
  
  \code{\link{timeDate}} and \code{\link{as.timeDate}}
  for creation of and conversion to \code{"timeDate"} objects

}
\examples{
## timeDate
tC = timeCalendar()

## convert 'timeDate' to a character vector
as.character(tC)
}

\keyword{chron}
