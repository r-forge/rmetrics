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
\method{round}{timeDate}(x, digits = c("days", "hours", "mins", "secs", "months", "years"))

\method{trunc}{timeDate}(x, units = c("days", "hours", "mins", "secs", "months", "years"),
    \dots)
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

  From \sQuote{timeDate} version \eqn{>} 4041.110, the units of
  rounding are the same as those for \code{round.POSIXt} and
  \code{trunc.POSIXt}. Note though that the default for the
  \sQuote{timeDate} methods is \code{"days"}, not \code{"secs"}.
  
}

\value{
  an object of class \code{"timeDate"}
}

\seealso{
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
# create a timeDate object
dts <- c("1989-09-28", "2001-01-15", "2004-08-30", "1990-02-09")
tms <- c(  "23:12:55.13",   "10:34:02.23",   "08:30:00.33",   "11:18:23.53")
td <- timeDate(paste(dts, tms), format = "\%Y-\%m-\%d \%H:\%M:\%S",
               zone = "GMT", FinCenter = "GMT")

## round
round(td) # same as round(td, "days")

round(td, "secs")
round(td, "mins")
round(td, "hours")
round(td, "days")
round(td, "months")
round(td, "years")


## truncate
trunc(td) # same as trunc(td, "days")

trunc(td, "secs")
trunc(td, "mins")
trunc(td, "hours")
trunc(td, "days")
trunc(td, "months")
trunc(td, "years")
}

\keyword{chron}
