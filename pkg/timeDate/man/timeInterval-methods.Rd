\name{timeInterval-methods}
\docType{methods}
\alias{timeInterval-methods}

\alias{timeInterval,ANY,ANY-method}
\alias{timeInterval,Date,missing-method}
\alias{timeInterval,missing,Date-method}
\alias{timeInterval,missing,POSIXt-method}
\alias{timeInterval,missing,timeDate-method}
\alias{timeInterval,POSIXt,missing-method}
\alias{timeInterval,timeDate,missing-method}
\alias{timeInterval,timeDate,timeDate-method}
\alias{timeInterval,timeInterval,missing-method}

\alias{timeInterval}

\concept{time interval}
\concept{union of time intervals}


\title{Create 'timeInterval' objects}

\description{

  Create objects from class \code{"timeInterval"}.

}

\usage{
timeInterval(left, right, ...)
}

\arguments{
  \item{left,right}{

    left and right sides of the intervals, typically \code{"timeDate"}
    objects of equal length. See individual methods for other
    possibilities.
    
  }
  \item{\dots}{
    further arguments for methods.
  }
}

\section{Methods}{
  \describe{

    \item{\code{signature(left = "ANY", right = "ANY")}}{

      the default method; the time interval is created from
      \code{"timeDate"} objects created from \code{left} and
      \code{right} using calls to \code{timeDate}. All \code{"..."}
      arguments are passed in both calls to \code{timeDate}.
	
    }

    \item{\code{signature(left = "Date", right = "missing")}}{
      
      convert each element of \code{left} into an interval containing
      it. The left edge of the interval is \code{trunc(left, units)} and
      its right end is \code{timeCeiling(left, units)}. If the date-time
      is already rounded to \code{units}, then it is taken as the left
      end of the interval and its right end is the next rounded value.
    
    }

    \item{\code{signature(left = "missing", right = "Date")}}{

      a time interval, aligned at \code{unit}, just to the left of the
      gien date-times. For \code{unit = "days"}, the result is the day
      before.
      
    }

    \item{\code{signature(left = "missing", right = "POSIXt")}}{
      %%  ~~describe this method here~~
    }

    \item{\code{signature(left = "missing", right = "timeDate")}}{
      %%  ~~describe this method here~~
    }

    \item{\code{signature(left = "POSIXt", right = "missing")}}{
      %%  ~~describe this method here~~
    }

    \item{\code{signature(left = "timeDate", right = "missing")}}{
      %%  ~~describe this method here~~
    }

    \item{\code{signature(left = "timeDate", right = "timeDate")}}{

      
    }

    \item{\code{signature(left = "timeInterval", right = "missing")}}{
      %%  ~~describe this method here~~
    }
  }
}

\seealso{
  \code{"\linkS4class{timeInterval}"}
}

\examples{
timeInterval(as.Date("2024-12-20"))
timeInterval(timeDate("2024-12-20 10:20:30"), unit = "days")
timeInterval(right = timeDate("2024-12-20 10:20:30"), unit = "days")

timeInterval(timeDate("2024-12-20 10:20:30"), unit = "hours")
timeInterval(right = timeDate("2024-12-20 10:20:30"), unit = "hours")

timeInterval(timeDate("2024-12-20 10:20:30"), unit = "mins")
timeInterval(right = timeDate("2024-12-20 10:20:30"), unit = "mins")
}

\keyword{methods}
\keyword{chron}
