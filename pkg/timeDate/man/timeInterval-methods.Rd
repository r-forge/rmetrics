\name{timeInterval-methods}
\docType{methods}
\alias{timeInterval-methods}

\alias{timeInterval}

\alias{timeInterval,timeDate,timeDate-method}
\alias{timeInterval,ANY,ANY-method}
\alias{timeInterval,timeDate,missing-method}
\alias{timeInterval,Date,missing-method}
\alias{timeInterval,POSIXt,missing-method}
\alias{timeInterval,missing,Date-method}
\alias{timeInterval,missing,POSIXt-method}
\alias{timeInterval,missing,timeDate-method}
\alias{timeInterval,timeInterval,missing-method}


\concept{time interval}
\concept{union of time intervals}


\title{Create 'timeInterval' objects}

\description{

  Create objects from class \code{"timeInterval"}.

}

\usage{
timeInterval(left, right, ...)

\S4method{timeInterval}{timeDate,timeDate}(left, right,...)
\S4method{timeInterval}{ANY,ANY}(left, right, ...)             
\S4method{timeInterval}{timeInterval,missing}(left,right,...)

\S4method{timeInterval}{timeDate,missing}(left, right, units = "days", ...)    
\S4method{timeInterval}{Date,missing}(left,right, units = "days", ...)        
\S4method{timeInterval}{POSIXt,missing}(left,right, units = "days", ...)      
\S4method{timeInterval}{missing,Date}(left,right, units = "days", ...)        
\S4method{timeInterval}{missing,POSIXt}(left,right, units = "days", ...)      
\S4method{timeInterval}{missing,timeDate}(left,right, units = "days", ...)    
}

\arguments{
  \item{left,right}{

    left and right sides of the intervals, typically \code{"timeDate"}
    objects of equal length.  See individual methods for other
    possibilities.
    
  }
  \item{units}{

    a character string specifying unit of time. Used only when
    \code{left} or \code{right} is missing, see section
    \sQuote{Details}.

  }
  
  \item{\dots}{
    further arguments for methods.
  }
}

\details{

  If both, \code{left} and \code{right}, are supplied they represent
  the edges of the intervals.

  If only \code{left} is specified, each value represents the rounded
  time interval corresponding to argument \code{units}. For example,
  2025-10-15 15:25:31 represents the interval \code{[2025-10-15 15:00,
  2025-10-15 16:00)} when \code{units = "mins"} and \code{[2025-10-15
  00:00, 2025-10-16 00:00)} when \code{units = "days"}.

  See \code{\link[=trunc.timeDate]{trunc}} for admissible values of
  \code{units} and other details of the truncation.

  Analogously, if only \code{right} is specified, its truncated value is
  used for the right edges of the intervals, while the left edges are
  obtained by subtracting one time unit from the right edges. In other
  word, each value in \code{right} represents the rounded time interval
  corresponding to the previous time unit.  For example, 2025-10-15
  15:25:31 represents the interval \code{[2025-10-15 14:00, 2025-10-15
  15:00)} when \code{units = "mins"} and \code{[2025-10-14 00:00,
  2025-10-15 00:00)} when \code{units = "days"}, see also
  \code{\link[=trunc.timeDate]{trunc}}.

}

\value{
  a \code{"timeInterval"} object
}

\author{Georgi N. Boshnakov}

\section{Methods}{
  \describe{

    \item{\code{signature(left = "timeDate", right = "timeDate")}}{
      
      creates a \code{"timeInterval"} object as the union of intervals
      represented by \code{[left[i],right[i])}. The union is transformed
      to a canonical form with no overlaps between the intervals and no
      touching edges, see class \code{"\linkS4class{timeInterval}"}. So
      the order and the number of intervals in the returned object may
      not be as in the input arguments.
      
    }

    \item{\code{signature(left = "ANY", right = "ANY")}}{

      the default method; the time interval is created from
      \code{"timeDate"} objects created from \code{left} and
      \code{right} using calls to \code{timeDate}. All \code{"..."}
      arguments are passed in both calls to \code{timeDate}.
	
    }

    \item{\code{signature(left = "timeDate", right = "missing")}}{
      %%  ~~describe this method here~~
    }
    \item{\code{signature(left = "Date", right = "missing")}}{
      %%  ~~describe this method here~~
    }
    \item{\code{signature(left = "POSIXt", right = "missing")}}{
      
      convert each element of \code{left} into an interval containing it
      and form the union. The left edge of each interval is
      \code{trunc(left, units)} and with corresponding right end
      \code{timeCeiling(left, units)}. If a date-time is already
      rounded to \code{units}, then it is taken as the left end of the
      interval and its right end is the next rounded value.
    
    }

    \item{\code{signature(left = "missing", right = "timeDate")}}{
    }
    \item{\code{signature(left = "missing", right = "Date")}}{
    }
    \item{\code{signature(left = "missing", right = "POSIXt")}}{

      a time interval, aligned at \code{unit}, just to the left of the
      given date-times. For example, for \code{unit = "days"}, the
      result is the day before.
      
    }

    \item{\code{signature(left = "timeInterval", right = "missing")}}{
      %%  ~~describe this method here~~
    }
  }
}

\seealso{
  class \code{"\linkS4class{timeInterval}"},

  \code{\link{in_int}} for set operations on \code{"timeInterval"} objects
}

\examples{
timeInterval(left = "2024-12-20", right = "2024-12-21")
timeInterval(left = "2024-12-20 10:00", right = "2024-12-21 11:00")

timeInterval(left = c("2024-12-20 10:00", "2024-12-22 10:00"),
             right = c("2024-12-21 11:00", "2024-12-23 11:00"))

## overlapping or touching intervals are combined
timeInterval(left = c("2024-12-20 10:00", "2024-12-21 10:00"),
             right = c("2024-12-21 11:00", "2024-12-23 11:00"))
timeInterval(left = c("2024-12-20 10:00", "2024-12-21 08:00"),
             right = c("2024-12-21 11:00", "2024-12-23 11:00"))

## create timeInterval by rounding down and up  times inside the desired unit
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
