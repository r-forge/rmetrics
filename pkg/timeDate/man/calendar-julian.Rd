\name{julian}
\alias{julian}

\alias{julian.timeDate}

\alias{atoms}
\alias{atoms,ANY-method}
\alias{atoms,timeDate-method}

\alias{months}
\alias{months.timeDate}

\alias{weekdays}
\alias{weekdays.timeDate}

\alias{quarters}
\alias{quarters.timeDate}

\alias{$,timeDate-method}


\concept{Julian day}
\concept{Julian date}


\title{Julian counts and calendar atoms}

\description{
    
  Returns Julian day counts, date/time atoms from a \code{"timeDate"} object,
  and extracts month atoms from a \code{"timeDate"} object.
       
}


\usage{
\method{julian}{timeDate}(x, origin = timeDate("1970-01-01"), 
       units = c("auto", "secs", "mins", "hours", "days", "weeks"), 
       zone = NULL, FinCenter = NULL, \dots)

\S4method{atoms}{timeDate}(x, \dots)

\method{months}{timeDate}(x, abbreviate = FALSE)

\method{weekdays}{timeDate}(x, abbreviate = FALSE)

\method{quarters}{timeDate}(x, abbreviate)

\S4method{$}{timeDate}(x, name)
}


\arguments{
  
  \item{x}{
    an object of class \code{"timeDate"}.
  }
  \item{origin}{
    a length-one object inheriting from class \code{"timeDate"}
    setting the origin for the julian counter.
  }
  \item{units}{
    a character string denoting the date/time units in which the 
    results are desired.
  }
  \item{zone}{
    the time zone or financial center where the data were recorded.
  }
  \item{FinCenter}{
    a character string with the location of the  
    financial center named as "continent/city". 
  }   
  \item{abbreviate}{
    currently not used.
  }
  \item{name}{
    
    one of \code{year}, \code{month}, \code{day}, \code{hour},
    \code{minute}, \code{second}, \code{wday} (or \code{weekday}),
    \code{wday0} (or \code{weekday0}), and
    \code{quarter}. Completion is available in interactive sessions.

  }
  \item{\dots}{
    arguments passed to other methods.
  }
}

\details{

  Generic functions to extract properties of \code{"timeDate"}
  objects. \code{julian} and \code{months} are generics from base R,
  while \code{atoms} is a generic defined in this package.

  \code{julian} extracts the number of days since \code{origin} (can be
  fractional), see also \code{\link[base]{julian}}.
  
  \code{atoms} extracts the calendar atoms from a \code{"timeDate"}
  object, i.e., the year, month, day, and optionally, hour, minute and
  second.  The result is a data frame with the financial center in
  atrribute \code{"control"}.
  
  \code{months} extracts the months, see section \sQuote{Note}.

  The dollar operator applied to a \code{"timeDate"} object,
  e.g. \code{td$name}, extracts a component of the date/time values as a
  numeric vector. Currently, \code{name} can be one of \code{year},
  \code{month}, \code{day}, \code{hour}, \code{minute}, \code{second},
  \code{wday} (or \code{weekday}), \code{wday0} (or \code{weekday0}),
  and \code{quarter}.  \code{wday0} (\code{weekday0}) starts with 0 (for
  Sunday), the meaning of the rest should be clear.

  In interactive sessions, completion is available for the dollar
  operator.

}

\value{
  
  for \code{julian}, a \code{difftime} object;
    
  for \code{atoms}, a \code{data.frame} with attribute \code{"control"}
  containing the financial center of the input vector \code{x}.  The
  data frame has the following components:

  \item{Y}{year,}
  \item{m}{month,}
  \item{d}{day,}
  \item{H}{hour,}
  \item{M}{minute,}
  \item{S}{second;}

  for \code{months}, a numeric vector with attribute \code{"control"}
  containing the financial center. (\strong{Note:} this use is
  deprecated, use \code{$month} instead.)

  for the dollar method, the corresponding component as numeric vector.
    
}
\note{

  \strong{Deprecation Warning:} a `timeDate` method for `months` has
  existed for a long time but it was returning a numeric vector, which
  is inconsistent with the other methods for months in base R (they
  return names of months). Returning a numeric vector when 'abbreviate'
  is missing is a temporary compromise, to avoid breaking old code but
  this should be considered deprecated. Use `td$month` to get the
  numbers.
  
}
\seealso{
  \code{\link{dayOfWeek}},
  \code{\link{dayOfYear}};

  the base R functions
  \code{\link[base]{julian}},
  \code{\link[base]{difftime}},
  \code{\link[base]{months}};
}

\examples{
## julian
tC = timeCalendar(2022)
julian(tC)[1:3]
   
## atoms
atoms(tC)
   
## months
months(tC)

weekdays(tC)
weekdays(tC, TRUE)

## the dollar method
tC$year
tC$month
tC$day
tC$hour
tC$minute
tC$second
tC$weekday
tC$weekday0

tC$quarter
}

\keyword{chron}
