\name{timeCeiling}
\alias{timeCeiling}
\alias{timeCeiling.POSIXt}
\alias{timeCeiling.timeDate}

\title{Ceiling (round up) for data-time objects}

\description{

  Round up a data-time object to the next time unit (day, mhour, month,
  etc.).
  
}

\usage{
timeCeiling(x, \dots)

\method{timeCeiling}{POSIXt}(x, 
    units = c("days", "hours", "mins", "secs", "months", "years"),
    \dots)

\method{timeCeiling}{timeDate}(x, 
    units = c("days", "hours", "mins", "secs", "months", "years"), 
    \dots)
}
\arguments{
  \item{x}{
    an object representing date-time.
  }
  \item{units}{
    a character string, one of the supported units of time.
  }
  \item{\dots}{
    further arguments for methods.
  }
}
\details{

  \code{timeCeiling} rounds up to the start of the next time unit, as
  specified by argument \code{units}.

}
\value{
  for the \code{"timeDate"} method, a \code{"timeDate"} object,
  
  for the \code{"POSIXt"} method, a \code{"POSIXlt"} object
}
\author{
  Georgi N. Boshnakov
}
\note{

  \code{base::ceiling} is generic but time methods cannot be defined for
  it since it has only one argument. The same holds for its relative
  \code{base::floor}.

  On the other hand, \code{base::trunc} and \code{base::round} accept
  further arguments and therefore \code{"timeDate"} methods are defined
  for them. In fact the method for \code{trunc} plays the role of
  \code{floor}.
  
}

\seealso{
  \code{\link{trunc.timeDate}}, \code{\link{trunc.POSIXt}},
  
  \code{\link{round.timeDate}}, \code{\link{round.POSIXt}},
}

\examples{
# create a timeDate object
dts <- c("1989-09-28", "2001-01-15", "2004-08-30", "1990-02-09")
tms <- c(  "23:12:55.13",   "10:34:02.23",   "08:30:00.33",   "11:18:23.53")
td <- timeDate(paste(dts, tms), format = "\%Y-\%m-\%d \%H:\%M:\%S",
               zone = "GMT", FinCenter = "GMT")

## timeCeiling
timeCeiling(td) # same as timeCeiling(td, "days")

timeCeiling(td, "secs")
timeCeiling(td, "mins")
timeCeiling(td, "hours")
timeCeiling(td, "days")
timeCeiling(td, "months")
timeCeiling(td, "years")

## rounding with "days" usually sets the time to midnight (the start of a day),
## but it may not exist
Sofia_to_DST_char <- c("1983-03-26 23:00:00",
    "1983-03-27 00:00:00", # change to DST; 0am doesn't exist in Sofia on this date
    "1983-03-27 01:00:00",
    "1983-03-27 02:00:00",
    "1983-03-27 03:00:00")

Sofia_to_DST <- timeDate(Sofia_to_DST_char, zone = "Sofia", FinCenter = "Sofia")
cbind(Sofia_to_DST_char, format(Sofia_to_DST@Data))

## on 27/03/1983 in Sofia the clock jumped at midnight to 1am, so the
## day started at 1am - hence the first time below.  the rest are on the
## next day, which starts at midnight.
timeCeiling(Sofia_to_DST) # same as timeCeiling(Sofia_to_DST, "days")

trunc(Sofia_to_DST)

## to avoid confusion, set non-existent times to NA
Sofia_to_DSTa <- timeDate(Sofia_to_DST_char, zone = "Sofia", FinCenter = "Sofia",
                          dst_gap = "NA")
Sofia_to_DSTa
timeCeiling(Sofia_to_DSTa)
}

\keyword{chron}

