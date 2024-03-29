\name{isWeekday}
\alias{isWeekday}

\alias{isWeekend}


\title{Weekdays and weekends}

\description{
  Tests if a date is a weekday or not.
}


\usage{
isWeekday(x, wday = 1:5)
isWeekend(x, wday = 1:5)
}

\arguments{
  \item{x}{
    an object of class \code{"timeDate"}.
  }
  \item{wday}{
    Specify which days should be considered as weekdays. By default
    from Mondays to Fridays.
  }
}

\value{
  a logical vector indicating if a date is a weekday or a weekend day
}

\examples{
## dates in april, current year
currentYear = getRmetricsOptions("currentYear")
tS = timeSequence(
   from = paste(currentYear, "-03-01", sep = ""),
   to = paste(currentYear, "-04-30", sep = ""))
tS

## subset of weekends
isWeekend(tS)
tS[isWeekend(tS)]
}

\keyword{chron}
