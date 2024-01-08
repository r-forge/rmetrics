\name{earlyCloseNYSE}
\alias{earlyCloseNYSE}


\title{Early closings of the New York Stock exchange}

\description{

  Get dates of early closings of the New York Stock exchange (NYSE).

}

\usage{
earlyCloseNYSE(year)
}

\arguments{
  \item{year}{
    a vector of integers representing years (4 digits).
  }
}
\details{

  \code{earlyCloseNYSE} gives the dates and times when NYSE was closed
  early. Some of these closing are scheduled (e.g. at 1pm on the day
  before or after a holiday), others are unscheduled.

  The information is incomplete, particularly after 2011. For those
  dates the values are computed using explicitly declared rules or, if
  not available, ones derived from recent years.
  
}
\value{
  a \code{"timeDate"} object containing the dates (with closing times)
  of early closings
}
\references{
  https://archive.fo/XecDq
}
\author{
  Georgi N. Boshnakov
}
\note{
  The function is somewhat experimental but the type of the result will
  not change. 
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  \code{\link{holidayNYSE}} for a list of NYSE holidays
}

\examples{
earlyCloseNYSE(1990)

earlyCloseNYSE(2022:2024) # early closings
holidayNYSE(2022:2024)    # holidays
## early closings & holidays combined
c(earlyCloseNYSE(2022:2024), holidayNYSE(2022:2024))
}

\keyword{chron}
