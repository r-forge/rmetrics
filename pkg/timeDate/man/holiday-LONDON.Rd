\name{holidayLONDON}
\alias{holidayLONDON}


\title{London Bank Holidays}

\description{
  Returns bank holidays in London.
}

\usage{
holidayLONDON(year = getRmetricsOptions("currentYear"))
}

\arguments{

  \item{year}{
    an integer value or vector of years, formatted as \code{YYYY}.
  }

}


\value{
  an object of class \code{"timeDate"}.
}

\details{

  There are 8 bank holidays in Britain every year: New Year's
  Day, Good Friday, Easter Monday, Spring (May), Last Monday of May, End
  of Summer (Last Monday) August, Christmas Eve, Christmas Day.

}

\author{
  Function contributed by Menon Murali,
  amended by Georgi N. Boshnakov
}

\examples{
## holidayLONDON -
   holidayLONDON()
   holidayLONDON(2008:2010)
}

\keyword{chron}
