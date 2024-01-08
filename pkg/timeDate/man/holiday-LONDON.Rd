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

  There are currently 8 bank holidays in Britain every year: New Year's
  Day, Good Friday, Easter Monday, Early Spring Holiday (first Monday of
  May), Spring Holiday (Last Monday of May), Summer Holiday (Last
  Monday of August), Christmas Day and Boxing Day.

  Some of these holidays are referred also by alternative names or may
  have had other names in the past. Also the rules according to which
  the dates for some of them are calculated have changed over time.

  Occasionaly there are one-off special holidays, usually related to
  significant Royal events. Also as one-off, the dates of some holidays
  are sometimes moved. For example, the Early spring holiday was moved
  several times to 8th May to coincide with Victory day on big
  anniversaries.

}

\author{
  Original function contributed by Menon Murali;
  amended, corrected and rewritten by Georgi N. Boshnakov
}

\examples{
## holidayLONDON
holidayLONDON()
holidayLONDON(2008:2010)
}

\keyword{chron}
